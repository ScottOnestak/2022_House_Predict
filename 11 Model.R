#2022 Election Project
#Author: Scott Onestak
#11 Model

#libraries
library(dplyr)
library(tidyr)
library(stringr)
library(h2o)

memory.limit(size=10000)
options(scipen=999)

#set current data
#theDate = Sys.Date()
theDate = as.Date("2022-10-26","%Y-%m-%d")

#read in data
train = read.csv("Data/Datasets/train.csv",header=T,stringsAsFactors=F)
test_base = read.csv("Data/Datasets/test_base.csv",header=T,stringsAsFactors=F)

gb_prev = read.csv("Data/Results/Generic_Ballot.csv",header=T,stringsAsFactors=F)
expected_seat_prev = read.csv("Data/Results/Expected_Seats.csv",header=T,stringsAsFactors=F)
majority_prev = read.csv("Data/Results/Majority.csv",header=T,stringsAsFactors=F)
gb_prev$date = as.Date(gb_prev$date,"%Y-%m-%d")
expected_seat_prev$date = as.Date(expected_seat_prev$date,"%Y-%m-%d")
majority_prev$date = as.Date(majority_prev$date,"%Y-%m-%d")

state_to_fips = read.csv("Data/State_to_FIPS.csv",header=T,stringsAsFactors=F)

pollster_bias = read.csv('Data/Polls/For Pollster Ratings/forModeling/pollster_bias.csv',header=T,stringsAsFactors=F)
partisan_bias = read.csv('Data/Polls/For Pollster Ratings/forModeling/partisan_bias.csv',header=T,stringsAsFactors=F)
avg_bias = read.csv('Data/Polls/For Pollster Ratings/forModeling/avg_bias.csv',header=T,stringsAsFactors=F)
colnames(pollster_bias)[1] = "cycle"
colnames(partisan_bias)[1] = "cycle"
colnames(avg_bias)[1] = "cycle"

election_results_raw = read.csv('Data/Election Results/Cleaned Results/votes_cleaned_data.csv',header=T,stringsAsFactors=F) %>%
                              filter(year >= 2018 & runoff == FALSE)

leans = read.csv('Data/Historical Leans/Leans.csv',header=T,stringsAsFactors=F)
colnames(leans)[1] = "year"

gb_acts = read.csv('Data/Polls/Generic Ballot/generic_ballot_actuals.csv',header=T,stringsAsFactors=F)
colnames(gb_acts) = c("cycle","GOP_act","DEM_act","diff_act")

fec2022 = read.delim("Data/FEC Data/fec2022.txt",header=F,sep="|",stringsAsFactors=F,quote="",fill=FALSE) %>% mutate(year=2022)
colnames(fec2022) = c("FEC_ID","CAND_NAME","CAND_ICI","PTY_CD","CAND_PTY_AFFILIATION","TOTAL_RECEIPTS","TRANS_FROM_AUTH",
                      "TOTAL_DISBURSEMENTS","TRANS_TO_AUTH_CMTE","COH_BOP","COH_COP","CAND_CONTRIB","CAND_LOANS","OTH_LOANS",
                      "CAND_LOAN_REPAY","OTHER_LOAN_REPAY","DEBTS_OWED_BY","TOTAL_INDIVIDUAL_CONTRIBUTIONS","STATE","DISTRICT",
                      "SPECIAL_ELECTION","PRIMARY_ELECTION","RUNOFF_ELECTION","GENERAL_ELECTION","GEN_ELECTION_PRCT",
                      "OTH_POL_CMTE_CONTRIB","PARTY_CONTRIB","CVG_END_DT","INDIV_REFUNDS","CMTE_REFUNDS","year")


gb_polls_2022 = read.csv("Data/Polls/2022 Polls/generic_ballot_polls.csv") %>%
                      select(pollster_rating_name,start_date,end_date,sample_size,population,partisan,cycle,dem,rep) %>%
                      filter(population %in% c("lv","rv") & pollster_rating_name != "Center Street PAC" & !is.na(sample_size)) %>%
                      rename("pollster" = "pollster_rating_name") %>%
                      mutate(partisan = ifelse(partisan=="REP","R",
                                               ifelse(partisan=="DEM","D",partisan)))
gb_polls_2022$start_date = as.Date(gb_polls_2022$start_date,"%m/%d/%y")
gb_polls_2022$end_date = as.Date(gb_polls_2022$end_date,"%m/%d/%y")
gb_polls_2022$mid_date = NA
gb_polls_2022$days_to_today = NA
for(i in seq(from=1,to=dim(gb_polls_2022)[1],by=1)){
  gb_polls_2022[i,"mid_date"] = mean.Date(c(gb_polls_2022[i,"start_date"],gb_polls_2022[i,"end_date"]))
  gb_polls_2022[i,"days_to_today"] = as.numeric(theDate - gb_polls_2022[i,"mid_date"])
}
gb_polls_2022 = gb_polls_2022 %>% filter(end_date <= theDate)

house_polls_2022 = read.csv("Data/Polls/2022 Polls/house_polls.csv") %>%
                      select(pollster_rating_name,start_date,end_date,election_date,sample_size,population,partisan,cycle,state,seat_number,stage,ranked_choice_reallocated,question_id,party,pct) %>%
                      filter(population %in% c("lv","rv") & election_date == "11/8/22" & stage %in% c("general","jungle primary") & pollster_rating_name != "Center Street PAC" & !is.na(sample_size)) %>%
                      rename("pollster" = "pollster_rating_name") %>%
                      mutate(partisan = ifelse(partisan=="REP","R",
                                               ifelse(partisan=="DEM","D",partisan)))
house_polls_2022$start_date = as.Date(house_polls_2022$start_date,"%m/%d/%y")
house_polls_2022$end_date = as.Date(house_polls_2022$end_date,"%m/%d/%y")
house_polls_2022$election_date = as.Date(house_polls_2022$election_date,"%m/%d/%y")
house_polls_2022$mid_date = NA
house_polls_2022$days_to_election = NA
for(i in seq(from=1,to=dim(house_polls_2022)[1],by=1)){
  house_polls_2022[i,"mid_date"] = mean.Date(c(house_polls_2022[i,"start_date"],house_polls_2022[i,"end_date"]))
  house_polls_2022[i,"days_to_election"] = as.numeric(house_polls_2022[i,"election_date"] - house_polls_2022[i,"mid_date"])
}
house_polls_2022 = house_polls_2022 %>% filter(end_date <= theDate)


#Build generic ballot
na_replace_val = quantile(unlist(pollster_bias %>% filter(cycle == 2022) %>% select(adv_score)),1)
gb_temp = gb_polls_2022 %>% filter(days_to_today >= 0) %>% 
                            mutate(pollster = ifelse(pollster=="Big Village/Opinion Research Corporation","CNN/Opinion Research Corp.",
                                                     ifelse(pollster=="Global Strategy Group/GBAO (Navigator Research)","Global Strategy Group",pollster))) %>% #update for FiveThirtyEight display name changes
                            arrange(desc(mid_date),pollster,population) %>%
                            group_by(pollster) %>%
                            filter(row_number()==1) %>%
                            left_join(.,pollster_bias %>% select(-count),by=c("cycle","pollster")) %>%
                            left_join(.,partisan_bias,by=c("cycle","partisan")) %>%
                            mutate(adjustment = ifelse(population == "rv",
                                                       ifelse(is.na(POLLSTER_BIAS),PARTISAN_BIAS-1.5,PARTISAN_BIAS+POLLSTER_BIAS-1.5),
                                                       ifelse(is.na(POLLSTER_BIAS),PARTISAN_BIAS,PARTISAN_BIAS+POLLSTER_BIAS))) %>%
                            mutate(DEM_adj = dem + .5*adjustment,
                                   GOP_adj = rep - .5*adjustment,
                                   adv_score = ifelse(is.na(adv_score),na_replace_val,adv_score)) %>%
                            mutate(sample_size_score = sqrt(sample_size/600),
                                   pollster_adv_score = ((adv_score - min(pollster_bias$adv_score)) / (max(pollster_bias$adv_score) - min(pollster_bias$adv_score)) - 1) * -1 + 0.5,
                                   time_score = exp(-0.1 * days_to_today)) %>%
                            mutate(theWeight = time_score * sample_size_score * pollster_adv_score)

gb_mean = gb_temp %>% group_by(cycle) %>%
                      summarise(DEM_GB_EST = round(weighted.mean(DEM_adj,theWeight),2),
                                GOP_GB_EST = round(weighted.mean(GOP_adj,theWeight),2))

gb_sd = gb_temp %>% mutate(DEM_SD = sqrt(((dem/100)*(1-dem/100))/sample_size)*100,
                           GOP_SD = sqrt(((rep/100)*(1-rep/100))/sample_size)*100) %>%
                    select(pollster,cycle,start_date,end_date,mid_date,sample_size,theWeight,DEM_adj,GOP_adj,DEM_SD,GOP_SD) %>%
                    left_join(.,gb_mean %>% rename("DEM_X" = "DEM_GB_EST", "GOP_X" = "GOP_GB_EST"),by="cycle") %>%
                    mutate(DEM_num = theWeight * (DEM_SD^2 + (DEM_adj - DEM_X)^2),
                           GOP_num = theWeight * (GOP_SD^2 + (GOP_adj - GOP_X)^2)) %>%
                    group_by(cycle) %>%
                    summarise(DEM_num = sum(DEM_num),
                              GOP_num = sum(GOP_num),
                              theWeight = sum(theWeight)) %>%
                    mutate(DEM_GB_SD = round(sqrt(DEM_num/theWeight),2),
                           GOP_GB_SD = round(sqrt(GOP_num/theWeight),2)) %>%
                    select(cycle,DEM_GB_SD,GOP_GB_SD)

gb_est = gb_mean %>% left_join(.,gb_sd,by="cycle")


#Build house polling estimates

##first handle RCV states AK (ME also has RCV, but no polls with ranked allocation)
ak = house_polls_2022 %>% filter(state %in% c("Alaska") & ranked_choice_reallocated == "true")
ak_sel = ak %>% group_by(pollster,start_date,end_date,mid_date,days_to_election,state,seat_number,population,question_id) %>% 
                summarise(count=n()) 
ak_selection = ak_sel %>% group_by(pollster,start_date,end_date,mid_date,days_to_election,state,seat_number,population) %>%
                          filter(count == min(count)) %>%
                          distinct()
alaska = ak %>% inner_join(.,ak_selection %>% select(-count),by=c("pollster","start_date","end_date","mid_date","days_to_election","state","seat_number","population","question_id")) %>%
                spread(.,key="party",value="pct") %>%
                select(pollster,start_date,end_date,mid_date,days_to_election,election_date,sample_size,population,
                       partisan,cycle,state,seat_number,mid_date,days_to_election,DEM,REP)

##now handle all others
oth = house_polls_2022 %>% filter(!state %in% c("Alaska") & ranked_choice_reallocated == "false" & 
                                   party %in% c("DEM","REP") & pollster != "Spry Strategies") %>% #Spry Strategies was only polling R v R in WA-04 (It's R v D)
                           spread(.,key="party",value="pct") %>%
                           select(pollster,start_date,end_date,mid_date,days_to_election,election_date,sample_size,
                                  population,partisan,cycle,state,seat_number,mid_date,days_to_election,DEM,REP) %>%
                           filter(!is.na(DEM) & !is.na(REP)) #The only NAs are ND, where the DEM withdrew, so it's a default win for GOP

house_polls_2022_cleaned = rbind(oth,alaska) %>% rename("Name" = "state") %>%
                                                 left_join(.,state_to_fips %>% select(Name,State),by="Name") %>%
                                                 mutate(District = paste(State,"-",str_pad(seat_number,2,pad="0"),sep="")) %>%
                                                 select(-c("State","Name","seat_number"))

house_temp = house_polls_2022_cleaned %>% filter(DEM + REP >= 75 & !is.na(sample_size) & days_to_election <= 125 & days_to_election > 0) %>% 
                                          arrange(cycle,District,pollster,days_to_election,population) %>%
                                          group_by(cycle,District,pollster,population) %>%
                                          filter(row_number()==1) %>%
                                          left_join(.,pollster_bias %>% select(-count),by=c("cycle","pollster")) %>%
                                          left_join(.,partisan_bias,by=c("cycle","partisan")) %>%
                                          mutate(adjustment = ifelse(population == "rv",
                                                                     ifelse(is.na(POLLSTER_BIAS),PARTISAN_BIAS-1.5,PARTISAN_BIAS+POLLSTER_BIAS-1.5),
                                                                     ifelse(is.na(POLLSTER_BIAS),PARTISAN_BIAS,PARTISAN_BIAS+POLLSTER_BIAS))) %>%
                                          mutate(DEM_adj = DEM + .5*adjustment,
                                                 GOP_adj = REP - .5*adjustment,
                                                 adv_score = ifelse(is.na(adv_score),na_replace_val,adv_score)) %>%
                                          mutate(sample_size_score = sqrt(sample_size/600),
                                                 pollster_adv_score = ((adv_score - min(pollster_bias$adv_score)) / (max(pollster_bias$adv_score) - min(pollster_bias$adv_score)) - 1) * -1 + 0.5,
                                                 time_score = exp(-0.01 * days_to_election),
                                                 penalty = ifelse(partisan == "",1,1-0.2)) %>%
                                          mutate(theWeight = time_score * sample_size_score * pollster_adv_score * penalty)

house_mean = house_temp %>% group_by(cycle,District) %>%
                            summarise(DEM_POLL_EST = round(weighted.mean(DEM_adj,theWeight),2),
                                      GOP_POLL_EST = round(weighted.mean(GOP_adj,theWeight),2),
                                      total_weight = sum(theWeight)) %>%
                            ungroup()

house_sd = house_temp %>% mutate(DEM_SD = sqrt(((DEM/100)*(1-DEM/100))/sample_size)*100,
                                 GOP_SD = sqrt(((REP/100)*(1-REP/100))/sample_size)*100) %>%
                          select(pollster,cycle,District,start_date,end_date,mid_date,sample_size,theWeight,
                                 DEM_adj,GOP_adj,DEM_SD,GOP_SD) %>%
                          left_join(.,house_mean %>% rename("DEM_X" = "DEM_POLL_EST", "GOP_X" = "GOP_POLL_EST"),by=c("cycle","District")) %>%
                          mutate(DEM_num = theWeight * (DEM_SD^2 + (DEM_adj - DEM_X)^2),
                                 GOP_num = theWeight * (GOP_SD^2 + (GOP_adj - GOP_X)^2)) %>%
                          group_by(cycle,District) %>%
                          summarise(DEM_num = sum(DEM_num),
                                    GOP_num = sum(GOP_num),
                                    theWeight = sum(theWeight)) %>%
                          mutate(DEM_POLL_SD = round(sqrt(DEM_num/theWeight),2),
                                 GOP_POLL_SD = round(sqrt(GOP_num/theWeight),2)) %>%
                          select(cycle,District,DEM_POLL_SD,GOP_POLL_SD)

house_est = house_mean %>% left_join(.,house_sd,by=c("cycle","District")) 


#Build incumbency for 2022 candidates
dem_cand = election_results_raw %>% filter(PARTY == "D") %>%
                                    group_by(year,STATE,DISTRICT) %>%
                                    summarise(count=n())

rep_cand = election_results_raw %>% filter(PARTY == "R") %>%
                                    group_by(year,STATE,DISTRICT) %>%
                                    summarise(count=n())

incumbent_build = election_results_raw %>% inner_join(.,dem_cand %>% filter(count==1) %>% select(-count),by=c("year","STATE","DISTRICT")) %>%
                                           inner_join(.,rep_cand %>% filter(count==1) %>% select(-count),by=c("year","STATE","DISTRICT")) %>%
                                           group_by(year,STATE,DISTRICT,PARTY,FEC_ID,INCUMBENT_IND) %>%
                                           summarise(VOTES = sum(candidate_votes,na.rm=T)) %>%
                                           ungroup() %>%
                                           left_join(.,election_results_raw %>% select(year,STATE,DISTRICT,total_votes) %>% distinct(),by=c("year","STATE","DISTRICT")) %>%
                                           mutate(District = paste(STATE,"-",str_pad(DISTRICT,2,pad="0"),sep=""),
                                                  prct = round(VOTES/total_votes*100,2)) 

rep_inc_build = incumbent_build %>% filter(PARTY=="R") %>%
                                    select(year,District,FEC_ID,INCUMBENT_IND,prct) %>%
                                    rename("FEC_ID_GOP"="FEC_ID",
                                           "INCUMBENT_IND_GOP"="INCUMBENT_IND",
                                           "GOP_PRCT"="prct")
dem_inc_build = incumbent_build %>% filter(PARTY=="D") %>%
                                    select(year,District,FEC_ID,INCUMBENT_IND,prct) %>%
                                    rename("FEC_ID_DEM"="FEC_ID",
                                           "INCUMBENT_IND_DEM"="INCUMBENT_IND",
                                           "DEM_PRCT"="prct")
incumbent_temp = rep_inc_build %>% inner_join(.,dem_inc_build,by=c("year","District")) %>%
                                   mutate(DIFF = GOP_PRCT - DEM_PRCT)

incumbent_data = test_base %>% filter(scenario==1) %>%
                               select(year,District,FEC_ID_GOP,INCUMBENT_IND_GOP,FEC_ID_DEM,INCUMBENT_IND_DEM) %>% 
                               filter((INCUMBENT_IND_GOP == 1 | INCUMBENT_IND_DEM == 1)) %>% 
                               mutate(year1 = year - 2,
                                      year2 = year - 4) %>%
                               left_join(.,incumbent_temp %>% mutate(year1 = year) %>%
                                           left_join(.,leans %>% select(year,District,LEAN_1),by=c("year","District")) %>%
                                                                 rename("cycle"="year") %>%
                                                                 left_join(.,gb_acts %>% select(cycle,diff_act),by="cycle") %>%
                                                                 mutate(PRED = LEAN_1 + diff_act) %>%
                                                                 mutate(DEM_COMP1 = DIFF - PRED) %>%
                                                                 select(year1,FEC_ID_DEM,DEM_COMP1) %>%
                                                                 mutate(INCUMBENT_IND_DEM = 1),
                                         by=c("year1","FEC_ID_DEM","INCUMBENT_IND_DEM")) %>%
                              left_join(.,incumbent_temp %>% mutate(year1 = year) %>%
                                          left_join(.,leans %>% select(year,District,LEAN_1),by=c("year","District")) %>%
                                          rename("cycle"="year") %>%
                                          left_join(.,gb_acts %>% select(cycle,diff_act),by="cycle") %>%
                                          mutate(PRED = LEAN_1 + diff_act) %>%
                                          mutate(GOP_COMP1 = DIFF - PRED) %>%
                                          select(year1,FEC_ID_GOP,GOP_COMP1) %>%
                                          mutate(INCUMBENT_IND_GOP = 1),
                                        by=c("year1","FEC_ID_GOP","INCUMBENT_IND_GOP")) %>%
                              left_join(.,incumbent_temp %>% mutate(year2 = year) %>%
                                          left_join(.,leans %>% select(year,District,LEAN_1),by=c("year","District")) %>%
                                          rename("cycle"="year") %>%
                                          left_join(.,gb_acts %>% select(cycle,diff_act),by="cycle") %>%
                                          mutate(PRED = LEAN_1 + diff_act) %>%
                                          mutate(DEM_COMP2 = DIFF - PRED) %>%
                                          select(year2,FEC_ID_DEM,DEM_COMP2) %>%
                                          mutate(INCUMBENT_IND_DEM = 1),
                                        by=c("year2","FEC_ID_DEM","INCUMBENT_IND_DEM")) %>%
                              left_join(.,incumbent_temp %>% mutate(year2 = year) %>%
                                          left_join(.,leans %>% select(year,District,LEAN_1),by=c("year","District")) %>%
                                          rename("cycle"="year") %>%
                                          left_join(.,gb_acts %>% select(cycle,diff_act),by="cycle") %>%
                                          mutate(PRED = LEAN_1 + diff_act) %>%
                                          mutate(GOP_COMP2 = DIFF - PRED) %>%
                                          select(year2,FEC_ID_GOP,GOP_COMP2) %>%
                                          mutate(INCUMBENT_IND_GOP = 1),
                                        by=c("year2","FEC_ID_GOP","INCUMBENT_IND_GOP")) %>%
                              mutate(COMP1 = ifelse(!is.na(DEM_COMP1) & !is.na(GOP_COMP1),(DEM_COMP1 + GOP_COMP1)/2,
                                                    ifelse(!is.na(DEM_COMP1),DEM_COMP1,
                                                           ifelse(!is.na(GOP_COMP1),GOP_COMP1,NA))),
                                     COMP2 = ifelse(!is.na(DEM_COMP2) & !is.na(GOP_COMP2),(DEM_COMP2 + GOP_COMP2)/2,
                                                    ifelse(!is.na(DEM_COMP2),DEM_COMP2,
                                                           ifelse(!is.na(GOP_COMP2),GOP_COMP2,NA)))) %>%
                              select(-c("DEM_COMP1","GOP_COMP1","DEM_COMP2","GOP_COMP2")) %>%
                              filter(!is.na(COMP1) | !is.na(COMP2)) %>% #Sometimes incumbent because nobody ran against them prior or special election... so no comparable results
                              mutate(COMP = ifelse(!is.na(COMP1),COMP1/2,COMP2/2)) %>%
                              select(year,District,COMP)


#Join everything together to create the test dataset
test = test_base %>% left_join(.,fec2022 %>% select(FEC_ID,year,TOTAL_RECEIPTS,TOTAL_DISBURSEMENTS,
                                        COH_COP,TOTAL_INDIVIDUAL_CONTRIBUTIONS,PARTY_CONTRIB,INDIV_REFUNDS) %>%
                                 rename("FEC_ID_DEM" = "FEC_ID",
                                        "TOTAL_RECEIPTS_DEM" = "TOTAL_RECEIPTS",
                                        "TOTAL_DISBURSEMENTS_DEM" = "TOTAL_DISBURSEMENTS",
                                        "COH_DEM" = "COH_COP",
                                        "INDIVIDUAL_CONTRIBUTIONS_DEM" = "TOTAL_INDIVIDUAL_CONTRIBUTIONS",
                                        "PARTY_CONTRIBUTIONS_DEM" = "PARTY_CONTRIB",
                                        "INDIVIDUAL_REFUNDS_DEM" = "INDIV_REFUNDS"),
                               by=c("year","FEC_ID_DEM")) %>%
                      left_join(.,fec2022 %>% select(FEC_ID,year,TOTAL_RECEIPTS,TOTAL_DISBURSEMENTS,
                                         COH_COP,TOTAL_INDIVIDUAL_CONTRIBUTIONS,PARTY_CONTRIB,INDIV_REFUNDS) %>%
                                  rename("FEC_ID_GOP" = "FEC_ID",
                                         "TOTAL_RECEIPTS_GOP" = "TOTAL_RECEIPTS",
                                         "TOTAL_DISBURSEMENTS_GOP" = "TOTAL_DISBURSEMENTS",
                                         "COH_GOP" = "COH_COP",
                                         "INDIVIDUAL_CONTRIBUTIONS_GOP" = "TOTAL_INDIVIDUAL_CONTRIBUTIONS",
                                         "PARTY_CONTRIBUTIONS_GOP" = "PARTY_CONTRIB",
                                         "INDIVIDUAL_REFUNDS_GOP" = "INDIV_REFUNDS"),
                                by=c("year","FEC_ID_GOP")) %>%
                      mutate(TOTAL_RECEIPTS_DEM = ifelse(is.na(TOTAL_RECEIPTS_DEM),0,TOTAL_RECEIPTS_DEM),
                             TOTAL_DISBURSEMENTS_DEM = ifelse(is.na(TOTAL_DISBURSEMENTS_DEM),0,TOTAL_DISBURSEMENTS_DEM),
                             COH_DEM = ifelse(is.na(COH_DEM),0,COH_DEM),
                             INDIVIDUAL_CONTRIBUTIONS_DEM = ifelse(is.na(INDIVIDUAL_CONTRIBUTIONS_DEM),0,INDIVIDUAL_CONTRIBUTIONS_DEM),
                             PARTY_CONTRIBUTIONS_DEM = ifelse(is.na(PARTY_CONTRIBUTIONS_DEM),0,PARTY_CONTRIBUTIONS_DEM),
                             INDIVIDUAL_REFUNDS_DEM = ifelse(is.na(INDIVIDUAL_REFUNDS_DEM),0,INDIVIDUAL_REFUNDS_DEM),
                             TOTAL_RECEIPTS_GOP = ifelse(is.na(TOTAL_RECEIPTS_GOP),0,TOTAL_RECEIPTS_GOP),
                             TOTAL_DISBURSEMENTS_GOP = ifelse(is.na(TOTAL_DISBURSEMENTS_GOP),0,TOTAL_DISBURSEMENTS_GOP),
                             COH_GOP = ifelse(is.na(COH_GOP),0,COH_GOP),
                             INDIVIDUAL_CONTRIBUTIONS_GOP = ifelse(is.na(INDIVIDUAL_CONTRIBUTIONS_GOP),0,INDIVIDUAL_CONTRIBUTIONS_GOP),
                             PARTY_CONTRIBUTIONS_GOP = ifelse(is.na(PARTY_CONTRIBUTIONS_GOP),0,PARTY_CONTRIBUTIONS_GOP),
                             INDIVIDUAL_REFUNDS_GOP = ifelse(is.na(INDIVIDUAL_REFUNDS_GOP),0,INDIVIDUAL_REFUNDS_GOP)) %>%
                      left_join(.,gb_est %>% rename("year"="cycle"),by="year") %>%
                      left_join(.,incumbent_data,by=c("year","District")) %>%
                      left_join(.,house_est %>% rename("year"="cycle"),by=c("year","District")) %>%
                      mutate(DEM_GB_SC = qnorm(p=pnorm(-1*gb_rnorm),mean=DEM_GB_EST,sd=DEM_GB_SD),
                             GOP_GB_SC = qnorm(p=pnorm(gb_rnorm),mean=GOP_GB_EST,sd=GOP_GB_SD),
                             EST_TWO_PARTY = ifelse(District %in% c("AK-01","ME-01","ME-02"),100,
                                                    100 - 0.66 - 2.53*LBT_IND - 1.35*GRN_IND - 3.19*IND_IND)) %>%
                      mutate(undecideds = 98.5 - DEM_GB_SC - GOP_GB_SC) %>%
                      mutate(GOP_UND_SC = min(max(qnorm(p=pnorm(und_rnorm),mean=0.6,sd=0.05),0),1)*undecideds) %>%
                      mutate(DEM_UND_SC = undecideds - GOP_UND_SC) %>%
                      mutate(GOP_GB = round(GOP_GB_SC + GOP_UND_SC,2),
                             DEM_GB = round(DEM_GB_SC + DEM_UND_SC,2),
                             GB_DIFF = round(GOP_GB_SC + GOP_UND_SC,2) - round(DEM_GB_SC + DEM_UND_SC,2)) %>%
                      mutate(DEM_POLL_EST_SC = ifelse(!is.na(DEM_POLL_EST),qnorm(p=pnorm(-1*gb_rnorm),mean=DEM_POLL_EST,sd=DEM_POLL_SD),NA),
                             GOP_POLL_EST_SC = ifelse(!is.na(GOP_POLL_EST),qnorm(p=pnorm(gb_rnorm),mean=GOP_POLL_EST,sd=GOP_POLL_SD),NA),
                             LEAN_PRED_SC = ifelse(is.na(COMP),LEAN_1 + GB_DIFF,LEAN_1 + GB_DIFF + COMP),
                             poll_weight = ifelse(is.na(total_weight),NA,
                                                  ifelse(total_weight>=1.5,0,1.5-total_weight))) %>%
                      mutate(POLL_UND_EST = EST_TWO_PARTY - DEM_POLL_EST_SC - GOP_POLL_EST_SC) %>%
                      mutate(GOP_POLL_UND_EST = (min(max(qnorm(p=pnorm(und_rnorm),mean=0.6,sd=0.05),0),1)+LEAN_1/100/2)*POLL_UND_EST) %>%
                      mutate(DEM_POLL_UND_EST = POLL_UND_EST - GOP_POLL_UND_EST) %>%
                      mutate(DEM_LEAN_SC = EST_TWO_PARTY/2 - LEAN_PRED_SC/2,
                             GOP_LEAN_SC = EST_TWO_PARTY/2 + LEAN_PRED_SC/2,
                             DEM_POLL_SC = DEM_POLL_EST_SC + DEM_POLL_UND_EST,
                             GOP_POLL_SC = GOP_POLL_EST_SC + GOP_POLL_UND_EST,
                             theWeight = ifelse(is.na(poll_weight),0,0.75*exp(-0.5*poll_weight))) %>%
                      mutate(DEM_PRED = ifelse(theWeight==0,round(DEM_LEAN_SC,2),round(theWeight * DEM_POLL_SC + (1-theWeight) * DEM_LEAN_SC,2)),
                             GOP_PRED = ifelse(theWeight==0,round(GOP_LEAN_SC,2),round(theWeight * GOP_POLL_SC + (1-theWeight) * GOP_LEAN_SC,2))) %>%
                      mutate(PRED_DIFF = GOP_PRED - DEM_PRED)
                      

#Prepare data for modeling                    
train = train %>% mutate(TOTAL_RECEIPTS_DEM_LN = ifelse(TOTAL_RECEIPTS_DEM <= 0,0,log(TOTAL_RECEIPTS_DEM)),
                         TOTAL_RECEIPTS_GOP_LN = ifelse(TOTAL_RECEIPTS_GOP <= 0,0,log(TOTAL_RECEIPTS_GOP)),
                         TOTAL_DISBURSEMENTS_DEM_LN = ifelse(TOTAL_DISBURSEMENTS_DEM <= 0,0,log(TOTAL_DISBURSEMENTS_DEM)),
                         TOTAL_DISBURSEMENTS_GOP_LN = ifelse(TOTAL_DISBURSEMENTS_GOP <= 0,0,log(TOTAL_DISBURSEMENTS_GOP)),
                         COH_DEM_LN = ifelse(COH_DEM <= 0,0,log(COH_DEM)),
                         COH_GOP_LN = ifelse(COH_GOP <= 0,0,log(COH_GOP)),
                         INDIVIDUAL_CONTRIBUTIONS_DEM_LN = ifelse(INDIVIDUAL_CONTRIBUTIONS_DEM<=0,0,log(INDIVIDUAL_CONTRIBUTIONS_DEM)),
                         INDIVIDUAL_CONTRIBUTIONS_GOP_LN = ifelse(INDIVIDUAL_CONTRIBUTIONS_GOP<=0,0,log(INDIVIDUAL_CONTRIBUTIONS_GOP))) %>%
                  mutate(REL_TOTAL_RECEIPTS_LN = TOTAL_RECEIPTS_GOP_LN - TOTAL_RECEIPTS_DEM_LN,
                         REL_DISBURSEMENT_LN = TOTAL_DISBURSEMENTS_GOP_LN - TOTAL_DISBURSEMENTS_DEM_LN,
                         REL_COH_LN = COH_GOP_LN - COH_DEM_LN,
                         REL_IND_CONTR_LN = INDIVIDUAL_CONTRIBUTIONS_GOP_LN - INDIVIDUAL_CONTRIBUTIONS_DEM_LN)

test = test %>% mutate(TOTAL_RECEIPTS_DEM_LN = ifelse(TOTAL_RECEIPTS_DEM <= 0,0,log(TOTAL_RECEIPTS_DEM)),
                       TOTAL_RECEIPTS_GOP_LN = ifelse(TOTAL_RECEIPTS_GOP <= 0,0,log(TOTAL_RECEIPTS_GOP)),
                       TOTAL_DISBURSEMENTS_DEM_LN = ifelse(TOTAL_DISBURSEMENTS_DEM <= 0,0,log(TOTAL_DISBURSEMENTS_DEM)),
                       TOTAL_DISBURSEMENTS_GOP_LN = ifelse(TOTAL_DISBURSEMENTS_GOP <= 0,0,log(TOTAL_DISBURSEMENTS_GOP)),
                       COH_DEM_LN = ifelse(COH_DEM <= 0,0,log(COH_DEM)),
                       COH_GOP_LN = ifelse(COH_GOP <= 0,0,log(COH_GOP)),
                       INDIVIDUAL_CONTRIBUTIONS_DEM_LN = ifelse(INDIVIDUAL_CONTRIBUTIONS_DEM<=0,0,log(INDIVIDUAL_CONTRIBUTIONS_DEM)),
                       INDIVIDUAL_CONTRIBUTIONS_GOP_LN = ifelse(INDIVIDUAL_CONTRIBUTIONS_GOP<=0,0,log(INDIVIDUAL_CONTRIBUTIONS_GOP))) %>%
                mutate(REL_TOTAL_RECEIPTS_LN = TOTAL_RECEIPTS_GOP_LN - TOTAL_RECEIPTS_DEM_LN,
                       REL_DISBURSEMENT_LN = TOTAL_DISBURSEMENTS_GOP_LN - TOTAL_DISBURSEMENTS_DEM_LN,
                       REL_COH_LN = COH_GOP_LN - COH_DEM_LN,
                       REL_IND_CONTR_LN = INDIVIDUAL_CONTRIBUTIONS_GOP_LN - INDIVIDUAL_CONTRIBUTIONS_DEM_LN)


#remove all objects from memory that are no longer needed
#rm(list=setdiff(ls(),c("train","test")))
#gc()


#create variable lists...drop variables that have super small significance (Candidate Indicator)
y_var = "GOP_win"
x_vars = setdiff(colnames(train),c("year","District","GOP_win","DEM_act","GOP_act","D","R","total_votes",
                                   "FEC_ID_DEM","FIRST_NAME_DEM","LAST_NAME_DEM","FEC_ID_GOP","FIRST_NAME_GOP",
                                   "LAST_NAME_GOP","GOP_IND","DEM_IND","TOTAL_RECEIPTS_DEM","TOTAL_DISBURSEMENTS_DEM",
                                   "COH_DEM","INDIVIDUAL_CONTRIBUTIONS_DEM","PARTY_CONTRIBUTIONS_DEM","INDIVIDUAL_REFUNDS_DEM",
                                   "TOTAL_RECEIPTS_GOP","TOTAL_DISBURSEMENTS_GOP","COH_GOP","INDIVIDUAL_CONTRIBUTIONS_GOP",
                                   "PARTY_CONTRIBUTIONS_GOP","INDIVIDUAL_REFUNDS_GOP","median_age","ppsm"))


#start H2O
h2o.init(max_mem_size = "16g")

train_data = as.h2o(train)
test_data = as.h2o(test)


theModel = h2o.gbm(x=x_vars,
                   y=y_var,
                   nfolds=10,
                   training_frame = train_data,
                   col_sample_rate = 1.0,
                   min_split_improvement = 0.0001,
                   learn_rate = 0.1,
                   max_depth = 10,
                   min_rows = 20,
                   ntrees = 50,
                   sample_rate = 0.8,
                   seed=123)

# H2ORegressionMetrics: gbm
# ** Reported on training data. **
#   
# MSE:  0.007048057
# RMSE:  0.08395271
# MAE:  0.03125731
# RMSLE:  0.06117685
# Mean Residual Deviance :  0.007048057
# R2: 0.97164


test$GOP_win = as.vector(h2o.predict(theModel,test_data))

h2o.shutdown(prompt=FALSE)

#Determine winner
set.seed(123)
test$random = runif(n=4000000,min=0,max=1)
test = test %>% mutate(GOP_seat = ifelse(GOP_win>=random,1,0))

#Get scenario breakdown
scenarios = test %>% group_by(scenario) %>%
                     summarise(GOP_seat = sum(GOP_seat)) %>%
                     ungroup() %>%
                     mutate(GOP_seat_final = GOP_seat + 23,
                            DEM_seat_final = (400 - GOP_seat) + 12) %>%
                     select(-c("GOP_seat"))

majority = scenarios %>% mutate(GOP_majority = ifelse(GOP_seat_final>=218,1,0)) %>%
                         summarise(GOP_majority = round(sum(GOP_majority)/n()*100,2)) %>%
                         mutate(DEM_majority = 100 - GOP_majority)

expected_seat = scenarios %>% summarise(GOP_expected_seat = round(mean(GOP_seat_final),0),
                                        DEM_expected_seat = round(mean(DEM_seat_final),0),
                                        GOP_expected_seat_10 = quantile(GOP_seat_final,.1),
                                        GOP_expected_seat_90 = quantile(GOP_seat_final,.9),
                                        DEM_expected_seat_10 = quantile(DEM_seat_final,.1),
                                        DEM_expected_seat_90 = quantile(DEM_seat_final,.9),
                                        GOP_expected_seat_SD = round(sd(GOP_seat_final),0),
                                        DEM_expected_seat_SD = round(sd(DEM_seat_final),0))


#Look at individual seats
seats = test %>% group_by(year,District,INCUMBENT_IND_DEM,FIRST_NAME_DEM,LAST_NAME_DEM,INCUMBENT_IND_GOP,FIRST_NAME_GOP,LAST_NAME_GOP) %>%
                 summarise(GOP_win = round(sum(GOP_seat)/n(),2)) %>%
                 ungroup()

defaults = read.csv("Data/Datasets/default_wins.csv",header=T,stringsAsFactors=F)

seats_final = rbind(seats,defaults) %>% mutate(DEM = ifelse(INCUMBENT_IND_DEM == 1,trimws(paste(FIRST_NAME_DEM," ",LAST_NAME_DEM," (I)",sep="")),
                                                                                   trimws(paste(FIRST_NAME_DEM," ",LAST_NAME_DEM,sep=""))),
                                               GOP = ifelse(INCUMBENT_IND_GOP == 1,trimws(paste(FIRST_NAME_GOP," ",LAST_NAME_GOP," (I)",sep="")),
                                                                                   trimws(paste(FIRST_NAME_GOP," ",LAST_NAME_GOP,sep=""))),
                                               DEM_PRCT = (1-GOP_win)*100,
                                               GOP_PRCT = GOP_win*100) %>%
                                        select(District,DEM,DEM_PRCT,GOP,GOP_PRCT) %>%
                                        arrange(District)

majority_out = rbind(majority_prev,majority %>% mutate(date = theDate))
expected_seat_out = rbind(expected_seat_prev,expected_seat %>% mutate(date = theDate))

scenario_out = scenarios %>% group_by(GOP_seat_final) %>%
                             summarise(probability = round(n()/10000*100,2)) %>%
                             mutate(control = ifelse(GOP_seat_final>=218,"GOP","DEM"))

gb_out = rbind(gb_prev,gb_est %>% mutate(date = theDate))

#write out the files
write.csv(gb_out,"Data/Results/Generic_Ballot.csv",row.names=F)
write.csv(seats_final,"Data/Results/Individual_Seats.csv",row.names = F)
write.csv(scenario_out,"Data/Results/Scenarios.csv",row.names = F)
write.csv(majority_out,"Data/Results/Majority.csv",row.names = F)
write.csv(expected_seat_out,"Data/Results/Expected_Seats.csv",row.names = F)



#create grid parameters
# max_depth_opt = c(3,5,8,10)
# min_rows_opt = c(10,20,30)
# sample_rate_opt = c(0.6, 0.8, 1.0)
# ntrees_opt = c(50,100,200)
# hyper_params = list(max_depth = max_depth_opt,
#                     min_rows=min_rows_opt,
#                     sample_rate = sample_rate_opt,
#                     ntrees=ntrees_opt)
# 
# search_criteria = list(strategy = "RandomDiscrete",
#                        stopping_metric = "mae",
#                        stopping_tolerance = 0.001,
#                        stopping_rounds = 10,
#                        max_runtime_secs = 60*60)

#create grid
# grid = h2o.grid(algorithm = "gbm",
#                 grid_id = "gbm_grid",
#                 x = x_vars,
#                 y = y_var,
#                 training_frame = train_data,
#                 nfolds=10,
#                 hyper_params = hyper_params,
#                 search_criteria=search_criteria,
#                 col_sample_rate = 1.0,
#                 min_split_improvement = 0.0001,
#                 learn_rate = 0.1,
#                 seed = 412)

#view grid results
# grid_performance = h2o.getGrid(grid_id = "gbm_grid",
#                                sort_by = "mse",
#                                decreasing = FALSE)
# theperformance = as.data.frame(grid_performance@summary_table)
# print(theperformance)

#     max_depth min_rows ntrees sample_rate          model_ids        mse
# 1          10       20     50         0.8 gbm_grid_model_106 0.02499676
# 2           8       20     50         0.8  gbm_grid_model_48 0.02505555
# 3           8       10    100         0.8  gbm_grid_model_20 0.02539142
# 4          10       10     50         0.6  gbm_grid_model_24 0.02539982
# 5           8       20    100         0.8  gbm_grid_model_74 0.02546260
# 6          10       20    100         0.8 gbm_grid_model_104 0.02551295
# 7           3       10     50         0.8  gbm_grid_model_62 0.02551563
# 8           8       10    200         0.8  gbm_grid_model_98 0.02552750
# 9           8       10     50         0.6  gbm_grid_model_13 0.02552966
# 10          5       10     50         0.6  gbm_grid_model_17 0.02556541
# 11          3       10    100         0.8  gbm_grid_model_14 0.02563954
# 12          8       10    200         0.6  gbm_grid_model_50 0.02565374
# 13          8       10     50         0.8  gbm_grid_model_59 0.02565389
# 14         10       10    100         0.6  gbm_grid_model_61 0.02568459
# 15          8       10    100         0.6   gbm_grid_model_2 0.02568798
# 16          5       20     50         0.8  gbm_grid_model_87 0.02570961
# 17          3       20     50         0.8  gbm_grid_model_84 0.02574811
# 18         10       10    200         0.6  gbm_grid_model_11 0.02589475
# 19          8       20    100         0.6   gbm_grid_model_6 0.02589847
# 20          3       10     50         0.6  gbm_grid_model_45 0.02590272
# 21          8       20     50         0.6  gbm_grid_model_34 0.02592434
# 22         10       20    200         0.8  gbm_grid_model_60 0.02594684
# 23          3       10    200         0.8  gbm_grid_model_80 0.02594889
# 24          3       20    100         0.8  gbm_grid_model_72 0.02595887
# 25          5       10    100         0.6  gbm_grid_model_42 0.02600555
# 26          3       20    100         0.6   gbm_grid_model_5 0.02601714
# 27         10       30     50         1.0 gbm_grid_model_102 0.02602491
# 28          5       20     50         0.6   gbm_grid_model_1 0.02602501
# 29          5       20    100         0.8  gbm_grid_model_66 0.02603710
# 30         10       20     50         0.6  gbm_grid_model_22 0.02605495
# 31          8       20     50         1.0   gbm_grid_model_8 0.02608089
# 32          5       10     50         0.8 gbm_grid_model_107 0.02608249
# 33          8       20    200         0.8  gbm_grid_model_29 0.02610007
# 34         10       30    100         1.0  gbm_grid_model_56 0.02611440
# 35          8       30     50         1.0  gbm_grid_model_43 0.02613682
# 36          3       10    100         1.0  gbm_grid_model_58 0.02617184
# 37          3       20     50         0.6  gbm_grid_model_32 0.02619007
# 38          8       30    100         1.0  gbm_grid_model_10 0.02619195
# 39          3       20    200         0.8  gbm_grid_model_68 0.02619229
# 40          3       10     50         1.0 gbm_grid_model_100 0.02622648
# 41          3       10    100         0.6  gbm_grid_model_37 0.02623113
# 42          3       30    200         0.6  gbm_grid_model_86 0.02624209
# 43          3       10    200         1.0  gbm_grid_model_81 0.02624340
# 44          8       20    200         0.6  gbm_grid_model_27 0.02624389
# 45         10       20     50         1.0  gbm_grid_model_70 0.02625446
# 46         10       30     50         0.6  gbm_grid_model_52 0.02626604
# 47          8       30     50         0.8  gbm_grid_model_82 0.02626904
# 48          5       10    100         0.8  gbm_grid_model_85 0.02628578
# 49          8       20    100         1.0  gbm_grid_model_26 0.02629190
# 50          3       30    100         0.6  gbm_grid_model_49 0.02633293
# 51          5       30     50         1.0   gbm_grid_model_7 0.02634642
# 52         10       20    100         0.6  gbm_grid_model_71 0.02637136
# 53          5       20     50         1.0 gbm_grid_model_108 0.02638030
# 54          5       30    100         1.0  gbm_grid_model_91 0.02638949
# 55          5       10    200         0.8  gbm_grid_model_64 0.02642289
# 56         10       10     50         0.8  gbm_grid_model_31 0.02644047
# 57          5       10    200         0.6  gbm_grid_model_39 0.02644095
# 58          3       20     50         1.0  gbm_grid_model_47 0.02644556
# 59          5       30    200         1.0 gbm_grid_model_103 0.02647026
# 60         10       30     50         0.8  gbm_grid_model_18 0.02648857
# 61          3       20    100         1.0   gbm_grid_model_3 0.02648880
# 62          3       30    100         0.8  gbm_grid_model_76 0.02649486
# 63         10       30    100         0.6  gbm_grid_model_25 0.02649692
# 64          3       20    200         0.6  gbm_grid_model_12 0.02651084
# 65          3       30    100         1.0  gbm_grid_model_93 0.02652474
# 66          5       30     50         0.8  gbm_grid_model_57 0.02652505
# 67         10       10    100         0.8  gbm_grid_model_28 0.02655706
# 68          3       30     50         0.8  gbm_grid_model_95 0.02656210
# 69          8       20    200         1.0  gbm_grid_model_16 0.02657240
# 70          5       20    100         0.6  gbm_grid_model_75 0.02657375
# 71          5       20    100         1.0  gbm_grid_model_89 0.02660317
# 72          5       20    200         0.8  gbm_grid_model_51 0.02661390
# 73          8       30    100         0.6  gbm_grid_model_38 0.02661497
# 74          5       30    100         0.6  gbm_grid_model_23 0.02661602
# 75          8       30     50         0.6   gbm_grid_model_4 0.02667532
# 76          8       30    200         1.0  gbm_grid_model_55 0.02668188
# 77          8       30    100         0.8  gbm_grid_model_88 0.02668555
# 78          3       30     50         0.6   gbm_grid_model_9 0.02669499
# 79         10       10    200         0.8  gbm_grid_model_69 0.02670336
# 80          5       30    100         0.8 gbm_grid_model_101 0.02670353
# 81          5       10    100         1.0  gbm_grid_model_73 0.02673808
# 82          5       20    200         0.6  gbm_grid_model_46 0.02674628
# 83          3       30     50         1.0  gbm_grid_model_83 0.02677304
# 84         10       30    200         1.0  gbm_grid_model_30 0.02679897
# 85          3       20    200         1.0  gbm_grid_model_35 0.02680367
# 86         10       20    100         1.0  gbm_grid_model_65 0.02684128
# 87          3       10    200         0.6  gbm_grid_model_36 0.02686981
# 88          5       30     50         0.6  gbm_grid_model_78 0.02687221
# 89          5       10    200         1.0 gbm_grid_model_105 0.02690140
# 90          5       10     50         1.0  gbm_grid_model_41 0.02691880
# 91         10       10    200         1.0  gbm_grid_model_40 0.02694492
# 92         10       20    200         0.6  gbm_grid_model_99 0.02695411
# 93         10       10    100         1.0  gbm_grid_model_15 0.02697333
# 94          3       30    200         0.8  gbm_grid_model_63 0.02700162
# 95          5       30    200         0.8  gbm_grid_model_33 0.02701577
# 96          5       20    200         1.0  gbm_grid_model_79 0.02704999
# 97         10       20    200         1.0  gbm_grid_model_67 0.02706811
# 98          8       30    200         0.6  gbm_grid_model_77 0.02708645
# 99          8       30    200         0.8  gbm_grid_model_44 0.02714015
# 100        10       10     50         1.0  gbm_grid_model_53 0.02715571
# 101         3       30    200         1.0  gbm_grid_model_54 0.02718293
# 102         8       10     50         1.0  gbm_grid_model_96 0.02718402
# 103        10       30    100         0.8  gbm_grid_model_19 0.02720128
# 104         8       10    100         1.0  gbm_grid_model_90 0.02721326
# 105        10       30    200         0.6  gbm_grid_model_94 0.02732747
# 106         5       30    200         0.6  gbm_grid_model_97 0.02733162
# 107         8       10    200         1.0  gbm_grid_model_21 0.02741210
# 108        10       30    200         0.8  gbm_grid_model_92 0.02792780


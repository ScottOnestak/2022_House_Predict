#2022 Election Project
#Author: Scott Onestak
#10 Build Train and Test Base Datasets

#libraries
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

#read in data

##Election Results
party_ind = read.csv('Data/Election Results/Cleaned Results/party_ind_data.csv',header=T,stringsAsFactors=F)
election_results_raw = read.csv('Data/Election Results/Cleaned Results/votes_cleaned_data.csv',header=T,stringsAsFactors=F) %>%
                          filter(year >= 2006 & runoff == FALSE)

#Generic Ballot
gb_acts = read.csv('Data/Polls/Generic Ballot/generic_ballot_actuals.csv',header=T,stringsAsFactors=F)
colnames(gb_acts) = c("cycle","GOP_act","DEM_act","diff_act")

#Build Undecideds data
undecideds = as.data.frame(rbind(c(2010,0.62,0.38),
                                 c(2012,0.34,0.66),
                                 c(2014,0.74,0.26),
                                 c(2016,0.64,0.36),
                                 c(2018,0.30,0.70),
                                 c(2020,0.72,0.28)))
colnames(undecideds) = c("cycle","GOP_und_prct","DEM_und_prct")

#Lean Data
leans = read.csv('Data/Historical Leans/Leans.csv',header=T,stringsAsFactors=F)
colnames(leans)[1] = "year"

#Census
census = read.csv("Data/Census_by_CDs/census_imputed_for_build.csv",header=T,stringsAsFactors=F)

##FEC data
fec_data = rbind(read.delim("Data/FEC Data/fec2010.txt",header=F,sep="|",stringsAsFactors=F,quote="",fill=FALSE) %>% mutate(year=2010),
                 read.delim("Data/FEC Data/fec2012.txt",header=F,sep="|",stringsAsFactors=F,quote="",fill=FALSE) %>% mutate(year=2012),
                 read.delim("Data/FEC Data/fec2014.txt",header=F,sep="|",stringsAsFactors=F,quote="",fill=FALSE) %>% mutate(year=2014),
                 read.delim("Data/FEC Data/fec2016.txt",header=F,sep="|",stringsAsFactors=F,quote="",fill=FALSE) %>% mutate(year=2016),
                 read.delim("Data/FEC Data/fec2018.txt",header=F,sep="|",stringsAsFactors=F,quote="",fill=FALSE) %>% mutate(year=2018),
                 read.delim("Data/FEC Data/fec2020.txt",header=F,sep="|",stringsAsFactors=F,quote="",fill=FALSE) %>% mutate(year=2020),
                 read.delim("Data/FEC Data/fec2022.txt",header=F,sep="|",stringsAsFactors=F,quote="",fill=FALSE) %>% mutate(year=2022))
colnames(fec_data) = c("FEC_ID","CAND_NAME","CAND_ICI","PTY_CD","CAND_PTY_AFFILIATION","TOTAL_RECEIPTS","TRANS_FROM_AUTH",
                       "TOTAL_DISBURSEMENTS","TRANS_TO_AUTH_CMTE","COH_BOP","COH_COP","CAND_CONTRIB","CAND_LOANS","OTH_LOANS",
                       "CAND_LOAN_REPAY","OTHER_LOAN_REPAY","DEBTS_OWED_BY","TOTAL_INDIVIDUAL_CONTRIBUTIONS","STATE","DISTRICT",
                       "SPECIAL_ELECTION","PRIMARY_ELECTION","RUNOFF_ELECTION","GENERAL_ELECTION","GEN_ELECTION_PRCT",
                       "OTH_POL_CMTE_CONTRIB","PARTY_CONTRIB","CVG_END_DT","INDIV_REFUNDS","CMTE_REFUNDS","year")

#historic House polls
house_polls = read.csv('Data/Polls/House Polls/house_polls_historical.csv',header=T,stringsAsFactors=F) %>%
                filter(election_date %in% c("2020-11-03","2018-11-06","2016-11-08","2014-11-04","2012-11-06","2010-11-02") & population %in% c("rv","lv"))
house_polls$start_date = as.Date(house_polls$start_date,"%Y-%m-%d")
house_polls$end_date = as.Date(house_polls$end_date,"%Y-%m-%d")
house_polls$election_date = as.Date(house_polls$election_date,"%Y-%m-%d")
house_polls$District = paste(house_polls$State,"-",str_pad(house_polls$seat_number,2,pad="0"),sep="")
house_polls$mid_date = NA
house_polls$days_to_election = NA
for(i in seq(from=1,to=dim(house_polls)[1],by=1)){
  house_polls[i,"mid_date"] = mean.Date(c(house_polls[i,"start_date"],house_polls[i,"end_date"]))
  house_polls[i,"days_to_election"] = as.numeric(house_polls[i,"election_date"] - house_polls[i,"mid_date"])
}
house_polls_cleaned = house_polls %>% filter(stage %in% c("general","jungle primary") & party %in% c("DEM","REP")) %>%
                                      group_by(District,pollster_rating_name,sample_size,population,partisan,cycle,party,days_to_election) %>%
                                      summarise(pct = sum(pct)) %>% #Combining all Rs and Ds from jungle primary together
                                      spread(key=party,val=pct) %>%
                                      filter(!is.na(REP) & !is.na(DEM)) #Filter out CDs in states like WA and CA that may have 2 Rs or 2Ds on the ballot

#pollster ratings
pollster_bias = read.csv('Data/Polls/For Pollster Ratings/forModeling/pollster_bias.csv',header=T,stringsAsFactors=F)
partisan_bias = read.csv('Data/Polls/For Pollster Ratings/forModeling/partisan_bias.csv',header=T,stringsAsFactors=F)
avg_bias = read.csv('Data/Polls/For Pollster Ratings/forModeling/avg_bias.csv',header=T,stringsAsFactors=F)
colnames(pollster_bias)[1] = "cycle"
colnames(partisan_bias)[1] = "cycle"
colnames(avg_bias)[1] = "cycle"






# ------------------------------------------    BUILD TRAIN DATASET   ------------------------------------------ #

#Build actual results for CDs that have both a Democrat and Republican on the ballot
#For jungle generals some southern states have, group all Republican and Democrat votes together
#   FEC ID will be assigned to the best performer of the party as a representative
dem_cand = election_results_raw %>% filter(PARTY == "D") %>%
                                    group_by(year,STATE,DISTRICT) %>%
                                    summarise(count=n())

dem_cand_name = rbind(dem_cand %>% filter(count==1) %>% 
                                   left_join(.,election_results_raw %>% filter(PARTY=="D") %>% 
                                                                        select(year,STATE,DISTRICT,FEC_ID,FIRST_NAME,LAST_NAME,INCUMBENT_IND),
                                             by=c("year","STATE","DISTRICT")),
                      dem_cand %>% filter(count>1) %>% left_join(.,election_results_raw %>% filter(PARTY=="D") %>%
                                                                                            select(year,STATE,DISTRICT,INCUMBENT_IND) %>%
                                                                                            group_by(year,STATE,DISTRICT) %>%
                                                                                            summarise(INCUMBENT_IND = max(INCUMBENT_IND)) %>%
                                                                                            ungroup(),
                                                                 by=c("year","STATE","DISTRICT")) %>%
                                                        mutate(FEC_ID = ifelse(year==2012 & STATE == "LA" & DISTRICT == 2,"H8LA02054",
                                                                               ifelse(year==2014 & STATE=="LA" & DISTRICT==1,"H6LA02173",
                                                                                      ifelse(year==2014 & STATE=="LA" & DISTRICT==6,"H4LA06161",
                                                                                             ifelse(year==2016 & STATE=="LA" & DISTRICT==1,"H4LA01147",
                                                                                                    ifelse(year==2016 & STATE=="LA" & DISTRICT==6,"H4LA06062",
                                                                                                           ifelse(year==2018 & STATE=="LA" & DISTRICT==1,"H8LA01122",
                                                                                                                  ifelse(year==2018 & STATE=="LA" & DISTRICT==3,"H8LA03094",
                                                                                                                         ifelse(year==2018 & STATE=="LA" & DISTRICT==6,"H8LA06154",
                                                                                                                                ifelse(year==2020 & STATE=="LA" & DISTRICT==3,"H0LA03190",
                                                                                                                                       ifelse(year==2020 & STATE=="LA" & DISTRICT==4,"H0LA04099",
                                                                                                                                              ifelse(year==2020 & STATE=="LA" & DISTRICT==5,"H0LA05070",
                                                                                                                                                     ifelse(year==2020 & STATE=="LA" & DISTRICT==2,"H8LA02054",
                                                                                                                                                            ifelse(year==2014 & STATE=="LA" & DISTRICT==2,"H8LA02054",
                                                                                                                                                                   ifelse(year==2016 & STATE=="LA" & DISTRICT==2,"H8LA02054",
                                                                                                                                                                          ifelse(year==2016 & STATE=="LA" & DISTRICT==3,"H6LA03163","NA"))))))))))))))),
                                                               FIRST_NAME = "",
                                                               LAST_NAME = "A DEMOCRAT"))
colnames(dem_cand_name) = c("year","STATE","DISTRICT","count","FEC_ID_DEM","FIRST_NAME_DEM","LAST_NAME_DEM","INCUMBENT_IND_DEM")

rep_cand = election_results_raw %>% filter(PARTY == "R") %>%
                                    group_by(year,STATE,DISTRICT) %>%
                                    summarise(count=n())

rep_cand_name = rbind(rep_cand %>%  filter(count==1) %>% 
                                    left_join(.,election_results_raw %>% filter(PARTY=="R") %>% 
                                                select(year,STATE,DISTRICT,FEC_ID,FIRST_NAME,LAST_NAME,INCUMBENT_IND),
                                              by=c("year","STATE","DISTRICT")),
                      rep_cand %>% filter(count>1) %>% left_join(.,election_results_raw %>% filter(PARTY=="R") %>%
                                                                   select(year,STATE,DISTRICT,INCUMBENT_IND) %>%
                                                                   group_by(year,STATE,DISTRICT) %>%
                                                                   summarise(INCUMBENT_IND = max(INCUMBENT_IND)) %>%
                                                                   ungroup(),
                                                                 by=c("year","STATE","DISTRICT")) %>%
                        mutate(FIRST_NAME = "",
                               LAST_NAME = "A REPUBLICAN",
                               FEC_ID = ifelse(year==2012 & STATE=="LA" & DISTRICT==1,"H0LA01087",
                                               ifelse(year==2012 & STATE=="LA" & DISTRICT==2,"H2LA02115",
                                                      ifelse(year==2012 & STATE=="LA" & DISTRICT==3,"H4LA07029",
                                                             ifelse(year==2014 & STATE=="LA" & DISTRICT==5,"H4LA05221",
                                                                    ifelse(year==2014 & STATE=="LA" & DISTRICT==6,"H4LA06153",
                                                                           ifelse(year==2016 & STATE=="LA" & DISTRICT==3,"H6LA03130",
                                                                                  ifelse(year==2016 & STATE=="LA" & DISTRICT==4,"H6LA04138",
                                                                                         ifelse(year==2016 & STATE=="LA" & DISTRICT==6,"H4LA06153",
                                                                                                ifelse(year==2018 & STATE=="LA" & DISTRICT==3,"H6LA03148",
                                                                                                       ifelse(year==2020 & STATE=="LA" & DISTRICT==2,"H0LA02259",
                                                                                                              ifelse(year==2020 & STATE=="LA" & DISTRICT==4,"H6LA04138",
                                                                                                                     ifelse(year==2020 & STATE=="LA" & DISTRICT==5,"H0LA05120",
                                                                                                                            ifelse(year==2014 & STATE=="LA" & DISTRICT==3,"H4LA07029",
                                                                                                                                   ifelse(year==2016 & STATE=="LA" & DISTRICT==5,"H4LA05221","NA"))))))))))))))))
colnames(rep_cand_name) = c("year","STATE","DISTRICT","count","FEC_ID_GOP","FIRST_NAME_GOP","LAST_NAME_GOP","INCUMBENT_IND_GOP")

election_results = election_results_raw %>% filter(year>=2010) %>% 
                                            inner_join(.,dem_cand %>% select(-count),by=c("year","STATE","DISTRICT")) %>%
                                            inner_join(.,rep_cand %>% select(-count),by=c("year","STATE","DISTRICT")) %>%
                                            group_by(year,STATE,DISTRICT,PARTY) %>%
                                            summarise(VOTES = sum(candidate_votes,na.rm=T)) %>%
                                            spread(.,key=PARTY,value=VOTES) %>%
                                            ungroup() %>%
                                            left_join(.,election_results_raw %>% select(year,STATE,DISTRICT,total_votes) %>% distinct(),by=c("year","STATE","DISTRICT")) %>%
                                            mutate(DEM_act = round(D / total_votes * 100,2),
                                                   GOP_act = round(R / total_votes * 100,2)) %>%
                                            mutate(DIFF_act = GOP_act - DEM_act,
                                                   GOP_win = ifelse(R > D,1,0)) %>%
                                            left_join(.,dem_cand_name %>% select(-count),by=c("year","STATE","DISTRICT")) %>%
                                            left_join(.,rep_cand_name  %>% select(-count),by=c("year","STATE","DISTRICT")) %>%
                                            mutate(District = paste(STATE,"-",str_pad(DISTRICT,2,pad="0"),sep="")) %>%
                                            select(year,District,GOP_win,DEM_act,GOP_act,D,R,total_votes,
                                                   FEC_ID_DEM,FIRST_NAME_DEM,LAST_NAME_DEM,INCUMBENT_IND_DEM,
                                                   FEC_ID_GOP,FIRST_NAME_GOP,LAST_NAME_GOP,INCUMBENT_IND_GOP)

#build vote share prediction


##First Build Incumbent Data
##Look at LEAN_1/LEAN_2 + GB + Incumbent Previous Relative Performance
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

incumbent_data = incumbent_temp %>% filter(year>=2010 & (INCUMBENT_IND_GOP == 1 | INCUMBENT_IND_DEM == 1)) %>% 
                                    mutate(year1 = year - 2,
                                           year2 = year - 4) %>%
                                    rename("cycle"="year") %>%
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
                                    filter(!is.na(COMP1) | !is.na(COMP2)) #Sometimes incumbent because nobody ran against them prior or special election... so no comparable results

lean_calc = election_results %>% mutate(DIFF = GOP_act - DEM_act) %>%
                                 left_join(.,leans %>% select(year,District,LEAN_1),by=c("year","District")) %>%
                                 rename("cycle"="year") %>%
                                 left_join(.,gb_acts %>% select(cycle,diff_act),by="cycle") %>%
                                 left_join(.,incumbent_data %>% mutate(COMP = ifelse(!is.na(COMP1),COMP1/2,COMP2/2)) %>%
                                             select(cycle,District,COMP),
                                           by=c("cycle","District")) %>%
                                 mutate(LEAN_PRED = ifelse(is.na(COMP),LEAN_1 + diff_act,LEAN_1 + diff_act + COMP)) %>%
                                 left_join(.,party_ind %>% rename("cycle"="year") %>%
                                                           mutate(District = paste(STATE,"-",str_pad(DISTRICT,2,pad="0"),sep="")) %>%
                                                           select(-c("STATE","DISTRICT")),
                                           by=c("cycle","District")) %>%
                                 mutate(expected_two_party_vote = 100 - 0.66 - 2.53*LBT_IND - 1.35*GRN_IND - 3.19*IND_IND) %>%
                                 mutate(GOP_LEAN = expected_two_party_vote/2 + LEAN_PRED/2,
                                        DEM_LEAN = expected_two_party_vote/2 - LEAN_PRED/2) %>%
                                 select(cycle,District,GOP_LEAN,DEM_LEAN)

adv_score_miss_replace = pollster_bias %>%
                              group_by(cycle) %>%
                              summarise(adv_score_replace = quantile(adv_score,1))

thepolls_calc = house_polls_cleaned %>% rename("pollster"="pollster_rating_name") %>%
                                        filter(DEM + REP >= 75 & !is.na(sample_size) & days_to_election <= 100) %>%
                                        arrange(cycle,District,pollster,days_to_election,population) %>%
                                        group_by(cycle,District,pollster) %>%
                                        filter(row_number()==1) %>%
                                        left_join(.,pollster_bias %>% select(-count),by=c("cycle","pollster")) %>%
                                        left_join(.,partisan_bias,by=c("cycle","partisan")) %>%
                                        mutate(day_join = round(days_to_election,0)) %>%
                                        mutate(adjustment = ifelse(population == "rv",
                                                                   ifelse(is.na(POLLSTER_BIAS),PARTISAN_BIAS-1.5,PARTISAN_BIAS+POLLSTER_BIAS-1.5),
                                                                   ifelse(is.na(POLLSTER_BIAS),PARTISAN_BIAS,PARTISAN_BIAS+POLLSTER_BIAS))) %>%
                                        left_join(.,adv_score_miss_replace,by="cycle") %>%
                                        mutate(DEM_adj = DEM + .5*adjustment,
                                               GOP_adj = REP - .5*adjustment,
                                               adv_score = ifelse(is.na(adv_score),adv_score_replace,adv_score)) %>%
                                        mutate(sample_size_score = sqrt(sample_size/600),
                                               pollster_adv_score = ((adv_score - min(pollster_bias$adv_score)) / (max(pollster_bias$adv_score) - min(pollster_bias$adv_score)) - 1) * -1 + 0.5,
                                               time_score = exp(-0.01 * days_to_election),
                                               penalty = ifelse(partisan == "",1,1-0.2)) %>%
                                        mutate(theWeight = time_score * sample_size_score * pollster_adv_score * penalty) %>%
                                        group_by(cycle,District) %>%
                                        summarise(DEM_POLL = round(weighted.mean(DEM_adj,theWeight),2),
                                                  GOP_POLL = round(weighted.mean(GOP_adj,theWeight),2),
                                                  total_weight = sum(theWeight)) %>%
                                        select(cycle,District,DEM_POLL,GOP_POLL,total_weight)


predicted_vote = lean_calc %>% left_join(.,thepolls_calc,by=c("cycle","District")) %>%
                               mutate(poll_weight = ifelse(is.na(total_weight),NA,
                                                           ifelse(total_weight>=3.5,0,3.5-total_weight))) %>%
                               mutate(theWeight = ifelse(is.na(poll_weight),0,0.9*exp(-0.015*poll_weight))) %>%
                               mutate(DEM_PRED = ifelse(theWeight==0,round(DEM_LEAN,2),round(theWeight * DEM_POLL + (1-theWeight) * DEM_LEAN,2)),
                                      GOP_PRED = ifelse(theWeight==0,round(GOP_LEAN,2),round(theWeight * GOP_POLL + (1-theWeight) * GOP_LEAN,2))) %>%
                               select(cycle,District,DEM_PRED,GOP_PRED) %>%
                               mutate(PRED_DIFF = GOP_PRED - DEM_PRED) %>%
                               rename("year"="cycle")


#join everything together to create the training dataset
train = election_results %>% left_join(.,predicted_vote,by=c("year","District")) %>%
                             left_join(.,census,by=c("year","District")) %>%
                             left_join(.,party_ind %>% mutate(District = paste(STATE,"-",str_pad(DISTRICT,2,pad="0"),sep="")) %>%
                                                       select(year,District,DEM_IND,REP_IND,LBT_IND,GRN_IND,IND_IND) %>%
                                                       rename("GOP_IND" = "REP_IND"),
                                       by=c("year","District")) %>%
                             left_join(.,leans %>% select(year,District,LEAN_1),by=c("year","District")) %>%
                             left_join(.,gb_acts %>% rename("year"="cycle",
                                                            "GOP_GB"="GOP_act",
                                                            "DEM_GB"="DEM_act",
                                                            "GB_DIFF"="diff_act"),
                                       by=c("year")) %>%
                             left_join(.,fec_data %>% filter(CAND_PTY_AFFILIATION=="DEM") %>%
                                                      select(FEC_ID,year,TOTAL_RECEIPTS,TOTAL_DISBURSEMENTS,
                                                             COH_COP,TOTAL_INDIVIDUAL_CONTRIBUTIONS,PARTY_CONTRIB,INDIV_REFUNDS) %>%
                                                      rename("FEC_ID_DEM" = "FEC_ID",
                                                             "TOTAL_RECEIPTS_DEM" = "TOTAL_RECEIPTS",
                                                             "TOTAL_DISBURSEMENTS_DEM" = "TOTAL_DISBURSEMENTS",
                                                             "COH_DEM" = "COH_COP",
                                                             "INDIVIDUAL_CONTRIBUTIONS_DEM" = "TOTAL_INDIVIDUAL_CONTRIBUTIONS",
                                                             "PARTY_CONTRIBUTIONS_DEM" = "PARTY_CONTRIB",
                                                             "INDIVIDUAL_REFUNDS_DEM" = "INDIV_REFUNDS"),
                                       by=c("year","FEC_ID_DEM")) %>%
                             left_join(.,fec_data %>% filter(CAND_PTY_AFFILIATION=="REP") %>%
                                                      select(FEC_ID,year,TOTAL_RECEIPTS,TOTAL_DISBURSEMENTS,
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
                                     INDIVIDUAL_REFUNDS_GOP = ifelse(is.na(INDIVIDUAL_REFUNDS_GOP),0,INDIVIDUAL_REFUNDS_GOP))

write.csv(train,"Data/Datasets/train.csv",row.names=F)





# ------------------------------------------    BUILD TEST BASE DATASET   ------------------------------------------ #

candidates2022 = read.csv("Data/2022 Candidates File/candidate_list.csv",header=T,stringsAsFactors=F)
party_ind_2022 = read.csv("Data/2022 Candidates File/party_ind_data.csv",header=T,stringsAsFactors=F)

#Check which candidates are missing from FEC data
fec_miss = candidates2022 %>% left_join(.,fec_data %>% select(-c("STATE","DISTRICT")),by=c("year","FEC_ID")) %>%
                              filter(is.na(CAND_NAME))

fec_low = candidates2022 %>% left_join(.,fec_data %>% select(-c("STATE","DISTRICT")),by=c("year","FEC_ID")) %>%
                             filter(!is.na(CAND_NAME)) %>%
                             filter(TOTAL_RECEIPTS<15000)

dem_cand_2022 = candidates2022 %>% filter(PARTY=="D") %>%
                                   mutate(District = paste(STATE,"-",str_pad(DISTRICT,2,pad="0"),sep="")) %>%
                                   rename("FEC_ID_DEM"="FEC_ID",
                                          "FIRST_NAME_DEM"="FIRST_NAME",
                                          "LAST_NAME_DEM"="LAST_NAME",
                                          "INCUMBENT_IND_DEM"="INCUMBENT_IND") %>%
                                    select(year,District,FEC_ID_DEM,FIRST_NAME_DEM,LAST_NAME_DEM,INCUMBENT_IND_DEM)

dem_districts_to_model = dem_cand_2022 %>% group_by(year,District) %>% summarise(count=n()) %>% filter(count==1)
dem_wins = dem_cand_2022 %>% group_by(year,District) %>% summarise(INCUMBENT_IND_DEM = max(INCUMBENT_IND_DEM),count=n()) %>% filter(count>1) %>%
                             select(-count) %>% mutate(FIRST_NAME_DEM = "",
                                                       LAST_NAME_DEM = "A DEMOCRAT",
                                                       INCUMBENT_IND_GOP = 0,
                                                       FIRST_NAME_GOP = "",
                                                       LAST_NAME_GOP = "",
                                                       GOP_win = 0)

rep_cand_2022 = candidates2022 %>% filter(PARTY=="R") %>%
                                   mutate(District = paste(STATE,"-",str_pad(DISTRICT,2,pad="0"),sep="")) %>%
                                   rename("FEC_ID_GOP"="FEC_ID",
                                          "FIRST_NAME_GOP"="FIRST_NAME",
                                          "LAST_NAME_GOP"="LAST_NAME",
                                          "INCUMBENT_IND_GOP"="INCUMBENT_IND") %>%
                                   select(year,District,FEC_ID_GOP,FIRST_NAME_GOP,LAST_NAME_GOP,INCUMBENT_IND_GOP)

rep_districts_to_model = rep_cand_2022 %>% group_by(year,District) %>% summarise(count=n()) %>% filter(count==1)
rep_wins = rep_cand_2022 %>% group_by(year,District) %>% summarise(count=n()) %>% filter(count>1)


districts_for_model = dem_districts_to_model %>% select(-count) %>%
                                                 inner_join(.,rep_districts_to_model %>% select(-count),by=c("year","District")) %>% 
                                                 left_join(.,dem_cand_2022,by=c("year","District")) %>% 
                                                 left_join(.,rep_cand_2022,by=c("year","District"))


default_wins = candidates2022 %>% mutate(District = paste(STATE,"-",str_pad(DISTRICT,2,pad="0"),sep="")) %>%
                                  filter(District %in% setdiff(sort(unlist(candidates2022 %>% mutate(District = paste(STATE,"-",str_pad(DISTRICT,2,pad="0"),sep="")) %>% select(District) %>% distinct())),
                                                               c(districts_for_model$District,dem_wins$District))) %>%
                                  mutate(FIRST_NAME_DEM = ifelse(PARTY=="D",FIRST_NAME,""),
                                         LAST_NAME_DEM = ifelse(PARTY=="D",LAST_NAME,""),
                                         INCUMBENT_IND_DEM = ifelse(PARTY=="D",INCUMBENT_IND,0),
                                         FIRST_NAME_GOP = ifelse(PARTY=="R",FIRST_NAME,""),
                                         LAST_NAME_GOP = ifelse(PARTY=="R",LAST_NAME,""),
                                         INCUMBENT_IND_GOP = ifelse(PARTY=="R",INCUMBENT_IND,0),
                                         GOP_win = ifelse(PARTY=="D",0,1)) %>%
                                  select(year,District,INCUMBENT_IND_DEM,FIRST_NAME_DEM,LAST_NAME_DEM,
                                         INCUMBENT_IND_GOP,FIRST_NAME_GOP,LAST_NAME_GOP,GOP_win)

#stack and write out all default wins
all_default_wins = rbind(default_wins,dem_wins)
write.csv(all_default_wins,"Data/Datasets/default_wins.csv",row.names = F)


#join data to create test base for static variables
test_base = districts_for_model %>% left_join(.,census,by=c("year","District")) %>%
                                    left_join(.,party_ind_2022 %>% mutate(District = paste(STATE,"-",str_pad(DISTRICT,2,pad="0"),sep="")) %>%
                                                                   select(year,District,DEM_IND,REP_IND,LBT_IND,GRN_IND,IND_IND) %>%
                                                                   rename("GOP_IND" = "REP_IND"),
                                              by=c("year","District")) %>%
                                    left_join(.,leans %>% select(year,District,LEAN_1),by=c("year","District"))


#see how many points to sampe in normal distribution until it evens out
sample_norm = NA
for(i in seq(from=100,to=10000,by=100)){
  temp = NA
  for(j in seq(from=1,to=100,by=1)){
    if(j==1){
      temp = mean(rnorm(n=i,mean=0,sd=1))
    } else {
      temp = c(temp,mean(rnorm(n=i,mean=0,sd=1)))
    }
  }
  
  if(i==100){
    sample_norm = as.data.frame(cbind(i,mean(temp),sd(temp)))
  } else {
    sample_norm = rbind(sample_norm,as.data.frame(cbind(i,mean(temp),sd(temp))))
  }
}
colnames(sample_norm) = c("sample_size","mean","sd")

ggplot(data=sample_norm,aes(x=sample_size,y=sd)) +
  geom_line() +
  geom_point() + 
  theme_minimal() +
  ggtitle("SD Change with Sample Size")

ggplot(data=sample_norm,aes(x=sample_size,y=mean)) +
  geom_line() +
  geom_point() + 
  theme_minimal() +
  ggtitle("Mean Change with Sample Size")

##Standard deviation appears quite stable by the time 10,000 is reached... will use that number of test cases

plot(density(rnorm(n=15000,mean=0,sd=1)))
##Curves look really smooth by 15000

#Create the base dataset from which to construct model on
set.seed(123)
gb_rnorms = rnorm(n=15000,mean=0,sd=1)
und_rnorms = rnorm(n=15000,mean=0,sd=1)
holder = NA
final_test_base = NA
subset_found = FALSE
subset = NA
subset1 = NA
subset2 = NA
subset3 = NA
for(i in seq(from=1,to=15000,by=1)){
  cat(i,"\n",sep="")
  temp = test_base %>% mutate(gb_rnorm = gb_rnorms[i],
                              und_rnorm = und_rnorms[i],
                              poll_rnorm = rnorm(n=400,mean=gb_rnorms[i],sd=1),
                              scenario = i)
  
  if(i%%250==1){
    holder = temp
  } else {
    holder = rbind(holder,temp)
  }
  
  #Stack results 
  if(i%%250==0){
    if(subset_found==FALSE){
      subset = holder
      subset_found = TRUE
    } else {
      subset = rbind(subset,holder)
    }
    holder = NA
  }
  
  #Create subset
  if(i==5000){
    subset1 = subset
    subset = NA
    subset_found = FALSE
  }
  
  if(i==10000){
    subset2 = subset
    subset = NA
    subset_found = FALSE
  }
  
  if(i==15000){
    subset3 = subset
    subset = NA
    subset_found = FALSE
  }
}
final_test_base = rbind(subset1,subset2,subset3)

write.csv(final_test_base,"Data/Datasets/test_base.csv",row.names = F)


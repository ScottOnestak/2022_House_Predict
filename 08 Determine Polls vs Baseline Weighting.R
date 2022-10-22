#2022 Election Project
#Author: Scott Onestak
#8 Determine Polls vs Baseline Weighting

#libraries
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(fredr)

options(scipen = 100)

#read in house polls data
house_polls = read.csv("Data/Polls/House Polls/house_polls_historical.csv",header=T,stringsAsFactors=F)

#read in pollster ratings
pollster_bias = read.csv('Data/Polls/For Pollster Ratings/forModeling/pollster_bias.csv',header=T,stringsAsFactors=F)
partisan_bias = read.csv('Data/Polls/For Pollster Ratings/forModeling/partisan_bias.csv',header=T,stringsAsFactors=F)
avg_bias = read.csv('Data/Polls/For Pollster Ratings/forModeling/avg_bias.csv',header=T,stringsAsFactors=F)
colnames(pollster_bias)[1] = "cycle"
colnames(partisan_bias)[1] = "cycle"
colnames(avg_bias)[1] = "cycle"

#read in generic ballot
gb_acts = read.csv('Data/Polls/Generic Ballot/generic_ballot_actuals.csv',header=T,stringsAsFactors=F)
colnames(gb_acts) = c("cycle","GOP_act","DEM_act","diff_act")
gb_polls = read.csv('Data/Polls/Generic Ballot/generic_ballot_historic.csv',header=T,stringsAsFactors=F)
gb_polls$start_date = as.Date(gb_polls$start_date,"%Y-%m-%d")
gb_polls$end_date = as.Date(gb_polls$end_date,"%Y-%m-%d")
gb_polls$election_date = as.Date(gb_polls$election_date,"%Y-%m-%d")

#read in lean data
leans = read.csv('Data/Historical Leans/Leans.csv',header=T,stringsAsFactors=F)
colnames(leans)[1] = "year"

#read in election results
party_ind = read.csv('Data/Election Results/Cleaned Results/party_ind_data.csv',header=T,stringsAsFactors=F)
election_results_raw = read.csv('Data/Election Results/Cleaned Results/votes_cleaned_data.csv',header=T,stringsAsFactors=F) %>%
                          filter(year >= 2006 & runoff == FALSE)

#read in House polls
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

#Build actual results for CDs that have both a Democrat and Republican on the ballot
#For jungle generals some southern states have, group all Republican and Democrat votes together
dem_cand = election_results_raw %>% filter(PARTY == "D") %>%
              group_by(year,STATE,DISTRICT) %>%
              summarise(count=n())

rep_cand = election_results_raw %>% filter(PARTY == "R") %>%
              group_by(year,STATE,DISTRICT) %>%
              summarise(count=n())

election_results = election_results_raw %>% filter(year>=2010) %>% inner_join(.,dem_cand %>% select(-count),by=c("year","STATE","DISTRICT")) %>%
                    inner_join(.,rep_cand %>% select(-count),by=c("year","STATE","DISTRICT")) %>%
                    group_by(year,STATE,DISTRICT,PARTY) %>%
                    summarise(VOTES = sum(candidate_votes,na.rm=T)) %>%
                    spread(.,key=PARTY,value=VOTES) %>%
                    ungroup() %>%
                    left_join(.,election_results_raw %>% select(year,STATE,DISTRICT,total_votes) %>% distinct(),by=c("year","STATE","DISTRICT")) %>%
                    mutate(DEM_act = round(D / total_votes * 100,2),
                           GOP_act = round(R / total_votes * 100,2)) %>%
                    mutate(District = paste(STATE,"-",str_pad(DISTRICT,2,pad="0"),sep="")) %>%
                    select(year,District,DEM_act,GOP_act)


#Build generic ballot across time
gb_polls$mid_date = NA
gb_polls$days_to_election = NA
for(i in seq(from=1,to=dim(gb_polls)[1],by=1)){
  gb_polls[i,"mid_date"] = mean.Date(c(gb_polls[i,"start_date"],gb_polls[i,"end_date"]))
  gb_polls[i,"days_to_election"] = as.numeric(gb_polls[i,"election_date"] - gb_polls[i,"mid_date"])
}

theYears = c(2010,2012,2014,2016,2018,2020)
started = FALSE
gb_build = NA
for(i in seq(from=1,to=length(theYears),by=1)){
  cat(theYears[i],"\n",sep="")
  election_date = unlist(gb_polls %>% filter(cycle == theYears[i]) %>% select(election_date) %>% distinct())
  start_date = election_date - 250
  end_date = election_date - 1
  holder = gb_polls %>% filter(cycle==theYears[i] & population %in% c("LV","RV") & !is.na(sample_size))
  for(j in seq(from=start_date,to=end_date,by=1)){
    temp = holder %>%
              mutate(days_to_curr = j - mid_date) %>%
              filter(days_to_curr >= 0) %>% 
              arrange(desc(mid_date),pollster,population) %>%
              group_by(pollster) %>%
              filter(row_number()==1) %>%
              left_join(.,pollster_bias %>% select(-count),by=c("cycle","pollster")) %>%
              left_join(.,avg_bias,by="cycle") %>%
              mutate(adjustment = ifelse(population == "RV",
                                         ifelse(is.na(POLLSTER_BIAS),avg_bias-1.5,POLLSTER_BIAS-1.5),
                                         ifelse(is.na(POLLSTER_BIAS),avg_bias,POLLSTER_BIAS))) %>%
              mutate(DEM_adj = DEM + .5*adjustment,
                     GOP_adj = GOP - .5*adjustment,
                     adv_score = replace_na(adv_score,quantile(unlist(pollster_bias %>% filter(cycle==theYears[i]) %>% select(adv_score)),1))) %>%
              mutate(sample_size_score = sqrt(sample_size/600),
                     pollster_adv_score = ((adv_score - min(pollster_bias$adv_score)) / (max(pollster_bias$adv_score) - min(pollster_bias$adv_score)) - 1) * -1 + 0.5,
                     time_score = exp(-0.1 * days_to_curr)) %>%
              mutate(theWeight = time_score * sample_size_score * pollster_adv_score)
    
    mean = temp %>%
              group_by(cycle) %>%
              summarise(DEM = round(weighted.mean(DEM_adj,theWeight),2),
                        GOP = round(weighted.mean(GOP_adj,theWeight),2),
                        count = n())
    
    moe = temp %>%
            mutate(DEM_SD = sqrt(((DEM/100)*(1-DEM/100))/sample_size)*100,
                   GOP_SD = sqrt(((GOP/100)*(1-GOP/100))/sample_size)*100) %>%
            select(pollster,cycle,start_date,end_date,mid_date,sample_size,theWeight,DEM_adj,GOP_adj,DEM_SD,GOP_SD) %>%
            left_join(.,mean %>% select(-count) %>% rename("DEM_X" = "DEM", "GOP_X" = "GOP"),by="cycle") %>%
            mutate(DEM_num = theWeight * (DEM_SD^2 + (DEM_adj - DEM_X)^2),
                   GOP_num = theWeight * (GOP_SD^2 + (GOP_adj - GOP_X)^2)) %>%
            group_by(cycle) %>%
            summarise(DEM_num = sum(DEM_num),
                      GOP_num = sum(GOP_num),
                      theWeight = sum(theWeight)) %>%
            mutate(DEM_SD = round(sqrt(DEM_num/theWeight),2),
                   GOP_SD = round(sqrt(GOP_num/theWeight),2)) %>%
            mutate(DEM_MOE = round(1.96 * DEM_SD,2),
                   GOP_MOE = round(1.96 * GOP_SD,2)) %>%
            select(cycle,DEM_SD,GOP_SD,DEM_MOE,GOP_MOE)
      
    theJoin = mean %>% left_join(.,moe,by="cycle") %>%
                mutate(DEM_min = round(DEM - DEM_MOE,2),
                       DEM_max = round(DEM + DEM_MOE,2),
                       GOP_min = round(GOP - GOP_MOE,2),
                       GOP_max = round(GOP + GOP_MOE,2),
                       days_to_election = election_date - j) %>%
                select(cycle,days_to_election,DEM,GOP,DEM_SD,GOP_SD,count,DEM_min,DEM_max,GOP_min,GOP_max) %>%
                mutate(diff = GOP - DEM,
                       diff_min = GOP_min - DEM_max,
                       diff_max = GOP_max - DEM_min) %>%
                inner_join(.,gb_acts,by="cycle") %>%
                mutate(miss = diff - diff_act,
                       abs_miss = abs(diff - diff_act))
    
    if(started==FALSE){
      gb_build = theJoin
      started = TRUE
    } else {
      gb_build = rbind(gb_build,theJoin)
    }
  }
}

#Write out the generic ballots to the folder
test = gb_build %>% filter(cycle == 2010)
png(filename = "Plots/Generic Ballot/Generic_Ballot_2010.png",width = 1920,height = 1080)
ggplot(test) +
  geom_line(aes(x=days_to_election,y=DEM),color="blue") + 
  geom_ribbon(aes(x=days_to_election,ymin=DEM_min,ymax=DEM_max),alpha=0.2,fill="blue",size=5) +
  geom_line(aes(x=days_to_election,y=GOP),color="red") +
  geom_ribbon(aes(x=days_to_election,ymin=GOP_min,ymax=GOP_max),alpha=0.2,fill="red",size=5) +
  scale_x_reverse(name="Days to Election") + 
  scale_y_continuous(name="Generic Ballot",limits=c(20,70)) +
  ggtitle("GENERIC BALLOT: 2010") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,size=18,face="bold"),
        axis.text = element_text(size=10),
        axis.title = element_text(size=15))
dev.off()

test = gb_build %>% filter(cycle == 2012)
png(filename = "Plots/Generic Ballot/Generic_Ballot_2012.png",width = 1920,height = 1080)
ggplot(test) +
  geom_line(aes(x=days_to_election,y=DEM),color="blue") + 
  geom_ribbon(aes(x=days_to_election,ymin=DEM_min,ymax=DEM_max),alpha=0.2,fill="blue",size=5) +
  geom_line(aes(x=days_to_election,y=GOP),color="red") +
  geom_ribbon(aes(x=days_to_election,ymin=GOP_min,ymax=GOP_max),alpha=0.2,fill="red",size=5) +
  scale_x_reverse(name="Days to Election") + 
  scale_y_continuous(name="Generic Ballot",limits=c(20,70)) +
  ggtitle("GENERIC BALLOT: 2012") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,size=18,face="bold"),
        axis.text = element_text(size=10),
        axis.title = element_text(size=15))
dev.off()

test = gb_build %>% filter(cycle == 2014)
png(filename = "Plots/Generic Ballot/Generic_Ballot_2014.png",width = 1920,height = 1080)
ggplot(test) +
  geom_line(aes(x=days_to_election,y=DEM),color="blue") + 
  geom_ribbon(aes(x=days_to_election,ymin=DEM_min,ymax=DEM_max),alpha=0.2,fill="blue",size=5) +
  geom_line(aes(x=days_to_election,y=GOP),color="red") +
  geom_ribbon(aes(x=days_to_election,ymin=GOP_min,ymax=GOP_max),alpha=0.2,fill="red",size=5) +
  scale_x_reverse(name="Days to Election") + 
  scale_y_continuous(name="Generic Ballot",limits=c(20,70)) +
  ggtitle("GENERIC BALLOT: 2014") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,size=18,face="bold"),
        axis.text = element_text(size=10),
        axis.title = element_text(size=15))
dev.off()

test = gb_build %>% filter(cycle == 2016)
png(filename = "Plots/Generic Ballot/Generic_Ballot_2016.png",width = 1920,height = 1080)
ggplot(test) +
  geom_line(aes(x=days_to_election,y=DEM),color="blue") + 
  geom_ribbon(aes(x=days_to_election,ymin=DEM_min,ymax=DEM_max),alpha=0.2,fill="blue",size=5) +
  geom_line(aes(x=days_to_election,y=GOP),color="red") +
  geom_ribbon(aes(x=days_to_election,ymin=GOP_min,ymax=GOP_max),alpha=0.2,fill="red",size=5) +
  scale_x_reverse(name="Days to Election") + 
  scale_y_continuous(name="Generic Ballot",limits=c(20,70)) +
  ggtitle("GENERIC BALLOT: 2016") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,size=18,face="bold"),
        axis.text = element_text(size=10),
        axis.title = element_text(size=15))
dev.off()

test = gb_build %>% filter(cycle == 2018)
png(filename = "Plots/Generic Ballot/Generic_Ballot_2018.png",width = 1920,height = 1080)
ggplot(test) +
  geom_line(aes(x=days_to_election,y=DEM),color="blue") + 
  geom_ribbon(aes(x=days_to_election,ymin=DEM_min,ymax=DEM_max),alpha=0.2,fill="blue",size=5) +
  geom_line(aes(x=days_to_election,y=GOP),color="red") +
  geom_ribbon(aes(x=days_to_election,ymin=GOP_min,ymax=GOP_max),alpha=0.2,fill="red",size=5) +
  scale_x_reverse(name="Days to Election") + 
  scale_y_continuous(name="Generic Ballot",limits=c(20,70)) +
  ggtitle("GENERIC BALLOT: 2018") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,size=18,face="bold"),
        axis.text = element_text(size=10),
        axis.title = element_text(size=15))
dev.off()

test = gb_build %>% filter(cycle == 2020)
png(filename = "Plots/Generic Ballot/Generic_Ballot_2020.png",width = 1920,height = 1080)
ggplot(test) +
  geom_line(aes(x=days_to_election,y=DEM),color="blue") + 
  geom_ribbon(aes(x=days_to_election,ymin=DEM_min,ymax=DEM_max),alpha=0.2,fill="blue",size=5) +
  geom_line(aes(x=days_to_election,y=GOP),color="red") +
  geom_ribbon(aes(x=days_to_election,ymin=GOP_min,ymax=GOP_max),alpha=0.2,fill="red",size=5) +
  scale_x_reverse(name="Days to Election") + 
  scale_y_continuous(name="Generic Ballot",limits=c(20,70)) +
  ggtitle("GENERIC BALLOT: 2020") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,size=18,face="bold"),
        axis.text = element_text(size=10),
        axis.title = element_text(size=15))
dev.off()

#Races don't look like they truly start to take shape until about 75 to 100 days out from an election (roughly August)
#Will not use polls prior to 100 days from the election because of this... the August polls will also probably be so old that they won't have much effect in the weighting


#Determine the effect of 3rd parties/missing DEM-REP candidates on the generic ballot total
third_party = gb_acts %>% mutate(total = GOP_act + DEM_act) %>% select(cycle,total) %>%
                          left_join(.,party_ind %>% mutate(third_party_ind = ifelse(LBT_IND == 1 | GRN_IND == 1 | DEM_IND == 0 | REP_IND == 0,1,0)) %>%
                                                    rename("cycle"="year") %>%
                                                    group_by(cycle) %>%
                                                    summarise(third_party_ind = sum(third_party_ind),
                                                              cds = n()) %>%
                                                    mutate(third_party = third_party_ind/cds*100),
                                    by="cycle")

third_lm = lm(total ~ third_party,data=third_party)
summary(third_lm)

third_party$predict = predict(third_lm,third_party)
plot(third_party$third_party,third_party$total)
lines(third_party$third_party,third_party$predict,col='blue')
#Actually a quite stong relationship between 3rd parties/missing DEM-REP candiates on the sum of the DEM-REP vote (R^2 = 0.62)
#Can use to predict 2022 total DEM-REP vote and predict undecideds

#Try to build a baseline generic ballot value
#Variables: Generic Ballot Polling Averages
#           Party in Power
#           House Election Polling (Including/Excluding Partisan Polls for 1 and 2 cycle leans)
#           Economic Variables

#Get Economic Variables
fredr_set_key("1e7ed343d3ccb82af108e43174bf8f1f")

median_real_earnings = fredr(series_id = "LES1252881600Q",
                             observation_start = as.Date("2000-01-01",format="%Y-%m-%d"),
                             frequency = "sa", units = "pc1") %>% select(date,value) %>% rename(median_real_earnings=value) %>%
  filter(str_detect(date,"-01-")) %>%
  mutate(cycle = substr(date,1,4))
median_real_earnings$cycle = as.numeric(median_real_earnings$cycle)

unemployment = fredr(series_id = "UNRATE",
                     observation_start = as.Date("2000-01-01",format="%Y-%m-%d"),
                     frequency = "sa") %>% select(date,value) %>% rename(unemployment=value) %>%
  filter(str_detect(date,"-01-")) %>%
  mutate(cycle = substr(date,1,4))
unemployment$cycle = as.numeric(unemployment$cycle)

gas = fredr(series_id = "APU000074714",
            observation_start = as.Date("2000-01-01",format="%Y-%m-%d"),
            frequency = "sa") %>% select(date,value) %>% rename(gas_price=value) %>%
  filter(str_detect(date,"-01-")) %>%
  mutate(cycle = substr(date,1,4))
gas$cycle = as.numeric(gas$cycle)

#Create dataset
addon = as.data.frame(cbind(2022,NA,NA,NA))
colnames(addon) = colnames(gb_acts)
forGBmodel = rbind(gb_acts,addon) %>%
                left_join(.,median_real_earnings %>% select(-date),by="cycle") %>%
                left_join(.,unemployment %>% select(-date),by="cycle") %>%
                left_join(.,gas %>% select(-date),by="cycle") %>%
                mutate(party_in_power = ifelse(cycle %in% c(2004,2006,2008,2018,2020),"R","D"),
                       midterm = ifelse(cycle %% 4 == 0,0,1))
forGBmodel$party_in_power = as.factor(forGBmodel$party_in_power)
forGBmodel$midterm = as.factor(forGBmodel$midterm)

train = forGBmodel %>% filter(cycle <= 2020)
test = forGBmodel %>% filter(cycle > 2020)

GOP_base_lm = lm(GOP_act ~ median_real_earnings + unemployment + gas_price + party_in_power + midterm + party_in_power * midterm,data=train)
summary(GOP_base_lm)

DEM_base_lm = lm(DEM_act ~ median_real_earnings + unemployment + gas_price + party_in_power + midterm + party_in_power * midterm,data=train)
summary(DEM_base_lm)

predict(GOP_base_lm,test)
predict(DEM_base_lm,test)
#Results aren't very informative and didn't work that well during backtesting
#Just simply use generic ballot with undecides allocated

#Determine how to allocate undecides in the generic ballot
gb_miss_by_days_to = gb_build %>% filter(days_to_election <= 100) %>%
                        mutate(party_in_power = ifelse(cycle %in% c(2004,2006,2008,2018,2020),"R","D"),
                               midterm = ifelse(cycle %% 4 == 0,0,1),
                               GOP_undecideds = GOP_act - GOP,
                               DEM_Undecideds = DEM_act - DEM) %>%
                        mutate(GOP_und_prct = round(GOP_undecideds / (GOP_undecideds + DEM_Undecideds),2)*100,
                               DEM_und_prct = round(DEM_Undecideds / (GOP_undecideds + DEM_Undecideds),2)*100)
gb_miss_by_days_to$party_in_power = as.factor(gb_miss_by_days_to$party_in_power)
gb_miss_by_days_to$midterm = as.factor(gb_miss_by_days_to$midterm)
gb_miss_by_days_to$cycle = as.factor(gb_miss_by_days_to$cycle)

ggplot(gb_miss_by_days_to %>% filter(days_to_election <= 30),aes(x=days_to_election,y=GOP_und_prct)) +
  geom_line(aes(color=cycle)) +
  scale_x_reverse(name="Days to Election")
#No real discernable pattern to model
#ASSUMPTION: 2022 will behave similarly to 2010 and 2014 midterms with GOP out of power... ~60% of undecideds break GOP
#            Center distribution about that 60% metric

#Compare different methods of predicting vote share

##Look at how much can be explained away through the district lean + generic ballot
lean1_gb_by_year = election_results %>% mutate(DIFF = GOP_act - DEM_act) %>%
                                        left_join(.,leans %>% select(year,District,LEAN_1),by=c("year","District")) %>%
                                        rename("cycle"="year") %>%
                                        left_join(.,gb_acts %>% select(cycle,diff_act),by="cycle") %>%
                                        mutate(PRED = LEAN_1 + diff_act) %>%
                                        mutate(res = (DIFF-PRED)^2,
                                               TSS = (DIFF-diff_act)^2) %>%
                                        group_by(cycle) %>%
                                        summarise(RSS = sum(res),
                                                  TSS = sum(TSS),
                                                  count = n()) %>%
                                        mutate(R2 = 1 - (RSS/TSS),
                                               type = "LEAN_1 + GB")
lean1_gb = lean1_gb_by_year %>% summarise(RSS = sum(RSS),
                                          TSS = sum(TSS),
                                          avg_R2 = mean(R2),
                                          count = sum(count)) %>%
                                mutate(R2 = 1 - (RSS/TSS),
                                       type = "LEAN_1 + GB")

lean1_comp_gb_by_year = election_results %>% mutate(DIFF = GOP_act - DEM_act) %>%
                                             filter(abs(DIFF) <= 10) %>%
                                             left_join(.,leans %>% select(year,District,LEAN_1),by=c("year","District")) %>%
                                             rename("cycle"="year") %>%
                                             left_join(.,gb_acts %>% select(cycle,diff_act),by="cycle") %>%
                                             mutate(PRED = LEAN_1 + diff_act) %>%
                                             mutate(res = (DIFF-PRED)^2,
                                                    TSS = (DIFF-diff_act)^2) %>%
                                             group_by(cycle) %>%
                                             summarise(RSS = sum(res),
                                                       TSS = sum(TSS),
                                                       count = n()) %>%
                                             mutate(R2 = 1 - (RSS/TSS),
                                                    type = "LEAN_1 + GB")
lean1_comp_gb = lean1_comp_gb_by_year %>% summarise(RSS = sum(RSS),
                                                    TSS = sum(TSS),
                                                    avg_R2 = mean(R2),
                                                    count = sum(count)) %>%
                                          mutate(R2 = 1 - (RSS/TSS),
                                                 type = "LEAN_1 + GB")

lean2_gb_by_year = election_results %>% mutate(DIFF = GOP_act - DEM_act) %>%
                                        left_join(.,leans %>% select(year,District,LEAN_1,LEAN_2),by=c("year","District")) %>%
                                        rename("cycle"="year") %>%
                                        left_join(.,gb_acts %>% select(cycle,diff_act),by="cycle") %>%
                                        mutate(PRED = ifelse(!is.na(LEAN_1) & !is.na(LEAN_2),(LEAN_1 + LEAN_2)/2 + diff_act,
                                                             ifelse(!is.na(LEAN_1),LEAN_1 + diff_act,LEAN_2 + diff_act))) %>%
                                        mutate(res = (DIFF-PRED)^2,
                                               TSS = (DIFF-diff_act)^2) %>%
                                        group_by(cycle) %>%
                                        summarise(RSS = sum(res),
                                                  TSS = sum(TSS),
                                                  count = n()) %>%
                                        mutate(R2 = 1 - (RSS/TSS),
                                               type = "LEAN_1 + LEAN_2 + GB")
lean2_gb = lean2_gb_by_year %>% summarise(RSS = sum(RSS),
                                          TSS = sum(TSS),
                                          avg_R2 = mean(R2),
                                          count = sum(count)) %>%
                                mutate(R2 = 1 - (RSS/TSS),
                                       type = "LEAN_1 + LEAN_2 + GB")

lean2_comp_gb_by_year = election_results %>%  mutate(DIFF = GOP_act - DEM_act) %>%
                                              filter(abs(DIFF) <= 10) %>%
                                              left_join(.,leans %>% select(year,District,LEAN_1,LEAN_2),by=c("year","District")) %>%
                                              rename("cycle"="year") %>%
                                              left_join(.,gb_acts %>% select(cycle,diff_act),by="cycle") %>%
                                              mutate(PRED = ifelse(!is.na(LEAN_1) & !is.na(LEAN_2),(LEAN_1 + LEAN_2)/2 + diff_act,
                                                                   ifelse(!is.na(LEAN_1),LEAN_1 + diff_act,LEAN_2 + diff_act))) %>%
                                              mutate(res = (DIFF-PRED)^2,
                                                     TSS = (DIFF-diff_act)^2) %>%
                                              group_by(cycle) %>%
                                              summarise(RSS = sum(res),
                                                        TSS = sum(TSS),
                                                        count = n()) %>%
                                              mutate(R2 = 1 - (RSS/TSS),
                                                     type = "LEAN_1 + LEAN_2 + GB")
lean2_comp_gb = lean2_comp_gb_by_year %>% summarise(RSS = sum(RSS),
                                                    TSS = sum(TSS),
                                                    avg_R2 = mean(R2),
                                                    count = sum(count)) %>%
                                          mutate(R2 = 1 - (RSS/TSS),
                                                 type = "LEAN_1 + LEAN_2 + GB")
###Using LEAN_1 and LEAN_2 is roughly even overall, but performance is better with just LEAN_1 when more metrics are added
###Will use LEAN_1 only going forward

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
                                           year2 = year - 4,
                                           FEC_ID_MATCH = ifelse(INCUMBENT_IND_GOP==1,FEC_ID_GOP,FEC_ID_DEM)) %>%
                                    rename("cycle"="year") %>%
                                    left_join(.,incumbent_temp %>% mutate(year1 = year) %>%
                                                                   left_join(.,leans %>% select(year,District,LEAN_1),by=c("year","District")) %>%
                                                                   rename("cycle"="year") %>%
                                                                   left_join(.,gb_acts %>% select(cycle,diff_act),by="cycle") %>%
                                                                   mutate(PRED = LEAN_1 + diff_act) %>%
                                                                   mutate(COMP1 = DIFF - PRED,
                                                                          FEC_ID_MATCH = ifelse(DIFF > 0,FEC_ID_GOP,FEC_ID_DEM)) %>%
                                                                   select(year1,FEC_ID_MATCH,COMP1),
                                              by=c("year1","FEC_ID_MATCH")) %>%
                                    left_join(.,incumbent_temp %>% mutate(year2 = year) %>%
                                                left_join(.,leans %>% select(year,District,LEAN_1),by=c("year","District")) %>%
                                                rename("cycle"="year") %>%
                                                left_join(.,gb_acts %>% select(cycle,diff_act),by="cycle") %>%
                                                mutate(PRED = LEAN_1 + diff_act) %>%
                                                mutate(COMP2 = DIFF - PRED,
                                                       FEC_ID_MATCH = ifelse(DIFF > 0,FEC_ID_GOP,FEC_ID_DEM)) %>%
                                                select(year2,FEC_ID_MATCH,COMP2),
                                              by=c("year2","FEC_ID_MATCH")) %>%
                                    filter(!is.na(COMP1) | !is.na(COMP2)) #Sometimes incumbent because nobody ran against them prior or special election... so no comparable results
                    
###Test out some different weightings 

###Use COMP1 if available... if not, use COMP2
lean_gb_comp1_by_year = election_results %>% mutate(DIFF = GOP_act - DEM_act) %>%
                                              left_join(.,leans %>% select(year,District,LEAN_1),by=c("year","District")) %>%
                                              rename("cycle"="year") %>%
                                              left_join(.,gb_acts %>% select(cycle,diff_act),by="cycle") %>%
                                              left_join(.,incumbent_data %>% mutate(COMP = ifelse(!is.na(COMP1),COMP1,COMP2)) %>%
                                                                             select(cycle,District,COMP),
                                                        by=c("cycle","District")) %>%
                                              mutate(PRED = ifelse(is.na(COMP),LEAN_1 + diff_act, LEAN_1 + diff_act + COMP)) %>%
                                              mutate(res = (DIFF-PRED)^2,
                                                     TSS = (DIFF-diff_act)^2) %>%
                                              group_by(cycle) %>%
                                              summarise(RSS = sum(res),
                                                        TSS = sum(TSS),
                                                        count = n()) %>%
                                              mutate(R2 = 1 - (RSS/TSS),
                                                     type = "LEAN + GB + COMP1 (COMP2 IF MISS)")
lean_gb_comp1 = lean_gb_comp1_by_year %>% summarise(RSS = sum(RSS),
                                                      TSS = sum(TSS),
                                                      avg_R2 = mean(R2),
                                                      count = sum(count)) %>%
                                            mutate(R2 = 1 - (RSS/TSS),
                                                   type = "LEAN + GB + COMP1 (COMP2 IF MISS)")

lean_comp_gb_comp1_by_year = election_results %>% mutate(DIFF = GOP_act - DEM_act) %>%
                                                  filter(abs(DIFF) <= 10) %>%
                                                  left_join(.,leans %>% select(year,District,LEAN_1),by=c("year","District")) %>%
                                                  rename("cycle"="year") %>%
                                                  left_join(.,gb_acts %>% select(cycle,diff_act),by="cycle") %>%
                                                  left_join(.,incumbent_data %>% mutate(COMP = ifelse(!is.na(COMP1),COMP1,COMP2)) %>%
                                                              select(cycle,District,COMP),
                                                            by=c("cycle","District")) %>%
                                                  mutate(PRED = ifelse(is.na(COMP),LEAN_1 + diff_act, LEAN_1 + diff_act + COMP)) %>%
                                                  mutate(res = (DIFF-PRED)^2,
                                                         TSS = (DIFF-diff_act)^2) %>%
                                                  group_by(cycle) %>%
                                                  summarise(RSS = sum(res),
                                                            TSS = sum(TSS),
                                                            count = n()) %>%
                                                  mutate(R2 = 1 - (RSS/TSS),
                                                         type = "LEAN + GB + COMP1 (COMP2 IF MISS)")
lean_comp_gb_comp1 = lean_comp_gb_comp1_by_year %>% summarise(RSS = sum(RSS),
                                                              TSS = sum(TSS),
                                                              avg_R2 = mean(R2),
                                                              count = sum(count)) %>%
                                                    mutate(R2 = 1 - (RSS/TSS),
                                                           type = "LEAN + GB + COMP1 (COMP2 IF MISS)")

###Use half of COMP1 if available... if not, use half of COMP2
lean_gb_comp1h_by_year = election_results %>% mutate(DIFF = GOP_act - DEM_act) %>%
                                              left_join(.,leans %>% select(year,District,LEAN_1),by=c("year","District")) %>%
                                              rename("cycle"="year") %>%
                                              left_join(.,gb_acts %>% select(cycle,diff_act),by="cycle") %>%
                                              left_join(.,incumbent_data %>% mutate(COMP = ifelse(!is.na(COMP1),COMP1/2,COMP2/2)) %>%
                                                          select(cycle,District,COMP),
                                                        by=c("cycle","District")) %>%
                                              mutate(PRED = ifelse(is.na(COMP),LEAN_1 + diff_act,LEAN_1 + diff_act + COMP)) %>%
                                              mutate(res = (DIFF-PRED)^2,
                                                     TSS = (DIFF-diff_act)^2) %>%
                                              group_by(cycle) %>%
                                              summarise(RSS = sum(res),
                                                        TSS = sum(TSS),
                                                        count = n()) %>%
                                              mutate(R2 = 1 - (RSS/TSS),
                                                     type = "LEAN_1 + GB + COMP1/2 (COMP2/2 IF MISS)")
lean_gb_comp1h = lean_gb_comp1h_by_year %>% summarise(RSS = sum(RSS),
                                                      TSS = sum(TSS),
                                                      avg_R2 = mean(R2),
                                                      count = sum(count)) %>%
                                              mutate(R2 = 1 - (RSS/TSS),
                                                     type = "LEAN_1 + GB + COMP1/2 (COMP2/2 IF MISS)")

lean_comp_gb_comp1h_by_year = election_results %>%  mutate(DIFF = GOP_act - DEM_act) %>%
                                                    filter(abs(DIFF) <= 10) %>%
                                                    left_join(.,leans %>% select(year,District,LEAN_1),by=c("year","District")) %>%
                                                    rename("cycle"="year") %>%
                                                    left_join(.,gb_acts %>% select(cycle,diff_act),by="cycle") %>%
                                                    left_join(.,incumbent_data %>% mutate(COMP = ifelse(!is.na(COMP1),COMP1/2,COMP2/2)) %>%
                                                                select(cycle,District,COMP),
                                                              by=c("cycle","District")) %>%
                                                    mutate(PRED = ifelse(is.na(COMP),LEAN_1 + diff_act,LEAN_1 + diff_act + COMP)) %>%
                                                    mutate(res = (DIFF-PRED)^2,
                                                           TSS = (DIFF-diff_act)^2) %>%
                                                    group_by(cycle) %>%
                                                    summarise(RSS = sum(res),
                                                              TSS = sum(TSS),
                                                              count = n()) %>%
                                                    mutate(R2 = 1 - (RSS/TSS),
                                                           type = "LEAN_1 + GB + COMP1/2 (COMP2/2 IF MISS)")
lean_comp_gb_comp1h = lean_comp_gb_comp1h_by_year %>% summarise(RSS = sum(RSS),
                                                                TSS = sum(TSS),
                                                                avg_R2 = mean(R2),
                                                                count = sum(count)) %>%
                                                      mutate(R2 = 1 - (RSS/TSS),
                                                             type = "LEAN_1 + GB + COMP1/2 (COMP2/2 IF MISS)")
####The half works better than the full value
####Tested combining COMP1 and COMP2 as well, but using COMP1 if available works better... will stick with the COMP1/2

#Determine How to Weight the House Polls... Only look at competitive districts (final margin within 10 points)
adv_score_miss_replace = pollster_bias %>%
                          group_by(cycle) %>%
                          summarise(adv_score_replace = quantile(adv_score,1))
gb_final = gb_build %>% filter(days_to_election == 1)  %>% mutate(diff_gb_final = GOP - DEM) %>% select(cycle,diff_gb_final)
decay = c(-0.5,-0.25,-0.1,-0.05,-0.01)
penalty = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8)
ann_poll_results = NA
agg_poll_results = NA
poll_results_found = FALSE
for(i in seq(from=1,to=length(decay),by=1)){
  holder = NA
  holder2 = NA
  
  thepolls_avg = house_polls_cleaned %>% rename("pollster"="pollster_rating_name") %>%
                                         filter(DEM + REP >= 75 & !is.na(sample_size) & days_to_election <= 100) %>%
                                         arrange(cycle,District,pollster,days_to_election,population) %>%
                                         group_by(cycle,District,pollster) %>%
                                         filter(row_number()==1) %>%
                                         left_join(.,pollster_bias %>% select(-count),by=c("cycle","pollster")) %>%
                                         left_join(.,partisan_bias,by=c("cycle","partisan")) %>%
                                         mutate(adjustment = ifelse(population == "rv",
                                                                    ifelse(is.na(POLLSTER_BIAS),PARTISAN_BIAS-1.5,PARTISAN_BIAS+POLLSTER_BIAS-1.5),
                                                                    ifelse(is.na(POLLSTER_BIAS),PARTISAN_BIAS,PARTISAN_BIAS+POLLSTER_BIAS))) %>%
                                         left_join(.,adv_score_miss_replace,by="cycle") %>%
                                         mutate(DEM_adj = DEM + .5*adjustment,
                                                GOP_adj = REP - .5*adjustment,
                                                adv_score = ifelse(is.na(adv_score),adv_score_replace,adv_score)) %>%
                                         mutate(sample_size_score = sqrt(sample_size/600),
                                                pollster_adv_score = ((adv_score - min(pollster_bias$adv_score)) / (max(pollster_bias$adv_score) - min(pollster_bias$adv_score)) - 1) * -1 + 0.5,
                                                time_score = exp(decay[i] * days_to_election)) %>%
                                         mutate(theWeight = time_score * sample_size_score * pollster_adv_score) %>%
                                         group_by(cycle,District) %>%
                                         summarise(DEM = round(weighted.mean(DEM_adj,theWeight),2),
                                                   GOP = round(weighted.mean(GOP_adj,theWeight),2),
                                                   count = n()) %>%
                                         left_join(.,election_results %>% rename("cycle"="year"),by=c("cycle","District")) %>%
                                         left_join(.,gb_acts %>% rename("DEM_gb"="DEM_act","GOP_gb"="GOP_act","diff_gb"="diff_act"),by="cycle") %>%
                                         mutate(diff = GOP - DEM,
                                                diff_act = GOP_act - DEM_act) %>%
                                         filter(abs(diff_act) <= 10) %>%
                                         mutate(TSS = (diff_act-diff_gb)^2,
                                                RSS = (diff_act-diff)^2) %>%
                                         group_by(cycle) %>%
                                         summarise(TSS = sum(TSS),
                                                   RSS = sum(RSS),
                                                   count=n()) %>%
                                         mutate(R2 = 1 - (RSS/TSS),
                                                type = paste("Simple w/ ",decay[i],sep=""))
  thepolls_comb = thepolls_avg %>% summarise(TSS = sum(TSS),
                                             RSS = sum(RSS),
                                             avg_R2 = mean(R2),
                                             count = sum(count)) %>%
                                   mutate(R2 = 1 - (RSS/TSS),
                                          type = paste("Simple w/ ",decay[i],sep=""))
  
  thepolls_avg_gbadj = house_polls_cleaned %>%  rename("pollster"="pollster_rating_name") %>%
                                                filter(DEM + REP >= 75 & !is.na(sample_size) & days_to_election <= 100) %>%
                                                arrange(cycle,District,pollster,days_to_election,population) %>%
                                                group_by(cycle,District,pollster) %>%
                                                filter(row_number()==1) %>%
                                                left_join(.,pollster_bias %>% select(-count),by=c("cycle","pollster")) %>%
                                                left_join(.,partisan_bias,by=c("cycle","partisan")) %>%
                                                mutate(day_join = round(days_to_election,0)) %>%
                                                left_join(.,gb_build %>% mutate(diff_gb_curr = GOP - DEM) %>% rename("day_join"="days_to_election") %>% select(cycle,day_join,diff_gb_curr),by=c("cycle","day_join")) %>%
                                                left_join(.,gb_final,by="cycle") %>%
                                                mutate(adjustment = ifelse(population == "rv",
                                                                           ifelse(is.na(POLLSTER_BIAS),PARTISAN_BIAS-1.5+diff_gb_curr-diff_gb_final,PARTISAN_BIAS+POLLSTER_BIAS-1.5+diff_gb_curr-diff_gb_final),
                                                                           ifelse(is.na(POLLSTER_BIAS),PARTISAN_BIAS+diff_gb_curr-diff_gb_final,PARTISAN_BIAS+POLLSTER_BIAS+diff_gb_curr-diff_gb_final))) %>%
                                                left_join(.,adv_score_miss_replace,by="cycle") %>%
                                                mutate(DEM_adj = DEM + .5*adjustment,
                                                       GOP_adj = REP - .5*adjustment,
                                                       adv_score = ifelse(is.na(adv_score),adv_score_replace,adv_score)) %>%
                                                mutate(sample_size_score = sqrt(sample_size/600),
                                                       pollster_adv_score = ((adv_score - min(pollster_bias$adv_score)) / (max(pollster_bias$adv_score) - min(pollster_bias$adv_score)) - 1) * -1 + 0.5,
                                                       time_score = exp(decay[i] * days_to_election)) %>%
                                                mutate(theWeight = time_score * sample_size_score * pollster_adv_score) %>%
                                                group_by(cycle,District) %>%
                                                summarise(DEM = round(weighted.mean(DEM_adj,theWeight),2),
                                                          GOP = round(weighted.mean(GOP_adj,theWeight),2),
                                                          count = n()) %>%
                                                left_join(.,election_results %>% rename("cycle"="year"),by=c("cycle","District")) %>%
                                                left_join(.,gb_acts %>% rename("DEM_gb"="DEM_act","GOP_gb"="GOP_act","diff_gb"="diff_act"),by="cycle") %>%
                                                mutate(diff = GOP - DEM,
                                                       diff_act = GOP_act - DEM_act) %>%
                                                filter(abs(diff_act) <= 10) %>%
                                                mutate(TSS = (diff_act-diff_gb)^2,
                                                       RSS = (diff_act-diff)^2) %>%
                                                group_by(cycle) %>%
                                                summarise(TSS = sum(TSS),
                                                          RSS = sum(RSS),
                                                          count=n()) %>%
                                                mutate(R2 = 1 - (RSS/TSS),
                                                       type = paste("Simple w/ ",decay[i]," and GB Adj",sep=""))
  thepolls_comb_gbadj = thepolls_avg_gbadj %>% summarise(TSS = sum(TSS),
                                                         RSS = sum(RSS),
                                                         avg_R2 = mean(R2),
                                                         count = sum(count)) %>%
                                                mutate(R2 = 1 - (RSS/TSS),
                                                       type = paste("Simple w/ ",decay[i]," and GB Adj",sep=""))
  
  holder = rbind(thepolls_avg,thepolls_avg_gbadj)
  holder2 = rbind(thepolls_comb,thepolls_comb_gbadj)
  
  for(j in seq(from=1,to=length(penalty),by=1)){
    thepolls_temp = house_polls_cleaned %>% rename("pollster"="pollster_rating_name") %>%
                                            filter(DEM + REP >= 75 & !is.na(sample_size) & days_to_election <= 100) %>%
                                            arrange(cycle,District,pollster,days_to_election,population) %>%
                                            group_by(cycle,District,pollster) %>%
                                            filter(row_number()==1) %>%
                                            left_join(.,pollster_bias %>% select(-count),by=c("cycle","pollster")) %>%
                                            left_join(.,partisan_bias,by=c("cycle","partisan")) %>%
                                            mutate(adjustment = ifelse(population == "rv",
                                                                       ifelse(is.na(POLLSTER_BIAS),PARTISAN_BIAS-1.5,PARTISAN_BIAS+POLLSTER_BIAS-1.5),
                                                                       ifelse(is.na(POLLSTER_BIAS),PARTISAN_BIAS,PARTISAN_BIAS+POLLSTER_BIAS))) %>%
                                            left_join(.,adv_score_miss_replace,by="cycle") %>%
                                            mutate(DEM_adj = DEM + .5*adjustment,
                                                   GOP_adj = REP - .5*adjustment,
                                                   adv_score = ifelse(is.na(adv_score),adv_score_replace,adv_score)) %>%
                                            mutate(sample_size_score = sqrt(sample_size/600),
                                                   pollster_adv_score = ((adv_score - min(pollster_bias$adv_score)) / (max(pollster_bias$adv_score) - min(pollster_bias$adv_score)) - 1) * -1 + 0.5,
                                                   time_score = exp(decay[i] * days_to_election),
                                                   penalty = ifelse(partisan == "",1,1-penalty[j])) %>%
                                            mutate(theWeight = time_score * sample_size_score * pollster_adv_score*penalty) %>%
                                            group_by(cycle,District) %>%
                                            summarise(DEM = round(weighted.mean(DEM_adj,theWeight),2),
                                                      GOP = round(weighted.mean(GOP_adj,theWeight),2),
                                                      count = n()) %>%
                                            left_join(.,election_results %>% rename("cycle"="year"),by=c("cycle","District")) %>%
                                            left_join(.,gb_acts %>% rename("DEM_gb"="DEM_act","GOP_gb"="GOP_act","diff_gb"="diff_act"),by="cycle") %>%
                                            mutate(diff = GOP - DEM,
                                                   diff_act = GOP_act - DEM_act) %>%
                                            filter(abs(diff_act) <= 10) %>%
                                            mutate(TSS = (diff_act-diff_gb)^2,
                                                   RSS = (diff_act-diff)^2) %>%
                                            group_by(cycle) %>%
                                            summarise(TSS = sum(TSS),
                                                      RSS = sum(RSS),
                                                      count=n()) %>%
                                            mutate(R2 = 1 - (RSS/TSS),
                                                   type = paste("Simple w/ ",decay[i]," and penalty ",penalty[j],sep=""))
                                          thepolls_comb = thepolls_avg %>% summarise(TSS = sum(TSS),
                                                                                     RSS = sum(RSS),
                                                                                     count = sum(count)) %>%
                                            mutate(R2 = 1 - (RSS/TSS),
                                                   type = paste("Simple w/ ",decay[i]," and penalty ",penalty[j],sep=""))
                                          
      thepolls_comb_temp = thepolls_temp %>% summarise(TSS = sum(TSS),
                                                       RSS = sum(RSS),
                                                       avg_R2 = mean(R2),
                                                       count = sum(count)) %>%
                                             mutate(R2 = 1 - (RSS/TSS),
                                                    type = paste("Simple w/ ",decay[i]," and penalty ",penalty[j],sep=""))
      
      thepolls_temp2 = house_polls_cleaned %>%  rename("pollster"="pollster_rating_name") %>%
                                                filter(DEM + REP >= 75 & !is.na(sample_size) & days_to_election <= 100) %>%
                                                arrange(cycle,District,pollster,days_to_election,population) %>%
                                                group_by(cycle,District,pollster) %>%
                                                filter(row_number()==1) %>%
                                                left_join(.,pollster_bias %>% select(-count),by=c("cycle","pollster")) %>%
                                                left_join(.,partisan_bias,by=c("cycle","partisan")) %>%
                                                mutate(day_join = round(days_to_election,0)) %>%
                                                left_join(.,gb_build %>% mutate(diff_gb_curr = GOP - DEM) %>% rename("day_join"="days_to_election") %>% select(cycle,day_join,diff_gb_curr),by=c("cycle","day_join")) %>%
                                                left_join(.,gb_final,by="cycle") %>%
                                                mutate(adjustment = ifelse(population == "rv",
                                                                           ifelse(is.na(POLLSTER_BIAS),PARTISAN_BIAS-1.5+diff_gb_curr-diff_gb_final,PARTISAN_BIAS+POLLSTER_BIAS-1.5+diff_gb_curr-diff_gb_final),
                                                                           ifelse(is.na(POLLSTER_BIAS),PARTISAN_BIAS+diff_gb_curr-diff_gb_final,PARTISAN_BIAS+POLLSTER_BIAS+diff_gb_curr-diff_gb_final))) %>%
                                                left_join(.,adv_score_miss_replace,by="cycle") %>%
                                                mutate(DEM_adj = DEM + .5*adjustment,
                                                       GOP_adj = REP - .5*adjustment,
                                                       adv_score = ifelse(is.na(adv_score),adv_score_replace,adv_score)) %>%
                                                mutate(sample_size_score = sqrt(sample_size/600),
                                                       pollster_adv_score = ((adv_score - min(pollster_bias$adv_score)) / (max(pollster_bias$adv_score) - min(pollster_bias$adv_score)) - 1) * -1 + 0.5,
                                                       time_score = exp(decay[i] * days_to_election),
                                                       penalty = ifelse(partisan == "",1,1-penalty[j])) %>%
                                                mutate(theWeight = time_score * sample_size_score * pollster_adv_score * penalty) %>%
                                                group_by(cycle,District) %>%
                                                summarise(DEM = round(weighted.mean(DEM_adj,theWeight),2),
                                                          GOP = round(weighted.mean(GOP_adj,theWeight),2),
                                                          count = n()) %>%
                                                left_join(.,election_results %>% rename("cycle"="year"),by=c("cycle","District")) %>%
                                                left_join(.,gb_acts %>% rename("DEM_gb"="DEM_act","GOP_gb"="GOP_act","diff_gb"="diff_act"),by="cycle") %>%
                                                mutate(diff = GOP - DEM,
                                                       diff_act = GOP_act - DEM_act) %>%
                                                filter(abs(diff_act) <= 10) %>%
                                                mutate(TSS = (diff_act-diff_gb)^2,
                                                       RSS = (diff_act-diff)^2) %>%
                                                group_by(cycle) %>%
                                                summarise(TSS = sum(TSS),
                                                          RSS = sum(RSS),
                                                          count=n()) %>%
                                                mutate(R2 = 1 - (RSS/TSS),
                                                       type = paste("Simple w/ ",decay[i]," and GB Adj and penalty ",penalty[j],sep=""))
      thepolls_comb_temp2 = thepolls_temp2 %>% summarise(TSS = sum(TSS),
                                                         RSS = sum(RSS),
                                                         avg_R2 = mean(R2),
                                                         count = sum(count)) %>%
                                               mutate(R2 = 1 - (RSS/TSS),
                                                      type = paste("Simple w/ ",decay[i]," and GB Adj and penalty ",penalty[j],sep=""))
      
      holder = rbind(holder,thepolls_temp,thepolls_temp2)
      holder2 = rbind(holder2,thepolls_comb_temp,thepolls_comb_temp2)
  }
  
  if(poll_results_found==FALSE){
    ann_poll_results = holder
    agg_poll_results = holder2
    poll_results_found = TRUE
  } else {
    ann_poll_results = rbind(ann_poll_results,holder)
    agg_poll_results = rbind(agg_poll_results,holder2)
  }
}
#R2 is only .38 in competitive districts, but these races are all so close, so it's a decent R2 value

#Determine best way to weight the baseline LEAN + GB and polling
lean_calc = election_results %>% mutate(DIFF = GOP_act - DEM_act) %>%
                                 left_join(.,leans %>% select(year,District,LEAN_1),by=c("year","District")) %>%
                                 rename("cycle"="year") %>%
                                 left_join(.,gb_acts %>% select(cycle,diff_act),by="cycle") %>%
                                 left_join(.,incumbent_data %>% mutate(COMP = ifelse(!is.na(COMP1),COMP1/2,COMP2/2)) %>%
                                             select(cycle,District,COMP),
                                           by=c("cycle","District")) %>%
                                 mutate(LEAN_PRED = ifelse(is.na(COMP),LEAN_1 + diff_act,LEAN_1 + diff_act + COMP),
                                        TSS = (DIFF-diff_act)^2) %>%
                                 select(cycle,District,DEM_act,GOP_act,DIFF,LEAN_PRED,TSS)

thepolls_calc = house_polls_cleaned %>% rename("pollster"="pollster_rating_name") %>%
                                        filter(DEM + REP >= 75 & !is.na(sample_size) & days_to_election <= 100) %>%
                                        arrange(cycle,District,pollster,days_to_election,population) %>%
                                        group_by(cycle,District,pollster) %>%
                                        filter(row_number()==1) %>%
                                        left_join(.,pollster_bias %>% select(-count),by=c("cycle","pollster")) %>%
                                        left_join(.,partisan_bias,by=c("cycle","partisan")) %>%
                                        mutate(day_join = round(days_to_election,0)) %>%
                                        left_join(.,gb_final,by="cycle") %>%
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
                                        summarise(DEM = round(weighted.mean(DEM_adj,theWeight),2),
                                                  GOP = round(weighted.mean(GOP_adj,theWeight),2),
                                                  total_weight = sum(theWeight),
                                                  count = n()) %>%
                                        mutate(POLL_PRED = GOP - DEM) %>%
                                        select(cycle,District,POLL_PRED,total_weight,count)

final_calc = lean_calc %>% left_join(.,thepolls_calc,by=c("cycle","District"))

test_max = c(0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1)
decay_weight = c(-0.25,-0.2,-0.15,-0.1,-0.05)
test_weights = c(4,5,6,7,8) #set the max for the number of polls
test_weights2 = c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5) #set the max for the total weight of polls

all_ann_R2 = NA
all_R2 = NA
comp_ann_R2 = NA
comp_R2 = NA
found = FALSE
#first do by number of polls
for(i in seq(from=1,to=length(test_weights),by=1)){
  for(j in seq(from=1,to=length(test_max),by=1)){
    for(k in seq(from=1,to=length(decay_weight),by=1)){
      final_temp = final_calc %>%
                mutate(poll_weight = ifelse(is.na(count),NA,
                                            ifelse(count>=test_weights[i],0,test_weights[i]-count))) %>%
                mutate(theWeight = ifelse(is.na(poll_weight),0,test_max[j]*exp(decay_weight[k]*poll_weight))) %>%
                mutate(PRED = ifelse(theWeight==0,LEAN_PRED,theWeight * POLL_PRED + (1-theWeight) * LEAN_PRED)) %>%
                mutate(RSS = (DIFF-PRED)^2)
      
      all_ann_calc = final_temp %>% group_by(cycle) %>%
                                    summarise(count = n(),
                                              TSS = sum(TSS),
                                              RSS = sum(RSS)) %>%
                                    mutate(R2 = 1 - (RSS/TSS),
                                           type = paste("Poll Count, Max: ",test_weights[i],", Max Prct: ",test_max[j]," Decay: ",decay_weight[k],sep=""))
      all_calc = all_ann_calc %>% summarise(count = sum(count),
                                            TSS = sum(TSS),
                                            RSS = sum(RSS),
                                            avg_R2 = mean(R2)) %>%
                                  mutate(R2 = 1 - (RSS/TSS),
                                         type = paste("Poll Count, Max: ",test_weights[i],", Max Prct: ",test_max[j]," Decay: ",decay_weight[k],sep=""))
      
      comp_ann_calc = final_temp %>% filter(abs(DIFF) < 10) %>%
                                     group_by(cycle) %>%
                                     summarise(count = n(),
                                               TSS = sum(TSS),
                                               RSS = sum(RSS)) %>%
                                     mutate(R2 = 1 - (RSS/TSS),
                                            type = paste("Poll Count, Max: ",test_weights[i],", Max Prct: ",test_max[j]," Decay: ",decay_weight[k],sep=""))
      comp_calc = comp_ann_calc %>% summarise(count = sum(count),
                                              TSS = sum(TSS),
                                              RSS = sum(RSS),
                                              avg_R2 = mean(R2)) %>%
                                    mutate(R2 = 1 - (RSS/TSS),
                                           type = paste("Poll Count, Max: ",test_weights[i],", Max Prct: ",test_max[j]," Decay: ",decay_weight[k],sep=""))
      
      if(found==FALSE){
        all_ann_R2 = all_ann_calc
        all_R2 = all_calc
        comp_ann_R2 = comp_ann_calc
        comp_R2 = comp_calc
        found = TRUE
      } else {
        all_ann_R2 = rbind(all_ann_R2)
        all_R2 = rbind(all_R2,all_calc)
        comp_ann_R2 = rbind(comp_ann_R2,comp_ann_calc)
        comp_R2 = rbind(comp_R2,comp_calc)
      }
      
    }
  }
}

#Now try by total weight
for(i in seq(from=1,to=length(test_weights2),by=1)){
  for(j in seq(from=1,to=length(test_max),by=1)){
    for(k in seq(from=1,to=length(decay_weight),by=1)){
      final_temp = final_calc %>%
                      mutate(poll_weight = ifelse(is.na(total_weight),NA,
                                                  ifelse(total_weight>=test_weights2[i],0,test_weights2[i]-total_weight))) %>%
                      mutate(theWeight = ifelse(is.na(poll_weight),0,test_max[j]*exp(decay_weight[k]*poll_weight))) %>%
                      mutate(PRED = ifelse(theWeight==0,LEAN_PRED,theWeight * POLL_PRED + (1-theWeight) * LEAN_PRED)) %>%
                      mutate(RSS = (DIFF-PRED)^2)
      
      all_ann_calc = final_temp %>% group_by(cycle) %>%
                                    summarise(count = n(),
                                              TSS = sum(TSS),
                                              RSS = sum(RSS)) %>%
                                    mutate(R2 = 1 - (RSS/TSS),
                                           type = paste("Poll Total Weight, Max: ",test_weights2[i],", Max Prct: ",test_max[j]," Decay: ",decay_weight[k],sep=""))
      all_calc = all_ann_calc %>% summarise(count = sum(count),
                                            TSS = sum(TSS),
                                            RSS = sum(RSS),
                                            avg_R2 = mean(R2)) %>%
                                  mutate(R2 = 1 - (RSS/TSS),
                                         type = paste("Poll Total Weight, Max: ",test_weights2[i],", Max Prct: ",test_max[j]," Decay: ",decay_weight[k],sep=""))
      
      comp_ann_calc = final_temp %>% filter(abs(DIFF) <= 10) %>%
                                     group_by(cycle) %>%
                                     summarise(count = n(),
                                               TSS = sum(TSS),
                                               RSS = sum(RSS)) %>%
                                     mutate(R2 = 1 - (RSS/TSS),
                                            type = paste("Poll Total Weight, Max: ",test_weights2[i],", Max Prct: ",test_max[j]," Decay: ",decay_weight[k],sep=""))
      comp_calc = comp_ann_calc %>% summarise(count = sum(count),
                                              TSS = sum(TSS),
                                              RSS = sum(RSS),
                                              avg_R2 = mean(R2)) %>%
                                    mutate(R2 = 1 - (RSS/TSS),
                                           type = paste("Poll Total Weight, Max: ",test_weights2[i],", Max Prct: ",test_max[j]," Decay: ",decay_weight[k],sep=""))
      
      if(found==FALSE){
        all_ann_R2 = all_ann_calc
        all_R2 = all_calc
        comp_ann_R2 = comp_ann_calc
        comp_R2 = comp_calc
        found = TRUE
      } else {
        all_ann_R2 = rbind(all_ann_R2)
        all_R2 = rbind(all_R2,all_calc)
        comp_ann_R2 = rbind(comp_ann_R2,comp_ann_calc)
        comp_R2 = rbind(comp_R2,comp_calc)
      }
      
    }
  }
}
#BEST WEIGHT: Poll Total Weight, Max: 3.5, Max Prct: 0.9 Decay: -0.15
#Competitive District R2 = 0.54
#For all CDs, R2 = 0.95

#Create Plots for R^2 improvement by stage
all_plot = as.data.frame(rbind(c("Partisan Lean + Generic Ballot",0.90),
                               c("Partisan Lean + Generic Ballot + Incumbent Previous Performance",.94),
                               c("Partisan Lean + Generic Ballot + Incumbent Previous Performance + Polling",0.95)))
colnames(all_plot) = c("Metrics","R2")
all_plot$R2 = as.numeric(all_plot$R2)

comp_plot = as.data.frame(rbind(c("Partisan Lean + Generic Ballot",-0.70),
                                c("Partisan Lean + Generic Ballot + Incumbent Previous Performance",-0.10),
                                c("Partisan Lean + Generic Ballot + Incumbent Previous Performance + Polling",0.54)))
colnames(comp_plot) = c("Metrics","R2")
comp_plot$R2 = as.numeric(comp_plot$R2)

png(filename = "Plots/Vote Estimate/All_CDs.png",width = 1920,height = 1080)
ggplot(data=all_plot, aes(x=Metrics, y=R2, fill=Metrics)) +
  geom_bar(stat="identity",color="black") +
  ggtitle("All Congressional Districts with DEM and GOP Candidates: R^2 at Model Stage") +
  ylim(limits=c(0,1)) +
  ylab("R^2") +
  xlab("Model Stage") +
  geom_text(aes(label=R2), vjust = -0.8, color="black", size=8)+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,size=18,face="bold"),
        axis.text = element_text(size=16),
        axis.title = element_text(size=20),
        legend.position="none")
dev.off()

png(filename = "Plots/Vote Estimate/Comp_CDs.png",width = 1920,height = 1080)
ggplot(data=comp_plot, aes(x=Metrics, y=R2, fill=Metrics)) +
  geom_bar(stat="identity",color="black") +
  ggtitle("Competitive Congressional Districts (Final Margin <= 10%): R^2 at Model Stage") +
  ylim(limits=c(-1,1)) +
  ylab("R^2") +
  xlab("Model Stage") +
  geom_text(aes(label=R2), vjust = -0.4, color="black", size=8)+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,size=18,face="bold"),
        axis.text = element_text(size=16),
        axis.title = element_text(size=20),
        legend.position="none")
dev.off()

#Determine Total 2-party vote using 3rd party candidates
forThirdPartyModel = election_results %>% left_join(.,party_ind %>% mutate(District = paste(STATE,"-",str_pad(DISTRICT,2,pad="0"),sep="")) %>%
                                                                    select(year,District,REP_IND,DEM_IND,LBT_IND,GRN_IND),
                                                    by=c("year","District")) %>% 
                                          filter(REP_IND == 1 & DEM_IND == 1) %>%
                                          mutate(third_party_vote = round(100 - DEM_act - GOP_act,2))

third_party_lm = lm(third_party_vote ~ LBT_IND + GRN_IND,data=forThirdPartyModel)
summary(third_party_lm)


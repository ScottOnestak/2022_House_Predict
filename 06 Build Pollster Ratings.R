#2022 Election Project
#Author: Scott Onestak
#6 Build Pollster Ratings

#libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(stringr)

options(scipen = 100)
options(max.print = 5000)

#read in data... data taken from FiveThirtyEight (https://github.com/fivethirtyeight/data/tree/master/pollster-ratings)
raw_polls = read.csv("Data/Polls/For Pollster Ratings/raw-polls.csv",header=T,stringsAsFactors=F)
banned_list = unlist(read.csv("Data/Polls/For Pollster Ratings/banned.csv",header=T,stringsAsFactors=F) %>%
                        filter(banned=="yes") %>% select(pollster) %>% distinct())

#Do some cleaning... I don't care how pollsters performed in primaries.  All model data will be for general.
#Keep on Rep vs Dem polls with some exceptions that are basically REP vs DEM
the_polls = raw_polls %>% filter(!type_simple == "Pres-P" &
                                 ((cand1_party=="DEM" & cand2_party=="REP") |
                                  (cand1_name=="Marco Rubio" & cand2_name=="Charlie Crist") |
                                  (cand1_name=="Angus S. King Jr." & cand2_party=="REP") |
                                  (cand1_name=="Pat Roberts" & cand2_name=="Gregory Orman")) &
                                 !pollster %in% banned_list) %>%
                          mutate(cand1_party = ifelse(cand1_name=="Angus S. King Jr.","DEM",cand1_party),
                                 cand2_party = ifelse(cand2_name=="Charlie Crist","DEM",
                                                      ifelse(cand2_name=="Gregory Orman","DEM",cand2_party))) %>%
                          mutate(REP_CANDIDATE = ifelse(cand1_party=="REP",cand1_name,cand2_name),
                                 REP_PRCT = ifelse(cand1_party=="REP",cand1_pct,cand2_pct),
                                 REP_ACT = ifelse(cand1_party=="REP",cand1_actual,cand2_actual),
                                 DEM_CANDIDATE = ifelse(cand1_party=="DEM",cand1_name,cand2_name),
                                 DEM_PRCT = ifelse(cand1_party=="DEM",cand1_pct,cand2_pct),
                                 DEM_ACT = ifelse(cand1_party=="DEM",cand1_actual,cand2_actual)) %>%
                          select(poll_id,race_id,year,race,location,type_simple,pollster,pollster_rating_id,
                                 methodology,partisan,polldate,electiondate,samplesize,
                                 REP_CANDIDATE,REP_PRCT,REP_ACT,DEM_CANDIDATE,DEM_PRCT,DEM_ACT) %>%
                          mutate(miss = (REP_PRCT - DEM_PRCT) - (REP_ACT - DEM_ACT),
                                 abs_miss = abs((REP_PRCT - DEM_PRCT) - (REP_ACT - DEM_ACT)))

#Because polls are time-sensative, sample size sensative, etc., try to estimate what the typical miss would be
the_polls$polldate = as.Date(the_polls$polldate,"%m/%d/%Y")
the_polls$electiondate = as.Date(the_polls$electiondate,"%m/%d/%Y")
the_polls$days_to_election = as.numeric(the_polls$electiondate - the_polls$polldate)
the_polls$methodology = as.factor(the_polls$methodology)
the_polls$type_simple = as.factor(the_polls$type_simple)
the_polls$partisan = as.factor(the_polls$partisan)
the_polls$pollster = as.factor(the_polls$pollster)

#Look at bias of polls over time
by_year = the_polls %>% filter(partisan == "") %>%
            group_by(year) %>% 
            summarise(count=n(),
                      avg_miss = round(mean(miss,na.rm=T),2),
                      med_miss = round(median(miss,na.rm=T),2),
                      avg_abs_miss = round(mean(abs_miss,na.rm=T),2),
                      med_abs_miss = round(median(abs_miss,na.rm=T),2)) %>%
            filter(year%%2==0) %>%
            mutate(even = 0)
all_miss_by_year = ggplot(by_year,aes(x=year)) +
                      geom_line(aes(y=avg_miss),color="firebrick") +
                      geom_line(aes(y=med_miss),color="skyblue") +
                      geom_line(aes(y=even),color="black") +
                      labs(title = "Miss by Year (Avg and Med)",
                           y = "Miss")
all_abs_miss_by_year = ggplot(by_year,aes(x=year)) +
                        geom_line(aes(y=avg_abs_miss),color="firebrick") +
                        geom_line(aes(y=med_abs_miss),color="skyblue") +
                        labs(title="Absolute Miss by Year (Avg and Med)",
                             y = "Absolute Miss")
png(filename = "Plots/Pollster Ratings/All_Miss_By_Year.png",width = 1920,height = 1080)
grid.arrange(all_miss_by_year, all_abs_miss_by_year, ncol=2)
dev.off()

by_year_house = the_polls %>% filter(partisan == "" & type_simple=="House-G") %>%
                  group_by(year) %>% 
                  summarise(count=n(),
                            avg_miss = round(mean(miss,na.rm=T),2),
                            med_miss = round(median(miss,na.rm=T),2),
                            avg_abs_miss = round(mean(abs_miss,na.rm=T),2),
                            med_abs_miss = round(median(abs_miss,na.rm=T),2)) %>%
                  filter(year%%2==0) %>%
                  mutate(even = 0)
hou_miss_by_year = ggplot(by_year_house,aes(x=year)) +
                    geom_line(aes(y=avg_miss),color="firebrick") +
                    geom_line(aes(y=med_miss),color="skyblue") +
                    geom_line(aes(y=even),color="black") +
                    labs(title = "Miss by Year for House Polling (Avg and Med)",
                         y = "Miss")
hou_abs_miss_by_year = ggplot(by_year_house,aes(x=year)) +
                        geom_line(aes(y=avg_abs_miss),color="firebrick") +
                        geom_line(aes(y=med_abs_miss),color="skyblue") +
                        labs(title="Absolute Miss by Year for House Polling (Avg and Med)",
                             y = "Absolute Miss")
png(filename = "Plots/Pollster Ratings/House_Miss_By_Year.png",width = 1920,height = 1080)
grid.arrange(hou_miss_by_year, hou_abs_miss_by_year, ncol=2)
dev.off()
#The House polling looks to have had an anti-GOP bias for a while, but that bias has grown
#It was close to 6%! in the average of 2020 polls (and that excluded partisan polls)

#Look at number of polls per pollster per year
pollster_by_year = the_polls %>% group_by(pollster,partisan,year) %>% summarise(count=n(),
                                                                                avg_miss = mean(miss,na.rm=T),
                                                                                med_miss = median(miss,na.rm=T),
                                                                                avg_abs_miss = mean(abs_miss,na.rm=T),
                                                                                med_abs_miss = median(abs_miss,na.rm=T))

#A look at some of the largest pollsters
#Quinnipiac
by_year_qui = the_polls %>% filter(partisan == "" & pollster == "Quinnipiac University") %>%
                group_by(year) %>% 
                summarise(count=n(),
                          avg_miss = round(mean(miss,na.rm=T),2),
                          med_miss = round(median(miss,na.rm=T),2),
                          avg_abs_miss = round(mean(abs_miss,na.rm=T),2),
                          med_abs_miss = round(median(abs_miss,na.rm=T),2)) %>%
                filter(year%%2==0) %>%
                mutate(even = 0)
qui_miss_by_year = ggplot(by_year_qui,aes(x=year)) +
                    geom_line(aes(y=avg_miss),color="firebrick") +
                    geom_line(aes(y=med_miss),color="skyblue") +
                    geom_line(aes(y=even),color="black") +
                    labs(title = "Quinnipiac Miss by Year (Avg and Med)",
                         y = "Miss")
qui_abs_miss_by_year = ggplot(by_year_qui,aes(x=year)) +
                        geom_line(aes(y=avg_abs_miss),color="firebrick") +
                        geom_line(aes(y=med_abs_miss),color="skyblue") +
                        labs(title="Quinnipiac Absolute Miss by Year (Avg and Med)",
                             y = "Absolute Miss")
png(filename = "Plots/Pollster Ratings/Quinnipiac_Miss_By_Year.png",width = 1920,height = 1080)
grid.arrange(qui_miss_by_year, qui_abs_miss_by_year, ncol=2)
dev.off()

#IBD/TIPP
by_year_ibd = the_polls %>% filter(partisan == "" & pollster == "IBD/TIPP") %>%
                group_by(year) %>% 
                summarise(count=n(),
                          avg_miss = round(mean(miss,na.rm=T),2),
                          med_miss = round(median(miss,na.rm=T),2),
                          avg_abs_miss = round(mean(abs_miss,na.rm=T),2),
                          med_abs_miss = round(median(abs_miss,na.rm=T),2)) %>%
                filter(year%%2==0) %>%
                mutate(even = 0)
ibd_miss_by_year = ggplot(by_year_ibd,aes(x=year)) +
                    geom_line(aes(y=avg_miss),color="firebrick") +
                    geom_line(aes(y=med_miss),color="skyblue") +
                    geom_line(aes(y=even),color="black") +
                    labs(title = "IBD/TIPP Miss by Year (Avg and Med)",
                         y = "Miss")
ibd_abs_miss_by_year = ggplot(by_year_ibd,aes(x=year)) +
                        geom_line(aes(y=avg_abs_miss),color="firebrick") +
                        geom_line(aes(y=med_abs_miss),color="skyblue") +
                        labs(title="IBD/TIPP Absolute Miss by Year (Avg and Med)",
                             y = "Absolute Miss")
png(filename = "Plots/Pollster Ratings/IBD_Miss_By_Year.png",width = 1920,height = 1080)
grid.arrange(ibd_miss_by_year, ibd_abs_miss_by_year, ncol=2)
dev.off()

#Selzer & Co.
by_year_sel = the_polls %>% filter(partisan == "" & pollster == "Selzer & Co.") %>%
                group_by(year) %>% 
                summarise(count=n(),
                          avg_miss = round(mean(miss,na.rm=T),2),
                          med_miss = round(median(miss,na.rm=T),2),
                          avg_abs_miss = round(mean(abs_miss,na.rm=T),2),
                          med_abs_miss = round(median(abs_miss,na.rm=T),2)) %>%
                filter(year%%2==0) %>%
                mutate(even = 0)
sel_miss_by_year = ggplot(by_year_sel,aes(x=year)) +
                    geom_line(aes(y=avg_miss),color="firebrick") +
                    geom_line(aes(y=med_miss),color="skyblue") +
                    geom_line(aes(y=even),color="black") +
                    labs(title = "Selzer & Co. Miss by Year (Avg and Med)",
                         y = "Miss")
sel_abs_miss_by_year = ggplot(by_year_sel,aes(x=year)) +
                        geom_line(aes(y=avg_abs_miss),color="firebrick") +
                        geom_line(aes(y=med_abs_miss),color="skyblue") +
                        labs(title="Selzer & Co. Absolute Miss by Year",
                             y = "Absolute Miss")
png(filename = "Plots/Pollster Ratings/Selzer_Miss_By_Year.png",width = 1920,height = 1080)
grid.arrange(sel_miss_by_year, sel_abs_miss_by_year, ncol=2)
dev.off()

#YouGov
by_year_you = the_polls %>% filter(partisan == "" & pollster == "YouGov") %>%
  group_by(year) %>% 
  summarise(count=n(),
            avg_miss = round(mean(miss,na.rm=T),2),
            med_miss = round(median(miss,na.rm=T),2),
            avg_abs_miss = round(mean(abs_miss,na.rm=T),2),
            med_abs_miss = round(median(abs_miss,na.rm=T),2)) %>%
  filter(year%%2==0) %>%
  mutate(even = 0)
you_miss_by_year = ggplot(by_year_you,aes(x=year)) +
                    geom_line(aes(y=avg_miss),color="firebrick") +
                    geom_line(aes(y=med_miss),color="skyblue") +
                    geom_line(aes(y=even),color="black") +
                    labs(title = "YouGov Miss by Year (Avg and Med)",
                         y = "Miss")
you_abs_miss_by_year = ggplot(by_year_you,aes(x=year)) +
                        geom_line(aes(y=avg_abs_miss),color="firebrick") +
                        geom_line(aes(y=med_abs_miss),color="skyblue") +
                        labs(title="YouGov Absolute Miss by Year (Avg and Med)",
                             y = "Absolute Miss")
png(filename = "Plots/Pollster Ratings/YouGov_Miss_By_Year.png",width = 1920,height = 1080)
grid.arrange(you_miss_by_year, you_abs_miss_by_year, ncol=2)
dev.off()





#Look at time and sample size
days_to = the_polls %>% group_by(type_simple,days_to_election) %>% summarise(abs_miss = mean(abs_miss))
days_to_gov = days_to %>% filter(type_simple=="Gov-G")
days_to_hou = days_to %>% filter(type_simple=="House-G")
days_to_pre = days_to %>% filter(type_simple=="Pres-G")
days_to_sen = days_to %>% filter(type_simple=="Sen-G")

png(filename = "Plots/Pollster Ratings/Pres_Abs_Miss_by_DTE.png",width = 1920,height = 1080)
ggplot(days_to_pre,aes(x=days_to_election,y=abs_miss)) +
  geom_point() +
  geom_smooth(method="lm",formula = y ~ x) +
  labs(title = "President - Absolute Miss by Days to Election")
dev.off()

png(filename = "Plots/Pollster Ratings/Gov_Abs_Miss_by_DTE.png",width = 1920,height = 1080)
ggplot(days_to_gov,aes(x=days_to_election,y=abs_miss)) +
  geom_point() +
  geom_smooth(method="lm",formula = y ~ x) +
  labs(title = "Governor - Absolute Miss by Days to Election")
dev.off()

png(filename = "Plots/Pollster Ratings/Sen_Abs_Miss_by_DTE.png",width = 1920,height = 1080)
ggplot(days_to_sen,aes(x=days_to_election,y=abs_miss)) +
  geom_point() +
  geom_smooth(method="lm",formula = y ~ x) +
  labs(title = "Senate - Absolute Miss by Days to Election")
dev.off()

png(filename = "Plots/Pollster Ratings/House_Abs_Miss_by_DTE.png",width = 1920,height = 1080)
ggplot(days_to_hou,aes(x=days_to_election,y=abs_miss)) +
  geom_point() +
  geom_smooth(method="lm",formula = y ~ x) +
  labs(title = "House - Absolute Miss by Days to Election")
dev.off()

#The House have the larger misses, but all look to have relatively linear relationships
#Those polls closer to the date of the election perform better

#sample size is not lineraly correlated, but sampling error looks to be... use that
samplesize = the_polls %>% group_by(type_simple,samplesize) %>% summarise(abs_miss = mean(abs_miss)) %>% mutate(sample_error = 1 / sqrt(samplesize))
samplesize_gov = samplesize %>% filter(type_simple=="Gov-G")
samplesize_hou = samplesize %>% filter(type_simple=="House-G")
samplesize_pre = samplesize %>% filter(type_simple=="Pres-G")
samplesize_sen = samplesize %>% filter(type_simple=="Sen-G")

png(filename = "Plots/Pollster Ratings/Pres_Abs_Miss_by_SE.png",width = 1920,height = 1080)
ggplot(samplesize_pre,aes(x=sample_error,y=abs_miss)) +
  geom_point() +
  geom_smooth(method="lm",formula = y ~ x) +
  labs(title = "President - Absolute Miss by Sample Error")
dev.off()

png(filename = "Plots/Pollster Ratings/Gov_Abs_Miss_by_SE.png",width = 1920,height = 1080)
ggplot(samplesize_gov,aes(x=sample_error,y=abs_miss)) +
  geom_point() +
  geom_smooth(method="lm",formula = y ~ x) +
  labs(title = "Governor - Absolute Miss by Sample Error")
dev.off()

png(filename = "Plots/Pollster Ratings/Sen_Abs_Miss_by_SE.png",width = 1920,height = 1080)
ggplot(samplesize_sen,aes(x=sample_error,y=abs_miss)) +
  geom_point() +
  geom_smooth(method="lm",formula = y ~ x) +
  labs(title = "Senate - Absolute Miss by Sample Error")
dev.off()

png(filename = "Plots/Pollster Ratings/House_Abs_Miss_by_SE.png",width = 1920,height = 1080)
ggplot(samplesize_hou,aes(x=sample_error,y=abs_miss)) +
  geom_point() +
  geom_smooth(method="lm",formula = y ~ x) +
  labs(title = "House - Absolute Miss by Sample Error")
dev.off()

#Sample error and absolute miss look to have a larger positive correlation at the House level
#This is probably because House races have smaller sample sizes in general
#There are diminishing marginal returns with sample size, so it makes sense the other races - usually with larger samples - wouldn't be as effected
the_polls$sampling_error = 1 / sqrt(the_polls$samplesize)

#Optimize for predictability
theScores = NA
theScores_found = FALSE

#Create Cutoffs to Test
cutoffs = c(3,5,10,20)
# samplesize_weight = c(0,1,2,3,4)
# time_weight = c(0,1,2,3,4)
# pollster_weight = c(1,2,3,4)

for(i in seq(from=2004,to=2020,by=2)){
  for(j in seq(from=4,to=10,by=2)){
    cat("YEAR: ",i," LOOKBACK: ",j,"\n",sep="")
    temp = the_polls %>% filter(year < i & year >= (i-j))
    test = the_polls %>% filter(year == i)
    
    #Try to control for the effects of the race being polled
    m1 = lm(miss ~ type_simple + partisan + sampling_error + days_to_election + log(days_to_election) + pollster,data=temp)
    summary(m1)
    
    m2 = lm(abs_miss ~ type_simple + partisan + sampling_error + days_to_election + log(days_to_election),data=temp)
    summary(m2)
    
    #Get the overall bias of the polling
    y_bar = round(mean(temp$miss),2)
    
    #Get coefficients for partisan poll
    MISS_COEF = as.data.frame(m1$coefficients)
    colnames(MISS_COEF) = "COEFFICIENT"
    MISS_COEF$COEFFICIENT = round(MISS_COEF$COEFFICIENT,2)
    MISS_COEF$VAR = str_replace_all(row.names(MISS_COEF),"pollster","")
    pollster_coef = MISS_COEF %>% filter(str_detect(row.names(MISS_COEF),"pollster")) %>% rename(pollster=VAR)
    
    ADJ_PARTISAN_R = round(as.numeric(unlist(MISS_COEF %>% filter(VAR == "partisanR") %>% select("COEFFICIENT"))),2)
    ADJ_PARTISAN_D = round(as.numeric(unlist(MISS_COEF %>% filter(VAR == "partisanD") %>% select("COEFFICIENT"))),2)
    
    #Create Partisan Bias Values
    partisan_temp = as.data.frame(rbind(cbind("",0),
                                        cbind("D",ADJ_PARTISAN_D),
                                        cbind("R",ADJ_PARTISAN_R)))
    colnames(partisan_temp) = c("partisan","PARTISAN_BIAS")
    partisan_temp$PARTISAN_BIAS = as.numeric(partisan_temp$PARTISAN_BIAS)
    
    temp_adj = temp %>% left_join(.,partisan_temp,by="partisan") %>%
                  mutate(DEM_ADJ = DEM_PRCT + 0.5*PARTISAN_BIAS,
                         REP_ADJ = REP_PRCT - 0.5*PARTISAN_BIAS) %>%
                  mutate(miss_adj = (REP_ADJ - DEM_ADJ) - (REP_ACT - DEM_ACT))
    
    #Get the comparison pollster (the one left out)
    #rel_adj = round(mean(unlist(temp %>% filter(pollster == setdiff(temp$pollster,pollster_coef$pollster)) %>% select(miss))),2)
    
    #Get intercept coefficient
    #int = round(unlist(MISS_COEF %>% filter(VAR == "(Intercept)") %>% select("COEFFICIENT")),2)
    
    #Create Pollster Bias Values
    # pollster_temp = temp %>% group_by(pollster) %>% summarise(count=n()) %>% 
    #                   left_join(.,pollster_coef,by="pollster") %>%
    #                   mutate(COEFFICIENT = replace_na(COEFFICIENT, 0)) %>%
    #                   mutate(POLLSTER_BIAS = COEFFICIENT - rel_adj + y_bar + int) %>%
    #                   select(-COEFFICIENT)
    
    pollster_temp = temp_adj %>% group_by(pollster) %>% 
                      summarise(count=n(),
                                POLLSTER_BIAS = round(mean(miss_adj),2))
    
    #Calculate Poll Performance vs Expecation
    temp$abs_miss_pred = round(predict(m2,temp),2)
    temp$abs_miss_res = round(temp$abs_miss - temp$abs_miss_pred,2)
    
    #Calculate Performance Relative to Other Pollsters
    temp$abs_miss_rel_res = NA
    temp$rel_pollsters = NA
    for(k in seq(from=1,to=dim(temp)[1],by=1)){
      holder = temp %>% filter(race==temp[k,"race"] & year==temp[k,"year"] & !pollster==temp[k,"pollster"])
      if(dim(holder)[1]>0){
        temp[k,"abs_miss_rel_res"] = round(temp[k,"abs_miss"] - mean(holder$abs_miss),2)
        temp[k,"rel_pollsters"] = length(unique(holder$pollster))
      } else {
        temp[k,"abs_miss_rel_res"] = 0
        temp[k,"rel_pollsters"] = 0
      }
    }
    
    #Calculate SD to score on
    pollster_sd = temp %>% left_join(.,pollster_temp,by="pollster") %>% left_join(.,partisan_temp,by="partisan") %>%
                    mutate(REP_ADJ = REP_PRCT - .5*POLLSTER_BIAS - .5*PARTISAN_BIAS,
                           DEM_ADJ = DEM_PRCT + .5*POLLSTER_BIAS + .5*PARTISAN_BIAS) %>% 
                    mutate(adj_miss = (REP_ADJ - DEM_ADJ) - (REP_ACT - DEM_ACT)) %>%
                    group_by(pollster) %>%
                    summarise(pollster_sd = round(sd(adj_miss),2)) %>%
                    replace(is.na(.), 0)
    pollster_temp = pollster_temp %>% left_join(.,pollster_sd,by="pollster")
    
    #Calculate Pollster Performance Score and Make Table
    temp$pollster_score = round((3 * temp$abs_miss_res + temp$rel_pollsters*temp$abs_miss_rel_res ) / (3 + temp$rel_pollsters),2)
    pollster_perf_temp = temp %>% group_by(pollster) %>% summarise(simple_score = round(mean(abs_miss_res,na.rm=T),2),
                                                                   adv_score = round(mean(pollster_score,na.rm=T),2))
    
    #Join Everything into One Dataset
    pollster_temp = pollster_temp %>% left_join(.,pollster_perf_temp,by="pollster")
    
    #Calculate Typical Miss Based on Different Methods to Average Polls
    for(k in seq(from=1,to=length(cutoffs),by=1)){
      theTest = test %>% left_join(.,pollster_temp %>% filter(count >= cutoffs[k] | (count >= 5 & adv_score <= quantile(pollster_temp$adv_score,.25))),by="pollster") %>%
                         left_join(.,partisan_temp,by="partisan") 
                  
      #Case 1: Straight Average (Base Case Scenario)
      str_avg = theTest %>% group_by(race_id,type_simple) %>%
                  summarise(REP_POLL = round(mean(REP_PRCT),2),
                            DEM_POLL = round(mean(DEM_PRCT),2),
                            REP_ACT = round(mean(REP_ACT),2),
                            DEM_ACT = round(mean(DEM_ACT),2)) %>%
                  mutate(miss = round((REP_POLL - DEM_POLL) - (REP_ACT - DEM_ACT),2)) %>%
                  mutate(abs_miss = abs(miss)) %>%
                  ungroup() %>%
                  group_by(type_simple) %>%
                  summarise(miss_avg = round(mean(miss),2),
                            miss_sd = round(sd(miss),2),
                            abs_miss_avg = round(mean(abs_miss),2),
                            abs_miss_sd = round(sd(abs_miss),2)) %>%
                  mutate(year = i,
                         lookback = j,
                         cutoff = cutoffs[k],
                         missing_replace = 0,
                         bias_adj = "no",
                         pollster_score_type = "none",
                         sample_size_weight = 0,
                         time_weight = 0,
                         pollster_weight = 0)
      
      if(theScores_found == FALSE){
        theScores = str_avg
        theScores_found = TRUE
      } else {
        theScores = rbind(theScores,str_avg)
      }
      
      
      
      for(m in seq(from=0.5,to=1,by=0.05)){
        test_temp = theTest %>%
                      mutate(POLLSTER_BIAS = replace_na(POLLSTER_BIAS,y_bar),
                             pollster_sd = replace_na(pollster_sd,quantile(pollster_temp$pollster_sd,m)),
                             simple_score = replace_na(simple_score,quantile(pollster_temp$simple_score,m)),
                             adv_score = replace_na(adv_score,quantile(pollster_temp$adv_score,m)),
                             PARTISAN_BIAS = replace_na(PARTISAN_BIAS,0))
        
        #Case 2: Straight Average with a Bias Adjustment
        str_avg_wbias = test_temp %>%
                          mutate(REP_ADJ = REP_PRCT - .5*POLLSTER_BIAS - .5*PARTISAN_BIAS,
                                 DEM_ADJ = DEM_PRCT + .5*POLLSTER_BIAS + .5*PARTISAN_BIAS) %>% group_by(race_id,type_simple) %>%
                          summarise(REP_POLL = round(mean(REP_ADJ),2),
                                    DEM_POLL = round(mean(DEM_ADJ),2),
                                    REP_ACT = round(mean(REP_ACT),2),
                                    DEM_ACT = round(mean(DEM_ACT),2)) %>%
                          mutate(miss = round((REP_POLL - DEM_POLL) - (REP_ACT - DEM_ACT),2)) %>%
                          mutate(abs_miss = abs(miss)) %>%
                          ungroup() %>%
                          group_by(type_simple) %>%
                          summarise(miss_avg = round(mean(miss),2),
                                    miss_sd = round(sd(miss),2),
                                    abs_miss_avg = round(mean(abs_miss),2),
                                    abs_miss_sd = round(sd(abs_miss),2)) %>%
                          mutate(year = i,
                                 lookback = j,
                                 cutoff = cutoffs[k],
                                 missing_replace = 0,
                                 bias_adj = "yes",
                                 pollster_score_type = "none",
                                 sample_size_weight = 0,
                                 time_weight = 0,
                                 pollster_weight = 0)
        
        if(theScores_found == FALSE){
          theScores = str_avg_wbias
          theScores_found = TRUE
        } else {
          theScores = rbind(theScores,str_avg_wbias)
        }
        
        
        #Create sample size, time, and pollster scores
        test_temp$samplesize_score = (test_temp$samplesize/600) ^ 0.5
        test_temp$time_score = ((1 -( test_temp$days_to_election)*0.0125 - log((test_temp$days_to_election))*0.28) - (1 - 30*0.0125 - log(30)*0.28)) / ((1 - 1*0.0125 - log(1)*0.28) - (1 - 30*0.0125 - log(30)*0.28)) * 0.5 + 0.5
        test_temp$pollster_simple_score = ((test_temp$simple_score - min(test_temp$simple_score))/(max(test_temp$simple_score)-min(test_temp$simple_score))-1)*-1+0.5
        test_temp$pollster_adv_score = ((test_temp$adv_score - min(test_temp$adv_score))/(max(test_temp$adv_score)-min(test_temp$adv_score))-1)*-1+0.5
        test_temp$pollster_sd_score = ((test_temp$pollster_sd - min(test_temp$pollster_sd))/(max(test_temp$pollster_sd)-min(test_temp$pollster_sd))-1)*-1+0.5
        test_temp$pollster_sd_score2 = ifelse(test_temp$pollster_sd <= 3, 1.5,
                                                ifelse(test_temp$pollster_sd <= 4, 1.25,
                                                       ifelse(test_temp$pollster_sd <= 6, 1,
                                                              ifelse(test_temp$pollster_sd <= 7.5, .75, .5))))
        
        #Case 3: Sample Size Weight Only
        weight_samplesize = test_temp %>%
                              mutate(REP_ADJ = REP_PRCT - .5*POLLSTER_BIAS - .5*PARTISAN_BIAS,
                                     DEM_ADJ = DEM_PRCT + .5*POLLSTER_BIAS + .5*PARTISAN_BIAS) %>% group_by(race_id,type_simple) %>%
                              summarise(REP_POLL = round(weighted.mean(REP_ADJ,samplesize_score),2),
                                        DEM_POLL = round(weighted.mean(DEM_ADJ,samplesize_score),2),
                                        REP_ACT = round(mean(REP_ACT),2),
                                        DEM_ACT = round(mean(DEM_ACT),2)) %>%
                              mutate(miss = round((REP_POLL - DEM_POLL) - (REP_ACT - DEM_ACT),2)) %>%
                              mutate(abs_miss = abs(miss)) %>%
                              ungroup() %>%
                              group_by(type_simple) %>%
                              summarise(miss_avg = round(mean(miss),2),
                                        miss_sd = round(sd(miss),2),
                                        abs_miss_avg = round(mean(abs_miss),2),
                                        abs_miss_sd = round(sd(abs_miss),2)) %>%
                              mutate(year = i,
                                     lookback = j,
                                     cutoff = cutoffs[k],
                                     missing_replace = m,
                                     bias_adj = "yes",
                                     pollster_score_type = "none",
                                     sample_size_weight = 1,
                                     time_weight = 0,
                                     pollster_weight = 0)
        
        if(theScores_found == FALSE){
          theScores = weight_samplesize
          theScores_found = TRUE
        } else {
          theScores = rbind(theScores,weight_samplesize)
        }
        
        #Case 4: Time Weight Only
        weight_time = test_temp %>%
                        mutate(REP_ADJ = REP_PRCT - .5*POLLSTER_BIAS - .5*PARTISAN_BIAS,
                               DEM_ADJ = DEM_PRCT + .5*POLLSTER_BIAS + .5*PARTISAN_BIAS) %>% group_by(race_id,type_simple) %>%
                        summarise(REP_POLL = round(weighted.mean(REP_ADJ,time_score),2),
                                  DEM_POLL = round(weighted.mean(DEM_ADJ,time_score),2),
                                  REP_ACT = round(mean(REP_ACT),2),
                                  DEM_ACT = round(mean(DEM_ACT),2)) %>%
                        mutate(miss = round((REP_POLL - DEM_POLL) - (REP_ACT - DEM_ACT),2)) %>%
                        mutate(abs_miss = abs(miss)) %>%
                        ungroup() %>%
                        group_by(type_simple) %>%
                        summarise(miss_avg = round(mean(miss),2),
                                  miss_sd = round(sd(miss),2),
                                  abs_miss_avg = round(mean(abs_miss),2),
                                  abs_miss_sd = round(sd(abs_miss),2)) %>%
                        mutate(year = i,
                               lookback = j,
                               cutoff = cutoffs[k],
                               missing_replace = m,
                               bias_adj = "yes",
                               pollster_score_type = "none",
                               sample_size_weight = 0,
                               time_weight = 1,
                               pollster_weight = 0)
        
        if(theScores_found == FALSE){
          theScores = weight_time
          theScores_found = TRUE
        } else {
          theScores = rbind(theScores,weight_time)
        }
        
        #Case 4: Pollster SD
        weight_sd = test_temp %>%
                        mutate(REP_ADJ = REP_PRCT - .5*POLLSTER_BIAS - .5*PARTISAN_BIAS,
                               DEM_ADJ = DEM_PRCT + .5*POLLSTER_BIAS + .5*PARTISAN_BIAS) %>% group_by(race_id,type_simple) %>%
                        summarise(REP_POLL = round(weighted.mean(REP_ADJ,pollster_sd_score),2),
                                  DEM_POLL = round(weighted.mean(DEM_ADJ,pollster_sd_score),2),
                                  REP_ACT = round(mean(REP_ACT),2),
                                  DEM_ACT = round(mean(DEM_ACT),2)) %>%
                        mutate(miss = round((REP_POLL - DEM_POLL) - (REP_ACT - DEM_ACT),2)) %>%
                        mutate(abs_miss = abs(miss)) %>%
                        ungroup() %>%
                        group_by(type_simple) %>%
                        summarise(miss_avg = round(mean(miss),2),
                                  miss_sd = round(sd(miss),2),
                                  abs_miss_avg = round(mean(abs_miss),2),
                                  abs_miss_sd = round(sd(abs_miss),2)) %>%
                        mutate(year = i,
                               lookback = j,
                               cutoff = cutoffs[k],
                               missing_replace = m,
                               bias_adj = "yes",
                               pollster_score_type = "Pollster SD",
                               sample_size_weight = 0,
                               time_weight = 0,
                               pollster_weight = 1)
                      
        if(theScores_found == FALSE){
          theScores = weight_sd
          theScores_found = TRUE
        } else {
          theScores = rbind(theScores,weight_sd)
        }
        
        
        #Case 4: Pollster SD
        weight_sd2 = test_temp %>%
                      mutate(REP_ADJ = REP_PRCT - .5*POLLSTER_BIAS - .5*PARTISAN_BIAS,
                             DEM_ADJ = DEM_PRCT + .5*POLLSTER_BIAS + .5*PARTISAN_BIAS) %>% group_by(race_id,type_simple) %>%
                      summarise(REP_POLL = round(weighted.mean(REP_ADJ,pollster_sd_score2),2),
                                DEM_POLL = round(weighted.mean(DEM_ADJ,pollster_sd_score2),2),
                                REP_ACT = round(mean(REP_ACT),2),
                                DEM_ACT = round(mean(DEM_ACT),2)) %>%
                      mutate(miss = round((REP_POLL - DEM_POLL) - (REP_ACT - DEM_ACT),2)) %>%
                      mutate(abs_miss = abs(miss)) %>%
                      ungroup() %>%
                      group_by(type_simple) %>%
                      summarise(miss_avg = round(mean(miss),2),
                                miss_sd = round(sd(miss),2),
                                abs_miss_avg = round(mean(abs_miss),2),
                                abs_miss_sd = round(sd(abs_miss),2)) %>%
                      mutate(year = i,
                             lookback = j,
                             cutoff = cutoffs[k],
                             missing_replace = m,
                             bias_adj = "yes",
                             pollster_score_type = "Pollster SD 2",
                             sample_size_weight = 0,
                             time_weight = 0,
                             pollster_weight = 1)
                    
        if(theScores_found == FALSE){
          theScores = weight_sd2
          theScores_found = TRUE
        } else {
          theScores = rbind(theScores,weight_sd2)
        }
        
        
        #Case 5: Weight All - Simple
        weight_all_simple = test_temp %>%
          mutate(REP_ADJ = REP_PRCT - .5*POLLSTER_BIAS - .5*PARTISAN_BIAS,
                 DEM_ADJ = DEM_PRCT + .5*POLLSTER_BIAS + .5*PARTISAN_BIAS,
                 theWeight = pollster_simple_score * time_score * samplesize_score) %>% 
          group_by(race_id,type_simple) %>%
          summarise(REP_POLL = round(weighted.mean(REP_ADJ,theWeight),2),
                    DEM_POLL = round(weighted.mean(DEM_ADJ,theWeight),2),
                    REP_ACT = round(mean(REP_ACT),2),
                    DEM_ACT = round(mean(DEM_ACT),2)) %>%
          mutate(miss = round((REP_POLL - DEM_POLL) - (REP_ACT - DEM_ACT),2)) %>%
          mutate(abs_miss = abs(miss)) %>%
          ungroup() %>%
          group_by(type_simple) %>%
          summarise(miss_avg = round(mean(miss),2),
                    miss_sd = round(sd(miss),2),
                    abs_miss_avg = round(mean(abs_miss),2),
                    abs_miss_sd = round(sd(abs_miss),2)) %>%
          mutate(year = i,
                 lookback = j,
                 cutoff = cutoffs[k],
                 missing_replace = m,
                 bias_adj = "yes",
                 pollster_score_type = "Simple - Multiplicitive",
                 sample_size_weight = 1,
                 time_weight = 1,
                 pollster_weight = 1)
        
        if(theScores_found == FALSE){
          theScores = weight_all_simple
          theScores_found = TRUE
        } else {
          theScores = rbind(theScores,weight_all_simple)
        }
        
        #Case 6: Weight All - Advanced
        weight_all_adv = test_temp %>%
                          mutate(REP_ADJ = REP_PRCT - .5*POLLSTER_BIAS - .5*PARTISAN_BIAS,
                                 DEM_ADJ = DEM_PRCT + .5*POLLSTER_BIAS + .5*PARTISAN_BIAS,
                                 theWeight = pollster_adv_score * time_score * samplesize_score) %>% 
                          group_by(race_id,type_simple) %>%
                          summarise(REP_POLL = round(weighted.mean(REP_ADJ,theWeight),2),
                                    DEM_POLL = round(weighted.mean(DEM_ADJ,theWeight),2),
                                    REP_ACT = round(mean(REP_ACT),2),
                                    DEM_ACT = round(mean(DEM_ACT),2)) %>%
                          mutate(miss = round((REP_POLL - DEM_POLL) - (REP_ACT - DEM_ACT),2)) %>%
                          mutate(abs_miss = abs(miss)) %>%
                          ungroup() %>%
                          group_by(type_simple) %>%
                          summarise(miss_avg = round(mean(miss),2),
                                    miss_sd = round(sd(miss),2),
                                    abs_miss_avg = round(mean(abs_miss),2),
                                    abs_miss_sd = round(sd(abs_miss),2)) %>%
                          mutate(year = i,
                                 lookback = j,
                                 cutoff = cutoffs[k],
                                 missing_replace = m,
                                 bias_adj = "yes",
                                 pollster_score_type = "Advanced - Multiplicitive",
                                 sample_size_weight = 1,
                                 time_weight = 1,
                                 pollster_weight = 1)
        
        if(theScores_found == FALSE){
          theScores = weight_all_adv
          theScores_found = TRUE
        } else {
          theScores = rbind(theScores,weight_all_adv)
        }
        
        #Case 7: Weight All - Pollster SD
        weight_all_sd = test_temp %>%
                          mutate(REP_ADJ = REP_PRCT - .5*POLLSTER_BIAS - .5*PARTISAN_BIAS,
                                 DEM_ADJ = DEM_PRCT + .5*POLLSTER_BIAS + .5*PARTISAN_BIAS,
                                 theWeight = pollster_sd_score * time_score * samplesize_score) %>% 
                          group_by(race_id,type_simple) %>%
                          summarise(REP_POLL = round(weighted.mean(REP_ADJ,theWeight),2),
                                    DEM_POLL = round(weighted.mean(DEM_ADJ,theWeight),2),
                                    REP_ACT = round(mean(REP_ACT),2),
                                    DEM_ACT = round(mean(DEM_ACT),2),
                                    count = n()) %>%
                          mutate(miss = round((REP_POLL - DEM_POLL) - (REP_ACT - DEM_ACT),2)) %>%
                          mutate(abs_miss = abs(miss)) %>%
                          ungroup() %>%
                          group_by(type_simple) %>%
                          summarise(miss_avg = round(mean(miss),2),
                                    miss_sd = round(sd(miss),2),
                                    abs_miss_avg = round(mean(abs_miss),2),
                                    abs_miss_sd = round(sd(abs_miss),2)) %>%
                          mutate(year = i,
                                 lookback = j,
                                 cutoff = cutoffs[k],
                                 missing_replace = m,
                                 bias_adj = "yes",
                                 pollster_score_type = "Pollster SD - Multiplicitive",
                                 sample_size_weight = 1,
                                 time_weight = 1,
                                 pollster_weight = 1)
        
        if(theScores_found == FALSE){
          theScores = weight_all_sd
          theScores_found = TRUE
        } else {
          theScores = rbind(theScores,weight_all_sd)
        }
        
        #Loop through different weights
        # for(x in seq(from=1,to=length(samplesize_weight),by=1)){
        #   for(y in seq(from=1,to=length(time_weight),by=1)){
        #     for(z in seq(from=1,to=length(pollster_weight),by=1)){
        #       if(samplesize_weight[x] == 0 & time_weight[y] == 0){
        #         test_temp$simple_weight = test_temp$pollster_simple_score * pollster_weight[z]
        #         test_temp$adv_weight = test_temp$pollster_adv_score * pollster_weight[z]
        #       } else if(samplesize_weight[x] == 0 & time_weight[y] > 0){
        #         test_temp$simple_weight = test_temp$time_score * time_weight[y] + test_temp$pollster_simple_score * pollster_weight[z]
        #         test_temp$adv_weight = test_temp$time_score * time_weight[y] + test_temp$pollster_adv_score * pollster_weight[z]
        #       } else if(samplesize_weight[x] > 0 & time_weight[y] == 0){
        #         test_temp$simple_weight = test_temp$samplesize_score * samplesize_weight[x] + test_temp$pollster_simple_score * pollster_weight[z]
        #         test_temp$adv_weight = test_temp$samplesize_score * samplesize_weight[x] + test_temp$pollster_adv_score * pollster_weight[z]
        #       } else {
        #         test_temp$simple_weight = test_temp$samplesize_score * samplesize_weight[x] + test_temp$time_score * time_weight[y] + test_temp$pollster_simple_score * pollster_weight[z]
        #         test_temp$adv_weight = test_temp$samplesize_score * samplesize_weight[x] + test_temp$time_score * time_weight[y] + test_temp$pollster_adv_score * pollster_weight[z]
        #       }
        #       
        #       #Case 7: Weight All - Simple (Additive)
        #       weight_add_simp = test_temp %>%
        #         mutate(REP_ADJ = REP_PRCT - .5*POLLSTER_BIAS - .5*PARTISAN_BIAS,
        #                DEM_ADJ = DEM_PRCT + .5*POLLSTER_BIAS + .5*PARTISAN_BIAS) %>% 
        #         group_by(race_id,type_simple) %>%
        #         summarise(REP_POLL = round(weighted.mean(REP_ADJ,simple_weight),2),
        #                   DEM_POLL = round(weighted.mean(DEM_ADJ,simple_weight),2),
        #                   REP_ACT = round(mean(REP_ACT),2),
        #                   DEM_ACT = round(mean(DEM_ACT),2)) %>%
        #         mutate(miss = round((REP_POLL - DEM_POLL) - (REP_ACT - DEM_ACT),2)) %>%
        #         mutate(abs_miss = abs(miss)) %>%
        #         ungroup() %>%
        #         group_by(type_simple) %>%
        #         summarise(miss_avg = round(mean(miss),2),
        #                   miss_sd = round(sd(miss),2),
        #                   abs_miss_avg = round(mean(abs_miss),2),
        #                   abs_miss_sd = round(sd(abs_miss),2)) %>%
        #         mutate(year = i,
        #                lookback = j,
        #                cutoff = cutoffs[k],
        #                missing_replace = m,
        #                bias_adj = "yes",
        #                pollster_score_type = "Simple - Additive",
        #                sample_size_weight = samplesize_weight[x],
        #                time_weight = time_weight[y],
        #                pollster_weight = pollster_weight[z])
        #       
        #       if(theScores_found == FALSE){
        #         theScores = weight_add_simp
        #         theScores_found = TRUE
        #       } else {
        #         theScores = rbind(theScores,weight_add_simp)
        #       }
        #       
        #       #Case 8: Weight All - Advanced (Additive)
        #       weight_add_adv = test_temp %>%
        #         mutate(REP_ADJ = REP_PRCT - .5*POLLSTER_BIAS - .5*PARTISAN_BIAS,
        #                DEM_ADJ = DEM_PRCT + .5*POLLSTER_BIAS + .5*PARTISAN_BIAS) %>% 
        #         group_by(race_id,type_simple) %>%
        #         summarise(REP_POLL = round(weighted.mean(REP_ADJ,adv_weight),2),
        #                   DEM_POLL = round(weighted.mean(DEM_ADJ,adv_weight),2),
        #                   REP_ACT = round(mean(REP_ACT),2),
        #                   DEM_ACT = round(mean(DEM_ACT),2)) %>%
        #         mutate(miss = round((REP_POLL - DEM_POLL) - (REP_ACT - DEM_ACT),2)) %>%
        #         mutate(abs_miss = abs(miss)) %>%
        #         ungroup() %>%
        #         group_by(type_simple) %>%
        #         summarise(miss_avg = round(mean(miss),2),
        #                   miss_sd = round(sd(miss),2),
        #                   abs_miss_avg = round(mean(abs_miss),2),
        #                   abs_miss_sd = round(sd(abs_miss),2)) %>%
        #         mutate(year = i,
        #                lookback = j,
        #                cutoff = cutoffs[k],
        #                missing_replace = m,
        #                bias_adj = "yes",
        #                pollster_score_type = "Advanced - Additive",
        #                sample_size_weight = samplesize_weight[x],
        #                time_weight = time_weight[y],
        #                pollster_weight = pollster_weight[z])
        #       
        #       if(theScores_found == FALSE){
        #         theScores = weight_add_adv
        #         theScores_found = TRUE
        #       } else {
        #         theScores = rbind(theScores,weight_add_adv)
        #       }
        #     }
        #   }
        # }
      }
      
    }
  }
}
theScores_final = theScores %>% distinct()
write.csv(theScores_final,"Data/Polls/For Pollster Ratings/theScores.csv",row.names=F)

#Try to pick best overall performance... pollster performance has really level shifted since 2014, so only using that time period
performance = theScores_final %>% 
                group_by(type_simple,lookback,cutoff,missing_replace,bias_adj,pollster_score_type,
                         sample_size_weight,time_weight,pollster_weight) %>%
                summarise(count=n(),
                          abs_miss_avg_agg = mean(abs_miss_avg),
                          abs_miss_avg_min = min(abs_miss_avg),
                          abs_miss_avg_max = max(abs_miss_avg),
                          miss_avg_agg = mean(miss_avg),
                          miss_avg_min = min(miss_avg),
                          miss_avg_max = max(miss_avg))

house_performance = performance %>% filter(type_simple == "House-G") %>% mutate(diff = miss_avg_max - miss_avg_min)
pres_performance = performance %>% filter(type_simple == "Pres-G") %>% mutate(diff = miss_avg_max - miss_avg_min)

#Selecting best model... which includes 20 poll cutoff or 5 polls for high performing pollsters
#Setting parameters to best performer: Advanced - Multiplicitive
#                                      Cutoff: 20
#                                      Lookback: 6
#                                      Missing Replace: 1.0
#                                      Bias Adjusted: Yes

#Including the y_bar does perform slightly better on average, but the range of misses grows by almost a full 1%, not worth it

#Create final dataset
avg_bias = NA
avg_bias_found = FALSE
pollster_bias = NA
pollster_bias_found = FALSE
partisan_bias = NA
partisan_bias_found = FALSE
for(i in seq(from=2004,to=2022,by=2)){
  cat(i,"\n",sep="")
  temp = the_polls %>% filter(year < i & year >= (i-6))
  
  #Try to control for the effects of the race being polled
  m1 = lm(miss ~ type_simple + partisan + sampling_error + days_to_election + log(days_to_election) + pollster,data=temp)
  summary(m1)
  
  m2 = lm(abs_miss ~ type_simple + partisan + sampling_error + days_to_election + log(days_to_election),data=temp)
  summary(m2)
  
  #Get the overall bias of the polling...stack these to use later in model
  y_bar = round(mean(temp$miss),2)
  
  if(avg_bias_found==FALSE){
    avg_bias = as.data.frame(cbind(i,y_bar))
    colnames(avg_bias) = c("year","avg_bias")
    avg_bias_found = TRUE
  } else {
    avg_bias_temp = as.data.frame(cbind(i,y_bar))
    colnames(avg_bias_temp) = c("year","avg_bias")
    avg_bias = rbind(avg_bias,avg_bias_temp)
  }
  
  #Get coefficients for partisan poll
  MISS_COEF = as.data.frame(m1$coefficients)
  colnames(MISS_COEF) = "COEFFICIENT"
  MISS_COEF$COEFFICIENT = round(MISS_COEF$COEFFICIENT,2)
  MISS_COEF$VAR = str_replace_all(row.names(MISS_COEF),"pollster","")
  pollster_coef = MISS_COEF %>% filter(str_detect(row.names(MISS_COEF),"pollster")) %>% rename(pollster=VAR)
  
  ADJ_PARTISAN_R = round(as.numeric(unlist(MISS_COEF %>% filter(VAR == "partisanR") %>% select("COEFFICIENT"))),2)
  ADJ_PARTISAN_D = round(as.numeric(unlist(MISS_COEF %>% filter(VAR == "partisanD") %>% select("COEFFICIENT"))),2)
  
  #Create Partisan Bias Values
  partisan_temp = as.data.frame(rbind(cbind("",0),
                                      cbind("D",ADJ_PARTISAN_D),
                                      cbind("R",ADJ_PARTISAN_R)))
  colnames(partisan_temp) = c("partisan","PARTISAN_BIAS")
  partisan_temp$PARTISAN_BIAS = as.numeric(partisan_temp$PARTISAN_BIAS)
  
  if(partisan_bias_found==FALSE){
    partisan_bias = partisan_temp %>% mutate(year=i) %>% select(year,partisan,PARTISAN_BIAS)
    partisan_bias_found = TRUE
  } else {
    partisan_bias = rbind(partisan_bias,
                          partisan_temp %>% mutate(year=i) %>% select(year,partisan,PARTISAN_BIAS))
  }
  
  temp_adj = temp %>% left_join(.,partisan_temp,by="partisan") %>%
    mutate(DEM_ADJ = DEM_PRCT + 0.5*PARTISAN_BIAS,
           REP_ADJ = REP_PRCT - 0.5*PARTISAN_BIAS) %>%
    mutate(miss_adj = (REP_ADJ - DEM_ADJ) - (REP_ACT - DEM_ACT))
  
  #Get the comparison pollster (the one left out)
  #rel_adj = round(mean(unlist(temp %>% filter(pollster == setdiff(temp$pollster,pollster_coef$pollster)) %>% select(miss))),2)
  
  #Get intercept coefficient
  #int = round(unlist(MISS_COEF %>% filter(VAR == "(Intercept)") %>% select("COEFFICIENT")),2)
  
  #Create Pollster Bias Values
  # pollster_temp = temp %>% group_by(pollster) %>% summarise(count=n()) %>% 
  #                   left_join(.,pollster_coef,by="pollster") %>%
  #                   mutate(COEFFICIENT = replace_na(COEFFICIENT, 0)) %>%
  #                   mutate(POLLSTER_BIAS = COEFFICIENT - rel_adj + y_bar + int) %>%
  #                   select(-COEFFICIENT)
  
  pollster_temp = temp_adj %>% group_by(pollster) %>% 
    summarise(count=n(),
              POLLSTER_BIAS = round(mean(miss_adj),2))
  
  #Calculate Poll Performance vs Expecation
  temp$abs_miss_pred = round(predict(m2,temp),2)
  temp$abs_miss_res = round(temp$abs_miss - temp$abs_miss_pred,2)
  
  #Calculate Performance Relative to Other Pollsters
  temp$abs_miss_rel_res = NA
  temp$rel_pollsters = NA
  for(k in seq(from=1,to=dim(temp)[1],by=1)){
    holder = temp %>% filter(race==temp[k,"race"] & year==temp[k,"year"] & !pollster==temp[k,"pollster"])
    if(dim(holder)[1]>0){
      temp[k,"abs_miss_rel_res"] = round(temp[k,"abs_miss"] - mean(holder$abs_miss),2)
      temp[k,"rel_pollsters"] = length(unique(holder$pollster))
    } else {
      temp[k,"abs_miss_rel_res"] = 0
      temp[k,"rel_pollsters"] = 0
    }
  }
  
  #Calculate Pollster Performance Score and Make Table
  temp$pollster_score = round((3 * temp$abs_miss_res + temp$rel_pollsters*temp$abs_miss_rel_res ) / (3 + temp$rel_pollsters),2)
  pollster_perf_temp = temp %>% group_by(pollster) %>% summarise(adv_score = round(mean(pollster_score,na.rm=T),2))
  
  #Join Everything into One Dataset
  pollster_temp = pollster_temp %>% left_join(.,pollster_perf_temp,by="pollster") %>% filter(count>=10 | (count>=5 & adv_score <= -1.5))
  
  if(pollster_bias_found==FALSE){
    pollster_bias = pollster_temp %>% mutate(year=i) %>% select(year,pollster,count,POLLSTER_BIAS,adv_score)
    pollster_bias_found = TRUE
  } else {
    pollster_bias = rbind(pollster_bias,
                          pollster_temp %>% mutate(year=i) %>% select(year,pollster,count,POLLSTER_BIAS,adv_score))
  }
}

write.csv(avg_bias,"Data/Polls/For Pollster Ratings/forModeling/avg_bias.csv",row.names=F)
write.csv(partisan_bias,"Data/Polls/For Pollster Ratings/forModeling/partisan_bias.csv",row.names=F)
write.csv(pollster_bias,"Data/Polls/For Pollster Ratings/forModeling/pollster_bias.csv",row.names=F)

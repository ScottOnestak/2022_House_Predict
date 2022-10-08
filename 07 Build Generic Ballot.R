#2022 Election Project
#Author: Scott Onestak
#7 Build Generic Ballot 

#libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(stringr)
library(fredr)
library(h2o)
library(modi)

#read in datasets
pollster_bias = read.csv('Data/Polls/For Pollster Ratings/forModeling/pollster_bias.csv',header=T,stringsAsFactors=F)
partisan_bias = read.csv('Data/Polls/For Pollster Ratings/forModeling/partisan_bias.csv',header=T,stringsAsFactors=F)
avg_bias = read.csv('Data/Polls/For Pollster Ratings/forModeling/avg_bias.csv',header=T,stringsAsFactors=F)
gb_acts = read.csv('Data/Polls/Generic Ballot/generic_ballot_actuals.csv',header=T,stringsAsFactors=F)
gb_polls = read.csv('Data/Polls/Generic Ballot/generic_ballot_historic.csv',header=T,stringsAsFactors=F)

#rename for joining
colnames(avg_bias)[1] = "cycle"
colnames(pollster_bias)[1] = "cycle"
colnames(gb_acts) = c("cycle","GOP_act","DEM_act","diff_act")

#configure the dates
gb_polls$start_date = as.Date(gb_polls$start_date,"%Y-%m-%d")
gb_polls$end_date = as.Date(gb_polls$end_date,"%Y-%m-%d")
gb_polls$election_date = as.Date(gb_polls$election_date,"%Y-%m-%d")

#calculate the midpoint date of the poll
gb_polls$mid_date = NA
gb_polls$days_to_election = NA
for(i in seq(from=1,to=dim(gb_polls)[1],by=1)){
  gb_polls[i,"mid_date"] = mean.Date(c(gb_polls[i,"start_date"],gb_polls[i,"end_date"]))
  gb_polls[i,"days_to_election"] = as.numeric(gb_polls[i,"election_date"] - gb_polls[i,"mid_date"])
}

#for purposes of sorting, rename some population
gb_polls$population = ifelse(gb_polls$population == "LV","LV","RV")

#determine best lookback cutoff and decay function
time = c(10,15,20,25,30)
year = c(2004,2006,2008,2010,2012,2014,2016,2018,2020)
election_dates = c("2004-11-02","2006-11-07","2008-11-04","2010-11-02","2012-11-06","2014-11-04","2016-11-08","2018-11-06","2020-11-03")

results = NA
for(i in seq(from=1,to=length(year),by=1)){
  theYear = year[i]
  for(j in seq(from=1,to=length(time),by=1)){
    theTime = time[j]
    
    temp = gb_polls %>% filter(cycle==theYear & days_to_election <= theTime & days_to_election >= 0) %>% 
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
                               adv_score = replace_na(adv_score,quantile(unlist(pollster_bias %>% filter(cycle==theYear) %>% select(adv_score)),1))) %>%
                        mutate(sample_size_score = sqrt(sample_size/600),
                               pollster_adv_score = ((adv_score - min(pollster_bias$adv_score)) / (max(pollster_bias$adv_score) - min(pollster_bias$adv_score)) - 1) * -1 + 0.5)
  
    
    #No adjustment
    none = temp %>% 
            group_by(cycle) %>% 
            summarise(DEM = mean(DEM),
                      GOP = mean(GOP)) %>%
            mutate(time = theTime,
                   type = "None") %>%
            select(cycle,time,type,DEM,GOP) %>%
            mutate(diff = GOP - DEM) %>%
            inner_join(.,gb_acts,by="cycle") %>%
            mutate(miss = diff - diff_act,
                   abs_miss = abs(diff - diff_act))
    
    #Get baseline average
    base = temp %>% 
              group_by(cycle) %>% 
              summarise(DEM = mean(DEM_adj),
                        GOP = mean(GOP_adj)) %>%
              mutate(time = theTime,
                     type = "Base") %>%
              select(cycle,time,type,DEM,GOP) %>%
              mutate(diff = GOP - DEM) %>%
              inner_join(.,gb_acts,by="cycle") %>%
              mutate(miss = diff - diff_act,
                     abs_miss = abs(diff - diff_act))
    
    #Sample size only
    ss = temp %>%
            group_by(cycle) %>%
            summarise(DEM = weighted.mean(DEM_adj,sample_size_score),
                      GOP = weighted.mean(GOP_adj,sample_size_score)) %>%
            mutate(time = theTime,
                   type = "Sample Size Only") %>%
            select(cycle,time,type,DEM,GOP) %>%
            mutate(diff = GOP - DEM) %>%
            inner_join(.,gb_acts,by="cycle") %>%
            mutate(miss = diff - diff_act,
                   abs_miss = abs(diff - diff_act))
    
    
    #Time - 25
    time_25 = temp %>%
                mutate(time_score = (log(days_to_election + 1) / log(theTime + 1) - 1) * -1 * 0.75 + 0.25) %>%
                group_by(cycle) %>%
                summarise(DEM = weighted.mean(DEM_adj,time_score),
                          GOP = weighted.mean(GOP_adj,time_score)) %>%
                mutate(time = theTime,
                       type = "Time - 25") %>%
                select(cycle,time,type,DEM,GOP) %>%
                mutate(diff = GOP - DEM) %>%
                inner_join(.,gb_acts,by="cycle") %>%
                mutate(miss = diff - diff_act,
                       abs_miss = abs(diff - diff_act))
    
    #Advanced - 25
    adv_25 = temp %>%
              mutate(time_score = (log(days_to_election + 1) / log(theTime + 1) - 1) * -1 * 0.75 + 0.25) %>%
              mutate(theWeight = time_score * sample_size_score * pollster_adv_score) %>%
              group_by(cycle) %>%
              summarise(DEM = weighted.mean(DEM_adj,theWeight),
                        GOP = weighted.mean(GOP_adj,theWeight)) %>%
              mutate(time = theTime,
                     type = "Advanced - 25") %>%
              select(cycle,time,type,DEM,GOP) %>%
              mutate(diff = GOP - DEM) %>%
              inner_join(.,gb_acts,by="cycle") %>%
              mutate(miss = diff - diff_act,
                     abs_miss = abs(diff - diff_act))
    
    #Time - 50
    time_50 = temp %>%
                mutate(time_score = (log(days_to_election + 1) / log(theTime + 1) - 1) * -1 * 0.50 + 0.50) %>%
                group_by(cycle) %>%
                summarise(DEM = weighted.mean(DEM_adj,time_score),
                          GOP = weighted.mean(GOP_adj,time_score)) %>%
                mutate(time = theTime,
                       type = "Time - 50") %>%
                select(cycle,time,type,DEM,GOP) %>%
                mutate(diff = GOP - DEM) %>%
                inner_join(.,gb_acts,by="cycle") %>%
                mutate(miss = diff - diff_act,
                       abs_miss = abs(diff - diff_act))
    
    #Advanced - 50
    adv_50 = temp %>%
              mutate(time_score = (log(days_to_election + 1) / log(theTime + 1) - 1) * -1 * 0.50 + 0.50) %>%
              mutate(theWeight = time_score * sample_size_score * pollster_adv_score) %>%
              group_by(cycle) %>%
              summarise(DEM = weighted.mean(DEM_adj,theWeight),
                        GOP = weighted.mean(GOP_adj,theWeight)) %>%
              mutate(time = theTime,
                     type = "Advanced - 50") %>%
              select(cycle,time,type,DEM,GOP) %>%
              mutate(diff = GOP - DEM) %>%
              inner_join(.,gb_acts,by="cycle") %>%
              mutate(miss = diff - diff_act,
                     abs_miss = abs(diff - diff_act))
    
    #Time - 75
    time_75 = temp %>%
                mutate(time_score = (log(days_to_election + 1) / log(theTime + 1) - 1) * -1 * 0.25 + 0.75) %>%
                group_by(cycle) %>%
                summarise(DEM = weighted.mean(DEM_adj,time_score),
                          GOP = weighted.mean(GOP_adj,time_score)) %>%
                mutate(time = theTime,
                       type = "Time - 75") %>%
                select(cycle,time,type,DEM,GOP) %>%
                mutate(diff = GOP - DEM) %>%
                inner_join(.,gb_acts,by="cycle") %>%
                mutate(miss = diff - diff_act,
                       abs_miss = abs(diff - diff_act))
    
    #Advanced - 75
    adv_75 = temp %>%
              mutate(time_score = (log(days_to_election + 1) / log(theTime + 1) - 1) * -1 * 0.25 + 0.75) %>%
              mutate(theWeight = time_score * sample_size_score * pollster_adv_score) %>%
              group_by(cycle) %>%
              summarise(DEM = weighted.mean(DEM_adj,theWeight),
                        GOP = weighted.mean(GOP_adj,theWeight)) %>%
              mutate(time = theTime,
                     type = "Advanced - 75") %>%
              select(cycle,time,type,DEM,GOP) %>%
              mutate(diff = GOP - DEM) %>%
              inner_join(.,gb_acts,by="cycle") %>%
              mutate(miss = diff - diff_act,
                     abs_miss = abs(diff - diff_act))
    
    #stack results
    if(i == 1 & j == 1){
      results = rbind(none,base,ss,time_25,time_50,time_75,adv_25,adv_50,adv_75)
    } else {
      results = rbind(none,results,base,ss,time_25,time_50,time_75,adv_25,adv_50,adv_75)
    }
  }
}

#Summarise Results
summary = results %>% group_by(time,type) %>%
            summarise(avg_miss = mean(miss),
                      avg_abs_miss = mean(abs_miss),
                      med_miss = median(miss),
                      med_abs_miss = median(abs_miss),
                      sd_miss = sd(miss),
                      sd_abs_miss = sd(abs_miss))

summary_minus2014 = results %>% filter(cycle != 2014) %>% group_by(time,type) %>%
                      summarise(avg_miss = mean(miss),
                                avg_abs_miss = mean(abs_miss),
                                med_miss = median(miss),
                                med_abs_miss = median(abs_miss),
                                sd_miss = sd(miss),
                                sd_abs_miss = sd(abs_miss))

#A lot of weightings perform very similarly.  However, the Advanced - 75 method has a much lower sd (most stable overall)
#Going with 20 will give enough time for spare time periods not directly before the election
#2014 has a large miss, but 2014 was simply a bad year for polling - not that the methodology is bad


#Get the generic ballot results... go through again and also calculate the standard deviations to view confidence intervals
gb_final = NA
for(i in seq(from=1,to=length(year),by=1)){
  theYear = year[i]
  
  temp = gb_polls %>% filter(cycle==theYear & days_to_election <= 21 & days_to_election >= 0) %>% 
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
                 adv_score = replace_na(adv_score,quantile(unlist(pollster_bias %>% filter(cycle==theYear) %>% select(adv_score)),1))) %>%
          mutate(sample_size_score = sqrt(sample_size/600),
                 pollster_adv_score = ((adv_score - min(pollster_bias$adv_score)) / (max(pollster_bias$adv_score) - min(pollster_bias$adv_score)) - 1) * -1 + 0.5)
  
  #Advanced - 25
  calc = temp %>%
            mutate(time_score = (log(days_to_election + 1) / log(21 + 1) - 1) * -1 * 0.75 + 0.25) %>%
            mutate(theWeight = time_score * sample_size_score * pollster_adv_score) %>%
            group_by(cycle) %>%
            summarise(DEM = round(weighted.mean(DEM_adj,theWeight),2),
                      GOP = round(weighted.mean(GOP_adj,theWeight),2),
                      DEM_SD = round(sqrt(weighted.var(DEM_adj,theWeight)),2),
                      GOP_SD = round(sqrt(weighted.var(GOP_adj,theWeight)),2),
                      count = n()) %>%
            mutate(DEM_min = round(DEM - 1.96 * (DEM_SD / sqrt(count)),2),
                   DEM_max = round(DEM + 1.96 * (DEM_SD / sqrt(count)),2),
                   GOP_min = round(GOP - 1.96 * (GOP_SD / sqrt(count)),2),
                   GOP_max = round(GOP + 1.96 * (GOP_SD / sqrt(count)),2)) %>%
            select(cycle,DEM,GOP,DEM_SD,GOP_SD,count,DEM_min,DEM_max,GOP_min,GOP_max) %>%
            mutate(diff = GOP - DEM,
                   diff_min = GOP_min - DEM_max,
                   diff_max = GOP_max - DEM_min) %>%
            inner_join(.,gb_acts,by="cycle") %>%
            mutate(miss = diff - diff_act,
                   abs_miss = abs(diff - diff_act))
  
  if(i==1){
    gb_final = calc
  } else {
    gb_final = rbind(gb_final,calc)
  }
}

png(filename = "Plots/Generic Ballot/Generic_Ballot_Historic_Performance.png",width = 1920,height = 1080)
ggplot(gb_final)+
  geom_segment(aes(x = diff_min, xend = diff_max, y = cycle, yend = cycle),colour="black", size = 3) +
  geom_point(aes(x = diff, y = cycle),colour = "blue",size = 10) +
  geom_point(aes(x = diff_act, y = cycle),colour = "firebrick",size = 10) +
  theme_classic() +
  scale_y_continuous(name="Election Year",breaks = seq(2002,2022,by=2),limits=c(2002, 2022)) +
  scale_x_continuous(name="Generic Ballot: GOP - DEM",breaks = seq(-18,16,by=2),limits=c(-18,12)) +
  ggtitle("Generic Ballot for Final 3 Weeks: Predicted (Blue) vs Actual (Red) with 95% Confidence Interval") +
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),
        axis.text = element_text(size=20),
        axis.title = element_text(size=20))
dev.off()



#See if we can build a model to more accurately predict the generic ballot than just polling
#Variables: Generic Ballot Polling Averages
#           Party in Power
#           House Election Polling (Including/Excluding Partisan Polls for 1 and 2 cycle leans)
#           Economic Variables
#Short answer... no, it doesn't right next to the election
#Further out, these variables could be more important

#Historical House polls
raw_polls = read.csv("Data/Polls/For Pollster Ratings/raw-polls.csv",header=T,stringsAsFactors=F)
banned_list = unlist(read.csv("Data/Polls/For Pollster Ratings/banned.csv",header=T,stringsAsFactors=F) %>%
                       filter(banned=="yes") %>% select(pollster) %>% distinct())
leans = read.csv("Data/Historical Leans/Leans.csv",header=T,stringsAsFactors=F) %>% rename("cycle"="Year")

the_polls = raw_polls %>% filter(type_simple == "House-G" &
                                 location != "US" &
                                 partisan == "" &
                                 year >= 2004 &
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
                     abs_miss = abs((REP_PRCT - DEM_PRCT) - (REP_ACT - DEM_ACT))) %>%
              rowwise() %>%
              mutate(District = paste(str_split(location,"-")[[1]][1],
                                      "-",
                                      str_pad(as.numeric(str_split(location,"-")[[1]][2]),2,pad="0"),sep="")) %>%
              left_join(.,partisan_bias,by=c("year","partisan")) %>%
              rename("cycle"="year") %>%
              left_join(.,pollster_bias %>% select(-count),by=c("cycle","pollster")) %>%
              left_join(.,avg_bias,by="cycle") %>%
              mutate(adjustment = ifelse(is.na(POLLSTER_BIAS),avg_bias+PARTISAN_BIAS,POLLSTER_BIAS+PARTISAN_BIAS)) %>%
              mutate(DEM_adj = DEM_PRCT + .5*adjustment,
                     GOP_adj = REP_PRCT - .5*adjustment) %>%
              mutate(DIFF = GOP_adj - DEM_adj) %>%
              left_join(.,leans %>% select("cycle","District","PERF_1"),by=c("cycle","District")) %>%
              mutate(rel_house_polling = round(DIFF - PERF_1,2)) %>%
              filter(abs(PERF_1) < 15) %>%
              group_by(cycle) %>%
              summarise(rel_house_polling_mean = round(mean(rel_house_polling),2),
                        rel_house_polling_median = round(median(rel_house_polling),2),
                        count = n()) %>%
              filter(cycle %% 2 == 0)

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
forGBmodel = gb_final %>% mutate(act_miss = GOP_act - DEM_act) %>%
              left_join(.,the_polls %>% select(-count),by="cycle") %>%
              left_join(.,median_real_earnings %>% select(-date),by="cycle") %>%
              left_join(.,unemployment %>% select(-date),by="cycle") %>%
              left_join(.,gas %>% select(-date),by="cycle") %>%
              mutate(party_in_power = ifelse(year %in% c(2004,2006,2008,2018,2020),"R","D"),
                     midterm = ifelse(year %% 4 == 0,0,1))
forGBmodel$party_in_power = as.factor(forGBmodel$party_in_power)
forGBmodel$midterm = as.factor(forGBmodel$midterm)

h2o.init(nthreads=-1,enable_assertions = FALSE)
forGBmodel_h2o = as.h2o(forGBmodel)
train = forGBmodel_h2o[c(1:8),]
test = forGBmodel_h2o[9,]

x = c("GOP","DEM","diff","rel_house_polling_mean","party_in_power","midterm","median_real_earnings","unemployment","gas_price")
y1 = "GOP_act"
y2 = "DEM_act"
y3 = "act_miss"

gb_test = h2o.randomForest(x = x, y = y1,
                          training_frame = train,
                          nfold = 4)
gb_test2 = h2o.gbm(x = x, y = y1, keep_cross_validation_models = TRUE, training_frame = train, min_rows = 3, ntrees = 10)
temp1 = as.vector(h2o.predict(gb_test,newdata=train))
temp2 = as.vector(h2o.predict(gb_test2,newdata=train))



gb_test3 = h2o.randomForest(x = x, y = y2,
                            training_frame = train,
                            nfold = 4)
gb_test4 = h2o.gbm(x = x, y = y2, keep_cross_validation_models = TRUE, training_frame = train, min_rows = 3, ntrees = 10)
temp3 = as.vector(h2o.predict(gb_test3,newdata=train))
temp4 = as.vector(h2o.predict(gb_test4,newdata=train))

temp1 - temp3
temp2 - temp4

h2o.shutdown(prompt=FALSE)

predict(gb_test2,train)

predict(gb_test,train) - predict(gb_test2,train)

abs((predict(gb_test,train) - predict(gb_test2,train)) - forGBmodel$act_miss)
mean(abs((predict(gb_test,train) - predict(gb_test2,train)) - forGBmodel$act_miss))
mean(abs(forGBmodel$diff - forGBmodel$act_miss))

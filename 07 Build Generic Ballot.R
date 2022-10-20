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
time = c(15,20,25,30,35,40)
year = c(2004,2006,2008,2010,2012,2014,2016,2018,2020)
decay = c(-0.5,-0.25,-0.1,-0.05)
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
                                                   ifelse(is.na(POLLSTER_BIAS),-1.5,POLLSTER_BIAS-1.5),
                                                   ifelse(is.na(POLLSTER_BIAS),0,POLLSTER_BIAS))) %>%
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
    
    holder = rbind(none,base,ss)
    
    for(k in seq(from=1,to=length(decay),by=1)){
      tim = temp %>%
                mutate(time_score = exp(decay[k] * days_to_election)) %>%
                group_by(cycle) %>%
                summarise(DEM = weighted.mean(DEM_adj,time_score),
                          GOP = weighted.mean(GOP_adj,time_score)) %>%
                mutate(time = theTime,
                       type = paste("Time ",decay[k],sep="")) %>%
                select(cycle,time,type,DEM,GOP) %>%
                mutate(diff = GOP - DEM) %>%
                inner_join(.,gb_acts,by="cycle") %>%
                mutate(miss = diff - diff_act,
                       abs_miss = abs(diff - diff_act))
      
      adv = temp %>%
                mutate(time_score = exp(decay[k] * days_to_election)) %>%
                mutate(theWeight = time_score * sample_size_score * pollster_adv_score) %>%
                group_by(cycle) %>%
                summarise(DEM = weighted.mean(DEM_adj,theWeight),
                          GOP = weighted.mean(GOP_adj,theWeight)) %>%
                mutate(time = theTime,
                       type = paste("Advanced ",decay[k],sep="")) %>%
                select(cycle,time,type,DEM,GOP) %>%
                mutate(diff = GOP - DEM) %>%
                inner_join(.,gb_acts,by="cycle") %>%
                mutate(miss = diff - diff_act,
                       abs_miss = abs(diff - diff_act))
      
      holder = rbind(holder,tim,adv)
    }
    
    #stack results
    if(i == 1 & j == 1){
      results = holder
    } else {
      results = rbind(results,holder)
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


#A lot of weightings perform very similarly.  However, the Advanced -0.1 method has a much lower sd (most stable overall)
#Going with 20 will give enough time for spare time periods not directly before the election


#Get the generic ballot results... go through again and also calculate the standard deviations to view confidence intervals
gb_final = NA
for(i in seq(from=1,to=length(year),by=1)){
  theYear = year[i]
  
  temp = gb_polls %>% filter(cycle==theYear & days_to_election <= 21 & days_to_election >= 0) %>% 
          arrange(desc(mid_date),pollster,population) %>%
          group_by(pollster) %>%
          filter(row_number()==1) %>%
          ungroup() %>%
          left_join(.,pollster_bias %>% select(-count),by=c("cycle","pollster")) %>%
          left_join(.,avg_bias,by="cycle") %>%
          mutate(adjustment = ifelse(population == "RV",
                                     ifelse(is.na(POLLSTER_BIAS),-1.5,POLLSTER_BIAS-1.5),
                                     ifelse(is.na(POLLSTER_BIAS),0,POLLSTER_BIAS))) %>%
          left_join(.,gb_acts %>% mutate(total = GOP_act + DEM_act) %>% select(cycle,total),by="cycle") %>%
          mutate(undecideds = total - DEM - GOP) %>%
          mutate(DEM_adj = DEM + .5*adjustment + .5*undecideds,
                 GOP_adj = GOP - .5*adjustment + .5*undecideds,
                 adv_score = replace_na(adv_score,quantile(unlist(pollster_bias %>% filter(cycle==theYear) %>% select(adv_score)),1))) %>%
          mutate(sample_size_score = sqrt(sample_size/600),
                 pollster_adv_score = ((adv_score - min(pollster_bias$adv_score)) / (max(pollster_bias$adv_score) - min(pollster_bias$adv_score)) - 1) * -1 + 0.5)
  
  #Advanced - 25
  calc = temp %>%
            mutate(time_score = exp(-0.1 * days_to_election)) %>%
            mutate(theWeight = time_score * sample_size_score * pollster_adv_score)
  
  mean = calc %>%
            group_by(cycle) %>%
            summarise(DEM = round(weighted.mean(DEM_adj,theWeight),2),
                      GOP = round(weighted.mean(GOP_adj,theWeight),2),
                      count = n()) 
  
  moe = calc %>%
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
                     GOP_max = round(GOP + GOP_MOE,2)) %>%
              select(cycle,DEM,GOP,DEM_SD,GOP_SD,count,DEM_min,DEM_max,GOP_min,GOP_max) %>%
              mutate(diff = GOP - DEM,
                     diff_min = GOP_min - DEM_max,
                     diff_max = GOP_max - DEM_min) %>%
              inner_join(.,gb_acts,by="cycle") %>%
              mutate(miss = diff - diff_act,
                     abs_miss = abs(diff - diff_act))
  
  if(i==1){
    gb_final = theJoin
  } else {
    gb_final = rbind(gb_final,theJoin)
  }
}

png(filename = "Plots/Generic Ballot/Generic_Ballot_GOP_Historic_Performance.png",width = 1920,height = 1080)
ggplot(gb_final)+
  geom_segment(aes(x = GOP_min, xend = GOP_max, y = cycle, yend = cycle),colour="black", size = 3) +
  geom_point(aes(x = GOP, y = cycle),colour = "blue",size = 10) +
  geom_point(aes(x = GOP_act, y = cycle),colour = "firebrick",size = 10) +
  theme_classic() +
  scale_y_continuous(name="Election Year",breaks = seq(2002,2022,by=2),limits=c(2002, 2022)) +
  scale_x_continuous(name="Generic Ballot: GOP",breaks = seq(30,60,by=5),limits=c(30,60)) +
  ggtitle("GOP Generic Ballot for Final 3 Weeks: Predicted (Blue) vs Actual (Red) with 95% Confidence Interval") +
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),
        axis.text = element_text(size=20),
        axis.title = element_text(size=20))
dev.off()

png(filename = "Plots/Generic Ballot/Generic_Ballot_DEM_Historic_Performance.png",width = 1920,height = 1080)
ggplot(gb_final)+
  geom_segment(aes(x = DEM_min, xend = DEM_max, y = cycle, yend = cycle),colour="black", size = 3) +
  geom_point(aes(x = DEM, y = cycle),colour = "blue",size = 10) +
  geom_point(aes(x = DEM_act, y = cycle),colour = "firebrick",size = 10) +
  theme_classic() +
  scale_y_continuous(name="Election Year",breaks = seq(2002,2022,by=2),limits=c(2002, 2022)) +
  scale_x_continuous(name="Generic Ballot: DEM",breaks = seq(35,65,by=5),limits=c(35,65)) +
  ggtitle("DEM Generic Ballot for Final 3 Weeks: Predicted (Blue) vs Actual (Red) with 95% Confidence Interval") +
  theme(plot.title = element_text(hjust = 0.5,size=24,face="bold"),
        axis.text = element_text(size=20),
        axis.title = element_text(size=20))
dev.off()


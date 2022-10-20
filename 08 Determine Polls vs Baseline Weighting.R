#2022 Election Project
#Author: Scott Onestak
#8 Determine Polls vs Baseline Weighting

#libraries
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(fredr)

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

#read in election results
party_ind = read.csv('Data/Election Results/Cleaned Results/party_ind_data.csv',header=T,stringsAsFactors=F)
election_results_raw = read.csv('Data/Election Results/Cleaned Results/votes_cleaned_data.csv',header=T,stringsAsFactors=F) %>%
                          filter(year >= 2010 & runoff == FALSE)


#Build actual results for CDs that have both a Democrat and Republican on the ballot
#For jungle generals some southern states have, group all Republican and Democrat votes together
dem_cand = election_results_raw %>% filter(PARTY == "D") %>%
              group_by(year,STATE,DISTRICT) %>%
              summarise(count=n())

rep_cand = election_results_raw %>% filter(PARTY == "R") %>%
              group_by(year,STATE,DISTRICT) %>%
              summarise(count=n())

election_results = election_results_raw %>% inner_join(.,dem_cand %>% select(-count),by=c("year","STATE","DISTRICT")) %>%
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
                               midterm = ifelse(cycle %% 4 == 0,0,1))
gb_miss_by_days_to$party_in_power = as.factor(gb_miss_by_days_to$party_in_power)
gb_miss_by_days_to$midterm = as.factor(gb_miss_by_days_to$midterm)

train = gb_miss_by_days_to %>% filter(cycle < 2020)
test = gb_miss_by_days_to %>% filter(cycle >= 2020)

GOP_days_to_lm = lm(GOP_act ~ GOP + diff + days_to_election + midterm + GOP*days_to_election + GOP*midterm,data=train)
summary(GOP_days_to_lm)
predict(GOP_days_to_lm,test)






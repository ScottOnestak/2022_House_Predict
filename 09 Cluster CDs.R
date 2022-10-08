#2022 Election Project
#Author: Scott Onestak
#9 Cluster CDs

#libraries
library(dplyr)
library(tidyr)
library(stringr)
library(cluster)
library(ggplot2)
library(sf)
library(modi)

#read in data
buildCluster = read.csv("Data/Cluster/buildCluster.csv",header = T, stringsAsFactors = F) %>%
                  mutate(LEAN_CHG = LEAN_2020 - LEAN_2016,
                         COLGROUP = ifelse(LEAN_2020 > 0,1,0))
buildCluster$COLGROUP = as.factor(buildCluster$COLGROUP)
theClusterMapping = read.csv("Data/Cluster/theClusterMapping.csv",header = T, stringsAsFactors = F)

#clusters are to better understand election performance across time
#therefore, the variables should be closely tied to the election results

#Look at correlations between similar variables
group1 = c("white","black","asian","hispanic","LEAN_2016","LEAN_2020","LEAN_CHG")
group2 = c("white_chg","black_chg","asian_chg","hispanic_chg","LEAN_2016","LEAN_2020","LEAN_CHG")
group3 = c("male","female","single","married","widowed","divorced","LEAN_2016","LEAN_2020","LEAN_CHG")
group4 = c("under35","b35to50","b50to65","over65","LEAN_2016","LEAN_2020","LEAN_CHG")
group5 = c("under35_chg","b35to50_chg","b50to65_chg","over65_chg","LEAN_2016","LEAN_2020","LEAN_CHG")
group6 = c("lfpr","unemployment","poverty","assistance","noninsured","veterans","LEAN_2016","LEAN_2020","LEAN_CHG")
group7 = c("nohighschool","highschool","somecollege","bachelors","masters","phd","LEAN_2016","LEAN_2020","LEAN_CHG")
group8 = c("college_educated","highly_educated","college_educated_chg","highly_educated_chg","LEAN_2016","LEAN_2020","LEAN_CHG")
group9 = c("owner_occupied","renter_occupied","family_hh","nonfamily_hh","median_house_value","median_rent","LEAN_2016","LEAN_2020","LEAN_CHG")
group10 = c("median_age","median_hh_income","per_capita_income","rel_pop_growth","LEAN_2016","LEAN_2020","LEAN_CHG")
group11 = c("ppsm","highlydense","dense","spread","highlyspread","sparse","density_score","LEAN_2016","LEAN_2020","LEAN_CHG")
group12 = c("highlydense_chg","dense_chg","spread_chg","highlyspread_chg","sparse_chg","LEAN_2016","LEAN_2020","LEAN_CHG")

#Customize panel function
thePanel = function(x,y){
  points(x,y,pch=20,col=c("lightblue","lightcoral")[buildCluster$COLGROUP])
  corr = round(cor(x,y),digits=2)
  out = paste("Corr = ",corr,sep="")
  usr = par("usr"); on.exit(par(usr))
  par(usr = c(0,1,0,1))
  text(0.5,.9,out,cex=2)
}

png(filename = "Plots/Cluster Correlation/CDs_by_race.png",width = 1920,height = 1080)
pairs(buildCluster[,group1],upper.panel = NULL, lower.panel = thePanel)
dev.off()

png(filename = "Plots/Cluster Correlation/CDs_by_race_chg.png",width = 1920,height = 1080)
pairs(buildCluster[,group2],upper.panel = NULL, lower.panel = thePanel)
dev.off()

png(filename = "Plots/Cluster Correlation/CDs_by_marital_status.png",width = 1920,height = 1080)
pairs(buildCluster[,group3],upper.panel = NULL, lower.panel = thePanel)
dev.off()

png(filename = "Plots/Cluster Correlation/CDs_by_age.png",width = 1920,height = 1080)
pairs(buildCluster[,group4],upper.panel = NULL, lower.panel = thePanel)
dev.off()

png(filename = "Plots/Cluster Correlation/CDs_by_age_chg.png",width = 1920,height = 1080)
pairs(buildCluster[,group5],upper.panel = NULL, lower.panel = thePanel)
dev.off()

png(filename = "Plots/Cluster Correlation/CDs_by_econ.png",width = 1920,height = 1080)
pairs(buildCluster[,group6],upper.panel = NULL, lower.panel = thePanel)
dev.off()

png(filename = "Plots/Cluster Correlation/CDs_by_edu.png",width = 1920,height = 1080)
pairs(buildCluster[,group7],upper.panel = NULL, lower.panel = thePanel)
dev.off()

png(filename = "Plots/Cluster Correlation/CDs_by_edu2.png",width = 1920,height = 1080)
pairs(buildCluster[,group8],upper.panel = NULL, lower.panel = thePanel)
dev.off()

png(filename = "Plots/Cluster Correlation/CDs_by_housing.png",width = 1920,height = 1080)
pairs(buildCluster[,group9],upper.panel = NULL, lower.panel = thePanel)
dev.off()

png(filename = "Plots/Cluster Correlation/CDs_by_income.png",width = 1920,height = 1080)
pairs(buildCluster[,group10],upper.panel = NULL, lower.panel = thePanel)
dev.off()

png(filename = "Plots/Cluster Correlation/CDs_by_density.png",width = 1920,height = 1080)
pairs(buildCluster[,group11],upper.panel = NULL, lower.panel = thePanel)
dev.off()

png(filename = "Plots/Cluster Correlation/CDs_by_density_chg.png",width = 1920,height = 1080)
pairs(buildCluster[,group12],upper.panel = NULL, lower.panel = thePanel)
dev.off()


#first, normalize data using min-max approach
toNormalize = setdiff(colnames(buildCluster),c("CD","id","COLGROUP"))
normalized = buildCluster
for(i in seq(from=1,to=length(toNormalize),by=1)){
  min = min(normalized[,toNormalize[i]])
  max = max(normalized[,toNormalize[i]])
  normalized[,toNormalize[i]] = (normalized[,toNormalize[i]] - min) / (max - min)
}

#Clusting the standard way won't result in good separation because CDs are more of a sliding scale than distinct groups
#Build a similarity score between CDs... demographics should be similar and CDs should move the same

#Get score for all CDs
holder = NA
holder_found = FALSE
neighbors = NA
neighbors_found = FALSE
all = NA
all_found = FALSE
for(j in seq(from=1,to=dim(buildCluster)[1],by=1)){
  curr_id = normalized[j,"id"]
  theYears = unlist(theClusterMapping %>% filter(id==curr_id) %>% select(cd_year) %>% distinct())
  tocheck = unique(unlist(theClusterMapping %>% filter(cd_year %in% theYears) %>% select(id)))
  cat("ID: ",j," ",curr_id,"\n",sep="")
  for(k in seq(from=1,to=length(tocheck),by=1)){
    temp_id = tocheck[k]
    if(temp_id != curr_id){
      demo_score = abs(as.numeric(normalized %>% filter(id==curr_id) %>% select(white)) - 
                       as.numeric(normalized %>% filter(id==temp_id) %>% select(white))) + 
                   abs(as.numeric(normalized %>% filter(id==curr_id) %>% select(black)) - 
                       as.numeric(normalized %>% filter(id==temp_id) %>% select(black))) +
                   abs(as.numeric(normalized %>% filter(id==curr_id) %>% select(asian)) - 
                       as.numeric(normalized %>% filter(id==temp_id) %>% select(asian))) +
                   abs(as.numeric(normalized %>% filter(id==curr_id) %>% select(hispanic)) - 
                       as.numeric(normalized %>% filter(id==temp_id) %>% select(hispanic))) +
                   abs(as.numeric(normalized %>% filter(id==curr_id) %>% select(under35)) - 
                       as.numeric(normalized %>% filter(id==temp_id) %>% select(under35))) +
                   abs(as.numeric(normalized %>% filter(id==curr_id) %>% select(over65)) - 
                       as.numeric(normalized %>% filter(id==temp_id) %>% select(over65))) +
                   abs(as.numeric(normalized %>% filter(id==curr_id) %>% select(density_score)) - 
                       as.numeric(normalized %>% filter(id==temp_id) %>% select(density_score))) +
                   abs(as.numeric(normalized %>% filter(id==curr_id) %>% select(unemployment)) - 
                       as.numeric(normalized %>% filter(id==temp_id) %>% select(unemployment))) + 
                   abs(as.numeric(normalized %>% filter(id==curr_id) %>% select(poverty)) - 
                       as.numeric(normalized %>% filter(id==temp_id) %>% select(poverty))) +
                   abs(as.numeric(normalized %>% filter(id==curr_id) %>% select(veterans)) - 
                       as.numeric(normalized %>% filter(id==temp_id) %>% select(veterans))) +
                   abs(as.numeric(normalized %>% filter(id==curr_id) %>% select(college_educated)) - 
                       as.numeric(normalized %>% filter(id==temp_id) %>% select(college_educated))) + 
                   abs(as.numeric(normalized %>% filter(id==curr_id) %>% select(college_educated_chg)) - 
                       as.numeric(normalized %>% filter(id==temp_id) %>% select(college_educated_chg))) +
                   abs(as.numeric(normalized %>% filter(id==curr_id) %>% select(owner_occupied)) - 
                       as.numeric(normalized %>% filter(id==temp_id) %>% select(owner_occupied))) +
                   abs(as.numeric(normalized %>% filter(id==curr_id) %>% select(median_house_value)) - 
                       as.numeric(normalized %>% filter(id==temp_id) %>% select(median_house_value))) +
                   abs(as.numeric(normalized %>% filter(id==curr_id) %>% select(single)) - 
                       as.numeric(normalized %>% filter(id==temp_id) %>% select(single))) + 
                   abs(as.numeric(normalized %>% filter(id==curr_id) %>% select(married)) - 
                       as.numeric(normalized %>% filter(id==temp_id) %>% select(married))) +
                   abs(as.numeric(normalized %>% filter(id==curr_id) %>% select(white_chg)) - 
                       as.numeric(normalized %>% filter(id==temp_id) %>% select(white_chg))) +
                   abs(as.numeric(normalized %>% filter(id==curr_id) %>% select(hispanic_chg)) - 
                       as.numeric(normalized %>% filter(id==temp_id) %>% select(hispanic_chg)))
                part_score = abs(as.numeric(normalized %>% filter(id==curr_id) %>% select(LEAN_2020)) - 
                                 as.numeric(normalized %>% filter(id==temp_id) %>% select(LEAN_2020))) +
                             abs(as.numeric(normalized %>% filter(id==curr_id) %>% select(LEAN_2016)) - 
                                 as.numeric(normalized %>% filter(id==temp_id) %>% select(LEAN_2016)))
      if(holder_found==FALSE){
        holder = as.data.frame(cbind(curr_id,temp_id,round(demo_score,2),round(part_score,2)))
        colnames(holder) = c("id","neighbor","demographic_score","partisan_score")
        holder_found = TRUE
      } else {
        temp = as.data.frame(cbind(curr_id,temp_id,round(demo_score,2),round(part_score,2)))
        colnames(temp) = c("id","neighbor","demographic_score","partisan_score")
        holder = rbind(holder,temp)
      }
    }
  }
  holder$demographic_score = as.numeric(holder$demographic_score)
  holder$partisan_score = as.numeric(holder$partisan_score)
  
  if(all_found==FALSE){
    all = holder
    all_found = TRUE
  } else {
    all = rbind(all,holder)
  }
  
  holder = NA
  holder_found = FALSE
}

write.csv(all,"Data/Cluster/All CD Scores/all_scores.csv",row.names=F)








#Determine closest neighbor optimized limits

#Read in data
all_scores = read.csv("Data/Cluster/All CD Scores/all_scores.csv",header=T,stringsAsFactors=F)

pollster_bias = read.csv('Data/Polls/For Pollster Ratings/forModeling/pollster_bias.csv',header=T,stringsAsFactors=F)
partisan_bias = read.csv('Data/Polls/For Pollster Ratings/forModeling/partisan_bias.csv',header=T,stringsAsFactors=F)
avg_bias = read.csv('Data/Polls/For Pollster Ratings/forModeling/avg_bias.csv',header=T,stringsAsFactors=F)
gb_acts = read.csv('Data/Polls/Generic Ballot/generic_ballot_actuals.csv',header=T,stringsAsFactors=F)
gb_polls = read.csv('Data/Polls/Generic Ballot/generic_ballot_historic.csv',header=T,stringsAsFactors=F)

raw_polls = read.csv("Data/Polls/For Pollster Ratings/raw-polls.csv",header=T,stringsAsFactors=F)
banned_list = unlist(read.csv("Data/Polls/For Pollster Ratings/banned.csv",header=T,stringsAsFactors=F) %>%
                       filter(banned=="yes") %>% select(pollster) %>% distinct())
leans = read.csv("Data/Historical Leans/Leans.csv",header=T,stringsAsFactors=F) %>% rename("year"="Year")

actuals = read.csv("Data/Election Results/1976-2020-house.csv",header=T,stringsAsFactors=F) %>%
            filter(year >= 2008 & stage == "GEN") %>%
            mutate(party = ifelse(party %in% c("DEMOCRATIC-FARM-LABOR","DEMOCRATIC-FARMER-LABOR","DEMOCRATIC-NPL","DEMOCRATIC-NONPARTISAN LEAGUE"),"DEMOCRAT",party)) %>%
            filter(party %in% c("DEMOCRAT","REPUBLICAN"))

#Build actual results for CDs that have both a Democrat and Republican on the ballot
#For jungle generals some southern states have, group all Republican and Democrat votes together
dem_cand = actuals %>% filter(party == "DEMOCRAT" & state_po != "DC") %>%
            group_by(year,state_po,district) %>%
            summarise(count=n())

rep_cand = actuals %>% filter(party == "REPUBLICAN" & state_po != "DC") %>%
            group_by(year,state_po,district) %>%
            summarise(count=n())

acts = actuals %>% inner_join(.,dem_cand %>% select(-count),by=c("year","state_po","district")) %>%
                   inner_join(.,rep_cand %>% select(-count),by=c("year","state_po","district")) %>%
                   group_by(year,state_po,district,party) %>%
                   summarise(VOTES = sum(candidatevotes,na.rm=T)) %>%
                   spread(.,key=party,value=VOTES) %>%
                   ungroup() %>%
                   left_join(.,actuals %>% select(year,state_po,district,totalvotes) %>% distinct(),by=c("year","state_po","district")) %>%
                   mutate(DEM_act = round(DEMOCRAT / totalvotes * 100,2),
                          GOP_act = round(REPUBLICAN / totalvotes * 100,2),
                          district = ifelse(district == 0,1,district)) %>%
                   mutate(District = paste(state_po,"-",str_pad(district,2,pad="0"),sep="")) %>%
                   select(year,District,DEM_act,GOP_act)

#build house polls
house_polls = raw_polls %>% filter(type_simple == "House-G" &
                                   location != "US" &
                                   year >= 2008 &
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
                left_join(.,pollster_bias %>% select(-count),by=c("year","pollster")) %>%
                left_join(.,avg_bias,by="year") %>%
                mutate(adjustment = ifelse(is.na(POLLSTER_BIAS),avg_bias+PARTISAN_BIAS,POLLSTER_BIAS+PARTISAN_BIAS)) %>%
                mutate(DEM_adj = DEM_PRCT + .5*adjustment,
                       GOP_adj = REP_PRCT - .5*adjustment) %>%
                mutate(DIFF = GOP_adj - DEM_adj) %>%
                left_join(.,leans %>% select("year","District","PERF_1"),by=c("year","District")) %>%
                mutate(rel_house_polling = round(DIFF - PERF_1,2)) 

#build generic ballot

#rename
colnames(gb_acts) = c("year","GOP_act","DEM_act","diff_act")

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


gb_final = NA
for(i in seq(from=2008,to=2020,by=2)){
  theYear = i
  
  temp = gb_polls %>% filter(cycle==theYear & days_to_election <= 21 & days_to_election >= 0) %>% 
            arrange(desc(mid_date),pollster,population) %>%
            group_by(pollster) %>%
            filter(row_number()==1) %>%
            rename("year"="cycle") %>%
            left_join(.,pollster_bias %>% select(-count),by=c("year","pollster")) %>%
            left_join(.,avg_bias,by="year") %>%
            mutate(adjustment = ifelse(population == "RV",
                                       ifelse(is.na(POLLSTER_BIAS),avg_bias-1.5,POLLSTER_BIAS-1.5),
                                       ifelse(is.na(POLLSTER_BIAS),avg_bias,POLLSTER_BIAS))) %>%
            mutate(DEM_adj = DEM + .5*adjustment,
                   GOP_adj = GOP - .5*adjustment,
                   adv_score = replace_na(adv_score,quantile(unlist(pollster_bias %>% filter(year==theYear) %>% select(adv_score)),1))) %>%
            mutate(sample_size_score = sqrt(sample_size/600),
                   pollster_adv_score = ((adv_score - min(pollster_bias$adv_score)) / (max(pollster_bias$adv_score) - min(pollster_bias$adv_score)) - 1) * -1 + 0.5)
          
  #Advanced - 25
  calc = temp %>%
          mutate(time_score = (log(days_to_election + 1) / log(21 + 1) - 1) * -1 * 0.75 + 0.25) %>%
          mutate(theWeight = time_score * sample_size_score * pollster_adv_score) %>%
          group_by(year) %>%
          summarise(DEM = round(weighted.mean(DEM_adj,theWeight),2),
                    GOP = round(weighted.mean(GOP_adj,theWeight),2),
                    count = n()) %>%
          select(year,DEM,GOP,count) %>%
          mutate(diff = GOP - DEM) %>%
          inner_join(.,gb_acts,by="year") %>%
          mutate(miss = diff - diff_act,
                 abs_miss = abs(diff - diff_act))
  
  if(i==2008){
    gb_final = calc
  } else {
    gb_final = rbind(gb_final,calc)
  }
}


#Determine cutoffs
demo_cut = c(0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5)
part_cut = c(0.05,0.10,0.15,0.2,0.25)

#Build closest neighbors by year
one_party = NA
one_party_found = FALSE
results = NA
results_found = FALSE
for(i in seq(from=2008,to=2020,by=2)){
  theTime = i
  cat(theTime,"\n",sep="")
  theCDs = unlist(theClusterMapping %>% filter(cd_year == i) %>% select(CD))
  theCDs_ids = unlist(theClusterMapping %>% filter(cd_year == i) %>% select(id))
  for(j in seq(from=1,to=length(theCDs),by=1)){
    theCD = theCDs[j]
    theCD_id = unlist(theClusterMapping %>% filter(cd_year == i & CD == theCD) %>% select(id))
    
    if(dim(acts %>% filter(year == theTime & District == theCD))[1]>0){
      temp = all_scores %>% filter(id == theCD_id & neighbor %in% theCDs_ids) %>%
        mutate(District = substr(neighbor,1,5),
               year = i) %>%
        inner_join(.,house_polls %>% select(year,District,DEM_adj,GOP_adj,DIFF,PERF_1,rel_house_polling),by=c("year","District"))
      
      for(y in seq(from=1,to=length(demo_cut),by=1)){
        for(z in seq(from=1,to=length(part_cut),by=1)){
          holder = temp %>% filter(demographic_score <= demo_cut[y] & partisan_score <= part_cut[z])
          
          #if there are polls, then take the average and apply latest lean
          #if not... take the generic ballot and apply latest lean
          if(dim(holder)[1]>0){
            est = round(mean(holder$rel_house_polling) +
                          as.numeric(unlist(leans %>% filter(year == theTime & District == theCD) %>% select(LEAN_1))),2) 
          } else {
            est = round(as.numeric(unlist(gb_final %>% filter(year == theTime) %>% select(diff))) +
                          as.numeric(unlist(leans %>% filter(year == theTime & District == theCD) %>% select(LEAN_1))),2)
          }
          
          if(results_found == FALSE){
            results = as.data.frame(cbind(i,theCD,est,dim(holder)[1],demo_cut[y],part_cut[z]))
            colnames(results) = c("year","District","diff_est","count","demo_cut","part_cut")
            results_found = TRUE
          } else {
            results_holder = as.data.frame(cbind(i,theCD,est,dim(holder)[1],demo_cut[y],part_cut[z]))
            colnames(results_holder) = c("year","District","diff_est","count","demo_cut","part_cut")
            results = rbind(results,results_holder)
          }
        }
      }
    } else {
      if(one_party_found == FALSE){
        one_party = as.data.frame(cbind(i,theCD))
        colnames(one_party) = c("year","District")
        one_party_found = TRUE
      } else {
        one_party_holder = as.data.frame(cbind(i,theCD))
        colnames(one_party_holder) = c("year","District")
        one_party = rbind(one_party,one_party_holder)
      }
    }
  }
}
results$year = as.numeric(results$year)
results$diff_est = as.numeric(results$diff_est)

summary = results %>% left_join(.,acts %>% mutate(diff_act = GOP_act - DEM_act) %>% select(year,District,diff_act),by=c("year","District")) %>%
                      mutate(diff = diff_act - diff_est,
                             abs_diff = abs(diff_act - diff_est)) %>%
                      group_by(demo_cut,part_cut) %>%
                      summarise(avg_diff = mean(diff,na.rm = T),
                                avg_abs_diff = mean(abs_diff,na.rm = T),
                                med_diff = median(diff,na.rm = T),
                                med_abs_diff = median(abs_diff,na.rm = T),
                                count = n())

summary_no0 = results %>% filter(count > 3) %>%
                          left_join(.,acts %>% mutate(diff_act = GOP_act - DEM_act) %>% select(year,District,diff_act),by=c("year","District")) %>%
                          mutate(diff = diff_act - diff_est,
                                 abs_diff = abs(diff_act - diff_est)) %>%
                          group_by(demo_cut,part_cut) %>%
                          summarise(avg_diff = mean(diff,na.rm = T),
                                    avg_abs_diff = mean(abs_diff,na.rm = T),
                                    med_diff = median(diff,na.rm = T),
                                    med_abs_diff = median(abs_diff,na.rm = T),
                                    count = n())









#read in 2022 map
map = st_sf(st_read(paste("Data/2022 Congressional District Shapefiles/CD_2022.shp",sep=""))) %>%
        st_transform(.,crs = 4326) %>%
        rename(State = state)

map_details = map %>% st_drop_geometry() %>% mutate(row = row_number())          

forMapping = map %>% 
              st_coordinates() %>%
              as.data.frame() %>%
              left_join(.,map_details,by=c("L3"="row")) %>%
              rename(long = X,
                     lat = Y)


map2022 = forMapping %>% filter(!State %in% c("AK","HI")) %>%
            left_join(.,diff %>% select(-id),by=c("name"="CD"))
ggplot() + 
      geom_polygon(data=map2022, aes(long,lat,group=L3,fill=demographic_score), colour = "black") + 
      scale_fill_gradient2(position="bottom",low="firebrick",high="white",midpoint = median(diff$demographic_score))
ggplot() + 
      geom_polygon(data=map2022, aes(long,lat,group=L3,fill=partisan_score), colour = "black") + 
      scale_fill_gradient2(position="bottom",low="firebrick",high="white",midpoint = median(diff$partisan_score))
ggplot() + 
      geom_polygon(data=map2022, aes(long,lat,group=L3,fill=close_neighbor), colour = "black") + 
      scale_fill_manual(values = c("white","firebrick"))

#Build map view
theClusterMapping_2022 = theClusterMapping %>% 
        left_join(.,buildCluster %>% select(id,cluster,LEAN_2016,LEAN_2020,LEAN_CHG),by="id") %>% 
        filter(cd_year == 2022)

map2022 = map %>% filter(!State %in% c("AK","HI")) %>%
            left_join(.,theClusterMapping_2022,by=c("name"="CD"))
plot(map2022["cluster"])


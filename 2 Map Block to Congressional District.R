#2022 Election Project
#Author: Scott Onestak
#2 Map Block to Congressional District

library(tidycensus)
library(tigris)
library(sf)
library(ggplot2)
library(tidyverse)
library(stringr)

acs_key = "2b79d333bb5d985abb33a41ae240be5f83f09f86"

#The States
# "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA",
# "KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM",
# "NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA",
# "WV","WI","WY"

#State to FIPS Mapping
state_to_fips = read.csv("Data/State_to_FIPS.csv",header = T,stringsAsFactors = F) %>%
                  mutate(STATEFP = formatC(as.numeric(FIPS),width=2,format="d",flag=0)) %>%
                  select(State,STATEFP)

#Set the state and year
#Run individual state/year instead of all at once since large states can take a while to map at the block level
curr_state = "WI"
theYear = 2010

#2010
theMap = st_sf(st_read("Data/Previous Congressional District Shapefiles/CD_2008.shp")) %>%
          rename(STATEFP = STATEFP10,
                 CD = CD111FP,
                 GEOID = GEOID10,
                 NAME = NAMELSAD10,
                 ALAND = ALAND10,
                 AWATER = AWATER10) %>%
          select(STATEFP,CD,GEOID,NAME,ALAND,AWATER,geometry) %>%
          mutate(CD = ifelse(CD=="00","01",CD)) %>%
          filter(as.numeric(STATEFP) <= 56 & as.numeric(STATEFP) != 11) %>%
          left_join(.,state_to_fips,by="STATEFP") %>%
          mutate(State = ifelse(is.na(State) & STATEFP == "08","CO",State)) %>%
          mutate(theName = paste(State,"-",CD,sep=""))


curr_map = theMap %>% filter(State == curr_state)
curr_map_df = curr_map %>% st_drop_geometry()
theblocks = blocks(state = curr_state,year=theYear) %>%
              rename(TRACT = TRACTCE10,
                     BLOCK = BLOCKCE10)

theCounties = get_acs(geography = "county", variables = "B03002_001", 
                      state=curr_state, geometry = TRUE, year = theYear, key=acs_key,cb=F) %>%
                mutate(COUNTYFP = str_sub(GEOID,start=3L)) %>%
                select(COUNTYFP,NAME,geometry)
theCounties$CD = NA  

theTracts = get_acs(geography = "tract", variables = "B03002_001",
                    state=curr_state, geometry = TRUE, year = theYear, key = acs_key, cb = FALSE) %>%
              mutate(COUNTYFP = str_sub(GEOID,start=3L,end=5L),
                     TRACT = str_sub(GEOID,start=6L)) %>%
              select(COUNTYFP,TRACT,geometry)
theTracts$CD = NA

#See if entire counties are in one congressional district, so that there's no need to check at the block level
cat("\nCounties to Find: ",dim(theCounties)[1],"\n",sep=" ")
for(i in seq(from=1,to=dim(theCounties)[1],by=1)){
  for(j in seq(from=1,to=dim(curr_map)[1],by=1)){
    if(length(unlist(st_contains(curr_map[j,],theCounties[i,])))>0){
      theCounties[i,"CD"] = curr_map_df[j,"theName"]
      break
    }
  }
}
theCountiesMap = theCounties %>% st_drop_geometry() %>% select(-NAME)
theblocks = theblocks %>% left_join(.,theCountiesMap,by="COUNTYFP")

theblocks_found = theblocks %>% filter(!is.na(CD))
theblocks_notfound = theblocks %>% filter(is.na(CD)) %>% select(-CD)

#See if entire tracts are in one congressional district, so that there's no need to check at the block level
#Only need to check tracts not already found at the county level
if(dim(theblocks_notfound)[1]>0){
  tracts_to_find = theTracts %>% 
    inner_join(.,
               unique(theblocks_notfound %>% st_drop_geometry() %>% select(COUNTYFP,TRACT)),
               by=c("COUNTYFP","TRACT"))
  cat("\nTracts to Find: ",dim(tracts_to_find)[1],"\n",sep=" ")
  last = 1
  for(i in seq(from=1,to=dim(tracts_to_find)[1],by=1)){
    if(i%%100==0){
      cat("Complete: ",i," ",round(i/dim(tracts_to_find)[1],3)*100,"\n",sep=" ")
    }
    
    if(length(unlist(st_contains(curr_map[last,],tracts_to_find[i,])))>0){
      tracts_to_find[i,"CD"] = curr_map_df[last,"theName"]
    } else {
      for(j in seq(from=1,to=dim(curr_map)[1],by=1)){
        if(length(unlist(st_contains(curr_map[j,],tracts_to_find[i,])))>0){
          tracts_to_find[i,"CD"] = curr_map_df[j,"theName"]
          last = j
          break
        }
      }
    }
  }
  theTractsMap = tracts_to_find %>% st_drop_geometry()
  theblocks_notfound = theblocks_notfound %>% left_join(.,theTractsMap,by=c("COUNTYFP","TRACT"))
  
  theblocks_found = rbind(theblocks_found,
                          theblocks_notfound %>% filter(!is.na(CD)))
  theblocks_notfound = theblocks_notfound %>% filter(is.na(CD))
}


#Find the individual blocks that couldn't be found at the county or tract level
last = 1
if(dim(theblocks_notfound)[1]>0){
  cat("\nBlocks to Find: ",dim(theblocks_notfound)[1],"\n",sep=" ")
  for(i in seq(from=1,to=dim(theblocks_notfound)[1],by=1)){
    if(i%%1000==0){
      cat("Complete: ",i," ",round(i/dim(theblocks_notfound)[1],3)*100,"\n",sep=" ")
    }
    
    if(length(unlist(st_contains(curr_map[last,],theblocks[i,])))>0){
      theblocks_notfound[i,"CD"] = curr_map_df[last,"theName"]
    } else {
      for(j in seq(from=1,to=dim(curr_map)[1],by=1)){
        if(length(unlist(st_contains(curr_map[j,],theblocks[i,])))>0){
          theblocks_notfound[i,"CD"] = curr_map_df[j,"theName"]
          last = j
          break
        }
      }
    }
  }
  
  #Stack results
  theblocks_found = rbind(theblocks_found,theblocks_found)
}

#write out and check subset
theblocks_subset = theblocks_found %>% st_drop_geometry() %>% select(GEOID10,CD)
if(any(is.na(theblocks_subset$CD))==FALSE){
  write.csv(theblocks_subset,paste("Data/Block_to_CD_Mappings/2010/",curr_state,".csv",sep=""),row.names=FALSE)
  cat("\nSuccess: ",curr_state,"\n",sep="")
} else {
  cat("\nMissing CD mappings: ",curr_state,"\n",sep="")
}


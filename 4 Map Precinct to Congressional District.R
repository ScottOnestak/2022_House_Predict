#2022 Election Project
#Author: Scott Onestak
#4 Map Precinct to Congressional District

library(tidycensus)
library(tigris)
library(sf)
library(ggplot2)
library(tidyverse)
library(stringr)
library(geojsonR)
library(jsonlite)
library(geojsonsf)

sf::sf_use_s2(FALSE)
options(scipen = 100)

#State to FIPS Mapping
state_to_fips = read.csv("Data/State_to_FIPS.csv",header = T,stringsAsFactors = F) %>%
                  mutate(STATEFP = formatC(as.numeric(FIPS),width=2,format="d",flag=0)) %>%
                  select(State,STATEFP) %>%
                  filter(!State %in% c('AS','GU','MP','PR','VI','DC'))

#Read in NYT data to use later
nyt = geojson_sf(
        gzcon(
          url("https://int.nyt.com/newsgraphics/elections/map-data/2020/national/precincts-with-results.geojson.gz", 'rb')
        )
      )
nyt$STATEFP = substr(nyt$GEOID,1,2)

theStates = c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA",
              "KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM",
              "NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA",
              "WV","WI","WY")
cdYears = c(2008,2010,2012,2014,2016,2018,2020,2022)


#Read in cds and store as objects
for(i in seq(from=1,to=length(cdYears),by=1)){
  #read in CDs
  cdYear = cdYears[i]
  if(cdYear <= 2020){
    if(cdYear != 2010 & cdYear != 2020){
      theCongress = 111 + (cdYear - 2008)/2
    } else if(cdYear == 2010){
      theCongress = 111 #2010 file was corrupted so just using 2008 map since no change
    } else {
      theCongress = 116 #the 2020 map is the 2018 map since no changes
    }
    
    if(cdYear <= 2010){
      temp = st_sf(st_read(paste("Data/Previous Congressional District Shapefiles/CD_",cdYear,".shp",sep=""))) %>%
              st_transform(.,crs = 4326) %>%
              rename(CD = paste("CD",theCongress,"FP",sep=""),
                     STATEFP = STATEFP10) %>%
              mutate(CD = ifelse(CD=="00","01",CD)) %>%
              filter(as.numeric(STATEFP) <= 56 & as.numeric(STATEFP) != 11 & CD != "ZZ") %>%
              left_join(.,state_to_fips,by="STATEFP") %>%
              mutate(State = ifelse(is.na(State) & STATEFP == "08","CO",State)) %>%
              mutate(name = paste(State,"-",CD,sep="")) %>%
              rename(district = CD)
    } else {
      temp = st_sf(st_read(paste("Data/Previous Congressional District Shapefiles/CD_",cdYear,".shp",sep=""))) %>%
              st_transform(.,crs = 4326) %>%
              rename(CD = paste("CD",theCongress,"FP",sep="")) %>%
              mutate(CD = ifelse(CD=="00","01",CD)) %>%
              filter(as.numeric(STATEFP) <= 56 & as.numeric(STATEFP) != 11 & CD != "ZZ") %>%
              left_join(.,state_to_fips,by="STATEFP") %>%
              mutate(State = ifelse(is.na(State) & STATEFP == "08","CO",State)) %>%
              mutate(name = paste(State,"-",CD,sep="")) %>%
              rename(district = CD)
    }
  } else {
    temp = st_sf(st_read(paste("Data/2022 Congressional District Shapefiles/CD_",cdYear,".shp",sep=""))) %>%
            st_transform(.,crs = 4326) %>%
            rename(State = state)
  }
  assign(paste("CD_",cdYear,sep=""),temp)
}
theCDs = list(CD_2008,CD_2010,CD_2012,CD_2014,CD_2016,CD_2018,CD_2020,CD_2022)

#Loop through states to stack results
theStack = NA
for(i in seq(from=1,to=length(theStates),by=1)){
  curr_state = theStates[i]
  cat(i,curr_state,"\n",sep=" ")
  
  #read in the precincts
  if(curr_state=="KY"){
    theprecincts_2016 = st_sf(st_read(paste("Data/Harvard Precinct Data/2016/",curr_state,"/",tolower(curr_state),"_2016.shp",sep=""))) %>%
                          rename(DEM = G16PREDCLI,
                                 REP = G16PRERTRU) %>%
                          st_transform(.,crs = 4326)
    
    #for counties reporting 2020 at the precinct level, use that data... if not, aggregate at county
    #highly populated county splits like Jefferson county have precinct, so it should be pretty accurate even with different CD cuts
    theCounties_2020 = counties(state="KY",cb=FALSE,year=2020) %>% mutate(COUNTY = toupper(NAME))
    theprecincts_2020 = nyt %>% filter(STATEFP == "21") %>% rename(DEM = votes_dem,REP = votes_rep) %>%
                          mutate(COUNTYFP = substr(GEOID,3,5)) %>%
                          left_join(.,theCounties_2020 %>% st_drop_geometry() %>% select(COUNTYFP,COUNTY),by="COUNTYFP") %>%
                          select(COUNTYFP,COUNTY,DEM,REP,geometry) %>%
                          st_transform(.,crs = 4326)
    missing_counties = theCounties_2020 %>% filter(!COUNTY %in% unlist(unique(theprecincts_2020$COUNTY)))
    
    #Read in Kentucky State Board of Election Data by County to append to the missing counties
    ky_sboe = read.csv("Data/KY_SBOE_County_2020/KY_2020.csv",header=T) %>% mutate(COUNTY = toupper(COUNTY))
    missing_counties = missing_counties %>% 
                        left_join(.,ky_sboe,by="COUNTY") %>% select(COUNTYFP,COUNTY,DEM,REP,geometry)  %>%
                        st_transform(.,crs = 4326)
    
    #stack the precincts and counties
    theprecincts_2020 = rbind(theprecincts_2020,missing_counties)
  } else if(curr_state=="WV"){
    #Read in precinct data... for counties missing precinct data, read in counties and append
    theCounties_2016 = counties(state="WV",cb=FALSE,year=2016) %>% mutate(COUNTY = toupper(NAME)) 
    
    
    #Read in OpenElections data file
    oe_file = read.csv("Data/OpenElections_WV_2016/20161108__wv__general__precinct__raw.csv",header=T) %>%
                filter(election_type == "general" & office %in% c("U.S. President","President")) %>%
                mutate(name_raw = toupper(name_raw)) %>%
                filter(name_raw %in% c("DONALD J. TRUMP","HILLARY CLINTON")) %>%
                mutate(party = ifelse(name_raw == "DONALD J. TRUMP","REP","DEM")) %>%
                mutate(COUNTY = toupper(parent_jurisdiction)) %>%
                select(party,COUNTY,votes) %>%
                group_by(COUNTY,party) %>%
                summarise(votes = sum(votes,na.rm=T)) %>%
                ungroup() %>%
                spread(.,key="party",value="votes")
    
    
    #Stack counties and precincts
    theprecincts_2016 = theCounties_2016 %>% left_join(.,oe_file,by="COUNTY") %>%
                          st_transform(.,crs = 4326)
    
    theprecincts_2020 = nyt %>% filter(STATEFP == "54") %>% rename(DEM = votes_dem,REP = votes_rep) %>%
                          st_transform(.,crs = 4326)
  } else {
    theprecincts_2016 = st_sf(st_read(paste("Data/Harvard Precinct Data/2016/",curr_state,"/",tolower(curr_state),"_2016.shp",sep=""))) %>%
                          rename_all(toupper) %>%
                          rename(DEM = G16PREDCLI,
                                 REP = G16PRERTRU) %>%
                          st_transform(.,crs = 4326) 
    theprecincts_2020 = st_sf(st_read(paste("Data/Harvard Precinct Data/2020/",curr_state,"/",tolower(curr_state),"_2020.shp",sep=""))) %>%
                          rename_all(toupper) %>%
                          rename(DEM = G20PREDBID,
                                 REP = G20PRERTRU) %>%
                          st_transform(.,crs = 4326)
  }
  
  #Assign unique identifier and get total area for precinct
  theprecincts_2016$PRECINCTID = as.numeric(rownames(theprecincts_2016))
  theprecincts_2016$AREA = as.numeric(st_area(theprecincts_2016))
  
  theprecincts_2020$PRECINCTID = as.numeric(rownames(theprecincts_2020))
  theprecincts_2020$AREA = as.numeric(st_area(theprecincts_2020))
  
  for(j in seq(from=1,to=length(cdYears),by=1)){
    cdYear = cdYears[j]
    
    #map blocks to the precincts 
    state_CDs = st_sf(as.data.frame(theCDs[j])) %>% filter(State == curr_state)
    
    #2016
    holder = st_intersection(st_make_valid(st_zm(state_CDs)),st_make_valid(st_zm(theprecincts_2016))) %>% 
              mutate(intersect_area = as.numeric(st_area(.))) %>% 
              st_drop_geometry() %>% 
              select(State,district,name,DEM,REP,PRECINCTID,intersect_area,AREA) %>%
              mutate(prct = round(intersect_area / AREA,2)) %>%
              filter(prct > 0) %>%
              mutate(DEM_VOTES_2016 = round(DEM * prct,0),
                     REP_VOTES_2016 = round(REP * prct,0)) %>%
              group_by(name) %>%
              summarise(DEM_VOTES_2016 = sum(DEM_VOTES_2016,na.rm=T),
                        REP_VOTES_2016 = sum(REP_VOTES_2016,na.rm=T)) %>%
              ungroup()
    holder$TOTAL_VOTES_2016 = holder$DEM_VOTES_2016 + holder$REP_VOTES_2016
    holder$DEM_PRCT_2016 = round(holder$DEM_VOTES_2016 / holder$TOTAL_VOTES_2016,4) * 100
    holder$REP_PRCT_2016 = round(holder$REP_VOTES_2016 / holder$TOTAL_VOTES_2016,4) * 100
    holder$LEAN_2016 = holder$REP_PRCT_2016 - holder$DEM_PRCT_2016 + (51.11 - 48.89)
    
    #2020
    holder2 = st_intersection(st_make_valid(st_zm(state_CDs)),st_make_valid(st_zm(theprecincts_2020))) %>% 
                mutate(intersect_area = as.numeric(st_area(.))) %>% 
                st_drop_geometry() %>% 
                select(State,district,name,DEM,REP,PRECINCTID,intersect_area,AREA) %>%
                mutate(prct = round(intersect_area / AREA,2)) %>%
                filter(prct > 0) %>%
                mutate(DEM_VOTES_2020 = round(DEM * prct,0),
                       REP_VOTES_2020 = round(REP * prct,0)) %>%
                group_by(name) %>%
                summarise(DEM_VOTES_2020 = sum(DEM_VOTES_2020,na.rm=T),
                          REP_VOTES_2020 = sum(REP_VOTES_2020,na.rm=T)) %>%
                ungroup()
    holder2$TOTAL_VOTES_2020 = holder2$DEM_VOTES_2020 + holder2$REP_VOTES_2020
    holder2$DEM_PRCT_2020 = round(holder2$DEM_VOTES_2020 / holder2$TOTAL_VOTES_2020,4) * 100
    holder2$REP_PRCT_2020 = round(holder2$REP_VOTES_2020 / holder2$TOTAL_VOTES_2020,4) * 100
    holder2$LEAN_2020 = holder2$REP_PRCT_2020 - holder2$DEM_PRCT_2020 + (52.27 - 47.73)
    
    final = holder %>% 
              left_join(.,holder2,by="name") %>% 
              mutate(year = cdYear) %>% 
              select(c("year",colnames(holder),colnames(holder2)))
    
    if(i==1 & j==1){
      theStack = final
    } else {
      theStack = rbind(theStack,final)
    }
  }
}


#Write out the 2-party results by the precinct mappings to 2008 - 2022 CDs
write.csv(theStack,'Data/Precinct_Two_Party_Results/2016_2020_results.csv',row.names=FALSE)





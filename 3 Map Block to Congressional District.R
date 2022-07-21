#2022 Election Project
#Author: Scott Onestak
#3 Map Block to Congressional District

library(tidycensus)
library(tigris)
library(sf)
library(ggplot2)
library(tidyverse)
library(stringr)

#Set parameters
#sf::sf_use_s2(FALSE)
#acs_key = "2b79d333bb5d985abb33a41ae240be5f83f09f86"

#Helpful Links
#https://www.census.gov/programs-surveys/geography/guidance/geo-identifiers.html
#https://www.census.gov/geographies/mapping-files/2019/dec/rdo/116-congressional-district-bef.html

#The States
# "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA",
# "KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM",
# "NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA",
# "WV","WI","WY"

#State to FIPS Mapping
state_to_fips = read.csv("Data/State_to_FIPS.csv",header = T,stringsAsFactors = F) %>%
                  mutate(STATEFP = formatC(as.numeric(FIPS),width=2,format="d",flag=0)) %>%
                  select(State,STATEFP) %>%
                  filter(!State %in% c('AS','GU','MP','PR','VI','DC'))

#Set the state and year
#Run individual state/year instead of all at once since large states can take up a lot of memory
#If memory isn't an issue, could do them all at once
curr_state = "WY"
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

#2022
# theMap = st_sf(st_read("Data/2022 Congressional District Shapefiles/CD_2022.shp")) %>%
#   rename(State = state,
#          CD = district,
#          NAME = name) %>%
#   select(State,CD,NAME,geometry) %>%
#   mutate(theName = NAME) %>%
#   st_transform(.,crs="NAD83")


curr_map = theMap %>% filter(State == curr_state)
curr_map$curr_map_row = as.numeric(rownames(curr_map))
curr_map_df = curr_map %>% st_drop_geometry()
theblocks = blocks(state = curr_state,year=theYear) %>%
              rename(TRACT = TRACTCE10,
                     BLOCK = BLOCKCE10) %>%
              arrange(GEOID10)
# theblocks = blocks(state = curr_state,year=theYear) %>%
#  rename(TRACT = TRACTCE20,
#         BLOCK = BLOCKCE20,
#         COUNTYFP = COUNTYFP20)  %>% 
#  arrange(GEOID20)
theblocks$theblocks_row = as.numeric(rownames(theblocks))
tofind = dim(theblocks)[1]


#map blocks to the congressional districts 
holder = as.data.frame(st_contains(curr_map,theblocks))
colnames(holder) = c("curr_map_row","theblocks_row")
holder = holder %>% 
          left_join(.,curr_map %>% st_drop_geometry() %>% select(theName,curr_map_row),by="curr_map_row") %>%
          rename(CD = theName)

theblocks = theblocks %>% left_join(.,holder %>% select(theblocks_row,CD),by="theblocks_row")

#write out and check subset
theblocks_subset = theblocks %>% st_drop_geometry() %>% select(GEOID10,CD) %>% arrange(GEOID10)
#theblocks_subset = theblocks %>% st_drop_geometry() %>% select(GEOID20,CD) %>% arrange(GEOID20)
missing = dim(theblocks_subset %>% filter(is.na(CD)))[1]

#do one final check to fill in NAs.  If missing, see if blocks next to it are the same CD.
if(any(is.na(theblocks_subset$CD))==TRUE){
  for(i in seq(from=1,to=dim(theblocks_subset)[1],by=1)){
    if(is.na(theblocks_subset[i,"CD"])){
      if(i==1){
        theNext = NA
        j = i + 1
        while(is.na(theNext) & j <= dim(theblocks_subset)[1]){
          if(!is.na(theblocks_subset[j,"CD"])){
            theNext = theblocks_subset[j,"CD"]
          }
          j = j + 1
        }
        
        if(!is.na(theNext)){
          theblocks_subset[i,"CD"] = theNext
        }
      } else if(i==dim(theblocks_subset)[1]){
        thePrev = theblocks_subset[i-1,"CD"]
        
        if(!is.na(thePrev)){
          theblocks_subset[i,"CD"] = thePrev
        }
      } else {
        thePrev = theblocks_subset[i-1,"CD"]
        theNext = NA
        thePrevID = theblocks_subset[i-1,1]
        theNextID = NA
        j = i + 1
        while(is.na(theNext) & j <= dim(theblocks_subset)[1]){
          if(!is.na(theblocks_subset[j,"CD"])){
            theNext = theblocks_subset[j,"CD"]
            theNextID = theblocks_subset[j,1]
          }
          j = j + 1
        }
        
        if(!is.na(theNext)){
          if(thePrev == theNext){
            theblocks_subset[i,"CD"] = thePrev
          } else if(j > dim(theblocks_subset)[1]){
            theblocks_subset[i,"CD"] = thePrev
          } else {
            if(substr(thePrevID,1,12) == substr(theblocks_subset[i,1],1,12)){ #Check Block Group Level First
              theblocks_subset[i,"CD"] = thePrev
            } else if(substr(theNextID,1,12) == substr(theblocks_subset[i,1],1,12)){
              theblocks_subset[i,"CD"] = theNext
            } else if(substr(thePrevID,1,11) == substr(theblocks_subset[i,1],1,11)){ #Check Tract Level Next
              theblocks_subset[i,"CD"] = thePrev
            } else if(substr(theNextID,1,11) == substr(theblocks_subset[i,1],1,11)){
              theblocks_subset[i,"CD"] = theNext
            } else if(substr(thePrevID,1,5) == substr(theblocks_subset[i,1],1,5)){ #Check County Level Next
              theblocks_subset[i,"CD"] = thePrev
            } else if(substr(theNextID,1,5) == substr(theblocks_subset[i,1],1,5)){
              theblocks_subset[i,"CD"] = theNext
            } else { #When all else fails, default to the previous
              theblocks_subset[i,"CD"] = thePrev
            }
          }
        } else {
          if(!is.na(thePrev) & (dim(theblocks_subset)[1]-i)<=10){
            theblocks_subset[i,"CD"] = thePrev
          }
        }
        
      }
    }
  }
}


if(any(is.na(theblocks_subset$CD))==FALSE & dim(theblocks_subset)[1]==tofind){
  write.csv(theblocks_subset,paste("Data/Block_to_CD_Mappings/2010/",curr_state,".csv",sep=""),row.names=FALSE)
  #write.csv(theblocks_subset,paste("Data/Block_to_CD_Mappings/2022/",curr_state,".csv",sep=""),row.names=FALSE)
  cat("\nSuccess: ",curr_state,"\n",sep="")
} else {
  cat("\nMissing CD mappings: ",curr_state,"\n",sep="")
}




#Code can be used to check if blocks mapping correctly (Can take a while for blocks to combine)
#I think it's quicker and easier to just spot check a couple of counties in the output file
#Total population when combined with the census data should be roughly eqaul for congressional districts as another check
# test = theblocks %>% 
#         group_by(CD) %>% 
#         summarise(geometry = st_union(geometry))
# plot(test["CD"])













#####  RUN AFTER RUNNING A FULL YEAR SUCCESSFULLY   ######
states = c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA",
           "KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM",
           "NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA",
           "WV","WI","WY")
year = 2010

holder = NA
for(i in seq(from=1,to=length(states),by=1)){
  cat(i," ",states[i],"\n",sep="")
  
  temp = read.csv(paste("Data/Block_to_CD_Mappings/",year,"/",states[i],".csv",sep="")) %>%
            mutate(state = states[i])
  
  #Stack the states
  if(length(holder)<=1){
    if(is.na(holder)){
      holder = temp
    } else {
      holder = rbind(holder,temp)
    }
  } else {
    holder = rbind(holder,temp)
  }
}

colnames(holder)[1] = "block"

#write out .txt file
write.table(holder,
            file=paste("Data/Block_to_CD_Mappings/",year,"/","Block_to_CD_Mappings.txt",sep=""),
            sep="\t",
            row.names = FALSE,
            col.names = TRUE)






#Code to transform 2012 - 2020 congressional files to same format
#Block to congressional district files were pulled from the Census website
#The 2020 mapping is the 2018 mapping.  There were no changes to congressional districts during that time.
years = c(2012,2014,2016,2018,2020)
cds = c(113,114,115,116,116)

for(i in seq(from=1,to=length(years),by=1)){
  temp = read.table(paste("Data/Block_to_CD_Mappings/",years[i],"/National_CD",cds[i],".txt",sep=""),
                    sep=",",header=T,stringsAsFactors=F) %>%
            arrange(BLOCKID)
  temp$BLOCKID = str_pad(as.character(temp$BLOCKID), 15, pad = "0")
  colnames(temp) = c("block","cd_num")
  temp$STATEFP = str_sub(temp$block,1,2)
  temp = temp %>% 
            filter(cd_num != 'ZZ') %>%
            mutate(cd_num = ifelse(cd_num == "00","01",cd_num)) %>%
            inner_join(.,state_to_fips,by="STATEFP") %>% 
            mutate(CD = paste(State,"-",cd_num,sep="")) %>%
            select("block","CD","State") %>%
            rename(state=State)
  
  if(length(unique(temp$CD))==435){
    write.table(temp,
                file=paste("Data/Block_to_CD_Mappings/",years[i],"/","Block_to_CD_Mappings.txt",sep=""),
                sep="\t",
                row.names = FALSE,
                col.names = TRUE)
    cat("Success: ",years[i],"\n",sep="")
  } else {
    cat("Fail: ",years[i]," CDs: ",length(unique(temp$CD)),"\n",sep="")
  }
}

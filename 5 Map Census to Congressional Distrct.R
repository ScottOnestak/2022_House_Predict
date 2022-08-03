#2022 Election Project
#Author: Scott Onestak
#5 Map Census to Congressional District

#read in packages
library(dplyr)
library(tidyr)
library(stringr)
library(sf)

options(scipen = 100)

#Define loop variables
theStates = c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA",
              "KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM",
              "NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA",
              "WV","WI","WY")
theYears = c(2010,2012,2014,2016,2018,2020,2022)
censusYear = c(2013,2014,2015,2016,2017,2018,2019,2020)
problems = NA
problems_found = FALSE
stack = NA
stack_found = FALSE

#Read in Data
block_rel = read.table("Data/Block Group Relationship/tab20_blkgrp20_blkgrp10_natl.txt",sep="|", header=T, stringsAsFactors = F)
colnames(block_rel)[1] = "OID_BLKGRP_20"
block_rel$GEOID_BLKGRP_10 = as.character(block_rel$GEOID_BLKGRP_10)
block_rel$GEOID_BLKGRP_20 = as.character(block_rel$GEOID_BLKGRP_20)

area_2010 = st_sf(st_read("Data/Block Group Shapefiles/2010/cb_2019_us_bg_500k.shp")) %>% st_drop_geometry() %>% 
              mutate(id = as.numeric(GEOID),
                     LAND = ALAND / 2589988) #Convert square meters to square miles
area_2010$id = as.character(area_2010$id)
area_2020 = st_sf(st_read("Data/Block Group Shapefiles/2020/cb_2020_us_bg_500k.shp")) %>% st_drop_geometry() %>% 
              mutate(id = as.numeric(GEOID),
                     LAND = ALAND / 2589988) #Convert square meters to square miles
area_2020$id = as.character(area_2020$id)

#Update block_rel names to match current names... AZ, SD, and VA update
block_rel = block_rel %>% mutate(GEOID_BLKGRP_20 = ifelse(GEOID_BLKGRP_20=="40190027041","40190027011",
                                                          ifelse(GEOID_BLKGRP_20=="40190027042","40190027012",
                                                                 ifelse(GEOID_BLKGRP_20=="40190029061","40190029031",
                                                                        ifelse(GEOID_BLKGRP_20=="40190041181","40194105011",
                                                                               ifelse(GEOID_BLKGRP_20=="40190041211","40194105021",
                                                                                      ifelse(GEOID_BLKGRP_20=="40190041251","40194105031",
                                                                                             ifelse(GEOID_BLKGRP_20=="40190041252","40194105032",
                                                                                                    ifelse(GEOID_BLKGRP_20=="40190052001","40194704001",
                                                                                                           ifelse(GEOID_BLKGRP_20=="40190052002","40194704002",
                                                                                                                  ifelse(GEOID_BLKGRP_20=="40190052003","40194704003",
                                                                                                                         ifelse(GEOID_BLKGRP_20=="40190052004","40194704004",
                                                                                                                                ifelse(GEOID_BLKGRP_20=="40190053001","40194705001",
                                                                                                                                       ifelse(GEOID_BLKGRP_20=="40190053002","40194705002",
                                                                                                                                              ifelse(GEOID_BLKGRP_20=="60371370001","60379304011",
                                                                                                                                                     ifelse(GEOID_BLKGRP_20=="60371370002","60378002043",GEOID_BLKGRP_20))))))))))))))),
                                 GEOID_BLKGRP_10 = ifelse(GEOID_BLKGRP_10 == "461139405001","461029405001",
                                                          ifelse(GEOID_BLKGRP_10 == "461139405002","461029405002",
                                                                 ifelse(GEOID_BLKGRP_10 == "461139405003","461029405003",
                                                                        ifelse(GEOID_BLKGRP_10 == "461139408001","461029408001",
                                                                               ifelse(GEOID_BLKGRP_10 == "461139408002","461029408002",
                                                                                      ifelse(GEOID_BLKGRP_10 == "461139408003","461029408003",
                                                                                             ifelse(GEOID_BLKGRP_10 == "461139409001","461029409001",
                                                                                                    ifelse(GEOID_BLKGRP_10 == "461139409002","461029409002",
                                                                                                           ifelse(GEOID_BLKGRP_10 == "461139409003","461029409003",
                                                                                                                  ifelse(GEOID_BLKGRP_10 == "515150501001","510190501001",
                                                                                                                         ifelse(GEOID_BLKGRP_10 == "515150501002","510190501002",
                                                                                                                                ifelse(GEOID_BLKGRP_10 == "515150501003","510190501003",
                                                                                                                                       ifelse(GEOID_BLKGRP_10 == "515150501004","510190501004",
                                                                                                                                              ifelse(GEOID_BLKGRP_10 == "515150501005","510190501005",
                                                                                                                                                     ifelse(GEOID_BLKGRP_10 == "22700001001","21580001001",
                                                                                                                                                            ifelse(GEOID_BLKGRP_10 == "22700001002","21580001002",
                                                                                                                                                                   ifelse(GEOID_BLKGRP_10 == "22700001003","21580001003",
                                                                                                                                                                          ifelse(GEOID_BLKGRP_10 == "22700001004","21580001004",
                                                                                                                                                                                 ifelse(GEOID_BLKGRP_10=="40190027041","40190027011",
                                                                                                                                                                                        ifelse(GEOID_BLKGRP_10=="40190027042","40190027012",
                                                                                                                                                                                               ifelse(GEOID_BLKGRP_10=="40190029061","40190029031",
                                                                                                                                                                                                      ifelse(GEOID_BLKGRP_10=="40190041181","40194105011",
                                                                                                                                                                                                             ifelse(GEOID_BLKGRP_10=="40190041211","40194105021",
                                                                                                                                                                                                                    ifelse(GEOID_BLKGRP_10=="40190041251","40194105031",
                                                                                                                                                                                                                           ifelse(GEOID_BLKGRP_10=="40190041252","40194105032",
                                                                                                                                                                                                                                  ifelse(GEOID_BLKGRP_10=="40190052001","40194704001",
                                                                                                                                                                                                                                         ifelse(GEOID_BLKGRP_10=="40190052002","40194704002",
                                                                                                                                                                                                                                                ifelse(GEOID_BLKGRP_10=="40190052003","40194704003",
                                                                                                                                                                                                                                                       ifelse(GEOID_BLKGRP_10=="40190052004","40194704004",
                                                                                                                                                                                                                                                              ifelse(GEOID_BLKGRP_10=="40190053001","40194705001",
                                                                                                                                                                                                                                                                     ifelse(GEOID_BLKGRP_10=="40190053002","40194705002",
                                                                                                                                                                                                                                                                            ifelse(GEOID_BLKGRP_10=="60371370001","60379304011",
                                                                                                                                                                                                                                                                                   ifelse(GEOID_BLKGRP_10=="60371370002","60378002043",GEOID_BLKGRP_10))))))))))))))))))))))))))))))))))

for(i in seq(from=1,to=length(theYears),by=1)){
  theYear = theYears[i]
  mapping = read.table(paste("Data/Block_to_CD_Mappings/",theYear,"/Block_to_CD_Mappings.txt",sep=""),header = T, stringsAsFactors = F)
  
  for(j in seq(from=1,to=length(theStates),by=1)){
    theState = theStates[j]
    cat("\nMAP YEAR: ",theYear," STATE: ",j," : ",theState,"\n",sep="")
    census = read.csv(paste("Data/Census/",theState,".csv",sep=""),header = T,stringsAsFactors = F)
    census$id = as.character(census$id)
    census[census < 0] = NA #replace <= with NAs
    
    substr_length = nchar(census[1,"id"])
    curr_mapping = mapping %>% filter(state == theState) %>% mutate(id = substr(block,1,substr_length))
    
    #test = census %>% filter(year<2020) %>% group_by(id) %>% summarise(count = n()) %>% filter(count<7)
    #test2 = census %>% filter(id %in% unlist(test$id))
    
    #Fix county renames and empty areas to drop
    if(theState=="AK"){
      #Wade Hampton Census Area was renamed to Kusilvak in 2015... rename for data to match
      census_keep = census %>% filter(county!=270)
      census_update = census %>% filter(county==270) %>%
        mutate(county=158) %>%
        mutate(id=paste(state,
                        formatC(as.numeric(county),width=3,format="d",flag=0),
                        formatC(as.numeric(tract),width=6,format="d",flag=0),
                        formatC(as.numeric(blockgroup),width=1,format="d",flag=0),
                        sep=""))
      census = rbind(census_keep,census_update) %>% arrange(id)
      
      curr_mapping = curr_mapping %>%
        mutate(id = ifelse(id == "22700001001","21580001001",
                           ifelse(id == "22700001002","21580001002",
                                  ifelse(id == "22700001003","21580001003",
                                         ifelse(id == "22700001004","21580001004",id)))))
    } else if(theState=="CT" & theYear != 2010 & theYear != 2022){
      #Remove block group out in the water with no population that isn't on mapping files
      census = census %>% filter(!id == 90079901000)
    } else if(theState=="IL" & theYear != 2010 & theYear != 2022){
      #Remove block groups with no population that aren't on the mapping files
      census = census %>% filter(!id %in% c(170979900000,170319900000))
    } else if(theState=="MI" & theYear != 2010 & theYear != 2022){
      #Remove block groups with no population that aren't on the mapping files
      census = census %>% filter(!id %in% c(261059900000,261579900000,260059900000,260999901000))
    } else if(theState=="SD"){
      #Shannon County renamed Oglala County in 2014
      census_keep = census %>% filter(!county==113)
      census_update = census %>% filter(county==113) %>%
        mutate(county=102) %>%
        mutate(id=paste(state,
                        formatC(as.numeric(county),width=3,format="d",flag=0),
                        formatC(as.numeric(tract),width=6,format="d",flag=0),
                        formatC(as.numeric(blockgroup),width=1,format="d",flag=0),
                        sep=""))
      census = rbind(census_keep,census_update) %>% arrange(id)
      
      curr_mapping = curr_mapping %>%
        mutate(id = ifelse(id == 461139405001,461029405001,
                           ifelse(id == 461139405002,461029405002,
                                  ifelse(id == 461139405003,461029405003,
                                         ifelse(id == 461139408001,461029408001,
                                                ifelse(id == 461139408002,461029408002,
                                                       ifelse(id == 461139408003,461029408003,
                                                              ifelse(id == 461139409001,461029409001,
                                                                     ifelse(id == 461139409002,461029409002,
                                                                            ifelse(id == 461139409003,461029409003,id))))))))))
    } else if(theState=="VA"){
      #Bedford City got rolled into Bedford County in 2014
      census_keep = census %>% filter(!county==515)
      census_update = census %>% filter(county==515) %>%
        mutate(county=19) %>%
        mutate(id=paste(state,
                        formatC(as.numeric(county),width=3,format="d",flag=0),
                        formatC(as.numeric(tract),width=6,format="d",flag=0),
                        formatC(as.numeric(blockgroup),width=1,format="d",flag=0),
                        sep=""))
      census = rbind(census_keep,census_update) %>% arrange(id)
      
      curr_mapping = curr_mapping %>%
        mutate(id = ifelse(id == 515150501001,510190501001,
                           ifelse(id == 515150501002,510190501002,
                                  ifelse(id == 515150501003,510190501003,
                                         ifelse(id == 515150501004,510190501004,
                                                ifelse(id == 515150501005,510190501005,id))))))
    }
    
    #Append on urban - rural density data
    census = rbind(census %>% filter(year < 2020) %>%
                     left_join(.,area_2010 %>% select("id","LAND"),by="id"),
                   census %>% filter(year >= 2020) %>%
                     left_join(.,area_2020 %>% select("id","LAND"),by="id")) %>%
              mutate(ppsm = total_population / LAND) %>%
              mutate(class = ifelse(ppsm > 2500,"Highly Dense",
                                    ifelse(ppsm > 1000,"Dense",
                                           ifelse(ppsm > 500,"Spread",
                                                  ifelse(ppsm > 100, "Highly Spread","Sparse")))))
    
    #Fix tract and block level mismatches in data
    if(theState=="AZ"){
      #Match up mismatches between census and mappings at the tract level
      #Census Tract 004118 -> 410501
      #Census Tract 004121 -> 410502
      #Census Tract 004125 -> 410503
      #Census Tract 005200 -> 470400
      #Census Tract 005300 -> 470500
      census_keep = census %>% filter(!id %in% c('40190027041','40190027042','40190029061','40190041181','40190041211',
                                                 '40190041251','40190041252','40190052001','40190052002','40190052003',
                                                 '40190052004','40190053001','40190053002'))
      census_update = census %>% filter(id %in% c('40190027041','40190027042','40190029061','40190041181','40190041211',
                                                  '40190041251','40190041252','40190052001','40190052002','40190052003',
                                                  '40190052004','40190053001','40190053002')) %>%
        mutate(id = ifelse(id=='40190027041','40190027011',
                           ifelse(id=='40190027042','40190027012',
                                  ifelse(id=='40190029061','40190029031',
                                         ifelse(id=='40190041181','40194105011',
                                                ifelse(id=='40190041211','40194105021',
                                                       ifelse(id=='40190041251','40194105031',
                                                              ifelse(id=='40190041252','40194105032',
                                                                     ifelse(id=='40190052001','40194704001',
                                                                            ifelse(id=='40190052002','40194704002',
                                                                                   ifelse(id=='40190052003','40194704003',
                                                                                          ifelse(id=='40190052004','40194704004',
                                                                                                 ifelse(id=='40190053001','40194705001',
                                                                                                        ifelse(id=='40190053002','40194705002',id))))))))))))))
      census = rbind(census_keep,census_update)
      
      curr_mapping = curr_mapping %>%
        mutate(id = ifelse(id=='40190027041','40190027011',
                           ifelse(id=='40190027042','40190027012',
                                  ifelse(id=='40190029061','40190029031',
                                         ifelse(id=='40190041181','40194105011',
                                                ifelse(id=='40190041211','40194105021',
                                                       ifelse(id=='40190041251','40194105031',
                                                              ifelse(id=='40190041252','40194105032',
                                                                     ifelse(id=='40190052001','40194704001',
                                                                            ifelse(id=='40190052002','40194704002',
                                                                                   ifelse(id=='40190052003','40194704003',
                                                                                          ifelse(id=='40190052004','40194704004',
                                                                                                 ifelse(id=='40190053001','40194705001',
                                                                                                        ifelse(id=='40190053002','40194705002',id))))))))))))))
    } else if(theState=="CA"){
      #Census Tract/Block Group 1370001 -> 9304011
      #Census Tract/Block Group 1370002 -> 8002043
      census_keep = census %>% filter(!id %in% c('60371370001','60371370002'))
      census_update = census %>% filter(id %in% c('60371370001','60371370002')) %>%
        mutate(id = ifelse(id=='60371370001','60379304011',
                           ifelse(id=='60371370002','60378002043',id)))
      census = rbind(census_keep,census_update) %>% arrange(id)
      
      curr_mapping = curr_mapping %>% 
        mutate(id = ifelse(id=='60371370001','60379304011',
                           ifelse(id=='60371370002','60378002043',id)))
    } else if(theState=="NY"){
      #County: 53
      #Census Tract 940101 -> 030101
      #Census Tract 940102 -> 030102
      #Census Tract 940103 -> 030103
      #Census Tract 940200 -> 030200
      #Census Tract 940300 -> 030300
      #Census Tract 940401 -> 030401
      #Census Tract 940403 -> 030403
      #Census Tract 940600 -> 030600
      #Census Tract 940700 -> 030402
      
      #County: 65
      #Census Tract 940000 -> 024800
      #Census Tract 940100 -> 024700
      #Census Tract 940200 -> 024900
      
      #County: 85
      #Census Tract 009700 -> 008900
      census_update = census %>% filter(year < 2020) %>% 
        filter((county==53 & substr(id,6,11) %in% c("030101","030102","030103","030200","030300",
                                                    "030401","030403","030600","030402")) |
                 (county==65 & substr(id,6,11) %in% c("024800","024700","024900")) |
                 (county==85 & !substr(id,6,11) %in% c("008900") & blockgroup==0))
      census_keep = census %>% filter(!id %in% unlist(unique(census_update %>% select(id))))
      census_update = census_update %>%
        mutate(tract = ifelse(county==53 & tract==30101,940101,
                              ifelse(county==53 & tract==30102,940102,
                                     ifelse(county==53 & tract==30103,940103,
                                            ifelse(county==53 & tract==30200,940200,
                                                   ifelse(county==53 & tract==30300,940300,
                                                          ifelse(county==53 & tract==30401,940401,
                                                                 ifelse(county==53 & tract==30403,940403,
                                                                        ifelse(county==53 & tract==30600,940600,
                                                                               ifelse(county==53 & tract==30402,940700,
                                                                                      ifelse(county==65 & tract==024800,940000,
                                                                                             ifelse(county==65 & tract==024700,940100,
                                                                                                    ifelse(county==65 & tract==024900,940200,
                                                                                                           ifelse(county==85 & tract==008900,009700,tract)))))))))))))) %>%
        mutate(id=paste(state,
                        formatC(as.numeric(county),width=3,format="d",flag=0),
                        formatC(as.numeric(tract),width=6,format="d",flag=0),
                        formatC(as.numeric(blockgroup),width=1,format="d",flag=0),
                        sep=""))
      
      census = rbind(census_keep,census_update) %>% arrange(id)
      census = census %>% filter(!id == "360850097020") #only shows up fo 2020 and no population
      
      if(theYear != 2010 & theYear != 2022){
        curr_mapping = curr_mapping %>% filter(!id == "360850089000")
      }
    }
    
    
    #Loop through census years
    #The 2nd check was removed because areas with 0 population were showing up... as long as all census data can map to the CDs, then it's good
    for(k in seq(from=1,to=length(censusYear),by=1)){
      curr_year = censusYear[k]
      
      #Check data mapping
      if(theYear <= 2020 & curr_year < 2020){
        curr_problem = FALSE
        census10 = census %>% filter(year < 2020)
        theMapping = curr_mapping %>% group_by(CD,state,id) %>% summarise(count = n()) %>% ungroup %>%
          left_join(.,curr_mapping %>% group_by(state,id) %>% summarise(total = n()) %>% ungroup(),by=c("state","id")) %>%
          mutate(prct = count / total) %>%
          select(-c("count","total"))
        
        if(!identical(setdiff(census10$id,theMapping$id),character(0)) & !identical(setdiff(census10$id,theMapping$id),numeric(0))) {
          if(problems_found == FALSE){
            problems = as.data.frame(cbind(theState,theYear,curr_year,toString(setdiff(census10$id,theMapping$id))))
            colnames(problems) = c("State","Map_Year","Census_Year","Problems")
            problems_found = TRUE
          } else {
            temp = as.data.frame(cbind(theState,theYear,curr_year,toString(setdiff(census10$id,theMapping$id))))
            colnames(temp) = c("State","Map_Year","Census_Year","Problems")
            problems = rbind(problems,temp)
          }
          curr_problem = TRUE
        }
        
        # if(!identical(setdiff(theMapping$id,census10$id),character(0)) & !identical(setdiff(theMapping$id,census10$id),numeric(0))) {
        #   if(problems_found == FALSE){
        #     problems = as.data.frame(cbind(theState,theYear,curr_year,toString(setdiff(theMapping$id,census10$id))))
        #     colnames(problems) = c("State","Map_Year","Census_Year","Problems")
        #     problems_found = TRUE
        #   } else {
        #     temp = as.data.frame(cbind(theState,theYear,curr_year,toString(setdiff(theMapping$id,census10$id))))
        #     colnames(temp) = c("State","Map_Year","Census_Year","Problems")
        #     problems = rbind(problems,temp)
        #   }
        #   curr_problem = TRUE
        # }
      } else if(theYear <= 2020 & curr_year >= 2020){
        if(!all(setdiff(unlist(unique(census %>% filter(year < 2020) %>% select(id))),
                        unlist(unique(census %>% filter(year >= 2020) %>% select(id)))) %in% block_rel$GEOID_BLKGRP_10)){
          if(problems_found == FALSE){
            problems = as.data.frame(cbind(theState,theYear,curr_year,"Not All 2020 Mappings Found"))
            colnames(problems) = c("State","Map_Year","Census_Year","Problems")
            problems_found = TRUE
          } else {
            temp = as.data.frame(cbind(theState,theYear,curr_year,"Not All 2020 Mappings Found"))
            colnames(temp) = c("State","Map_Year","Census_Year","Problems")
            problems = rbind(problems,temp)
          }
          curr_problem = TRUE
        } else {
          mapping_holder = block_rel %>% filter(GEOID_BLKGRP_20 %in% unlist(unique(census %>% filter(year >= 2020) %>% select(id))) |
                                                GEOID_BLKGRP_10 %in% unlist(unique(census %>% filter(year < 2020) %>% select(id)))) %>%
            select(GEOID_BLKGRP_10,GEOID_BLKGRP_20,AREALAND_BLKGRP_10,AREALAND_BLKGRP_20,AREALAND_PART)
          
          holder = curr_mapping %>% 
            left_join(.,mapping_holder,by=c("id"="GEOID_BLKGRP_10")) %>%
            mutate(name = GEOID_BLKGRP_20,
                   part = AREALAND_PART / AREALAND_BLKGRP_10) %>%
            select(block,CD,state,name,part) %>%
            rename(id = name) 
          
          theMapping = holder %>% group_by(CD,state,id) %>% summarise(count = sum(part)) %>% ungroup %>%
            left_join(.,holder %>% group_by(state,id) %>% summarise(total = sum(part)) %>% ungroup(),by=c("state","id")) %>%
            mutate(prct = count / total) %>%
            select(-c("count","total"))
          
          census20 = census %>% filter(year >= 2020)
          
          if(!identical(setdiff(census20$id,theMapping$id),character(0)) & !identical(setdiff(census20$id,theMapping$id),numeric(0))) {
            if(problems_found == FALSE){
              problems = as.data.frame(cbind(theState,theYear,curr_year,toString(setdiff(census20$id,theMapping$id))))
              colnames(problems) = c("State","Map_Year","Census_Year","Problems")
              problems_found = TRUE
            } else {
              temp = as.data.frame(cbind(theState,theYear,curr_year,toString(setdiff(census20$id,theMapping$id))))
              colnames(temp) = c("State","Map_Year","Census_Year","Problems")
              problems = rbind(problems,temp)
            }
            curr_problem = TRUE
          }
          
          # if(!identical(setdiff(theMapping$id,census20$id),character(0)) & !identical(setdiff(theMapping$id,census20$id),numeric(0))) {
          #   if(problems_found == FALSE){
          #     problems = as.data.frame(cbind(theState,theYear,curr_year,toString(setdiff(theMapping$id,census20$id))))
          #     colnames(problems) = c("State","Map_Year","Census_Year","Problems")
          #     problems_found = TRUE
          #   } else {
          #     temp = as.data.frame(cbind(theState,theYear,curr_year,toString(setdiff(theMapping$id,census20$id))))
          #     colnames(temp) = c("State","Map_Year","Census_Year","Problems")
          #     problems = rbind(problems,temp)
          #   }
          #   curr_problem = TRUE
          # }
        } 
      } else if(theYear > 2020 & curr_year < 2020){
        if(!all(setdiff(unlist(unique(census %>% filter(year >= 2020) %>% select(id))),
                        unlist(unique(census %>% filter(year < 2020) %>% select(id)))) %in% block_rel$GEOID_BLKGRP_20)){
          if(problems_found == FALSE){
            problems = as.data.frame(cbind(theState,theYear,curr_year,"Not All 2010 Mappings Found"))
            colnames(problems) = c("State","Map_Year","Census_Year","Problems")
            problems_found = TRUE
          } else {
            temp = as.data.frame(cbind(theState,theYear,curr_year,"Not All 2010 Mappings Found"))
            colnames(temp) = c("State","Map_Year","Census_Year","Problems")
            problems = rbind(problems,temp)
          }
          curr_problem = TRUE
        } else {
          mapping_holder = block_rel %>% filter(GEOID_BLKGRP_10 %in% unlist(unique(census %>% filter(year < 2020) %>% select(id)))) %>%
            select(GEOID_BLKGRP_10,GEOID_BLKGRP_20,AREALAND_BLKGRP_10,AREALAND_BLKGRP_20,AREALAND_PART)
          
          holder = curr_mapping %>% 
            left_join(.,mapping_holder,by=c("id"="GEOID_BLKGRP_20")) %>%
            mutate(name = ifelse(is.na(GEOID_BLKGRP_10),id,GEOID_BLKGRP_10),
                   part = AREALAND_PART / AREALAND_BLKGRP_20) %>%
            select(block,CD,state,name,part) %>%
            rename(id = name) 
          
          theMapping = holder %>% group_by(CD,state,id) %>% summarise(count = sum(part)) %>% ungroup %>%
            left_join(.,holder %>% group_by(state,id) %>% summarise(total = sum(part)) %>% ungroup(),by=c("state","id")) %>%
            mutate(prct = count / total) %>%
            select(-c("count","total"))
          
          census10 = census %>% filter(year < 2020)
          
          if(!identical(setdiff(census10$id,theMapping$id),character(0)) & !identical(setdiff(census10$id,theMapping$id),numeric(0))) {
            if(problems_found == FALSE){
              problems = as.data.frame(cbind(theState,theYear,curr_year,toString(setdiff(census10$id,theMapping$id))))
              colnames(problems) = c("State","Map_Year","Census_Year","Problems")
              problems_found = TRUE
            } else {
              temp = as.data.frame(cbind(theState,theYear,curr_year,toString(setdiff(census10$id,theMapping$id))))
              colnames(temp) = c("State","Map_Year","Census_Year","Problems")
              problems = rbind(problems,temp)
            }
            curr_problem = TRUE
          }
          
          # if(!identical(setdiff(theMapping$id,census10$id),character(0)) & !identical(setdiff(theMapping$id,census10$id),numeric(0))) {
          #   if(problems_found == FALSE){
          #     problems = as.data.frame(cbind(theState,theYear,curr_year,toString(setdiff(theMapping$id,census10$id))))
          #     colnames(problems) = c("State","Map_Year","Census_Year","Problems")
          #     problems_found = TRUE
          #   } else {
          #     temp = as.data.frame(cbind(theState,theYear,curr_year,toString(setdiff(theMapping$id,census10$id))))
          #     colnames(temp) = c("State","Map_Year","Census_Year","Problems")
          #     problems = rbind(problems,temp)
          #   }
          #   curr_problem = TRUE
          # }
        } 
      } else {
        curr_problem = FALSE
        census20 = census %>% filter(year >= 2020)
        theMapping = curr_mapping %>% group_by(CD,state,id) %>% summarise(count = n()) %>% ungroup %>%
          left_join(.,curr_mapping %>% group_by(state,id) %>% summarise(total = n()) %>% ungroup(),by=c("state","id")) %>%
          mutate(prct = count / total) %>%
          select(-c("count","total"))
        
        if(!identical(setdiff(census20$id,theMapping$id),character(0)) & !identical(setdiff(census20$id,theMapping$id),numeric(0))) {
          if(problems_found == FALSE){
            problems = as.data.frame(cbind(theState,theYear,curr_year,toString(setdiff(census20$id,theMapping$id))))
            colnames(problems) = c("State","Map_Year","Census_Year","Problems")
            problems_found = TRUE
          } else {
            temp = as.data.frame(cbind(theState,theYear,curr_year,toString(setdiff(census20$id,theMapping$id))))
            colnames(temp) = c("State","Map_Year","Census_Year","Problems")
            problems = rbind(problems,temp)
          }
          curr_problem = TRUE
        }
        
        # if(!identical(setdiff(theMapping$id,census20$id),character(0)) & !identical(setdiff(theMapping$id,census20$id),numeric(0))) {
        #   if(problems_found == FALSE){
        #     problems = as.data.frame(cbind(theState,theYear,curr_year,toString(setdiff(theMapping$id,census20$id))))
        #     colnames(problems) = c("State","Map_Year","Census_Year","Problems")
        #     problems_found = TRUE
        #   } else {
        #     temp = as.data.frame(cbind(theState,theYear,curr_year,toString(setdiff(theMapping$id,census20$id))))
        #     colnames(temp) = c("State","Map_Year","Census_Year","Problems")
        #     problems = rbind(problems,temp)
        #   }
        #   curr_problem = TRUE
        # }
      }
      
      
      #if no problems, aggregate the data
      if(curr_problem == FALSE){
        curr_census = theMapping %>% inner_join(.,census %>% filter(year == curr_year) %>% 
                                                 select(-c("name","state","state_name","county","tract","blockgroup","year")),
                                               by="id") %>%
          mutate(total_population = prct * total_population,
                 white_population = prct * white_population,
                 black_population = prct * black_population,
                 asian_population = prct * asian_population,
                 hispanic_population = prct * hispanic_population,
                 male_under35 = prct * male_under35,
                 male_35to50 = prct * male_35to50,
                 male_50to65 = prct * male_50to65,
                 male_over65 = prct * male_over65,
                 female_under35 = prct * female_under35,
                 female_35to50 = prct * female_35to50,
                 female_50to65 = prct * female_50to65,
                 female_over65 = prct * female_over65,
                 votingage_male = prct * votingage_male,
                 votingage_female = prct * votingage_female,
                 total_votingage = prct * total_votingage,
                 male = prct * male,
                 male_single = prct * male_single,
                 male_married = prct * male_married,
                 male_widowed = prct * male_widowed,
                 male_divorced = prct * male_divorced,
                 female = prct * female,
                 female_single = prct * female_single,
                 female_married = prct * female_married,
                 female_widowed = prct * female_widowed,
                 female_divorced = prct * female_divorced,
                 labor_population = prct * labor_population,
                 labor_force = prct * labor_force,
                 unemployed = prct * unemployed,
                 education_total = prct * education_total,
                 highschool = prct * highschool,
                 somecollege = prct * somecollege,
                 bachelors = prct * bachelors,
                 masters = prct * masters,
                 phd = prct * phd,
                 nohighschool = prct * nohighschool,
                 veterans = prct * veterans,
                 owner_occupied = prct * owner_occupied,
                 renter_occupied = prct * renter_occupied,
                 family_hhs = prct * family_hhs,
                 nonfamily_hhs = prct * nonfamily_hhs,
                 poverty_hhs = prct * poverty_hhs,
                 below_poverty_hhs = prct * below_poverty_hhs,
                 insurance_total = prct * insurance_total,
                 noinsurance = prct * noinsurance,
                 total_assistance_hhs = prct * total_assistance_hhs,
                 public_assistance_hhs = prct * public_assistance_hhs)
        
        summary1 = curr_census %>% group_by(CD) %>%
          summarise(total_population = sum(total_population,na.rm=T),
                    white_population = sum(white_population,na.rm=T),
                    black_population = sum(black_population,na.rm=T),
                    asian_population = sum(asian_population,na.rm=T),
                    hispanic_population = sum(hispanic_population,na.rm=T),
                    male_under35 = sum(male_under35,na.rm=T),
                    male_35to50 = sum(male_35to50,na.rm=T),
                    male_50to65 = sum(male_50to65,na.rm=T),
                    male_over65 = sum(male_over65,na.rm=T),
                    female_under35 = sum(female_under35,na.rm=T),
                    female_35to50 = sum(female_35to50,na.rm=T),
                    female_50to65 = sum(female_50to65,na.rm=T),
                    female_over65 = sum(female_over65,na.rm=T),
                    votingage_male = sum(votingage_male,na.rm=T),
                    votingage_female = sum(votingage_female,na.rm=T),
                    total_votingage = sum(total_votingage,na.rm=T),
                    male = sum(male,na.rm=T),
                    male_single = sum(male_single,na.rm=T),
                    male_married = sum(male_married,na.rm=T),
                    male_widowed = sum(male_widowed,na.rm=T),
                    male_divorced = sum(male_divorced,na.rm=T),
                    female = sum(female,na.rm=T),
                    female_single = sum(female_single,na.rm=T),
                    female_married = sum(female_married,na.rm=T),
                    female_widowed = sum(female_widowed,na.rm=T),
                    female_divorced = sum(female_divorced,na.rm=T),
                    labor_population = sum(labor_population,na.rm=T),
                    labor_force = sum(labor_force,na.rm=T),
                    unemployed = sum(unemployed,na.rm=T),
                    education_total = sum(education_total,na.rm=T),
                    highschool = sum(highschool,na.rm=T),
                    somecollege = sum(somecollege,na.rm=T),
                    bachelors = sum(bachelors,na.rm=T),
                    masters = sum(masters,na.rm=T),
                    phd = sum(phd,na.rm=T),
                    nohighschool = sum(nohighschool,na.rm=T),
                    veterans = sum(veterans,na.rm=T),
                    owner_occupied = sum(owner_occupied,na.rm=T),
                    renter_occupied = sum(renter_occupied,na.rm=T),
                    family_hhs = sum(family_hhs,na.rm=T),
                    nonfamily_hhs = sum(nonfamily_hhs,na.rm=T),
                    poverty_hhs = sum(poverty_hhs,na.rm=T),
                    below_poverty_hhs = sum(below_poverty_hhs,na.rm=T),
                    insurance_total = sum(insurance_total,na.rm=T),
                    noinsurance = sum(noinsurance,na.rm=T),
                    total_assistance_hhs = sum(total_assistance_hhs,na.rm=T),
                    public_assistance_hhs = sum(public_assistance_hhs,na.rm=T)) %>%
          ungroup() %>%
          mutate(white_prct = round(white_population / total_population,3),
                 black_prct = round(black_population / total_population,3),
                 asian_prct = round(asian_population / total_population,3),
                 hispanic_prct = round(hispanic_population / total_population,3),
                 male_prct = round(male / (male + female),3),
                 female_prct = round(female / (male + female),3),
                 male_single_prct = round(male_single / (male + female),3),
                 male_married_prct = round(male_married / (male + female),3),
                 male_widowed_prct = round(male_widowed / (male + female),3),
                 male_divorced_prct = round(male_divorced / (male + female),3),
                 female_single_prct = round(female_single / (male + female),3),
                 female_married_prct = round(female_married / (male + female),3),
                 female_widowed_prct = round(female_widowed / (male + female),3),
                 female_divorced_prct = round(female_divorced / (male + female),3),
                 single_prct = round((male_single + female_single) / (male + female),3),
                 married_prct = round((male_married + female_married) / (male + female),3),
                 widowed_prct = round((male_widowed + female_widowed) / (male + female),3),
                 divorced_prct = round((male_divorced + female_divorced) / (male + female),3),
                 male_votingage_prct = round(votingage_male / total_votingage,3),
                 female_votingage_prct = round(votingage_female / total_votingage,3),
                 male_under35_prct = round(male_under35 / total_votingage,3),
                 male_35to50_prct = round(male_35to50 / total_votingage,3),
                 male_50to65_prct = round(male_50to65 / total_votingage,3),
                 male_over65_prct = round(male_over65 / total_votingage,3),
                 female_under35_prct = round(female_under35 / total_votingage,3),
                 female_35to50_prct = round(female_35to50 / total_votingage,3),
                 female_50to65_prct = round(female_50to65 / total_votingage,3),
                 female_over65_prct = round(female_over65 / total_votingage,3),
                 under35_prct = round((male_under35 + female_under35) / total_votingage,3),
                 b35to50_prct = round((male_35to50 + female_35to50) / total_votingage,3),
                 b50to65_prct = round((male_50to65 + female_50to65) / total_votingage,3),
                 over65_prct = round((male_over65 + female_over65) / total_votingage,3),
                 veterans_prct = round(veterans / total_votingage,3),
                 lfpr = round(labor_force / labor_population,3),
                 unemployment_rate = round(unemployed / labor_force,3),
                 poverty_rate = round(below_poverty_hhs / poverty_hhs,3),
                 assistance_rate = round(public_assistance_hhs / total_assistance_hhs,3),
                 noninsured_rate = round(noinsurance / insurance_total,3),
                 nohighschool_prct = round(nohighschool / education_total,3),
                 highschool_prct = round(highschool / education_total,3),
                 somecollege_prct = round(somecollege / education_total,3),
                 bachelors_prct = round(bachelors / education_total,3),
                 masters_prct = round(masters / education_total,3),
                 phd_prct = round(phd / education_total,3),
                 college_educated_prct = round((bachelors + masters + phd) / education_total,3),
                 highly_educated_prct = round((masters + phd) / education_total,3),
                 owner_occupied_prct = round(owner_occupied / (owner_occupied + renter_occupied),3),
                 renter_occupied_prct = round(renter_occupied / (owner_occupied + renter_occupied),3),
                 family_hh_prct = round(family_hhs / (family_hhs + nonfamily_hhs),3),
                 nonfamily_hh_prct = round(nonfamily_hhs / (family_hhs + nonfamily_hhs),3)) %>%
          select(CD,total_population,white_prct,black_prct,asian_prct,hispanic_prct,
                 male_prct,female_prct,
                 male_single_prct,male_married_prct,male_widowed_prct,male_divorced_prct,
                 female_single_prct,female_married_prct,female_widowed_prct,female_divorced_prct,
                 single_prct,married_prct,widowed_prct,divorced_prct,
                 male_votingage_prct,female_votingage_prct,
                 male_under35_prct,male_35to50_prct,male_50to65_prct,male_over65_prct,
                 female_under35_prct,female_35to50_prct,female_50to65_prct,female_over65_prct,
                 under35_prct,b35to50_prct,b50to65_prct,over65_prct,
                 veterans_prct,
                 lfpr,unemployment_rate,poverty_rate,assistance_rate,noninsured_rate,
                 nohighschool_prct,highschool_prct,somecollege_prct,bachelors_prct,masters_prct,phd_prct,college_educated_prct,highly_educated_prct,
                 owner_occupied_prct,renter_occupied_prct,family_hh_prct,nonfamily_hh_prct)
        
        summary2 = curr_census %>% 
          mutate(total_population = ifelse(is.na(median_house_value),NA,total_population)) %>% 
          group_by(CD) %>%
          summarise(median_house_value = round(weighted.mean(median_house_value,total_population,na.rm = T),0))
        
        summary3 = curr_census %>% 
          mutate(total_population = ifelse(is.na(median_rent),NA,total_population)) %>% 
          group_by(CD) %>%
          summarise(median_rent = round(weighted.mean(median_rent,total_population,na.rm = T),0))
        
        summary4 = curr_census %>% 
          mutate(total_population = ifelse(is.na(median_age),NA,total_population)) %>% 
          group_by(CD) %>%
          summarise(median_age = round(weighted.mean(median_age,total_population,na.rm = T),1))
        
        summary5 = curr_census %>% 
          mutate(total_population = ifelse(is.na(median_hh_income),NA,total_population)) %>% 
          group_by(CD) %>%
          summarise(median_hh_income = round(weighted.mean(median_hh_income,total_population,na.rm = T),0))
        
        summary6 = curr_census %>% 
          mutate(total_population = ifelse(is.na(per_capita_income),NA,total_population)) %>% 
          group_by(CD) %>%
          summarise(per_capita_income = round(weighted.mean(per_capita_income,total_population,na.rm = T),0))
        
        summary7 = curr_census %>% 
          mutate(total_population = ifelse(is.na(ppsm),NA,total_population)) %>% 
          group_by(CD) %>%
          summarise(ppsm = round(weighted.mean(ppsm,total_population,na.rm = T),0))
        
        summary8 = curr_census %>%
          group_by(CD,class) %>%
          summarise(pop = sum(total_population,na.rm = T)) %>%
          filter(!is.na(class)) %>%
          spread(.,class,pop) %>%
          mutate(highlydense_prct = round(`Highly Dense` / (`Highly Dense` + `Dense` + `Spread` + `Highly Spread` + `Sparse`),3),
                 dense_prct = round(`Dense` / (`Highly Dense` + `Dense` + `Spread` + `Highly Spread` + `Sparse`),3),
                 spread_prct = round(`Spread` / (`Highly Dense` + `Dense` + `Spread` + `Highly Spread` + `Sparse`),3),
                 highlyspread_prct = round(`Highly Spread` / (`Highly Dense` + `Dense` + `Spread` + `Highly Spread` + `Sparse`),3),
                 sparse_prct = round(`Sparse` / (`Highly Dense` + `Dense` + `Spread` + `Highly Spread` + `Sparse`),3)) %>%
          select(CD,highlydense_prct,dense_prct,spread_prct,highlyspread_prct,sparse_prct)
        
        summary = summary1 %>% 
          left_join(.,summary2,by="CD") %>%
          left_join(.,summary3,by="CD") %>%
          left_join(.,summary4,by="CD") %>%
          left_join(.,summary5,by="CD") %>%
          left_join(.,summary6,by="CD") %>%
          left_join(.,summary7,by="CD") %>%
          left_join(.,summary8,by="CD") %>%
          mutate(year = curr_year,
                 cd_year = theYear)
        
        
        #stack summary results
        if(stack_found==FALSE){
          stack = summary
          stack_found = TRUE
        } else {
          stack = rbind(stack,summary)
        }
        
      }
    }
  }
}


#Write out the file
if(problems_found==FALSE){
  write.csv(stack,"Data/Census_by_CDs/census_raw.csv",row.names = FALSE)
}


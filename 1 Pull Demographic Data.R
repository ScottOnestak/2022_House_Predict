#2022 Election Project
#Author: Scott Onestak
#1 Pull Demographic Data

#Bring in packages
library(acs)
library(tidyverse)

#set acs api key
acs_key = "2b79d333bb5d985abb33a41ae240be5f83f09f86"

#pull acs data down to block group (lowest acs data level)
theStates = c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA",
              "KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM",
              "NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA",
              "WV","WI","WY")
years = c(2013,2014,2015,2016,2017,2018,2019,2020)

for(i in seq(from=49,to=length(theStates),by=1)){
  cat(theStates[i],"\n",sep="")
  theGeo = geo.make(state=theStates[i],county="*",tract="*",block.group="*")
  stack = NA
  
  for(j in seq(from=1,to=length(years),by=1)){
    age = NA
    economic = NA
    education = NA
    housing = NA
    housing2 = NA
    housing3 = NA
    income = NA
    marital_status = NA
    race = NA
    test = NA
    holder = NA
    
    test = acs.fetch(endyear=years[j],span=5,geography=theGeo,table.number="B03002",key=acs_key)
    race = data.frame(name = as.character(geography(test)$NAME), 
                      state = as.numeric(geography(test)$state),
                      state_name = theStates[i],
                      county = as.numeric(geography(test)$county),
                      tract = as.numeric(geography(test)$tract),
                      blockgroup = as.numeric(geography(test)$blockgroup),
                      id = paste(formatC(as.numeric(geography(test)$state),width=2,format="d",flag=0),
                                 formatC(as.numeric(geography(test)$county),width=3,format="d",flag=0),
                                 formatC(as.numeric(geography(test)$tract),width=6,format="d",flag=0),
                                 formatC(as.numeric(geography(test)$blockgroup),width=1,format="d",flag=0),
                                 sep=""),
                      year = years[j],
                      total_population = as.numeric(estimate(test[,"B03002_001"])),
                      white_population = as.numeric(estimate(test[,"B03002_003"])),
                      black_population = as.numeric(estimate(test[,"B03002_004"])),
                      asian_population = as.numeric(estimate(test[,"B03002_006"])),
                      hispanic_population = as.numeric(estimate(test[,"B03002_012"])))
    
    test = acs.fetch(endyear=years[j],span=5,geography=theGeo,table.number="B01001",key=acs_key)
    age = data.frame(name = as.character(geography(test)$NAME), 
                     id = paste(formatC(as.numeric(geography(test)$state),width=2,format="d",flag=0),
                                formatC(as.numeric(geography(test)$county),width=3,format="d",flag=0),
                                formatC(as.numeric(geography(test)$tract),width=6,format="d",flag=0),
                                formatC(as.numeric(geography(test)$blockgroup),width=1,format="d",flag=0),
                                sep=""),
                     male_under35 = as.numeric(estimate(test[,"B01001_007"])) +
                       as.numeric(estimate(test[,"B01001_008"])) +
                       as.numeric(estimate(test[,"B01001_009"])) +
                       as.numeric(estimate(test[,"B01001_010"])) +
                       as.numeric(estimate(test[,"B01001_011"])) +
                       as.numeric(estimate(test[,"B01001_012"])),
                     male_35to50 = as.numeric(estimate(test[,"B01001_013"])) +
                       as.numeric(estimate(test[,"B01001_014"])) +
                       as.numeric(estimate(test[,"B01001_015"])),
                     male_50to65 = as.numeric(estimate(test[,"B01001_016"])) +
                       as.numeric(estimate(test[,"B01001_017"])) +
                       as.numeric(estimate(test[,"B01001_018"])) +
                       as.numeric(estimate(test[,"B01001_019"])),
                     male_over65 = as.numeric(estimate(test[,"B01001_020"])) +
                       as.numeric(estimate(test[,"B01001_021"])) +
                       as.numeric(estimate(test[,"B01001_022"])) +
                       as.numeric(estimate(test[,"B01001_023"])) +
                       as.numeric(estimate(test[,"B01001_024"])) +
                       as.numeric(estimate(test[,"B01001_025"])),
                     female_under35 = as.numeric(estimate(test[,"B01001_031"])) +
                       as.numeric(estimate(test[,"B01001_032"])) +
                       as.numeric(estimate(test[,"B01001_033"])) +
                       as.numeric(estimate(test[,"B01001_034"])) +
                       as.numeric(estimate(test[,"B01001_035"])) +
                       as.numeric(estimate(test[,"B01001_036"])),
                     female_35to50 = as.numeric(estimate(test[,"B01001_037"])) +
                       as.numeric(estimate(test[,"B01001_038"])) +
                       as.numeric(estimate(test[,"B01001_039"])),
                     female_50to65 = as.numeric(estimate(test[,"B01001_040"])) +
                       as.numeric(estimate(test[,"B01001_041"])) +
                       as.numeric(estimate(test[,"B01001_042"])) +
                       as.numeric(estimate(test[,"B01001_043"])),
                     female_over65 = as.numeric(estimate(test[,"B01001_044"])) +
                       as.numeric(estimate(test[,"B01001_045"])) +
                       as.numeric(estimate(test[,"B01001_046"])) +
                       as.numeric(estimate(test[,"B01001_047"])) +
                       as.numeric(estimate(test[,"B01001_048"])) +
                       as.numeric(estimate(test[,"B01001_049"]))) %>%
      mutate(votingage_male = male_under35+male_35to50+male_50to65+male_over65,
             votingage_female = female_under35+female_35to50+female_50to65+female_over65,
             total_votingage = male_under35+male_35to50+male_50to65+male_over65+
               female_under35+female_35to50+female_50to65+female_over65)
    
    test = acs.fetch(endyear=years[j],span=5,geography=theGeo,table.number="B19113",key=acs_key)
    income = data.frame(name = as.character(geography(test)$NAME), 
                        id = paste(formatC(as.numeric(geography(test)$state),width=2,format="d",flag=0),
                                   formatC(as.numeric(geography(test)$county),width=3,format="d",flag=0),
                                   formatC(as.numeric(geography(test)$tract),width=6,format="d",flag=0),
                                   formatC(as.numeric(geography(test)$blockgroup),width=1,format="d",flag=0),
                                   sep=""),
                        median_family_income = as.numeric(estimate(test[,"B19113_001"])))
    
    test = acs.fetch(endyear=years[j],span=5,geography=theGeo,table.number="B23025",key=acs_key)
    economic = data.frame(name = as.character(geography(test)$NAME), 
                          id = paste(formatC(as.numeric(geography(test)$state),width=2,format="d",flag=0),
                                     formatC(as.numeric(geography(test)$county),width=3,format="d",flag=0),
                                     formatC(as.numeric(geography(test)$tract),width=6,format="d",flag=0),
                                     formatC(as.numeric(geography(test)$blockgroup),width=1,format="d",flag=0),
                                     sep=""),
                          labor_population = as.numeric(estimate(test[,"B23025_001"])),
                          labor_force = as.numeric(estimate(test[,"B23025_002"])),
                          unemployed = as.numeric(estimate(test[,"B23025_005"])))
    
    test = acs.fetch(endyear=years[j],span=5,geography=theGeo,table.number="B12001",key=acs_key)
    marital_status = data.frame(name = as.character(geography(test)$NAME), 
                                id = paste(formatC(as.numeric(geography(test)$state),width=2,format="d",flag=0),
                                           formatC(as.numeric(geography(test)$county),width=3,format="d",flag=0),
                                           formatC(as.numeric(geography(test)$tract),width=6,format="d",flag=0),
                                           formatC(as.numeric(geography(test)$blockgroup),width=1,format="d",flag=0),
                                           sep=""),
                                male = as.numeric(estimate(test[,"B12001_002"])),
                                male_single = as.numeric(estimate(test[,"B12001_003"])),
                                male_married = as.numeric(estimate(test[,"B12001_004"])),
                                male_widowed = as.numeric(estimate(test[,"B12001_009"])),
                                male_divorced = as.numeric(estimate(test[,"B12001_010"])),
                                female = as.numeric(estimate(test[,"B12001_011"])),
                                female_single = as.numeric(estimate(test[,"B12001_012"])),
                                female_married = as.numeric(estimate(test[,"B12001_013"])),
                                female_widowed = as.numeric(estimate(test[,"B12001_018"])),
                                female_divorced = as.numeric(estimate(test[,"B12001_019"])))
    
    test = acs.fetch(endyear=years[j],span=5,geography=theGeo,table.number="B21002",key=acs_key)
    veterans = data.frame(name = as.character(geography(test)$NAME), 
                          id = paste(formatC(as.numeric(geography(test)$state),width=2,format="d",flag=0),
                                     formatC(as.numeric(geography(test)$county),width=3,format="d",flag=0),
                                     formatC(as.numeric(geography(test)$tract),width=6,format="d",flag=0),
                                     formatC(as.numeric(geography(test)$blockgroup),width=1,format="d",flag=0),
                                     sep=""),
                          veterans = as.numeric(estimate(test[,"B21002_001"])))
    
    test = acs.fetch(endyear=years[j],span=5,geography=theGeo,table.number="B15003",key=acs_key)
    education = data.frame(name = as.character(geography(test)$NAME), 
                           id = paste(formatC(as.numeric(geography(test)$state),width=2,format="d",flag=0),
                                      formatC(as.numeric(geography(test)$county),width=3,format="d",flag=0),
                                      formatC(as.numeric(geography(test)$tract),width=6,format="d",flag=0),
                                      formatC(as.numeric(geography(test)$blockgroup),width=1,format="d",flag=0),
                                      sep=""),
                           education_total = as.numeric(estimate(test[,"B15003_001"])),
                           highschool = as.numeric(estimate(test[,"B15003_017"])) +
                             as.numeric(estimate(test[,"B15003_018"])),
                           somecollege = as.numeric(estimate(test[,"B15003_019"])) +
                             as.numeric(estimate(test[,"B15003_020"])) +
                             as.numeric(estimate(test[,"B15003_021"])),
                           bachelors = as.numeric(estimate(test[,"B15003_022"])),
                           masters = as.numeric(estimate(test[,"B15003_023"])),
                           phd = as.numeric(estimate(test[,"B15003_024"])) +
                             as.numeric(estimate(test[,"B15003_025"]))) %>%
      mutate(nohighschool = education_total - highschool - somecollege - bachelors - masters - phd)
    
    test = acs.fetch(endyear=years[j],span=5,geography=theGeo,table.number="B25008",key=acs_key)
    housing = data.frame(name = as.character(geography(test)$NAME), 
                         id = paste(formatC(as.numeric(geography(test)$state),width=2,format="d",flag=0),
                                    formatC(as.numeric(geography(test)$county),width=3,format="d",flag=0),
                                    formatC(as.numeric(geography(test)$tract),width=6,format="d",flag=0),
                                    formatC(as.numeric(geography(test)$blockgroup),width=1,format="d",flag=0),
                                    sep=""),
                         owner_occupied = as.numeric(estimate(test[,"B25008_002"])),
                         renter_occupied = as.numeric(estimate(test[,"B25008_003"])))
    
    test = acs.fetch(endyear=years[j],span=5,geography=theGeo,table.number="B25077",key=acs_key)
    housing2 = data.frame(name = as.character(geography(test)$NAME), 
                          id = paste(formatC(as.numeric(geography(test)$state),width=2,format="d",flag=0),
                                     formatC(as.numeric(geography(test)$county),width=3,format="d",flag=0),
                                     formatC(as.numeric(geography(test)$tract),width=6,format="d",flag=0),
                                     formatC(as.numeric(geography(test)$blockgroup),width=1,format="d",flag=0),
                                     sep=""),
                          median_house_value = as.numeric(estimate(test[,"B25077_001"])))
    
    test = acs.fetch(endyear=years[j],span=5,geography=theGeo,table.number="B25058",key=acs_key)
    housing3 = data.frame(name = as.character(geography(test)$NAME), 
                          id = paste(formatC(as.numeric(geography(test)$state),width=2,format="d",flag=0),
                                     formatC(as.numeric(geography(test)$county),width=3,format="d",flag=0),
                                     formatC(as.numeric(geography(test)$tract),width=6,format="d",flag=0),
                                     formatC(as.numeric(geography(test)$blockgroup),width=1,format="d",flag=0),
                                     sep=""),
                          median_rent = as.numeric(estimate(test[,"B25058_001"])))
    
    holder = race %>% 
      left_join(.,age,by=c("name","id")) %>%
      left_join(.,income,by=c("name","id")) %>%
      left_join(.,economic,by=c("name","id")) %>%
      left_join(.,marital_status,by=c("name","id")) %>%
      left_join(.,veterans,by=c("name","id")) %>%
      left_join(.,education,by=c("name","id")) %>%
      left_join(.,housing,by=c("name","id")) %>%
      left_join(.,housing2,by=c("name","id")) %>%
      left_join(.,housing3,by=c("name","id"))
    
    if(j==1){
      stack = holder
    } else {
      stack = rbind(stack,holder)
    }
  }
  
  #Write out the data to folder
  write.csv(stack,paste("Data/Census/",theStates[i],".csv",sep=""),row.names=FALSE)
  rm(list = setdiff(ls(),c("acs_key","theStates","years","i","j")))
  gc()
}



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


#install tables... only need to run once
#acs.tables.install()


#May want to choose to run in chunks... it's a lot of data to pull down
#Tables were pulled individually instead of altogether... when pulled together, the data seemed misaligned in some years
for(i in seq(from=1,to=length(theStates),by=1)){
  cat(theStates[i],"\n",sep="")
  theGeo = geo.make(state=theStates[i],county="*",tract="*",block.group="*")
  stack = NA
  
  for(j in seq(from=1,to=length(years),by=1)){
    age = NA
    age2 = NA
    edu = NA
    hhs = NA
    hisp = NA
    hou1 = NA
    hou2 = NA
    hou3 = NA
    inc1 = NA
    inc2 = NA
    ins = NA
    lab = NA
    mar = NA
    pass = NA
    pop = NA
    pov = NA
    test = NA
    holder = NA
    vet = NA
    
    #population
    test = acs.fetch(endyear=years[j],span=5,geography=theGeo,
                     table.number=c("B02001"),
                     key=acs_key)
    pop = data.frame(name = as.character(geography(test)$NAME), 
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
                       total_population = as.numeric(estimate(test[,"B02001_001"])),
                       white_population = as.numeric(estimate(test[,"B02001_002"])),
                       black_population = as.numeric(estimate(test[,"B02001_003"])),
                       asian_population = as.numeric(estimate(test[,"B02001_005"])))
    
    
    #hispanic
    test = acs.fetch(endyear=years[j],span=5,geography=theGeo,
                     table.number=c("B03003"),
                     key=acs_key)
    hisp = data.frame(name = as.character(geography(test)$NAME), 
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
                      hispancic_population = as.numeric(estimate(test[,"B03003_003"])))
    
    #age
    test = acs.fetch(endyear=years[j],span=5,geography=theGeo,
                     table.number=c("B01002"),
                     key=acs_key)
    age = data.frame(name = as.character(geography(test)$NAME), 
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
                      median_age = as.numeric(estimate(test[,"B01002_001"])))
    
    #poverty
    test = acs.fetch(endyear=years[j],span=5,geography=theGeo,
                     table.number=c("B17010"),
                     key=acs_key)
    pov = data.frame(name = as.character(geography(test)$NAME), 
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
                     poverty_hhs = as.numeric(estimate(test[,"B17010_001"])),
                     below_poverty_hhs = as.numeric(estimate(test[,"B17010_002"])))
    
    #insurance
    test = acs.fetch(endyear=years[j],span=5,geography=theGeo,
                     table.number=c("B27010"),
                     key=acs_key)
    ins = data.frame(name = as.character(geography(test)$NAME), 
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
                     insurance_total = as.numeric(estimate(test[,"B27010_001"])),
                     noinsurance = as.numeric(estimate(test[,"B27010_017"])) +
                                   as.numeric(estimate(test[,"B27010_033"])) +
                                   as.numeric(estimate(test[,"B27010_050"])) +
                                   as.numeric(estimate(test[,"B27010_066"])))
    
    #hhs
    test = acs.fetch(endyear=years[j],span=5,geography=theGeo,
                     table.number=c("B11001"),
                     key=acs_key)
    hhs = data.frame(name = as.character(geography(test)$NAME), 
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
                     family_hhs = as.numeric(estimate(test[,"B11001_002"])),
                     nonfamily_hhs = as.numeric(estimate(test[,"B11001_007"])))
    
    #age2
    test = acs.fetch(endyear=years[j],span=5,geography=theGeo,
                     table.number=c("B01001"),
                     key=acs_key)
    age2 = data.frame(name = as.character(geography(test)$NAME), 
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
                                     as.numeric(estimate(test[,"B01001_049"])))
    
    #inc1
    test = acs.fetch(endyear=years[j],span=5,geography=theGeo,
                     table.number=c("B19013"),
                     key=acs_key)
    inc1 = data.frame(name = as.character(geography(test)$NAME), 
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
                      median_hh_income = as.numeric(estimate(test[,"B19013_001"])))
    
    #inc2
    test = acs.fetch(endyear=years[j],span=5,geography=theGeo,
                     table.number=c("B19301"),
                     key=acs_key)
    inc2 = data.frame(name = as.character(geography(test)$NAME), 
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
                      per_capita_income = as.numeric(estimate(test[,"B19301_001"])))
    
    #public assistance
    test = acs.fetch(endyear=years[j],span=5,geography=theGeo,
                     table.number=c("B19057"),
                     key=acs_key)
    pass = data.frame(name = as.character(geography(test)$NAME), 
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
                      total_assistance_hhs = as.numeric(estimate(test[,"B19057_001"])),
                      public_assistance_hhs = as.numeric(estimate(test[,"B19057_002"])))
    
    #labor
    test = acs.fetch(endyear=years[j],span=5,geography=theGeo,
                     table.number=c("B23025"),
                     key=acs_key)
    lab = data.frame(name = as.character(geography(test)$NAME), 
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
                     labor_population = as.numeric(estimate(test[,"B23025_001"])),
                     labor_force = as.numeric(estimate(test[,"B23025_002"])),
                     unemployed = as.numeric(estimate(test[,"B23025_005"])))
    
    #marital status
    test = acs.fetch(endyear=years[j],span=5,geography=theGeo,
                     table.number=c("B12001"),
                     key=acs_key)
    mar = data.frame(name = as.character(geography(test)$NAME), 
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
    
    #veterans
    test = acs.fetch(endyear=years[j],span=5,geography=theGeo,
                     table.number=c("B21002"),
                     key=acs_key)
    vet = data.frame(name = as.character(geography(test)$NAME), 
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
                     veterans = as.numeric(estimate(test[,"B21002_001"])))
    
    #education
    test = acs.fetch(endyear=years[j],span=5,geography=theGeo,
                     table.number=c("B15003"),
                     key=acs_key)
    edu = data.frame(name = as.character(geography(test)$NAME), 
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
                     education_total = as.numeric(estimate(test[,"B15003_001"])),
                     highschool = as.numeric(estimate(test[,"B15003_017"])) +
                                  as.numeric(estimate(test[,"B15003_018"])),
                     somecollege = as.numeric(estimate(test[,"B15003_019"])) +
                                   as.numeric(estimate(test[,"B15003_020"])) +
                                   as.numeric(estimate(test[,"B15003_021"])),
                     bachelors = as.numeric(estimate(test[,"B15003_022"])),
                     masters = as.numeric(estimate(test[,"B15003_023"])) +
                               as.numeric(estimate(test[,"B15003_024"])),
                     phd = as.numeric(estimate(test[,"B15003_025"])))
    
    #housing1
    test = acs.fetch(endyear=years[j],span=5,geography=theGeo,
                     table.number=c("B25008"),
                     key=acs_key)
    hou1 = data.frame(name = as.character(geography(test)$NAME), 
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
                     owner_occupied = as.numeric(estimate(test[,"B25008_002"])),
                     renter_occupied = as.numeric(estimate(test[,"B25008_003"])))
    
    #housing2
    test = acs.fetch(endyear=years[j],span=5,geography=theGeo,
                     table.number=c("B25077"),
                     key=acs_key)
    hou2 = data.frame(name = as.character(geography(test)$NAME), 
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
                      median_house_value = as.numeric(estimate(test[,"B25077_001"])))
    
    #housing3
    test = acs.fetch(endyear=years[j],span=5,geography=theGeo,
                     table.number=c("B25058"),
                     key=acs_key)
    hou3 = data.frame(name = as.character(geography(test)$NAME), 
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
                      median_rent = as.numeric(estimate(test[,"B25058_001"])))
    
    holder = rbind(age %>% select(id),
                   age2 %>% select(id),
                   edu %>% select(id),
                   hhs %>% select(id),
                   hisp %>% select(id),
                   hou1 %>% select(id),
                   hou2 %>% select(id),
                   hou3 %>% select(id),
                   inc1 %>% select(id),
                   inc2 %>% select(id),
                   ins %>% select(id),
                   lab %>% select(id),
                   mar %>% select(id),
                   pass %>% select(id),
                   pop %>% select(id),
                   pov %>% select(id),
                   vet %>% select(id)) %>% distinct() %>%
              left_join(.,pop,by="id") %>%
              left_join(.,hisp,by=c("id","name","state","state_name","county","tract","blockgroup","year")) %>%
              left_join(.,age,by=c("id","name","state","state_name","county","tract","blockgroup","year")) %>%
              left_join(.,age2,by=c("id","name","state","state_name","county","tract","blockgroup","year")) %>%
              left_join(.,edu,by=c("id","name","state","state_name","county","tract","blockgroup","year")) %>%
              left_join(.,hhs,by=c("id","name","state","state_name","county","tract","blockgroup","year")) %>%
              left_join(.,hou1,by=c("id","name","state","state_name","county","tract","blockgroup","year")) %>%
              left_join(.,hou2,by=c("id","name","state","state_name","county","tract","blockgroup","year")) %>%
              left_join(.,hou3,by=c("id","name","state","state_name","county","tract","blockgroup","year")) %>%
              left_join(.,inc1,by=c("id","name","state","state_name","county","tract","blockgroup","year")) %>%
              left_join(.,inc2,by=c("id","name","state","state_name","county","tract","blockgroup","year")) %>%
              left_join(.,ins,by=c("id","name","state","state_name","county","tract","blockgroup","year")) %>%
              left_join(.,lab,by=c("id","name","state","state_name","county","tract","blockgroup","year")) %>%
              left_join(.,mar,by=c("id","name","state","state_name","county","tract","blockgroup","year")) %>%
              left_join(.,pass,by=c("id","name","state","state_name","county","tract","blockgroup","year")) %>%
              left_join(.,pov,by=c("id","name","state","state_name","county","tract","blockgroup","year")) %>%
              left_join(.,vet,by=c("id","name","state","state_name","county","tract","blockgroup","year")) %>%
              mutate(votingage_male = male_under35+male_35to50+male_50to65+male_over65,
                     votingage_female = female_under35+female_35to50+female_50to65+female_over65,
                     total_votingage = male_under35+male_35to50+male_50to65+male_over65+
                                       female_under35+female_35to50+female_50to65+female_over65,
                     nohighschool = education_total - highschool - somecollege - bachelors - masters - phd)
    
    if(j==1){
      stack = holder
    } else {
      stack = rbind(stack,holder)
    }
    
    #Clean up
    rm(list = setdiff(ls(),c("acs_key","theStates","years","i","j","stack","theGeo")))
    gc()
  }
  
  #Write out the data to folder
  write.csv(stack,paste("Data/Census/",theStates[i],".csv",sep=""),row.names=FALSE)
  rm(list = setdiff(ls(),c("acs_key","theStates","years","i","j")))
  gc()
}

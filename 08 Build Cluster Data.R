#2022 Election Project
#Author: Scott Onestak
#8 Build Cluster Data

#Read in packages
library(dplyr)
library(tidyr)
library(fredr)
library(flexmix)
library(ggplot2)

options(scipen = 100)

#FRED API
fredr_set_key("1e7ed343d3ccb82af108e43174bf8f1f")

#Read in raw data
census_raw = read.csv("Data/Census_by_CDs/census_raw.csv",header = T,stringsAsFactors = F) %>%
                mutate(state = substr(CD,1,2))
two_party = read.csv("Data/Precinct_Two_Party_Results/2016_2020_results.csv",header = T,stringsAsFactors = F)

#2008 CDs are the same as the 2010... duplicate with changed CD year and append
census_raw = rbind(census_raw,
                   census_raw %>% filter(cd_year == 2010) %>% mutate(cd_year = 2008))

#Set looper variables
cdYears = c(2008,2010,2012,2014,2016,2018,2020,2022)
theStates = sort(unlist(census_raw %>% mutate(states = substr(CD,1,2) ) %>% select(states) %>% distinct()))

#Get total population data and calculate US growth
fred_pop = fredr(series_id = "POPTHM",
                 observation_start = as.Date("2001-01-01",format="%Y-%m-%d"),
                 frequency = "a") %>% select(date,value) %>% 
  mutate(year = as.numeric(substr(date,1,4)),
         value = value * 1000) %>%
  rename(pop = value) %>%
  select(year,pop) %>%
  filter(!is.na(pop))
fred_pop$pop = as.numeric(fred_pop$pop)

#Get total population growth over time
fred_clusters = as.numeric((unlist(fred_pop %>% filter(year == 2020) %>% select(pop)) - 
                            unlist(fred_pop %>% filter(year == 2013) %>% select(pop))) / 
                            unlist(fred_pop %>% filter(year == 2013) %>% select(pop)))

fred_pop = fred_pop %>% inner_join(.,fred_pop %>% mutate(year = year+1) %>% rename(pop_prev = pop),by=c("year")) %>%
  mutate(us_pop_growth = (pop - pop_prev) / pop_prev) %>%
  select(-pop_prev)

#Pulling Housing Price Index and Rent Price Index to Adjust Values to 2020
fred_housing = fredr(series_id = "USSTHPI",
                     observation_start = as.Date("2013-01-01",format="%Y-%m-%d"),
                     frequency = "a") %>% 
                  filter(!is.na(value)) %>%
                  mutate(year = as.numeric(substr(date,1,4))) %>%
                  select(year,value)
fred_housing_2020 = as.numeric(unlist(fred_housing %>% filter(year == 2020) %>% select(value)))
fred_housing$housing_index = fred_housing_2020 / fred_housing$value
fred_housing = fred_housing %>% select(year,housing_index)

fred_rent = fredr(series_id = "CUUR0000SEHA",
                  observation_start = as.Date("2013-01-01",format="%Y-%m-%d"),
                  frequency = "a") %>% 
              filter(!is.na(value)) %>%
              mutate(year = as.numeric(substr(date,1,4))) %>%
              select(year,value)
fred_rent_2020 = as.numeric(unlist(fred_rent %>% filter(year == 2020) %>% select(value)))
fred_rent$rent_index = fred_rent_2020 / fred_rent$value
fred_rent = fred_rent %>% select(year,rent_index)

#Create data for clusters
theClusters = NA
for(i in seq(from=1,to=length(cdYears),by=1)){
  theList = unlist(unique(census_raw %>% filter(cd_year == cdYears[i]) %>% select(CD)))
  for(j in seq(from=1,to=length(theList),by=1)){
    curr = census_raw %>% filter(cd_year == cdYears[i] & CD == theList[j]) %>% 
              left_join(.,fred_housing,by="year") %>%
              left_join(.,fred_rent,by="year") %>%
              mutate(median_house_value_adj = median_house_value * housing_index,
                     median_rent_adj = median_rent * rent_index,
                     density_score = highlydense_prct * 5 + dense_prct * 4 + spread_prct * 3 +
                                        highlyspread_prct * 2 + sparse_prct * 1)
    
    #build variables for CD
    rel_pop_growth = round(as.numeric((
                                 unlist(curr %>% filter(year == 2020) %>% select(total_population)) - 
                                 unlist(curr %>% filter(year == 2013) %>% select(total_population))) / 
                                 unlist(curr %>% filter(year == 2013) %>% select(total_population))) - fred_clusters,4)
    
    #average
    white = mean(curr$white_prct)
    black = mean(curr$black_prct)
    asian = mean(curr$asian_prct)
    hispanic = mean(curr$hispanic_prct)
    male = mean(curr$male_prct)
    female = mean(curr$female_prct)
    single = mean(curr$single_prct)
    married = mean(curr$married_prct)
    widowed = mean(curr$widowed_prct)
    divorced = mean(curr$divorced_prct)
    under35 = mean(curr$under35_prct)
    b35to50 = mean(curr$b35to50_prct)
    b50to65 = mean(curr$b50to65_prct)
    over65 = mean(curr$over65_prct)
    veterans = mean(curr$veterans_prct)
    lfpr = mean(curr$lfpr)
    unemployment = mean(curr$unemployment_rate)
    poverty = mean(curr$poverty_rate)
    assistance = mean(curr$assistance_rate)
    noninsured = mean(curr$noninsured_rate)
    nohighschool = mean(curr$nohighschool_prct)
    highschool = mean(curr$highschool_prct)
    somecollege = mean(curr$somecollege_prct)
    bachelors = mean(curr$bachelors_prct)
    masters = mean(curr$masters_prct)
    phd = mean(curr$phd_prct)
    college_educated = mean(curr$college_educated_prct)
    highly_educated = mean(curr$highly_educated_prct)
    owner_occupied = mean(curr$owner_occupied_prct)
    renter_occupied = mean(curr$renter_occupied_prct)
    family_hh = mean(curr$family_hh_prct)
    nonfamily_hh = mean(curr$nonfamily_hh_prct)
    median_house_value = mean(curr$median_house_value_adj)
    median_rent = mean(curr$median_rent_adj)
    median_age = mean(curr$median_age)
    median_hh_income = mean(curr$median_hh_income)
    per_capita_income = mean(curr$per_capita_income)
    ppsm = mean(curr$ppsm)
    highlydense = mean(curr$highlydense_prct)
    dense = mean(curr$dense_prct)
    spread = mean(curr$spread_prct)
    highlyspread = mean(curr$highlyspread_prct)
    sparse = mean(curr$sparse_prct)
    density_score = mean(curr$density_score)
    
    #change
    white_chg = round(as.numeric(
                  unlist(curr %>% filter(year == 2020) %>% select(white_prct)) - 
                  unlist(curr %>% filter(year == 2013) %>% select(white_prct))),4)
    black_chg = round(as.numeric(
                  unlist(curr %>% filter(year == 2020) %>% select(black_prct)) - 
                  unlist(curr %>% filter(year == 2013) %>% select(black_prct))),4)
    asian_chg = round(as.numeric(
                  unlist(curr %>% filter(year == 2020) %>% select(asian_prct)) - 
                  unlist(curr %>% filter(year == 2013) %>% select(asian_prct))),4)
    hispanic_chg = round(as.numeric(
                  unlist(curr %>% filter(year == 2020) %>% select(hispanic_prct)) - 
                  unlist(curr %>% filter(year == 2013) %>% select(hispanic_prct))),4)
    single_chg = round(as.numeric(
                  unlist(curr %>% filter(year == 2020) %>% select(single_prct)) - 
                  unlist(curr %>% filter(year == 2013) %>% select(single_prct))),4)
    married_chg = round(as.numeric(
                  unlist(curr %>% filter(year == 2020) %>% select(married_prct)) - 
                  unlist(curr %>% filter(year == 2013) %>% select(married_prct))),4)
    college_educated_chg = round(as.numeric(
                  unlist(curr %>% filter(year == 2020) %>% select(college_educated_prct)) - 
                  unlist(curr %>% filter(year == 2013) %>% select(college_educated_prct))),4)
    highly_educated_chg = round(as.numeric(
                  unlist(curr %>% filter(year == 2020) %>% select(highly_educated_prct)) - 
                  unlist(curr %>% filter(year == 2013) %>% select(highly_educated_prct))),4)
    under35_chg = round(as.numeric(
                  unlist(curr %>% filter(year == 2020) %>% select(under35_prct)) - 
                  unlist(curr %>% filter(year == 2013) %>% select(under35_prct))),4)
    b35to50_chg = round(as.numeric(
                  unlist(curr %>% filter(year == 2020) %>% select(b35to50_prct)) - 
                  unlist(curr %>% filter(year == 2013) %>% select(b35to50_prct))),4)
    b50to65_chg = round(as.numeric(
                  unlist(curr %>% filter(year == 2020) %>% select(b50to65_prct)) - 
                  unlist(curr %>% filter(year == 2013) %>% select(b50to65_prct))),4)
    over65_chg = round(as.numeric(
                  unlist(curr %>% filter(year == 2020) %>% select(over65_prct)) - 
                  unlist(curr %>% filter(year == 2013) %>% select(over65_prct))),4)
    highlydense_chg = round(as.numeric(
                  unlist(curr %>% filter(year == 2020) %>% select(highlydense_prct)) - 
                  unlist(curr %>% filter(year == 2013) %>% select(highlydense_prct))),4)
    dense_chg = round(as.numeric(
                  unlist(curr %>% filter(year == 2020) %>% select(dense_prct)) - 
                  unlist(curr %>% filter(year == 2013) %>% select(dense_prct))),4)
    spread_chg = round(as.numeric(
                  unlist(curr %>% filter(year == 2020) %>% select(spread_prct)) - 
                  unlist(curr %>% filter(year == 2013) %>% select(spread_prct))),4)
    highlyspread_chg = round(as.numeric(
                  unlist(curr %>% filter(year == 2020) %>% select(highlyspread_prct)) - 
                  unlist(curr %>% filter(year == 2013) %>% select(highlyspread_prct))),4)
    sparse_chg = round(as.numeric(
                  unlist(curr %>% filter(year == 2020) %>% select(sparse_prct)) - 
                  unlist(curr %>% filter(year == 2013) %>% select(sparse_prct))),4)
    density_score_chg = round(as.numeric(
                  unlist(curr %>% filter(year == 2020) %>% select(density_score)) - 
                  unlist(curr %>% filter(year == 2013) %>% select(density_score))),4)
    
    #create dataset
    holder = as.data.frame(cbind(cdYears[i],theList[j],rel_pop_growth,
                                 white,black,asian,hispanic,white_chg,black_chg,asian_chg,hispanic_chg,
                                 male,female,single,married,widowed,divorced,single_chg,married_chg,
                                 under35,b35to50,b50to65,over65,under35_chg,b35to50_chg,b50to65_chg,over65_chg,
                                 veterans,lfpr,unemployment,poverty,assistance,noninsured,
                                 nohighschool,highschool,somecollege,bachelors,masters,phd,college_educated,highly_educated,
                                 college_educated_chg,highly_educated_chg,
                                 owner_occupied,renter_occupied,family_hh,nonfamily_hh,
                                 median_house_value,median_rent,median_age,median_hh_income,per_capita_income,
                                 ppsm,highlydense,dense,spread,highlyspread,sparse,density_score,
                                 highlydense_chg,dense_chg,spread_chg,highlyspread_chg,sparse_chg,density_score_chg))
    
    if(i == 1 & j == 1){
      theClusters = holder
    } else {
      theClusters = rbind(theClusters,holder)
    }
  }
}
colnames(theClusters)[1:2] = c("cd_year","CD")
theClusters$cd_year = as.numeric(theClusters$cd_year)

#Remove duplicates... for example, if 2012 - 2020 are the same CD map, then just keep the first one for custering
theClusters_nodups = theClusters %>% 
                        group_by(across(c(-cd_year))) %>% 
                        summarise(year = min(cd_year)) %>%
                        ungroup() %>%
                        left_join(.,two_party %>% select(year,name,
                                                         DEM_PRCT_2016,DEM_PRCT_2020,
                                                         REP_PRCT_2016,REP_PRCT_2020,
                                                         LEAN_2016,LEAN_2020),
                                  by=c("year"="year","CD"="name")) %>%
                        mutate(id = paste(CD,year,sep="-")) %>%
                        select(-year) %>%
                        arrange(id)

#Create mapping for the clusters to use later
theClusters_mapping = theClusters %>% 
                        inner_join(.,theClusters_nodups %>% select(-c(DEM_PRCT_2016,DEM_PRCT_2020,
                                                                      REP_PRCT_2016,REP_PRCT_2020,
                                                                      LEAN_2016,LEAN_2020)),
                                   by=c(setdiff(colnames(theClusters),c("cd_year")))) %>%
                        select(cd_year,CD,id) %>%
                        arrange(id)

#Write out files for clustering
write.csv(theClusters_nodups,"Data/Cluster/buildCluster.csv",row.names=FALSE)
write.csv(theClusters_mapping,"Data/Cluster/theClusterMapping.csv",row.names=FALSE)


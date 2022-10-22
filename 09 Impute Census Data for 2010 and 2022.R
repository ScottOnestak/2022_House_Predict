#2022 Election Project
#Author: Scott Onestak
#9 Impute Census Data for 2010 and 2022

#libraries
library(dplyr)

#read in census data by CDs
census_by_CDs = read.csv("Data/Census_by_CDs/census_raw.csv")

theYears = sort(unique(census_by_CDs$cd_year))

found = FALSE
theStack = NA

for(i in seq(from=1,to=length(theYears),by=1)){
  cat(theYears[i],"\n",sep="")
  if(theYears[i] %in% c(2010,2012,2022)){
    
    theCDs = sort(unlist(census_by_CDs %>% filter(cd_year == theYears[i]) %>% select(CD) %>% distinct()))
    for(j in seq(from=1,to=length(theCDs),by=1)){
      #create subset as temp
      temp = census_by_CDs %>% filter(cd_year == theYears[i] & CD == theCDs[j])
      
      #create test dataset to use if R^2 above 85%
      test = as.data.frame(theYears[i])
      colnames(test) = c("year")
      
      #race
      test_lm = lm(white_prct ~ year,data=temp)
      if(var(temp$white_prct)==0){
        if(theYears[i] %in% c(2010,2012)){
          white_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(white_prct)))
        } else {
          white_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(white_prct)))
        }
      } else {
        if(as.numeric(summary(test_lm)[8])>.85){
          white_prct = min(max(predict(test_lm,test),0),1)
        } else {
          if(theYears[i] %in% c(2010,2012)){
            white_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(white_prct)))
          } else {
            white_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(white_prct)))
          }
        }
      }
      
      
      test_lm = lm(black_prct ~ year,data=temp)
      if(var(temp$black_prct)==0){
        if(theYears[i] %in% c(2010,2012)){
          black_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(black_prct)))
        } else {
          black_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(black_prct)))
        }
      } else {
        if(as.numeric(summary(test_lm)[8])>.85){
          black_prct = min(max(predict(test_lm,test),0),1)
        } else {
          if(theYears[i] %in% c(2010,2012)){
            black_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(black_prct)))
          } else {
            black_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(black_prct)))
          }
        }
      }
      
      
      test_lm = lm(asian_prct ~ year,data=temp)
      if(var(temp$asian_prct)==0){
        if(theYears[i] %in% c(2010,2012)){
          asian_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(asian_prct)))
        } else {
          asian_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(asian_prct)))
        }
      } else {
        if(as.numeric(summary(test_lm)[8])>.85){
          asian_prct = min(max(predict(test_lm,test),0),1)
        } else {
          if(theYears[i] %in% c(2010,2012)){
            asian_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(asian_prct)))
          } else {
            asian_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(asian_prct)))
          }
        }
      }
      
      
      test_lm = lm(hispanic_prct ~ year,data=temp)
      if(var(temp$hispanic_prct)==0){
        if(theYears[i] %in% c(2010,2012)){
          hispanic_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(hispanic_prct)))
        } else {
          hispanic_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(hispanic_prct)))
        }
      } else {
        if(as.numeric(summary(test_lm)[8])>.85){
          hispanic_prct = min(max(predict(test_lm,test),0),1)
        } else {
          if(theYears[i] %in% c(2010,2012)){
            hispanic_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(hispanic_prct)))
          } else {
            hispanic_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(hispanic_prct)))
          }
        }
      }
      
      
      #age
      test_lm = lm(under35_prct ~ year,data=temp)
      if(var(temp$under35_prct)==0){
        if(theYears[i] %in% c(2010,2012)){
          under35_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(under35_prct)))
        } else {
          under35_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(under35_prct)))
        }
      } else {
        if(as.numeric(summary(test_lm)[8])>.85){
          under35_prct = min(max(predict(test_lm,test),0),1)
        } else {
          if(theYears[i] %in% c(2010,2012)){
            under35_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(under35_prct)))
          } else {
            under35_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(under35_prct)))
          }
        }
      }
      
      
      test_lm = lm(b35to50_prct ~ year,data=temp)
      if(var(temp$b35to50_prct)==0){
        if(theYears[i] %in% c(2010,2012)){
          b35to50_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(b35to50_prct)))
        } else {
          b35to50_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(b35to50_prct)))
        }
      } else {
        if(as.numeric(summary(test_lm)[8])>.85){
          b35to50_prct = min(max(predict(test_lm,test),0),1)
        } else {
          if(theYears[i] %in% c(2010,2012)){
            b35to50_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(b35to50_prct)))
          } else {
            b35to50_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(b35to50_prct)))
          }
        }
      }
      
      
      test_lm = lm(b50to65_prct ~ year,data=temp)
      if(var(temp$b50to65_prct)==0){
        if(theYears[i] %in% c(2010,2012)){
          b50to65_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(b50to65_prct)))
        } else {
          b50to65_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(b50to65_prct)))
        }
      } else {
        if(as.numeric(summary(test_lm)[8])>.85){
          b50to65_prct = min(max(predict(test_lm,test),0),1)
        } else {
          if(theYears[i] %in% c(2010,2012)){
            b50to65_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(b50to65_prct)))
          } else {
            b50to65_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(b50to65_prct)))
          }
        }
      }
      
      
      test_lm = lm(over65_prct ~ year,data=temp)
      if(var(temp$over65_prct)==0){
        if(theYears[i] %in% c(2010,2012)){
          over65_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(over65_prct)))
        } else {
          over65_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(over65_prct)))
        }
      } else {
        if(as.numeric(summary(test_lm)[8])>.85){
          over65_prct = min(max(predict(test_lm,test),0),1)
        } else {
          if(theYears[i] %in% c(2010,2012)){
            over65_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(over65_prct)))
          } else {
            over65_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(over65_prct)))
          }
        }
      }
      
      
      test_lm = lm(median_age ~ year,data=temp)
      if(var(temp$median_age)==0){
        if(theYears[i] %in% c(2010,2012)){
          median_age = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(median_age)))
        } else {
          median_age = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(median_age)))
        }
      } else {
        if(as.numeric(summary(test_lm)[8])>.85){
          median_age = min(max(predict(test_lm,test),0),1)
        } else {
          if(theYears[i] %in% c(2010,2012)){
            median_age = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(median_age)))
          } else {
            median_age = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(median_age)))
          }
        }
      }
      
      
      #veteran
      test_lm = lm(veterans_prct ~ year,data=temp)
      if(var(temp$veterans_prct)==0){
        if(theYears[i] %in% c(2010,2012)){
          veterans_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(veterans_prct)))
        } else {
          veterans_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(veterans_prct)))
        }
      } else {
        if(as.numeric(summary(test_lm)[8])>.85){
          veterans_prct = min(max(predict(test_lm,test),0),1)
        } else {
          if(theYears[i] %in% c(2010,2012)){
            veterans_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(veterans_prct)))
          } else {
            veterans_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(veterans_prct)))
          }
        }
      }
      
      
      #economic
      test_lm = lm(lfpr ~ year,data=temp)
      if(var(temp$lfpr)==0){
        if(theYears[i] %in% c(2010,2012)){
          lfpr = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(lfpr)))
        } else {
          lfpr = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(lfpr)))
        }
      } else {
        if(as.numeric(summary(test_lm)[8])>.85){
          lfpr = min(max(predict(test_lm,test),0),1)
        } else {
          if(theYears[i] %in% c(2010,2012)){
            lfpr = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(lfpr)))
          } else {
            lfpr = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(lfpr)))
          }
        }
      }
      
      
      test_lm = lm(unemployment_rate ~ year,data=temp)
      if(var(temp$unemployment_rate)==0){
        if(theYears[i] %in% c(2010,2012)){
          unemployment_rate = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(unemployment_rate)))
        } else {
          unemployment_rate = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(unemployment_rate)))
        }
      } else {
        if(as.numeric(summary(test_lm)[8])>.85){
          unemployment_rate = min(max(predict(test_lm,test),0),1)
        } else {
          if(theYears[i] %in% c(2010,2012)){
            unemployment_rate = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(unemployment_rate)))
          } else {
            unemployment_rate = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(unemployment_rate)))
          }
        }
      }
      
      
      test_lm = lm(poverty_rate ~ year,data=temp)
      if(var(temp$poverty_rate)==0){
        if(theYears[i] %in% c(2010,2012)){
          poverty_rate = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(poverty_rate)))
        } else {
          poverty_rate = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(poverty_rate)))
        }
      } else {
        if(as.numeric(summary(test_lm)[8])>.85){
          poverty_rate = min(max(predict(test_lm,test),0),1)
        } else {
          if(theYears[i] %in% c(2010,2012)){
            poverty_rate = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(poverty_rate)))
          } else {
            poverty_rate = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(poverty_rate)))
          }
        }
      }
      
      
      test_lm = lm(assistance_rate ~ year,data=temp)
      if(var(temp$assistance_rate)==0){
        if(theYears[i] %in% c(2010,2012)){
          assistance_rate = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(assistance_rate)))
        } else {
          assistance_rate = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(assistance_rate)))
        }
      } else {
        if(as.numeric(summary(test_lm)[8])>.85){
          assistance_rate = min(max(predict(test_lm,test),0),1)
        } else {
          if(theYears[i] %in% c(2010,2012)){
            assistance_rate = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(assistance_rate)))
          } else {
            assistance_rate = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(assistance_rate)))
          }
        }
      }
      
      
      #education
      test_lm = lm(college_educated_prct ~ year,data=temp)
      if(var(temp$college_educated_prct)==0){
        if(theYears[i] %in% c(2010,2012)){
          college_educated_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(college_educated_prct)))
        } else {
          college_educated_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(college_educated_prct)))
        }
      } else {
        if(as.numeric(summary(test_lm)[8])>.85){
          college_educated_prct = min(max(predict(test_lm,test),0),1)
        } else {
          if(theYears[i] %in% c(2010,2012)){
            college_educated_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(college_educated_prct)))
          } else {
            college_educated_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(college_educated_prct)))
          }
        }
      }
      
      
      #housing
      test_lm = lm(owner_occupied_prct ~ year,data=temp)
      if(var(temp$owner_occupied_prct)==0){
        if(theYears[i] %in% c(2010,2012)){
          owner_occupied_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(owner_occupied_prct)))
        } else {
          owner_occupied_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(owner_occupied_prct)))
        }
      } else {
        if(as.numeric(summary(test_lm)[8])>.85){
          owner_occupied_prct = min(max(predict(test_lm,test),0),1)
        } else {
          if(theYears[i] %in% c(2010,2012)){
            owner_occupied_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(owner_occupied_prct)))
          } else {
            owner_occupied_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(owner_occupied_prct)))
          }
        }
      }
      renter_occupied_prct = 1 - owner_occupied_prct
      
      test_lm = lm(family_hh_prct ~ year,data=temp)
      if(var(temp$family_hh_prct)==0){
        if(theYears[i] %in% c(2010,2012)){
          family_hh_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(family_hh_prct)))
        } else {
          family_hh_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(family_hh_prct)))
        }
      } else {
        if(as.numeric(summary(test_lm)[8])>.85){
          family_hh_prct = min(max(predict(test_lm,test),0),1)
        } else {
          if(theYears[i] %in% c(2010,2012)){
            family_hh_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(family_hh_prct)))
          } else {
            family_hh_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(family_hh_prct)))
          }
        }
      }
      nonfamily_hh_prct = 1 - family_hh_prct
      
      #ppsm
      test_lm = lm(ppsm ~ year,data=temp)
      if(var(temp$ppsm)==0){
        if(theYears[i] %in% c(2010,2012)){
          ppsm = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(ppsm)))
        } else {
          ppsm = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(ppsm)))
        }
      } else {
        if(as.numeric(summary(test_lm)[8])>.85){
          ppsm = min(max(predict(test_lm,test),0),1)
        } else {
          if(theYears[i] %in% c(2010,2012)){
            ppsm = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(ppsm)))
          } else {
            ppsm = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(ppsm)))
          }
        }
      }
      
      
      test_lm = lm(highlydense_prct ~ year,data=temp)
      if(var(temp$highlydense_prct)==0){
        if(theYears[i] %in% c(2010,2012)){
          highlydense_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(highlydense_prct)))
        } else {
          highlydense_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(highlydense_prct)))
        }
      } else {
        if(as.numeric(summary(test_lm)[8])>.85){
          highlydense_prct = min(max(predict(test_lm,test),0),1)
        } else {
          if(theYears[i] %in% c(2010,2012)){
            highlydense_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(highlydense_prct)))
          } else {
            highlydense_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(highlydense_prct)))
          }
        }
      }
      
      
      test_lm = lm(dense_prct ~ year,data=temp)
      if(var(temp$dense_prct)==0){
        if(theYears[i] %in% c(2010,2012)){
          dense_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(dense_prct)))
        } else {
          dense_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(dense_prct)))
        }
      } else {
        if(as.numeric(summary(test_lm)[8])>.85){
          dense_prct = min(max(predict(test_lm,test),0),1)
        } else {
          if(theYears[i] %in% c(2010,2012)){
            dense_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(dense_prct)))
          } else {
            dense_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(dense_prct)))
          }
        }
      }
      
      
      test_lm = lm(spread_prct ~ year,data=temp)
      if(var(temp$spread_prct)==0){
        if(theYears[i] %in% c(2010,2012)){
          spread_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(spread_prct)))
        } else {
          spread_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(spread_prct)))
        }
      } else {
        if(as.numeric(summary(test_lm)[8])>.85){
          spread_prct = min(max(predict(test_lm,test),0),1)
        } else {
          if(theYears[i] %in% c(2010,2012)){
            spread_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(spread_prct)))
          } else {
            spread_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(spread_prct)))
          }
        }
      }
      
      
      test_lm = lm(highlyspread_prct ~ year,data=temp)
      if(var(temp$highlyspread_prct)==0){
        if(theYears[i] %in% c(2010,2012)){
          highlyspread_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(highlyspread_prct)))
        } else {
          highlyspread_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(highlyspread_prct)))
        }
      } else {
        if(as.numeric(summary(test_lm)[8])>.85){
          highlyspread_prct = min(max(predict(test_lm,test),0),1)
        } else {
          if(theYears[i] %in% c(2010,2012)){
            highlyspread_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(highlyspread_prct)))
          } else {
            highlyspread_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(highlyspread_prct)))
          }
        }
      }
      
      
      test_lm = lm(sparse_prct ~ year,data=temp)
      if(var(temp$sparse_prct)==0){
        if(theYears[i] %in% c(2010,2012)){
          sparse_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(sparse_prct)))
        } else {
          sparse_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(sparse_prct)))
        }
      } else {
        if(as.numeric(summary(test_lm)[8])>.85){
          sparse_prct = min(max(predict(test_lm,test),0),1)
        } else {
          if(theYears[i] %in% c(2010,2012)){
            sparse_prct = as.numeric(unlist(temp %>% filter(year == min(temp$year)) %>% select(sparse_prct)))
          } else {
            sparse_prct = as.numeric(unlist(temp %>% filter(year == max(temp$year)) %>% select(sparse_prct)))
          }
        }
      }
      
      
      #combine the variables
      holder = as.data.frame(cbind(theYears[i],theCDs[j],white_prct,black_prct,asian_prct,hispanic_prct,
                                   under35_prct,b35to50_prct,b50to65_prct,over65_prct,median_age,veterans_prct,
                                   lfpr,unemployment_rate,poverty_rate,assistance_rate,college_educated_prct,
                                   owner_occupied_prct,renter_occupied_prct,family_hh_prct,nonfamily_hh_prct,
                                   ppsm,highlydense_prct,dense_prct,spread_prct,highlyspread_prct,sparse_prct))
      colnames(holder)[1:2] = c("year","District")
      
      #stack results
      if(found==FALSE){
        theStack = holder
        found = TRUE
      } else {
        theStack = rbind(theStack,holder)
      }
    }
  } else {
    holder = census_by_CDs %>% filter(cd_year == theYears[i] & year == theYears[i]) %>%
                               rename("District"="CD") %>%
                               select(year,District,white_prct,black_prct,asian_prct,hispanic_prct,
                                      under35_prct,b35to50_prct,b50to65_prct,over65_prct,median_age,veterans_prct,
                                      lfpr,unemployment_rate,poverty_rate,assistance_rate,college_educated_prct,
                                      owner_occupied_prct,renter_occupied_prct,family_hh_prct,nonfamily_hh_prct,
                                      ppsm,highlydense_prct,dense_prct,spread_prct,highlyspread_prct,sparse_prct)
    
    #stack results
    if(found==FALSE){
      theStack = holder
      found = TRUE
    } else {
      theStack = rbind(theStack,holder)
    }
  }
}

#do a check to ensure every year has 435 CDs
check = theStack %>% group_by(year) %>% summarise(count=n())

#write out file to build dataset
write.csv(theStack,"Data/Census_by_CDs/census_imputed_for_build.csv",row.names=FALSE)






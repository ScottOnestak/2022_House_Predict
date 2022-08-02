#Determine Urbanicity Cutoffs

library(dplyr)
library(stringr)
library(sf)

area_2020 = st_sf(st_read("Data/Block Group Shapefiles/2020/cb_2020_us_bg_500k.shp")) %>%
              mutate(id = as.numeric(GEOID),
                     LAND = ALAND / 2589988) #Convert square meters to square miles

census = read.csv(paste("Data/Census/PA.csv",sep=""),header = T,stringsAsFactors = F) %>% filter(year == 2020)

allegheny = census %>% filter(county == 3)
lawrence = census %>% filter(county == 73)
western_pa = census %>% filter(county %in% c(3,7,19,73,85,125))

allegheny_map = area_2020 %>% inner_join(.,allegheny,by="id") %>% mutate(ppsm = total_population / LAND) %>%
                  mutate(class = ifelse(ppsm > 2500,"Highly Dense",
                                        ifelse(ppsm > 1000,"Dense",
                                               ifelse(ppsm > 500,"Spread",
                                                      ifelse(ppsm > 100, "Highly Spread","Sparse")))))
lawrence_map = area_2020 %>% inner_join(.,lawrence,by="id") %>% mutate(ppsm = total_population / LAND) %>%
                mutate(class = ifelse(ppsm > 2500,"Highly Dense",
                                      ifelse(ppsm > 1000,"Dense",
                                             ifelse(ppsm > 500,"Spread",
                                                    ifelse(ppsm > 100, "Highly Spread","Sparse")))))
western_pa_map = area_2020 %>% inner_join(.,western_pa,by="id") %>% mutate(ppsm = total_population / LAND) %>%
            mutate(class = ifelse(ppsm > 2500,"Highly Dense",
                                  ifelse(ppsm > 1000,"Dense",
                                         ifelse(ppsm > 500,"Spread",
                                                ifelse(ppsm > 100, "Highly Spread","Sparse")))))

plot(allegheny_map['ppsm'])
plot(allegheny_map['class'])

allegheny_map = allegheny_map %>% group_by(tract) %>% summarise(total_population = sum(total_population,na.rm=T))

plot(lawrence_map['ppsm'])
plot(lawrence_map['class'])

plot(western_pa_map['class'])
#Scrape 2010 Polls from NYT Website

#libraries
library(dplyr)
library(stringr)
library(sf)
library(httr)
library(xml2)
library(selectr)
library(rvest)

#read in files necessary
state_to_fips = read.csv("Data/State_to_FIPS.csv",header = T,stringsAsFactors = F) %>%
  mutate(STATEFP = formatC(as.numeric(FIPS),width=2,format="d",flag=0)) %>%
  select(State,STATEFP) %>%
  filter(!State %in% c('AS','GU','MP','PR','VI','DC'))

theCDs = st_sf(st_read("Data/Previous Congressional District Shapefiles/CD_2008.shp")) %>% 
            st_drop_geometry() %>%
            rename(STATEFP = STATEFP10,
                   CD = CD111FP,
                   GEOID = GEOID10,
                   NAME = NAMELSAD10,
                   ALAND = ALAND10,
                   AWATER = AWATER10) %>%
            select(STATEFP,CD,GEOID,NAME) %>%
            mutate(CD = ifelse(CD=="00","01",CD)) %>%
            filter(as.numeric(STATEFP) <= 56 & as.numeric(STATEFP) != 11) %>%
            left_join(.,state_to_fips,by="STATEFP") %>%
            mutate(State = ifelse(is.na(State) & STATEFP == "08","CO",State)) %>%
            group_by(State) %>%
            summarise(count = n()) %>%
            ungroup()
          
states = read.csv("Data/State_to_FIPS.csv",header = T,stringsAsFactors = F) %>% 
            filter(!State %in% c("AS","GU","MP","PR","VI")) %>%
            mutate(names = str_replace_all(tolower(Name)," ","-")) %>%
            select(State,names)

theCDs = theCDs %>% left_join(.,states,by="State")

#Scrape polls
started = FALSE
found = NA
for(i in seq(from=1,to=dim(theCDs)[1],by=1)){
  theState = unlist(theCDs[i,"names"])
  cds = unlist(theCDs[i,"count"])
  
  for(j in seq(from=1,to=cds,by=1)){
    cat(i,theState,j,"\n",sep=" ")
    theURL = paste("https://www.nytimes.com/elections/2010/forecasts/house/",theState,"/",j,".html",sep="")
    temp = GET(theURL)
    
    #if website status good
    if(temp$status_code == 200){
      holder = read_html(temp) %>% html_table(fill=TRUE)
      
      #find the right table... identified with "POLL" column
      for(k in seq(from=1,to=length(holder),by=1)){
        holder2 = as.data.frame(holder %>% .[k])
        theColNames = toupper(colnames(holder2))
        
        if(any(theColNames == "POLL")){
          
          #update column names so tables will stack
          for(z in seq(from=1,to=length(theColNames),by=1)){
            if(str_detect(theColNames[z],"DEMOCRAT")){
              theColNames[z] = "DEMOCRAT"
            }
            
            if(str_detect(theColNames[z],"REPUBLICAN")){
              theColNames[z] = "REPUBLICAN"
            }
          }
          
          #update column names and create table for stacking
          colnames(holder2) = theColNames
          holder2 = holder2 %>% select(DATE,POLL,SAMPLE.SIZE,DEMOCRAT,REPUBLICAN) %>%
                      mutate(State = as.character(theCDs[i,"State"]),
                             District = j) %>%
                      select(State,District,DATE,POLL,SAMPLE.SIZE,DEMOCRAT,REPUBLICAN)
          
          if(any(colnames(holder2) == "DATE") & 
             any(colnames(holder2) == "POLL") &
             any(colnames(holder2) == "SAMPLE.SIZE") &
             any(colnames(holder2) == "DEMOCRAT") &
             any(colnames(holder2) == "REPUBLICAN")){
            if(started == FALSE){
              found = holder2
              started = TRUE
            } else {
              found = rbind(found,holder2)
            }
          }
          
        }
      }
    }
    
    #Sleep to not overload website
    Sys.sleep(4)
  }
}

#write out polls
write.csv(found,"Data/Polls/House Polls/Scraped_2010_Polls/2010_polls.csv",row.names = F)



#Aggregate Election Results

library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(readxl)
library(fuzzyjoin)

years = c(2004,2006,2008,2010,2012,2014,2016,2018)

found = FALSE
votes_data = NA
party_data = NA

for(i in seq(from=1,to=length(years),by=1)){
  if(years[i] == 2004){
    theData = read_excel(paste("Data/Election Results/FEC/",years[i],".xls",sep=""),sheet = paste(years[i]," US HOUSE & SENATE RESULTS",sep=""),col_types = "text") %>% 
                filter(!DISTRICT %in% c("H","S") & !`STATE ABBREVIATION` %in% c("","AS","DC","GU","MP","PR","VI"))
  } else if(years[i] == 2006) {
    theData = read_excel(paste("Data/Election Results/FEC/",years[i],".xls",sep=""),sheet = paste(years[i]," US House & Senate Results",sep=""),col_types = "text") %>% 
                filter(!str_detect(DISTRICT,"H") & !str_detect(DISTRICT,"S") & !`STATE ABBREVIATION` %in% c("","AS","DC","GU","MP","PR","VI"))
  } else if(years[i] == 2008) {
    theData = read_excel(paste("Data/Election Results/FEC/",years[i],".xls",sep=""),sheet = paste(years[i]," House and Senate Results",sep=""),col_types = "text") %>% 
                filter(!str_detect(DISTRICT,"H") & !str_detect(DISTRICT,"S") & !`STATE ABBREVIATION` %in% c("","AS","DC","GU","MP","PR","VI"))
  } else if(years[i] == 2010) {
    theData = read_excel(paste("Data/Election Results/FEC/",years[i],".xls",sep=""),sheet = paste(years[i]," US House & Senate Results",sep=""),col_types = "text") %>% 
                filter(!str_detect(DISTRICT,"H") & !str_detect(DISTRICT,"S") & !`STATE ABBREVIATION` %in% c("","AS","DC","GU","MP","PR","VI"))
  } else if(years[i] == 2012) {
    theData = read_excel(paste("Data/Election Results/FEC/",years[i],".xls",sep=""),sheet = paste(years[i]," US House & Senate Results",sep=""),col_types = "text") %>% 
                filter(!D %in% c("H","S") & !`STATE ABBREVIATION` %in% c("","AS","DC","GU","MP","PR","VI"))
  } else if(years[i] == 2014) {
    theData = read_excel(paste("Data/Election Results/FEC/",years[i],".xls",sep=""),sheet = "2014 US House Results by State",col_types = "text") %>%
                filter(!D %in% c("H","S") & !`STATE ABBREVIATION` %in% c("","AS","DC","GU","MP","PR","VI"))
  } else if(years[i] == 2016) {
    theData = read_excel(paste("Data/Election Results/FEC/",years[i],".xlsx",sep=""),sheet = paste(years[i], " US House Results by State",sep=""),col_types = "text") %>%
                filter(!D %in% c("H","S") & !`STATE ABBREVIATION` %in% c("","AS","DC","GU","MP","PR","VI"))
  } else {
    theData = read_excel(paste("Data/Election Results/FEC/",years[i],".xlsx",sep=""),sheet = paste(years[i], " US House Results by State",sep=""),col_types = "text") %>%
                filter(!DISTRICT %in% c("H","S") & !`STATE ABBREVIATION` %in% c("","AS","DC","GU","MP","PR","VI"))
  }
  
  #No runoffs in 2010, 2018... fill in with NAs
  if(years[i]==2010 | years[i]==2018){
    theData$`GE RUNOFF` = NA
  }
  
  #Rename to District
  if(years[i]>=2012 & years[i]<=2016){
    theData = theData %>% rename("DISTRICT"="D")
  }
  
  #Trim WS off Party Name
  theData$PARTY = trimws(theData$PARTY)
  
  #Change party name for Peter Welch in 2008, 2016
  if(years[i] %in% c(2008,2016)){
    theData$PARTY = ifelse(theData$PARTY == "D/R","D",theData$PARTY)
  }
  
  #Rename some districts in the 2010 file that had both a general and special
  if(years[i]==2010){
    theData$DISTRICT = ifelse(theData$DISTRICT == "03 - FULL TERM","03",
                              ifelse(theData$DISTRICT == "29 - FULL TERM","29",theData$DISTRICT))
    theData = theData %>% filter(!str_detect(DISTRICT,"UNEXPIRED"))
  }
  
  #Rename some districts in the 2012 file that had both a general and special
  if(years[i]==2012){
    theData$DISTRICT = ifelse(theData$DISTRICT == "01 - FULL TERM","01",
                              ifelse(theData$DISTRICT == "04 - FULL TERM","04",
                                     ifelse(theData$DISTRICT == "10 - FULL TERM","10",
                                            ifelse(theData$DISTRICT == "11 - FULL TERM","11",theData$DISTRICT))))
    theData = theData %>% filter(!str_detect(DISTRICT,"UNEXPIRED"))
  }
  
  #Rename some districts in the 2014 file that had both a general and special
  if(years[i]==2014){
    theData$PARTY = ifelse(theData$PARTY == "R ","R",theData$PARTY)
    theData$`TOTAL VOTES` = ifelse(theData$`TOTAL VOTES`=="Total District Votes (Full Term):","District Votes:",theData$`TOTAL VOTES`)
    theData$DISTRICT = ifelse(theData$DISTRICT == "01 - FULL TERM","01",
                              ifelse(theData$DISTRICT == "07 - FULL TERM","07",
                                     ifelse(theData$DISTRICT == "12 - FULL TERM","12",theData$DISTRICT)))
    theData = theData %>% filter(!str_detect(DISTRICT,"UNEXPIRED"))
  }
  
  #Rename some districts in the 2016 file that had both a general and special
  if(years[i]==2016){
    theData$DISTRICT = ifelse(theData$DISTRICT == "01 - FULL TERM","01",
                              ifelse(theData$DISTRICT == "02 - FULL TERM","02",theData$DISTRICT))
    theData = theData %>% filter(!str_detect(DISTRICT,"UNEXPIRED"))
  }
  
  #Rename some districts in the 2018 file that had both a general and special
  if(years[i]==2018){
    theData$`TOTAL VOTES` = ifelse(theData$`TOTAL VOTES`=="Full Term District Votes:","District Votes:",theData$`TOTAL VOTES`)
    theData$DISTRICT = ifelse(theData$DISTRICT == "07 - FULL TERM","07",
                              ifelse(theData$DISTRICT == "13-FULL TERM","13",
                                     ifelse(theData$DISTRICT == "15 - FULL TERM","15",
                                            ifelse(theData$DISTRICT == "25 - FULL TERM","25",theData$DISTRICT))))
    theData = theData %>% filter(!str_detect(DISTRICT,"UNEXPIRED"))
  }
  
  #For years > 2006, different name fields
  if(years[i]>2006){
    
    
    if(years[i]==2008){
      theData = theData %>% rename("FEC ID" = "FEC ID#",
                                   "FIRST NAME" = "CANDIDATE NAME (First)",
                                   "LAST NAME" = "Candidate Name (Last)",
                                   "INCUMBENT INDICATOR" = "INCUMBENT INDICATOR (I)")
    } else if(years[i] <= 2010) {
      theData = theData %>% rename("FEC ID" = "FEC ID#",
                                   "FIRST NAME" = "CANDIDATE NAME (First)",
                                   "LAST NAME" = "CANDIDATE NAME (Last)",
                                   "INCUMBENT INDICATOR" = "INCUMBENT INDICATOR (I)")
    } else if(years[i] <= 2016) {
      theData = theData %>% rename("FEC ID" = "FEC ID#",
                                   "FIRST NAME" = "CANDIDATE NAME (First)",
                                   "LAST NAME" = "CANDIDATE NAME (Last)",
                                   "INCUMBENT INDICATOR" = "(I)",
                                   "GENERAL" = "GENERAL VOTES",
                                   "GE RUNOFF" = "GE RUNOFF ELECTION VOTES (LA)")
    } else {
      theData = theData %>% rename("FEC ID" = "FEC ID#",
                                   "FIRST NAME" = "CANDIDATE NAME (First)",
                                   "LAST NAME" = "CANDIDATE NAME (Last)",
                                   "INCUMBENT INDICATOR" = "(I)",
                                   "GENERAL" = "GENERAL VOTES")
    }
  }
  
  #Transform unopposed to 1 vote in the general field
  theData$GENERAL = ifelse(toupper(theData$GENERAL) == "UNOPPOSED",1,theData$GENERAL)
  
  #Convert fields to aggregate to numerics
  theData$GENERAL = as.numeric(theData$GENERAL)
  theData$`GE RUNOFF` = as.numeric(theData$`GE RUNOFF`)
  
  #Get total votes for the general and runoff elections
  total_votes = theData %>% filter(`TOTAL VOTES` == "District Votes:") %>% 
                            filter(!str_detect(DISTRICT,"\\*")) %>% 
                            select(c("STATE ABBREVIATION","DISTRICT","GENERAL","GE RUNOFF")) %>%
                            mutate(GENERAL = ifelse(is.na(GENERAL),1,GENERAL)) %>%
                            rename("STATE"="STATE ABBREVIATION") %>%
                            group_by(STATE,DISTRICT) %>%
                            filter(GENERAL == max(GENERAL,na.rm = F))
  
  #FL-15 is missing district votes for the 2012 file...append
  if(years[i]==2012){
    append = as.data.frame(cbind("FL","15",1,NA))
    colnames(append) = c("STATE","DISTRICT","GENERAL","GE RUNOFF")
    append$GENERAL = as.numeric(append$GENERAL)
    append$`GE RUNOFF` = as.numeric(append$`GE RUNOFF`)
    total_votes = rbind(total_votes,append)
  }
                            
  general_votes = total_votes %>% select(c("STATE","DISTRICT","GENERAL")) %>% rename("total_votes" = "GENERAL")
  runoff_votes = total_votes %>% select(c("STATE","DISTRICT","GE RUNOFF")) %>% rename("total_votes" = "GE RUNOFF") %>%
                    filter(!is.na(total_votes))
  
  #Get candidate votes
  cand_list = theData %>% filter(!is.na(GENERAL) & !str_detect(DISTRICT,"\\*")) %>% filter(PARTY %in% c("D","DEM","D*","DEM*","D/WF*","DFL","DNL","D/IP","DEM/PRO/WF","D/PRO/WF","D/PRO/WF/IP","D/IP/PRO/WF","D/PRO/WF*","D/WF","W(D)/D","W (D)/D","W(DEM)/DEM","W (DEM)/DEM","W(DEM)/DEM*","D(UND)",
                                                                                                        "R","R*","REP","REP*","R/CON","R/CON*","REP/CON/IP*","R/CON*/IP*","R/IP","R/TRP","REP/TRP","W(R)/R","W (R)/R","W(REP)/REP","W (REP)/REP","REP/IP*","R ","R/TRP","GOP")) %>%
                          select("STATE ABBREVIATION","DISTRICT","FEC ID","FIRST NAME","LAST NAME","INCUMBENT INDICATOR","PARTY") %>%
                          mutate(PARTY = ifelse(PARTY %in% c("R","R*","REP","REP*","R/CON","R/CON*","REP/CON/IP*","R/CON*/IP*","R/IP","R/TRP","REP/TRP","W(R)/R","W (R)/R","W(REP)/REP","W (REP)/REP","REP/IP*","R ","R/TRP","GOP"),"R","D"))
  votes_gen = theData %>% filter(!is.na(GENERAL) & !str_detect(DISTRICT,"\\*")) %>% 
                          select(c("STATE ABBREVIATION","DISTRICT","FEC ID","GENERAL")) %>%
                          inner_join(.,cand_list,by=c("STATE ABBREVIATION","DISTRICT","FEC ID")) %>%
                          rename("STATE"="STATE ABBREVIATION",
                                 "FEC_ID"="FEC ID",
                                 "FIRST_NAME"="FIRST NAME",
                                 "LAST_NAME"="LAST NAME",
                                 "INCUMBENT_INDICATOR"="INCUMBENT INDICATOR")%>%
                          group_by(STATE,DISTRICT,FEC_ID,FIRST_NAME,LAST_NAME,INCUMBENT_INDICATOR,PARTY) %>%
                          summarise(candidate_votes = sum(GENERAL)) %>%
                          ungroup()
  votes_run = theData %>% filter(!is.na(`GE RUNOFF`) & !str_detect(DISTRICT,"\\*")) %>% 
                          select(c("STATE ABBREVIATION","DISTRICT","FEC ID","GE RUNOFF")) %>%
                          inner_join(.,cand_list,by=c("STATE ABBREVIATION","DISTRICT","FEC ID")) %>%
                          rename("STATE"="STATE ABBREVIATION",
                                 "FEC_ID"="FEC ID",
                                 "FIRST_NAME"="FIRST NAME",
                                 "LAST_NAME"="LAST NAME",
                                 "INCUMBENT_INDICATOR"="INCUMBENT INDICATOR",
                                 "GE_RUNOFF"="GE RUNOFF")%>%
                          group_by(STATE,DISTRICT,FEC_ID,FIRST_NAME,LAST_NAME,INCUMBENT_INDICATOR,PARTY) %>%
                          summarise(candidate_votes = sum(GE_RUNOFF)) %>%
                          ungroup()
  
  #Join datasets
  general = votes_gen %>% left_join(.,general_votes,by=c("STATE","DISTRICT")) %>% mutate(runoff = FALSE)
  runoff = votes_run %>% left_join(.,runoff_votes,by=c("STATE","DISTRICT")) %>% mutate(runoff = TRUE)
  votes = rbind(general,runoff) %>% mutate(year = years[i],
                                           DISTRICT = ifelse(DISTRICT == "00","01",DISTRICT),
                                           INCUMBENT_IND = ifelse(is.na(INCUMBENT_INDICATOR),0,1)) %>%
                                    select(year,STATE,DISTRICT,FEC_ID,FIRST_NAME,LAST_NAME,INCUMBENT_IND,PARTY,candidate_votes,total_votes,runoff)
  
  #Check
  check = votes %>% group_by(STATE,DISTRICT) %>%
                    summarise(sum_votes = sum(candidate_votes,na.rm=T),
                              total_votes = mean(total_votes)) %>%
                    mutate(prct_of_votes = sum_votes/total_votes)
  
  #Build party indicators for later
  rep_ind = theData %>% filter(!is.na(GENERAL) & PARTY %in% c("R","R*","REP","REP*","R/CON","R/CON*","R/CON*/IP*","REP/CON/IP*","R/IP","R/TRP","REP/TRP","W(R)/R","W (REP)/REP","W(REP)/REP","REP/IP*","R ","GOP")) %>% select("STATE ABBREVIATION","DISTRICT") %>%
              rename("STATE"="STATE ABBREVIATION") %>% distinct() %>% mutate(REP_IND = 1)
  dem_ind = theData %>% filter(!is.na(GENERAL) & PARTY %in% c("D","DEM","D*","DEM*","DFL","DNL","D/IP","DEM/PRO/WF","D/PRO/WF","D/PRO/WF/IP","D/IP/PRO/WF","D/PRO/WF*","D/WF","D/WF*","W(D)/D","W (DEM)/DEM","W(DEM)/DEM","W(DEM)/DEM*","D(UND)")) %>% select("STATE ABBREVIATION","DISTRICT") %>%
              rename("STATE"="STATE ABBREVIATION") %>% distinct() %>% mutate(DEM_IND = 1)
  lbt_ind = theData %>% filter(!is.na(GENERAL) & PARTY %in% c("LBT","L","LIB","W(LBT)/LBT","W(LIB)/LIB","LIB/IP*","LIB/PG")) %>% select("STATE ABBREVIATION","DISTRICT") %>%
              rename("STATE"="STATE ABBREVIATION") %>% distinct() %>% mutate(LBT_IND = 1)
  grn_ind = theData %>% filter(!is.na(GENERAL) & PARTY %in% c("GRN","G","GRE","GRE*","W(GRE)/GRE")) %>% select("STATE ABBREVIATION","DISTRICT") %>%
              rename("STATE"="STATE ABBREVIATION") %>% distinct() %>% mutate(GRN_IND = 1)
  party_ind = general_votes %>% select("STATE","DISTRICT") %>%
                left_join(.,rep_ind,by=c("STATE","DISTRICT")) %>%
                left_join(.,dem_ind,by=c("STATE","DISTRICT")) %>%
                left_join(.,lbt_ind,by=c("STATE","DISTRICT")) %>%
                left_join(.,grn_ind,by=c("STATE","DISTRICT")) %>%
                mutate(REP_IND = ifelse(is.na(REP_IND),0,1),
                       DEM_IND = ifelse(is.na(DEM_IND),0,1),
                       LBT_IND = ifelse(is.na(LBT_IND),0,1),
                       GRN_IND = ifelse(is.na(GRN_IND),0,1),
                       DISTRICT = ifelse(DISTRICT == "00","01",DISTRICT),
                       year = years[i]) %>%
                select(year,STATE,DISTRICT,REP_IND,DEM_IND,LBT_IND,GRN_IND)
  
  # setdiff(sort(unique(theData$PARTY)),c("D","DEM","D*","DEM*","D/WF*","DFL","DNL","D/IP","DEM/PRO/WF","D/PRO/WF","D/PRO/WF/IP","D/IP/PRO/WF","D/PRO/WF*","D/WF","W(D)/D","W (D)/D","W(DEM)/DEM","W (DEM)/DEM","W(DEM)/DEM*","D(UND)"
  #                                       "R","R*","REP","REP*","R/CON","R/CON*/IP*","REP/CON/IP*","R/IP","R/TRP","W(R)/R","W (R)/R","W(REP)/REP","W (REP)/REP","REP/IP*","R ","R/TRP","REP/TRP","R/CON*","GOP"))
  
  #stack results
  if(dim(total_votes)[1] == 435 & dim(check)[1] == 435){
    if(found == FALSE){
      votes_data = votes
      party_data = party_ind
      found = TRUE
    } else {
      votes_data = rbind(votes_data,votes)
      party_data = rbind(party_data,party_ind)
    }
  } else {
    cat("ERROR: ",years[i],"\n",sep="")
  }
}


#Read in MIT data to check against
mit_data = read.csv("Data/Election Results/1976-2020-house.csv",header=T,stringsAsFactors=F)
tocheck = mit_data %>% filter(year >= 2004 & year <= 2018) %>% 
            mutate(party = ifelse(party %in% c("DEMOCRATIC-FARM-LABOR","DEMOCRATIC-FARMER-LABOR","DEMOCRATIC-NPL","DEMOCRATIC-NONPARTISAN LEAGUE"),"DEMOCRAT",party)) %>%
            filter(party %in% c("REPUBLICAN","DEMOCRAT")) %>%
            filter(stage == "GEN" & special == FALSE) %>%
            select(year,state_po,district,party) %>% 
            mutate(val = 1) %>%
            distinct() %>%
            spread(.,key=party,value=val,fill=0) %>%
            mutate(DISTRICT = ifelse(district==0,"01",str_pad(district,2,pad="0"))) %>%
            rename("STATE"="state_po") %>%
            select(year,STATE,DISTRICT,DEMOCRAT,REPUBLICAN) %>%
            full_join(.,party_data %>% select(-LBT_IND,-GRN_IND),by=c("year","STATE","DISTRICT")) %>%
            mutate(REP_DIFF=ifelse(REPUBLICAN==REP_IND,0,1),
                   DEM_DIFF=ifelse(DEMOCRAT==DEM_IND,0,1)) %>%
            filter(REP_DIFF != 0 | DEM_DIFF != 0)

#Good with the differences between the data
#2004: TX-18: TOM BAZAN is an Independent - not a Republican
#2016: AZ-03: JAIME VASQUEZ is a write-in candidate - not a Republican


#There is no FEC file for 2020 (that isn't a PDF)... take from FiveThirtyEight and join on FEC data
five_thirty_eight = read.csv("Data/Election Results/five_thirty_eight_election_results_house.csv",header=T,stringsAsFactors=F) %>%
                      filter(cycle == 2020) %>%
                      mutate(DISTRICT = str_pad(trimws(str_replace_all(toupper(office_seat_name),"DISTRICT","")),2,pad="0"),
                             votes = ifelse(is.na(votes),1,votes)) %>% #uncontested elections
                      filter(special == "false" & is.na(party)) %>%
                      select(state_abbrev,DISTRICT,candidate_name,ballot_party,votes,stage) %>%
                      rename("STATE"="state_abbrev",
                             "PARTY"="ballot_party")
fec_2020_data = read.csv("Data/Election Results/2020_FEC_Data.csv",header=T,stringsAsFactors=F) %>%
                      rowwise() %>%
                      mutate(DISTRICT = ifelse(district==0,"01",str_pad(district,2,pad="0")),
                             FIRST_NAME = str_split(name,",")[[1]][2],
                             LAST_NAME = str_split(name,",")[[1]][1],
                             INCUMBENT_IND = ifelse(incumbent_challenge=="I",1,0),
                             PARTY = ifelse(party=="REP","R",
                                            ifelse(party %in% c("DEM","DFL"),"D",
                                                   ifelse(name=="GALVIN, ALYSE","D",party)))) %>%
                      mutate(candidate_name = paste(FIRST_NAME,LAST_NAME,sep=" ")) %>%
                      rename("STATE"="state",
                             "FEC_ID"="candidate_id") %>%
                      select(STATE,DISTRICT,FIRST_NAME,LAST_NAME,candidate_name,PARTY,INCUMBENT_IND,FEC_ID)


#Get 2020 total votes
total_votes_gen_2020 = five_thirty_eight %>% filter(STATE != "PR" & stage %in% c("general","jungle primary")) %>% 
                          group_by(STATE,DISTRICT) %>%
                          summarise(total_votes=sum(votes)) %>%
                          ungroup()
total_votes_run_2020 = five_thirty_eight %>% filter(STATE != "PR" & stage %in% c("runoff")) %>% 
                        group_by(STATE,DISTRICT) %>%
                        summarise(total_votes=sum(votes)) %>%
                        ungroup()

#Get votes for D and R candidates in 2020
candidate_votes_2020 = five_thirty_eight %>% filter(STATE != "PR" & stage %in% c("general","jungle primary") & PARTY %in% c("DEM","REP")) %>% 
                          rename("candidate_votes"="votes") %>%
                          mutate(PARTY = ifelse(PARTY=="REP","R",
                                                ifelse(PARTY=="DEM","D",""))) %>%
                          select(-stage) %>%
                          left_join(.,total_votes_gen_2020,by=c("STATE","DISTRICT")) %>%
                          mutate(runoff = FALSE)
candidate_votes_run_2020 = five_thirty_eight %>% filter(STATE != "PR" & stage %in% c("runoff") & PARTY %in% c("DEM","REP")) %>% 
                            rename("candidate_votes"="votes") %>%
                            mutate(PARTY = ifelse(PARTY=="REP","R",
                                                  ifelse(PARTY=="DEM","D",""))) %>%
                            select(-stage) %>%
                            left_join(.,total_votes_run_2020,by=c("STATE","DISTRICT")) %>%
                            mutate(runoff = TRUE)
votes_2020 = rbind(candidate_votes_2020,candidate_votes_run_2020) %>% arrange(STATE,DISTRICT,runoff)

check_2020 = candidate_votes_2020 %>% group_by(STATE,DISTRICT) %>%
                summarise(sum_votes = sum(candidate_votes,na.rm=T),
                          total_votes = mean(total_votes)) %>%
                mutate(prct_of_votes = sum_votes/total_votes)

#Build party data file to append
party_2020 = five_thirty_eight %>% filter(STATE != "PR" & stage %in% c("general","jungle primary") & PARTY %in% c("DEM","REP","LIB","GRE")) %>%
              select(STATE,DISTRICT,PARTY) %>%
              distinct() %>%
              mutate(var=1) %>%
              spread(.,key=PARTY,value=var,fill=0) %>%
              rename("DEM_IND" = "DEM",
                     "REP_IND" = "REP",
                     "LBT_IND" = "LIB",
                     "GRN_IND" = "GRE") %>%
              mutate(year=2020)

#Try to join to FEC Data
append = votes_2020 %>% rowwise() %>%
          mutate(LAST_NAME = stri_reverse(str_split(stri_reverse(trimws(str_replace_all(toupper(candidate_name),"JR.","")))," ")[[1]][1])) %>% 
          left_join(.,fec_2020_data,by=c("STATE","DISTRICT","PARTY","LAST_NAME")) %>%
          filter(is.na(FEC_ID) | FEC_ID != "H0CA25253") %>% #Removing Christopher Smith matching to Christy Smith
          mutate(year=2020) %>%
          select(year,STATE,DISTRICT,FEC_ID,FIRST_NAME,LAST_NAME,INCUMBENT_IND,PARTY,candidate_votes,total_votes,runoff)

#There are around 30 that don't match... can look these up on the FEC website manually
#Append to file and then update from there

votes_data = rbind(votes_data,append)
party_data = rbind(party_data,party_2020)

write.csv(votes_data,"Data/Election Results/Cleaned Results/votes_raw.csv",row.names = F)
write.csv(party_data,"Data/Election Results/Cleaned Results/party_ind_data.csv",row.names = F)

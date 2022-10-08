#2022 Election Project
#Author: Scott Onestak
#Build 2022 US Shapefile

library(tidycensus)
library(tigris)
library(sf)
library(tidyverse)


#Get all state data to use for states with only one congressional district (i.e. no redistricting data)
states = states(cb = FALSE,year = 2020)


#AL
AL = st_sf(st_read("Data/2022 Congressional District Shapefiles/AL/AL.shp")) %>%
        mutate(Districts = ifelse(is.na(Districts),"7",Districts)) %>%
        mutate(state = "AL",
               district=as.numeric(Districts),
               name = paste("AL-",sprintf("%02d",as.numeric(Districts)),sep="")) %>%
        select(state,district,name,geometry) %>%
        st_transform(.,crs = 4326)
      
#AK
AK = states %>% filter(STUSPS == "AK") %>%
        mutate(state = "AK",
               district = 1,
               name = "AK-01") %>%
        select(state,district,name,geometry) %>%
        st_transform(.,crs = 4326)

#AZ
AZ = st_sf(st_read("Data/2022 Congressional District Shapefiles/AZ/AZ.shp")) %>%
  mutate(state = "AZ",
         district = as.numeric(District),
         name = paste("AZ-",sprintf("%02d",as.numeric(District)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#AR
AR = st_sf(st_read("Data/2022 Congressional District Shapefiles/AR/AR.shp")) %>%
  mutate(state = "AR",
         district = as.numeric(DISTRICT),
         name = paste("AR-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#CA
CA = st_sf(st_read("Data/2022 Congressional District Shapefiles/CA/CA.shp")) %>%
  mutate(state = "CA",
         district = as.numeric(DISTRICT),
         name = paste("CA-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#CO
CO = st_sf(st_read("Data/2022 Congressional District Shapefiles/CO/CO.shp")) %>%
  mutate(state="CO",
         district = as.numeric(District),
         name = paste("CO-",sprintf("%02d",as.numeric(District)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#CT
CT = st_sf(st_read("Data/2022 Congressional District Shapefiles/CT/CT.shp")) %>%
  
  mutate(state="CT",
         district = as.numeric(DISTRICT),
         name = paste("CT-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  filter(!is.na(district)) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#DE
DE = states %>% filter(STUSPS == "DE") %>%
  mutate(state = "DE",
         district = 1,
         name = "DE-01") %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#FL
FL = st_sf(st_read("Data/2022 Congressional District Shapefiles/FL/FL.shp")) %>%
  mutate(state="FL",
         district = as.numeric(DISTRICT),
         name = paste("FL-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#GA
GA = st_sf(st_read("Data/2022 Congressional District Shapefiles/GA/GA.shp")) %>%
  mutate(state="GA",
         district = as.numeric(DISTRICT),
         name = paste("GA-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#HI
HI = st_sf(st_read("Data/2022 Congressional District Shapefiles/HI/HI.shp")) %>%
  mutate(state="HI",
         district = as.numeric(DISTRICT),
         name = paste("HI-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#ID
ID = st_sf(st_read("Data/2022 Congressional District Shapefiles/ID/ID.shp")) %>%
  mutate(state="ID",
         district = as.numeric(DISTRICT),
         name = paste("ID-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#IL
IL = st_sf(st_read("Data/2022 Congressional District Shapefiles/IL/IL.shp")) %>%
  mutate(state="IL",
         district = as.numeric(DISTRICT),
         name = paste("IL-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#IN
IN = st_sf(st_read("Data/2022 Congressional District Shapefiles/IN/IN.shp")) %>%
  mutate(state="IN",
         district = as.numeric(DISTRICT),
         name = paste("IN-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#IA
IA = st_sf(st_read("Data/2022 Congressional District Shapefiles/IA/IA.shp")) %>%
  mutate(state="IA",
         district = as.numeric(DISTRICT),
         name = paste("IA-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#KS
KS = st_sf(st_read("Data/2022 Congressional District Shapefiles/KS/KS.shp")) %>%
  mutate(state="KS",
         district = as.numeric(DISTRICT),
         name = paste("KS-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#KY
KY = st_sf(st_read("Data/2022 Congressional District Shapefiles/KY/KY.shp")) %>%
  mutate(state="KY",
         district = as.numeric(DISTRICT),
         name = paste("KY-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#LA
LA = st_sf(st_read("Data/2022 Congressional District Shapefiles/LA/LA.shp")) %>%
  mutate(state="LA",
         district = as.numeric(DISTRICT_I),
         name = paste("LA-",sprintf("%02d",as.numeric(DISTRICT_I)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#ME
ME = st_sf(st_read("Data/2022 Congressional District Shapefiles/ME/ME.shp")) %>%
  mutate(state="ME",
         district = as.numeric(NAME),
         name = paste("ME-",sprintf("%02d",as.numeric(NAME)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#MD
MD = st_sf(st_read("Data/2022 Congressional District Shapefiles/MD/MD.shp")) %>%
  mutate(state="MD",
         district = as.numeric(DISTRICT),
         name = paste("MD-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#MA
MA = st_sf(st_read("Data/2022 Congressional District Shapefiles/MA/MA.shp")) %>%
  mutate(state="MA",
         district = as.numeric(DISTRICT),
         name = paste("MA-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#MI
MI = st_sf(st_read("Data/2022 Congressional District Shapefiles/MI/MI.shp")) %>%
  mutate(state="MI",
         district = as.numeric(DISTRICT),
         name = paste("MI-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#MN
MN = st_sf(st_read("Data/2022 Congressional District Shapefiles/MN/MN.shp")) %>%
  mutate(state="MN",
         district = as.numeric(DISTRICT),
         name = paste("MN-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#MS
MS = st_sf(st_read("Data/2022 Congressional District Shapefiles/MS/MS.shp")) %>%
  mutate(state="MS",
         district = as.numeric(ID),
         name = paste("MS-",sprintf("%02d",as.numeric(ID)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#MO
MO = st_sf(st_read("Data/2022 Congressional District Shapefiles/MO/MO.shp")) %>%
  mutate(state="MO",
         district = as.numeric(DISTRICT),
         name = paste("MO-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#MT
MT = st_sf(st_read("Data/2022 Congressional District Shapefiles/MT/MT.shp")) %>%
  mutate(state="MT",
         district = as.numeric(ID),
         name = paste("MT-",sprintf("%02d",as.numeric(ID)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#NE
NE = st_sf(st_read("Data/2022 Congressional District Shapefiles/NE/NE.shp")) %>%
  mutate(state="NE",
         district = as.numeric(DISTRICT),
         name = paste("NE-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#NV
NV = st_sf(st_read("Data/2022 Congressional District Shapefiles/NV/NV.shp")) %>%
  mutate(state="NV",
         district = as.numeric(DISTRICTNO),
         name = paste("NV-",sprintf("%02d",as.numeric(DISTRICTNO)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#NH
NH = st_sf(st_read("Data/2022 Congressional District Shapefiles/NH/NH.shp")) %>%
  mutate(state="NH",
         district = as.numeric(CONG2022),
         name = paste("NH-",sprintf("%02d",as.numeric(CONG2022)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#NJ
NJ = st_sf(st_read("Data/2022 Congressional District Shapefiles/NJ/NJ.shp")) %>%
  mutate(state="NJ",
         district = as.numeric(District),
         name = paste("NJ-",sprintf("%02d",as.numeric(District)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#NM
NM = st_sf(st_read("Data/2022 Congressional District Shapefiles/NM/NM.shp")) %>%
  mutate(state="NM",
         district = as.numeric(DISTRICT),
         name = paste("NM-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#NY
NY = st_sf(st_read("Data/2022 Congressional District Shapefiles/NY/NY.shp")) %>%
  mutate(state="NY",
         district = as.numeric(DISTRICT),
         name = paste("NY-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#NC
NC = st_sf(st_read("Data/2022 Congressional District Shapefiles/NC/NC.shp")) %>%
  mutate(state="NC",
         district = as.numeric(District_A),
         name = paste("NC-",sprintf("%02d",as.numeric(District_A)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#ND
ND = states %>% filter(STUSPS == "ND") %>%
  mutate(state = "ND",
         district = 1,
         name = "ND-01") %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#OH
OH = st_sf(st_read("Data/2022 Congressional District Shapefiles/OH/OH.shp")) %>%
  mutate(state="OH",
         district = as.numeric(CONG_DIST),
         name = paste("OH-",sprintf("%02d",as.numeric(CONG_DIST)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#OK
OK = st_sf(st_read("Data/2022 Congressional District Shapefiles/OK/OK.shp")) %>%
  mutate(state="OK",
         district = as.numeric(DISTRICT),
         name = paste("OK-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#OR
OR = st_sf(st_read("Data/2022 Congressional District Shapefiles/OR/OR.shp")) %>%
  mutate(state="OR",
         district = as.numeric(DISTRICT),
         name = paste("OR-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#PA
PA = st_sf(st_read("Data/2022 Congressional District Shapefiles/PA/PA.shp")) %>%
  mutate(state="PA",
         district = as.numeric(DISTRICT),
         name = paste("PA-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)


#RI
RI = st_sf(st_read("Data/2022 Congressional District Shapefiles/RI/RI.shp")) %>%
  mutate(state="RI",
         district = as.numeric(DISTRICT),
         name = paste("RI-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#SC
SC = st_sf(st_read("Data/2022 Congressional District Shapefiles/SC/SC.shp")) %>%
  mutate(state="SC",
         district = as.numeric(DISTRICT),
         name = paste("SC-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#SD
SD = states %>% filter(STUSPS == "SD") %>%
  mutate(state = "SD",
         district = 1,
         name = "SD-01") %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#TN
TN = st_sf(st_read("Data/2022 Congressional District Shapefiles/TN/TN.shp")) %>%
  mutate(state="TN",
         district = as.numeric(DISTRICT),
         name = paste("TN-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#TX
TX = st_sf(st_read("Data/2022 Congressional District Shapefiles/TX/TX.shp")) %>%
  mutate(state="TX",
         district = as.numeric(District),
         name = paste("TX-",sprintf("%02d",as.numeric(District)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#UT
UT = st_sf(st_read("Data/2022 Congressional District Shapefiles/UT/UT.shp")) %>%
  mutate(state="UT",
         district = as.numeric(DISTRICTNO),
         name = paste("UT-",sprintf("%02d",as.numeric(DISTRICTNO)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#VA
VA = st_sf(st_read("Data/2022 Congressional District Shapefiles/VA/VA.shp")) %>%
  mutate(state="VA",
         district = as.numeric(DISTRICT),
         name = paste("VA-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#VT
VT = states %>% filter(STUSPS == "VT") %>%
  mutate(state = "VT",
         district = 1,
         name = "VT-01") %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#WA
WA = st_sf(st_read("Data/2022 Congressional District Shapefiles/WA/WA.shp")) %>%
  mutate(state="WA",
         district = as.numeric(DISTRICT),
         name = paste("WA-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#WV
WV = st_sf(st_read("Data/2022 Congressional District Shapefiles/WV/WV.shp")) %>%
  mutate(state="WV",
         district = as.numeric(DISTRICT),
         name = paste("WV-",sprintf("%02d",as.numeric(DISTRICT)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#WI
WI = st_sf(st_read("Data/2022 Congressional District Shapefiles/WI/WI.shp")) %>%
  mutate(state="WI",
         district = as.numeric(NAME),
         name = paste("WI-",sprintf("%02d",as.numeric(NAME)),sep="")) %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#WY
WY = states %>% filter(STUSPS == "WY") %>%
  mutate(state = "WY",
         district = 1,
         name = "WY-01") %>%
  select(state,district,name,geometry) %>%
  st_transform(.,crs = 4326)

#Combine all maps
US = rbind(AK,AL,AR,AZ,CA,CO,CT,DE,FL,GA,HI,IA,ID,IL,IN,KS,KY,LA,MA,MD,ME,MI,MN,MO,
           MS,MT,NC,ND,NE,NJ,NM,NV,NH,NY,OH,OK,OR,PA,RI,SC,SD,TN,TX,UT,VA,VT,WA,WI,WV,WY)

US_plot = US %>% filter(!state %in% c("AK","HI"))
plot(US_plot["state"])

#Write out 2022 Map
st_write(US,"Data/2022 Congressional District Shapefiles/CD_2022.shp")


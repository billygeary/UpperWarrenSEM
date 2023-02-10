
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(leaflet)
library(measurements)

# Read in the data files
#C:/Users/bg47/

trap_data = read.csv("~/Dropbox/Billy/_Research/_PhD/07_UpperWarren_WA/data/from Adrian Feb19/Mammal data/FF_allSpeciesData_SurveySites_190225.csv")
extra_trap_data = read.csv("~/Dropbox/Billy/_Research/_PhD/07_UpperWarren_WA/data/from Adrian Feb19/Mammal data/FF_allSpeciesData_SurveySitesExtra_190227.csv", header=TRUE, check.names = FALSE)
daily_effort = read.csv("~/Dropbox/Billy/_Research/_PhD/07_UpperWarren_WA/data/from Adrian Feb19/Mammal data/DailyTrapEffort_10022020.csv")
session_effort = read.csv("~/Dropbox/Billy/_Research/_PhD/07_UpperWarren_WA/data/from Adrian Feb19/Mammal data/SessionTrapEffort_11022020.csv")

# Join the transect data from the original Upper Warren area (in Wayne et al. 2017), plus the extra data from nearby transects. 
all_trap_data = rbind(trap_data, extra_trap_data)


# Select the transects we want to look at (n = 12), chosen in conversation with Adrian Wayne and Marike Maxwell, Feb 2019
# Full data: Keninup, Warrup, Balban (Universal), Camelar, Boyicup, Moopinup, Chariup, Winnejup, Corbal
# Extra data: Tone, Poorginup & Dorgedup
transects = c("51KEN/01", "51POS/01", "51BOY/01", "51CHP/01", "51FMC/01","51COR/01",  "51WAR/01", "WEB", "WNB",
              "51FMC/02", "51CUP/01", "51DRG/01", "51MYA/01") 

my_trap_data = all_trap_data %>% 
  filter(VERNACULAR %in% c("Woylie", "Chuditch", "Quenda", "Common Brushtail Possum")) %>% # Select species of interes
  filter(CAPTURE_CODE != "NT") %>% # Remove the individuals that weren't tagged. Per discussion with Marika Maxwell
  filter(AGE == "A" | AGE == "S") %>% # Remove Juveniles and Infants from dataset. Per MM email 24/02/2020
  filter(SSI_LABEL %in% transects) %>% # Grab the transects we want to look at (as agreed with AW and MM)
  mutate(TRP_DATEv1 = as.Date(TRP_DATE, format = c("%d/%m/%y"))) %>%
  mutate(TRP_DATEv2 = as.Date(TRP_DATE, format = c("%d/%m/%Y"))) %>%# Convert date data to actual dates that we can filter by
  mutate(TRP_DATE = if_else(TRP_DATEv1 > as.Date("01/01/2019", format = "%d/%m/%Y"), as.Date(TRP_DATEv2), as.Date(TRP_DATEv1))) %>%
  #filter(TRP_DATE >"2000-01-01" & TRP_DATE < "2019-12-31") %>% #Select the dates that we want (i.e. 2000-2019)
  mutate(YEAR = year(TRP_DATE)) %>%
  mutate(TRANSECT = ifelse(SSI_LABEL == "51KEN/01", "Keninup",  # Create a new column with actual transect names, to combine WEB and WNB etc
                           ifelse(SSI_LABEL == "51POS/01", "Moopinup",
                                  ifelse(SSI_LABEL == "51BOY/01", "Boyicup",
                                         ifelse(SSI_LABEL == "51CHP/01", "Chariup",
                                                ifelse(SSI_LABEL == "51FMC/01", "Camelar",
                                                       ifelse(SSI_LABEL == "51COR/01", "Corbal",
                                                              ifelse(SSI_LABEL == "51WAR/01", "Warrup",
                                                                     ifelse(SSI_LABEL == "WEB", "Winnejup",
                                                                            ifelse(SSI_LABEL == "WNB", "Winnejup",
                                                                                   ifelse(SSI_LABEL == "51FMC/02", "Balban",
                                                                                          ifelse(SSI_LABEL == "51CUP/01", "Tone",
                                                                                                 ifelse(SSI_LABEL == "51DRG/01", "Dordagup",
                                                                                                        ifelse(SSI_LABEL == "51MYA/01", "Porginup", NA
                                                                                                        )))))))))))))) %>%
  mutate(TRP_NUMBER = as.numeric(gsub("\\D", "", SPT_LABEL))) %>% # Extract just the numbers for each trap on each transect.
  mutate(TRP_NUMBER = ifelse(SSI_LABEL == "WEB", as.numeric(TRP_NUMBER) + 33, TRP_NUMBER)) %>%
  droplevels() %>% mutate(SiteIDa = paste0(TRANSECT,"_",SPT_LABEL),
                          SiteID = paste0(TRANSECT,"_",TRP_NUMBER),
                          AnimalID = ANIMAL_NO)

transect.lookup = my_trap_data %>% dplyr::select(TRANSECT, SSI_LABEL) %>% distinct()

## Read in trap points

trap_points = read_sf("~/Dropbox/Billy/_Research/_PhD/07_UpperWarren_WA/data/from Adrian Feb19/Spatial data/Trapping/trap_points_compiled.shp")

sub.transect.lookup = read.csv("~/Dropbox/Billy/_Research/_PhD/07_UpperWarren_WA/data/from Adrian Feb19/Spatial data/Trapping/sub.transect.lookup.csv")
trap_points = full_join(trap_points, sub.transect.lookup, by = c("TRANSECT", "SiteID"))

trap_points$SiteID = gsub("Balban (Universal)","Balban",trap_points$SiteID, fixed=TRUE)
trap_points$TRANSECT = gsub("Balban (Universal)","Balban",trap_points$TRANSECT, fixed=TRUE)

# Buffer around each point by 500 metres
trap_buffer = trap_points %>% st_transform("+init=epsg:32750") %>% st_buffer(dist=300)
transect_buffer = trap_buffer %>% group_by(TRANSECT, SUB_TRANSECT_ID, HALF_TRANSECT_ID) %>% summarise()


# leaflet(st_transform(trap_points, "+proj=longlat +datum=WGS84")) %>%
# addProviderTiles(provider="Esri.WorldImagery") %>%
# addMarkers(label=trap_points$SUB_TRANSECT_ID)


## Trap effort

#################################################################################
##### Make table of Survey Effort for each cage trap site ####
#################################################################################
# Read in and join trapping effort data
# This table specifies the effort at each transect and in terms of trap nights (max 200 - 50 traps over four nights)

session_effort = session_effort %>% 
  mutate(Start = as.Date(Start, format = "%d/%m/%y"),
         Finish = as.Date(Finish, format = "%d/%m/%y"),
         TRANSECT = gsub("Balban (Universal)", "Balban", TRANSECT, fixed=TRUE)) %>%
  filter(Start >"2000-01-01" & Start < "2019-12-31") %>% #Select the dates that we want (i.e. 2000-2019)
  mutate(YEAR = year(Start)) %>%
  mutate(TRP_ROUND = paste0(months(Start),"_",YEAR)) %>% 
  filter(Include == "Y") %>% # Filter out things we don't want to include per MM discussions 
  dplyr::select(SSI_LABEL, TRANSECT, TRP_ROUND, YEAR, Start, Finish, Trap.Effort, Notes) %>% arrange(Start) 

session_effort = session_effort %>%
  group_by(TRANSECT, YEAR) %>% mutate(SessionYearID = row_number())

session.lookup = data.frame(session= unique(session_effort$TRP_ROUND))
session.lookup$SessionID = 1:length(session.lookup$session)
session.lookup$SessionYear = gsub("\\D","", session.lookup$session)

session_effort = left_join(session_effort, session.lookup, by=c("TRP_ROUND"="session"))

session_effort.round = session_effort %>% mutate(Occ1 = Start, Occ2=Start+1, Occ3 =Start+2, Occ4=Start+3)

session_effort_long = pivot_longer(session_effort.round, cols = c(Occ1, Occ2, Occ3, Occ4), names_to = "Occasion", values_to = "TRP_DATE")

session_effort_long = session_effort_long %>% group_by(SSI_LABEL, TRANSECT, YEAR, SessionYear) %>% mutate(OccYear = row_number()) %>% mutate(OccYear = paste0("Occ", OccYear)) %>% mutate(SessionID_Year = as.numeric(SessionYear)-1999)

#ggplot(session_effort_long) + geom_point(aes(x=Start, y=Trap.Effort)) + facet_wrap(~TRANSECT)

session_effort_wide = session_effort_long %>% pivot_wider(id_cols=c("SSI_LABEL", "TRANSECT", "SessionID",), 
                                                          names_from = "Occasion", values_from="Trap.Effort")

# session_effort_wide_year = session_effort_long %>% pivot_wider(id_cols=c("SSI_LABEL", "TRANSECT", "SessionID_Year", "SessionYear"), 
#                                                           names_from = "OccYear", values_from="Trap.Effort")



full.effort = expand.grid(unique(session_effort_long$TRANSECT), unique(session_effort_long$SessionID))
colnames(full.effort) = c("TRANSECT", "SessionID")

full.effort = left_join(full.effort, session_effort_wide, by=c("TRANSECT", "SessionID"))

full.effort$Occ1 = ifelse(full.effort$Occ1 > 0 & !is.na(full.effort$Occ1), 1, 0)
full.effort$Occ2 = ifelse(full.effort$Occ2 > 50 & !is.na(full.effort$Occ2), 1, 0)
full.effort$Occ3 = ifelse(full.effort$Occ3 > 100 & !is.na(full.effort$Occ3), 1, 0)
full.effort$Occ4 = ifelse(full.effort$Occ4 > 150 & !is.na(full.effort$Occ4), 1, 0)


contracted.sessions = session_effort_long %>% mutate(Month = month(Start)) %>% 
  mutate(EarlyLate = ifelse(Month<7, "Early", "Late")) %>% mutate(Year_Period = paste0(YEAR,"_",EarlyLate)) %>%
  group_by(Year_Period) %>% mutate(PeriodID = cur_group_id())


# Join effort data with trap data to create column of nights
#my_trap_data = left_join(my_trap_data, session_effort_long, by=c("TRANSECT", "TRP_DATE"))
my_trap_data = left_join(my_trap_data, session_effort_long, by=c("TRANSECT", "TRP_DATE"))

my_trap_data = my_trap_data %>%
  mutate(SurveyID = paste0(TRANSECT, "_",SessionID)) %>%
  mutate(Occ = gsub("Occ","",Occasion))

## Work out which sessions to remove so we can concatenate the data to half year 'Periods' 
## Need to do this so there is only one session per transect for each period
session.period = contracted.sessions %>% group_by(TRANSECT, SessionID, PeriodID, TRP_ROUND) %>% summarise()
period.table = xtabs(~session.period$TRANSECT + session.period$PeriodID)

# This says the following Periods have mulitple trapping sessions:
## Keninup, Period 17,18,19,23
## Warrup, Period 23
## Moopinup, Period 29 -> Remove session 69 due to April, rather than March
# Logic for choosing sessions to remove:
### - Where a trapping session did not occur in sync with other transects in that Period (i.e. similar month)

## Get capture data for each species
cap.rates = list()
for (i in unique(my_trap_data$VERNACULAR)){
  data = my_trap_data %>% filter(VERNACULAR==i) %>% 
    dplyr::select(SessionID, AnimalID, Occ, SiteID, TRANSECT, Start, Finish) %>%
    filter(!(TRANSECT == "Keninup" & SessionID %in% c(36,37,37,38,39,41,42,44,45,53))) %>% # Filter out extra Keninup Sessions
    filter(!(TRANSECT == "Warrup" & SessionID == 53)) %>% # Filter out extra Warrup Session
    filter(!(TRANSECT == 'Moopinup' & SessionID == 69)) %>%
    filter(AnimalID != "") %>%
    mutate(Occ = as.numeric(Occ)) %>%
    rename(Session="SessionID", ID="AnimalID", Occasion="Occ")
  
  counts = data %>% group_by(TRANSECT, SiteID, Start, Finish, Session) %>% 
    summarise(Caps = n(),
              UniqueCaptures = length(unique(ID)),
              Effort = max(Occasion))
  counts$Effort = counts$Finish - counts$Start + 1
  counts$CapRate = counts$Caps/as.numeric(counts$Effort)
  counts$Captures = counts$Caps
  counts$Species = i
  cap.rates[[i]] <- counts
}


# cap.rates = do.call('rbind', cap.rates)
# cap.rates$Captures = ifelse(is.na(cap.rates$Caps) & is.na(cap.rates$Effort), 0, cap.rates$Caps)
# cap.rates = cap.rates %>% pivot_wider(names_from = "Species", values_from = "Captures") %>%
#   dplyr::select(TRANSECT, SiteID, Start, Finish, Session, Effort, 9:12)

###################
#### BY PERIOD ####
###################
woylies = cap.rates$Woylie %>% ungroup() %>% dplyr::select(TRANSECT, SiteID, Session, UniqueCaptures) %>%
  rename("Woylie_Caps" = UniqueCaptures)
chuditch = cap.rates$Chuditch %>% ungroup() %>% dplyr::select(TRANSECT, SiteID, Session, UniqueCaptures) %>%
  rename("Chuditch_Caps" = UniqueCaptures)
quenda = cap.rates$Quenda %>% ungroup() %>% dplyr::select(TRANSECT, SiteID, Session, UniqueCaptures) %>%
  rename("Quenda_Caps" = UniqueCaptures)
koomal = cap.rates$`Common Brushtail Possum` %>% ungroup() %>% dplyr::select(TRANSECT, SiteID, Session, UniqueCaptures) %>%
  rename("Koomal_Caps" = UniqueCaptures)


# Set up data
trap.points.df = trap_points %>% dplyr::select(TRANSECT, SiteID) %>% st_drop_geometry()
trap.points.df = cbind(st_coordinates(trap_points), trap.points.df)
site.table = expand.grid(SiteID = trap.points.df$SiteID, Session = 1:90)
trap.points.df = full_join(trap.points.df, site.table)

#full.effort = left_join(full.effort, period.lookup)

transect.effort = full.effort %>% mutate(Effort = Occ1 + Occ2 + Occ3 + Occ4) %>%
  rename("Session" = SessionID) %>% dplyr::select(TRANSECT, Session, Effort)
trap.points.df = left_join(trap.points.df, transect.effort, by = c("TRANSECT", "Session"))
period.lookup = contracted.sessions %>% group_by(SessionID, PeriodID, YEAR, EarlyLate) %>% 
  summarise() %>% ungroup() 

# Woylies 
woylies= left_join(trap.points.df, woylies, by=c("TRANSECT", "SiteID", "Session"))
woylies = woylies %>% mutate(Woylie_Caps = ifelse(Effort > 0 & is.na(Woylie_Caps), 0, Woylie_Caps))
woylie.data = left_join(woylies, period.lookup, by=c("Session" = "SessionID"))
test = woylie.data %>% group_by(X, Y, TRANSECT, SiteID, PeriodID) %>% summarise(Effort = sum(Effort), Woylie_Caps = sum(Woylie_Caps, na.rm=TRUE))
woylie.data = test %>% mutate(Woylie_Caps = ifelse(Effort == 0, NA, Woylie_Caps)) %>%
  dplyr::select(!Effort) %>% pivot_wider(names_from = PeriodID, values_from=Woylie_Caps) %>% as.data.frame()

# Chudtich
chuditch= left_join(trap.points.df, chuditch, by=c("TRANSECT", "SiteID", "Session"))
chuditch = chuditch %>% mutate(Chuditch_Caps = ifelse(Effort > 0 & is.na(Chuditch_Caps), 0, Chuditch_Caps))
chuditch.data = left_join(chuditch, period.lookup, by=c("Session" = "SessionID"))
test = chuditch.data %>% group_by(X, Y, TRANSECT, SiteID, PeriodID) %>% summarise(Effort = sum(Effort), Chuditch_Caps = sum(Chuditch_Caps, na.rm=TRUE))
chuditch.data = test %>% mutate(Chuditch_Caps = ifelse(Effort == 0, NA, Chuditch_Caps)) %>%
  dplyr::select(!Effort) %>% pivot_wider(names_from = PeriodID, values_from=Chuditch_Caps) %>% as.data.frame()

# Quenda
quenda= left_join(trap.points.df, quenda, by=c("TRANSECT", "SiteID", "Session"))
quenda = quenda %>% mutate(Quenda_Caps = ifelse(Effort > 0 & is.na(Quenda_Caps), 0, Quenda_Caps))
quenda.data = left_join(quenda, period.lookup, by=c("Session" = "SessionID"))
test = quenda.data %>% group_by(X, Y, TRANSECT, SiteID, PeriodID) %>% summarise(Effort = sum(Effort), Quenda_Caps = sum(Quenda_Caps, na.rm=TRUE))
quenda.data = test %>% mutate(Quenda_Caps = ifelse(Effort == 0, NA, Quenda_Caps)) %>%
  dplyr::select(!Effort) %>% pivot_wider(names_from = PeriodID, values_from=Quenda_Caps) %>% as.data.frame()

# Koomal
koomal= left_join(trap.points.df, koomal, by=c("TRANSECT", "SiteID", "Session"))
koomal = koomal %>% mutate(Koomal_Caps = ifelse(Effort > 0 & is.na(Koomal_Caps), 0, Koomal_Caps))
koomal.data = left_join(koomal, period.lookup, by=c("Session" = "SessionID"))
test = koomal.data %>% group_by(X, Y, TRANSECT, SiteID, PeriodID) %>% summarise(Effort = sum(Effort), Koomal_Caps = sum(Koomal_Caps, na.rm=TRUE))
koomal.data = test %>% mutate(Koomal_Caps = ifelse(Effort == 0, NA, Koomal_Caps)) %>%
  dplyr::select(!Effort) %>% pivot_wider(names_from = PeriodID, values_from=Koomal_Caps) %>% as.data.frame()


# Effort
effort.info = left_join(trap.points.df, period.lookup, by=c("Session" = "SessionID"))
effort = effort.info %>%  group_by(X, Y, TRANSECT, SiteID, PeriodID) %>% summarise(Effort = sum(Effort)) %>% pivot_wider(names_from = PeriodID, values_from=Effort) %>% as.data.frame()

# Year
year = effort.info %>% group_by(X, Y, TRANSECT, SiteID, PeriodID, YEAR) %>% summarise() %>% pivot_wider(names_from = PeriodID, values_from=YEAR) %>% as.data.frame()

# Season
season = effort.info %>% group_by(X, Y, TRANSECT, SiteID, PeriodID, EarlyLate) %>% summarise() %>% pivot_wider(names_from = PeriodID, values_from=EarlyLate)  %>% as.data.frame()


capture.data = list(woylie = woylie.data, 
                    chuditch = chuditch.data, 
                    quenda = quenda.data, 
                    koomal = koomal.data, 
                    effort = effort, 
                    year = year, 
                    season = season)


#save(capture.data, file="Data_Clean/UW_uniquecapdata_inputforSSM_02072021.RData")


##################################
#### TRANSECT SCALE BY PERIOD ####
##################################

woylies = cap.rates$Woylie %>% ungroup() %>% dplyr::select(TRANSECT, SiteID, Session, UniqueCaptures) %>%
  rename("Woylie_Caps" = UniqueCaptures) %>%
  group_by(TRANSECT, Session) %>% summarise(Woylie_Caps = sum(Woylie_Caps))
chuditch = cap.rates$Chuditch %>% ungroup() %>% dplyr::select(TRANSECT, SiteID, Session, UniqueCaptures) %>%
  rename("Chuditch_Caps" = UniqueCaptures) %>%
  group_by(TRANSECT, Session) %>% summarise(Chuditch_Caps = sum(Chuditch_Caps))
quenda = cap.rates$Quenda %>% ungroup() %>% dplyr::select(TRANSECT, SiteID, Session, UniqueCaptures) %>%
  rename("Quenda_Caps" = UniqueCaptures) %>%
  group_by(TRANSECT, Session) %>% summarise(Quenda_Caps = sum(Quenda_Caps))
koomal = cap.rates$`Common Brushtail Possum` %>% ungroup() %>% dplyr::select(TRANSECT, SiteID, Session, UniqueCaptures) %>%
  rename("Koomal_Caps" = UniqueCaptures) %>%
  group_by(TRANSECT, Session) %>% summarise(Koomal_Caps = sum(Koomal_Caps))

# Set up data
trap.points.df = trap_points %>% dplyr::select(TRANSECT, SiteID) %>% st_drop_geometry()
trap.points.df = cbind(st_coordinates(trap_points), trap.points.df)
site.table = expand.grid(SiteID = trap.points.df$SiteID, Session = 1:90)
trap.points.df = full_join(trap.points.df, site.table)

transect.df = trap.points.df %>% group_by(TRANSECT, Session) %>% summarise()

#full.effort = left_join(full.effort, period.lookup)

transect.effort = session_effort_wide %>% mutate(TotalEffort = Occ1 + Occ2 + Occ3 + Occ4) %>%
  rename("Session" = SessionID) %>% ungroup() %>% dplyr::select(TRANSECT, Session, TotalEffort) %>%
  filter(!(TRANSECT == "Keninup" & Session %in% c(36,37,37,38,39,41,42,44,45,53))) %>% # Filter out extra Keninup Sessions
  filter(!(TRANSECT == "Warrup" & Session == 53)) %>% # Filter out extra Warrup Session
  filter(!(TRANSECT == 'Moopinup' & Session == 69))
transect.df = left_join(transect.df, transect.effort, by = c("TRANSECT", "Session"))
period.lookup = contracted.sessions %>% group_by(SessionID, PeriodID, YEAR, EarlyLate) %>% 
  summarise() %>% ungroup() 

# Woylies 
woylies= left_join(transect.df, woylies, by=c("TRANSECT", "Session"))
woylies = woylies %>% mutate(Woylie_Caps = ifelse(TotalEffort > 0 & is.na(Woylie_Caps), 0, Woylie_Caps))
woylie.data = left_join(woylies, period.lookup, by=c("Session" = "SessionID"))
test = woylie.data %>% group_by(TRANSECT, PeriodID) %>% summarise(Effort = sum(TotalEffort,na.rm=TRUE), Woylie_Caps = sum(Woylie_Caps, na.rm=TRUE))
woylie.data = test %>% mutate(Woylie_Caps = ifelse(Effort == 0, NA, Woylie_Caps)) %>%
  dplyr::select(!Effort) %>% pivot_wider(names_from = PeriodID, values_from=Woylie_Caps) %>% as.data.frame()

# Koomal
koomal= left_join(transect.df, koomal, by=c("TRANSECT", "Session"))
koomal = koomal %>% mutate(Koomal_Caps = ifelse(TotalEffort > 0 & is.na(Koomal_Caps), 0, Koomal_Caps))
koomal.data = left_join(koomal, period.lookup, by=c("Session" = "SessionID"))
test = koomal.data %>% group_by(TRANSECT, PeriodID) %>% summarise(Effort = sum(TotalEffort,na.rm=TRUE), Koomal_Caps = sum(Koomal_Caps, na.rm=TRUE))
koomal.data = test %>% mutate(Koomal_Caps = ifelse(Effort == 0, NA, Koomal_Caps)) %>%
  dplyr::select(!Effort) %>% pivot_wider(names_from = PeriodID, values_from=Koomal_Caps) %>% as.data.frame()

# Chuditch
chuditch= left_join(transect.df, chuditch, by=c("TRANSECT", "Session"))
chuditch = chuditch %>% mutate(Chuditch_Caps = ifelse(TotalEffort > 0 & is.na(Chuditch_Caps), 0, Chuditch_Caps))
chuditch.data = left_join(chuditch, period.lookup, by=c("Session" = "SessionID"))
test = chuditch.data %>% group_by(TRANSECT, PeriodID) %>% summarise(Effort = sum(TotalEffort,na.rm=TRUE), Chuditch_Caps = sum(Chuditch_Caps, na.rm=TRUE))
chuditch.data = test %>% mutate(Chuditch_Caps = ifelse(Effort == 0, NA, Chuditch_Caps)) %>%
  dplyr::select(!Effort) %>% pivot_wider(names_from = PeriodID, values_from=Chuditch_Caps) %>% as.data.frame()

# Quenda
quenda= left_join(transect.df, quenda, by=c("TRANSECT", "Session"))
quenda = quenda %>% mutate(Quenda_Caps = ifelse(TotalEffort > 0 & is.na(Quenda_Caps), 0, Quenda_Caps))
quenda.data = left_join(quenda, period.lookup, by=c("Session" = "SessionID"))
test = quenda.data %>% group_by(TRANSECT, PeriodID) %>% summarise(Effort = sum(TotalEffort,na.rm=TRUE), Quenda_Caps = sum(Quenda_Caps, na.rm=TRUE))
quenda.data = test %>% mutate(Quenda_Caps = ifelse(Effort == 0, NA, Quenda_Caps)) %>%
  dplyr::select(!Effort) %>% pivot_wider(names_from = PeriodID, values_from=Quenda_Caps) %>% as.data.frame()

# Effort
effort.info = left_join(transect.df, period.lookup, by=c("Session" = "SessionID"))
effort = effort.info %>%  group_by(TRANSECT, PeriodID) %>% summarise(Effort = sum(TotalEffort,na.rm=TRUE)) %>% pivot_wider(names_from = PeriodID, values_from=Effort) %>% as.data.frame()

# Year
year = effort.info %>% group_by(TRANSECT, PeriodID, YEAR) %>% summarise() %>% pivot_wider(names_from = PeriodID, values_from=YEAR) %>% as.data.frame()

# Season
season = effort.info %>% group_by(TRANSECT, PeriodID, EarlyLate) %>% summarise() %>% pivot_wider(names_from = PeriodID, values_from=EarlyLate)  %>% as.data.frame()

transect.capture.data = list(woylie = woylie.data, 
                             chuditch = chuditch.data, 
                             quenda = quenda.data, 
                             koomal = koomal.data, 
                             effort = effort, 
                             year = year, 
                             season = season)

#save(transect.capture.data, file="Data_Clean/UW_uniquecapdata_transect_inputforSSM_16072021.RData")

################################################
#### Create the initial covariate dataframe ####
# Use this to add abundance estimates and covariate data for SEM 
dates = session_effort_long %>% ungroup() %>% dplyr::select(TRANSECT, SessionID, Start, Trap.Effort) %>% distinct()
trap.points.data = left_join(trap.points.df, dates, by=c("TRANSECT", "Session"="SessionID"))
trap.points.data = left_join(trap.points.data, period.lookup[,1:2], by=c("Session"="SessionID"))
trap.points.NM = trap.points.data
trap.points.data$JulianDate = lubridate::yday(trap.points.data$Start)
#trap.points.data = trap.points.data %>% dplyr::select(-Effort,X,Y)
trap.points.data = left_join(trap.points.data, woylies, by=c("TRANSECT", "SiteID", "Session", "X","Y","Effort"))
trap.points.data = left_join(trap.points.data, chuditch, by=c("TRANSECT", "SiteID", "Session", "X","Y","Effort"))
trap.points.data = left_join(trap.points.data, quenda, by=c("TRANSECT", "SiteID", "Session", "X","Y","Effort"))
trap.points.data = left_join(trap.points.data, koomal, by=c("TRANSECT", "SiteID", "Session", "X","Y","Effort"))

#write.csv(trap.points.data, "Data_Processing/trap.data.csv")

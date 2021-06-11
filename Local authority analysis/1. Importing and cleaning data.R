##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

#When QCOVID was introduced: which LA-s had the highest net addition (total and adj. per capita) ? [of QCOVID source]
#% shielding by GOR, split by before/after QCOVID
#% shielding by IMD income quintile, split by before/after QCOVID

##############################################
################### SETUP ####################
##############################################

#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,stringr,ggplot2,DescTools,data.table,
               tibble,pbapply,here,tidyverse,readxl)

#Clean up the global environment
rm(list = ls())

#Set directory where inputs are saved
rawdatadir <- "M:/Analytics/Networked Data Lab/Shielding/"

#Git directory
gitdir <- dirname(rstudioapi::getSourceEditorContext()$path)

#######################################################################
################### LOOKUP: LOCAL AUTHORITY TO HIGHER #################
#######################################################################

LA_to_higher_geo <- fread(paste0(rawdatadir,"Other data/Lookups/Local_Authority_District_to_Region_(April_2019)_Lookup_in_England.csv"), header=TRUE, sep=",", check.names=T)

##############################################
################### POPULATION ###############
##############################################

pop_by_LA <- read_excel(paste0(rawdatadir,"Other data/Mid-year population estimates/LA/ukmidyearestimates20192019ladcodes.xlsx"),
                        sheet = "MYE2 - Persons",skip=4) %>%
  select(.,Code,Name,`All ages`) %>%
  dplyr::rename(.,LAD19CD=Code,LAD19NM=Name,pop19=`All ages`) %>%
  filter(.,!is.na(LAD19NM))

#Join with geo-indicators to match English local authorities to regions

pop_by_LA <- dplyr::left_join(pop_by_LA,select(LA_to_higher_geo,"LAD19CD","RGN19CD","RGN19NM"),
                              by=c("LAD19CD"="LAD19CD"))

rm(LA_to_higher_geo)

#Save

fwrite(pop_by_LA, file = here::here("Clean data","pop_by_LA.csv"), sep = ",")

##############################################
################### DEPRIVATION ##############
##############################################

#LA level

IMD2019_LA <- fread(paste0(rawdatadir,"Other data/IMD/Local authority/download1677836969253211150.csv"), header=TRUE, sep=",", check.names=T) %>%
  filter(.,DateCode==2019)

ranks <- select(IMD2019_LA,'Measurement','Value','Indices.of.Deprivation') %>%
  filter(.,Measurement=="Rank of average score"&Indices.of.Deprivation=="a. Index of Multiple Deprivation (IMD)") %>%
  select(.,'Value') %>% unique(.) %>% arrange(.,Value)

#LA level - rank of average score

IMD2019_LA_rank_IMD <- filter(IMD2019_LA,Measurement=='Rank of average score'&
                                Indices.of.Deprivation=='a. Index of Multiple Deprivation (IMD)') %>%
  dplyr::rename(.,rank_imd=Value) %>% select(.,FeatureCode,rank_imd) %>%
  mutate(.,decile_imd=cut(rank_imd, breaks=c(0,quantile(ranks$Value, probs = seq(0, 1, 1/10))[-1]), labels=1:10),
         quintile_imd=cut(rank_imd, breaks=c(0,quantile(ranks$Value, probs = seq(0, 1, 1/5))[-1]), labels=1:5))

IMD2019_LA_rank_Income <- filter(IMD2019_LA,Measurement=='Rank of average score'&
                                   Indices.of.Deprivation=='b. Income Deprivation Domain') %>%
  dplyr::rename(.,rank_income=Value) %>% select(.,FeatureCode,rank_income) %>%
  mutate(.,decile_income=cut(rank_income, breaks=c(0,quantile(ranks$Value, probs = seq(0, 1, 1/10))[-1]), labels=1:10),
         quintile_income=cut(rank_income, breaks=c(0,quantile(ranks$Value, probs = seq(0, 1, 1/5))[-1]), labels=1:5))

#Wide format

IMD2019_LA_wide <- left_join(IMD2019_LA_rank_IMD,IMD2019_LA_rank_Income,by="FeatureCode")

rm(IMD2019_LA,ranks,IMD2019_LA_rank_IMD,IMD2019_LA_rank_Income)

##########################################################################
################### NUMBER OF SHIELDERS BY LA: England ################### 
##########################################################################

#March 2021
SPL_by_LA_20March <- fread(paste0(rawdatadir,"SPL/England/March 2021/Coronavirus Shielded Patient List, England - Open Data with CMO DG - LA - 2021-03-20.csv"), header=TRUE, sep=",", check.names=T)
SPL_by_LA_12March <- fread(paste0(rawdatadir,"SPL/England/March 2021/Coronavirus Shielded Patient List, England - Open Data with CMO DG - LA - 2021-03-12.csv"), header=TRUE, sep=",", check.names=T)
SPL_by_LA_06March <- fread(paste0(rawdatadir,"SPL/England/March 2021/Coronavirus Shielded Patient List, England - Open Data with CMO DG - LA - 2021-03-06.csv"), header=TRUE, sep=",", check.names=T) %>%
  dplyr::rename(.,Patient.Count=`Patient.Count..suppressed.`)

#February 2021
SPL_by_LA_26Feb <- fread(paste0(rawdatadir,"SPL/England/February 2021/Coronavirus Shielded Patient List, England - Open Data with CMO DG - LA - 2021-02-26.csv"), header=TRUE, sep=",", check.names=T) %>%
  dplyr::rename(.,Patient.Count=`Patient.Count..suppressed.`)
SPL_by_LA_22Feb <- fread(paste0(rawdatadir,"SPL/England/February 2021/Coronavirus Shielded Patient List, England - Open Data with CMO DG - LA - 2021-02-22.csv"), header=TRUE, sep=",", check.names=T) %>%
  dplyr::rename(.,Patient.Count=`Patient.Count..suppressed.`)
  #15 Feb is the first dataset after QCOVID
SPL_by_LA_15Feb <- fread(paste0(rawdatadir,"SPL/England/February 2021/Coronavirus Shielded Patient List, England - Open Data with CMO DG - LA - 2021-02-15.csv"), header=TRUE, sep=",", check.names=T) %>%
  dplyr::rename(.,Patient.Count=`Patient.Count..suppressed.`)
  #10 Feb is the last dataset before QCOVID  
SPL_by_LA_10Feb <- fread(paste0(rawdatadir,"SPL/England/February 2021/Coronavirus Shielded Patient List, England - Open Data with CMO DG - LA - 2021-02-10.csv"), header=TRUE, sep=",", check.names=T) %>%
  dplyr::rename(.,Patient.Count=`Patient.Count..suppressed.`)

#Append datasets from different dates
SPL_by_LA <- plyr::rbind.fill(SPL_by_LA_20March,
                              SPL_by_LA_12March,
                              SPL_by_LA_06March,
                              SPL_by_LA_26Feb,
                              SPL_by_LA_22Feb,
                              SPL_by_LA_15Feb,
                              SPL_by_LA_10Feb)

rm(SPL_by_LA_20March,SPL_by_LA_12March,SPL_by_LA_06March,SPL_by_LA_26Feb,SPL_by_LA_22Feb,SPL_by_LA_15Feb,SPL_by_LA_10Feb)

############ Merge in population numbers (you will lose some Local Authorities - the way the SPL is structured is non-standard)

#Manually fix code for England

SPL_by_LA$LA.Code[which(SPL_by_LA$LA.Code=="ENG")] <- "E92000001"

#Merge in population numbers

nrow(SPL_by_LA)
SPL_by_LA <- left_join(SPL_by_LA,pop_by_LA,by=c("LA.Code"="LAD19CD"))
nrow(SPL_by_LA)

#Manually add in population data for rows that add together 2 local authorities
#These are 'Hackney and City of London' and 'Cornwall and Isles of Scilly'

#Hackney and City of London
hackney_city_london <- filter(pop_by_LA,LAD19NM=="Hackney")$pop19+filter(pop_by_LA,LAD19NM=="City of London")$pop19
SPL_by_LA[SPL_by_LA$LA.Name %in% c("Hackney and City of London"),]$pop19 <- hackney_city_london
SPL_by_LA[SPL_by_LA$LA.Name %in% c("Hackney and City of London"),]$RGN19NM <- "London"

#Cornwall and Isles of Scilly
cornwall_scilly <- filter(pop_by_LA,LAD19NM=="Cornwall")$pop19+filter(pop_by_LA,LAD19NM=="Isles of Scilly")$pop19
SPL_by_LA[SPL_by_LA$LA.Name %in% c("Cornwall and Isles of Scilly"),]$pop19 <- cornwall_scilly
SPL_by_LA[SPL_by_LA$LA.Name %in% c("Cornwall and Isles of Scilly"),]$RGN19NM <- "South West"

############ Save

fwrite(SPL_by_LA, file = here::here("Clean data","SPL_by_LA.csv"), sep = ",")

############ Reduced dataset for all shielders

SPL_by_LA_All <- dplyr::filter(SPL_by_LA,Breakdown.Field=="ALL")

#You lose the following LAs: no exact match with latest population figures
# Buckinghamshire
#NHS England does not report numbers of shielding patients for ALL local authorities

SPL_by_LA_All <- SPL_by_LA_All %>%
  mutate(.,Patient.Count=as.numeric(Patient.Count)) %>%
  mutate(.,Shielders_pct=Patient.Count/pop19*100) %>%
  arrange(.,Extract.Date,desc(Shielders_pct)) %>% as.data.table()

############ Find how many added by QCOVID and merge into 'ALL' dataset

QCOVID_totals <- SPL_by_LA %>%
  filter(.,Breakdown.Field=="Disease Group",
         Breakdown.Value=="Added by COVID-19 Population Risk Assessment") %>%
  select(.,Extract.Date,LA.Code,Patient.Count) %>%
  dplyr::rename(.,Patient.Count.QCOVID=Patient.Count)

SPL_by_LA_All <- left_join(SPL_by_LA_All,QCOVID_totals,by=c("Extract.Date","LA.Code")) %>%
  mutate(.,Patient.Count.QCOVID=as.numeric(Patient.Count.QCOVID),
         Patient.Count.nonQCOVID=ifelse(is.na(Patient.Count.QCOVID),Patient.Count,Patient.Count-Patient.Count.QCOVID),
         Shielders_from_QCOVID_pct=Patient.Count.QCOVID/Patient.Count*100)

rm(QCOVID_totals)

############ Merge in deprivation

nrow(SPL_by_LA_All)
SPL_by_LA_All <- left_join(SPL_by_LA_All,IMD2019_LA_wide,by=c("LA.Code"="FeatureCode"))
nrow(SPL_by_LA_All)

############ Remove rows that don't have a region or population (ENGLAND and Buckinghamshire)

SPL_by_LA_All_incl_ENG <- SPL_by_LA_All
SPL_by_LA_All <- dplyr::filter(SPL_by_LA_All, !is.na(RGN19NM))

############ Save

fwrite(SPL_by_LA_All_incl_ENG, file = here::here("Clean data","SPL_by_LA_All_incl_ENG.csv"), sep = ",")
fwrite(SPL_by_LA_All, file = here::here("Clean data","SPL_by_LA_All.csv"), sep = ",")

############ Before/After dataset - long (Feb 10 vs. March 20)

before_after_data_long <- SPL_by_LA_All_incl_ENG %>%
  filter(.,Extract.Date %in% c("10/02/2021","20/03/2021")) %>%
  select(.,Extract.Date,LA.Code,LA.Name,Patient.Count,Patient.Count.QCOVID,
         Shielders_pct,Shielders_from_QCOVID_pct,decile_imd,quintile_imd,
         decile_income,quintile_income,RGN19NM,pop19) %>%
  mutate(.,before_after=ifelse(Extract.Date=="20/03/2021","after","before")) %>%
  select(.,-"Extract.Date")

fwrite(before_after_data_long, file = here::here("Clean data","before_after_data_long.csv"), sep = ",")

############ Before/After dataset - wide (Feb 10 vs. March 20)

before_after_data <- SPL_by_LA_All_incl_ENG %>%
  filter(.,Extract.Date %in% c("10/02/2021","20/03/2021")) %>%
  select(.,Extract.Date,LA.Code,LA.Name,Patient.Count,Patient.Count.QCOVID,
         Shielders_pct,Shielders_from_QCOVID_pct,decile_imd,quintile_imd,
         decile_income,quintile_income,RGN19NM,pop19) %>%
  mutate(.,before_after=ifelse(Extract.Date=="20/03/2021","after","before")) %>%
  select(.,-"Extract.Date") %>%
  pivot_wider(.,
    names_from = before_after,
    names_sep = "_",
    values_from = c(Patient.Count,Patient.Count.QCOVID,
                    Shielders_pct,Shielders_from_QCOVID_pct)) %>%
  mutate(.,Patient.Count_diff=Patient.Count_after-Patient.Count_before,
         Patient.Count_diff=Patient.Count_after-Patient.Count_before,
         Shielders_pct_diff=Shielders_pct_after-Shielders_pct_before)

fwrite(before_after_data, file = here::here("Clean data","before_after_data.csv"), sep = ",")
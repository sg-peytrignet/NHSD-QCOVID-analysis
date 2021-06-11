##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

##############################################
################### SETUP ####################
##############################################

#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,stringr,ggplot2,DescTools,data.table,
               dvmisc,tibble,pbapply,here,tidyverse,readxl)

#Clean up the global environment
rm(list = ls())

#Set directory where inputs are saved
rawdatadir <- "M:/Analytics/Networked Data Lab/Shielding/"

#Git directory
gitdir <- dirname(rstudioapi::getSourceEditorContext()$path)

#######################################################################
################### LOOKUP: LOCAL AUTHORITY TO HIGHER #################
#######################################################################

# LSOA_to_higher_geo_Apr21 <- fread(paste0(rawdatadir,"Other data/Lookups/LSOA_(2011)_to_Clinical_Commissioning_Groups_to_Sustainability_and_Transformation_Partnerships_(April_2021)_Lookup_in_England.csv"), header=TRUE, sep=",", check.names=T) %>%
#   select(.,LSOA11CD,CCG21CD,CCG21CDH,CCG21NM,STP21CD,STP21NM)

LSOA_to_higher_geo <- fread(paste0(rawdatadir,"Other data/Lookups/LSOA_(2011)_to_Clinical_Commissioning_Groups_to_Sustainability_and_Transformation_Partnerships_(April_2020)_Lookup_in_England.csv"), header=TRUE, sep=",", check.names=T) %>%
  select(.,LSOA11CD,CCG20CD,CCG20CDH,CCG20NM,STP20CD,STP20NM)

length(unique(LSOA_to_higher_geo$CCG20CD)) #135 CCGs in 2020
#length(unique(LSOA_to_higher_geo_Apr21$CCG21CD)) #106 CCGs in 2021
#rm(LSOA_to_higher_geo_Apr21)

##############################################
################### POPULATION ###############
##############################################

pop_by_LSOA <- read_excel(paste0(rawdatadir,"Other data/Mid-year population estimates/LSOA/SAPE22DT2-mid-2019-lsoa-syoa-estimates-unformatted.xlsx"),
                        sheet = "Mid-2019 Persons",skip=4) %>%
  select(.,`LSOA Code`,`LSOA Name`,`All Ages`) %>%
  dplyr::rename(.,LSOA11CD=`LSOA Code`,LSOA11NM=`LSOA Name`,pop19=`All Ages`) %>%
  filter(.,!is.na(LSOA11CD))

#Join with geo-indicators to match English local authorities to regions

pop_by_LSOA <- left_join(pop_by_LSOA,LSOA_to_higher_geo,
                              by=c("LSOA11CD"="LSOA11CD")) %>%
  filter(.,!is.na(CCG20CD))

rm(LSOA_to_higher_geo)

#Compute population by CCG

pop_by_LSOA_agg <- pop_by_LSOA %>%
  group_by(CCG20CD,CCG20CDH) %>%
  summarise(pop19=sum(pop19,na.rm=TRUE)) %>%
  ungroup()

##############################################
################### DEPRIVATION ##############
##############################################

IMD2019_LSOA <- fread(paste0(rawdatadir,"Other data/IMD/LSOA/Index_of_Multiple_Deprivation_(December_2019)_Lookup_in_England.csv"), header=TRUE, sep=",", check.names=T) %>%
  select(.,LSOA11CD,IMD19)

#Join with population and lookup data

IMD2019_LSOA <- left_join(IMD2019_LSOA,pop_by_LSOA,
                         by=c("LSOA11CD"="LSOA11CD"))

#Weighted average score by CCG and new quantiles

IMD2019_LSOA_agg <- IMD2019_LSOA %>%
  group_by(CCG20CD,CCG20CDH) %>%
  summarise(IMD19_CCG_wgt=weighted.mean(IMD19, pop19)) %>%
  ungroup() %>%
  mutate(.,IMD19_decile=ntile(IMD19_CCG_wgt, 10),
         IMD19_quintile=ntile(IMD19_CCG_wgt, 5))

##################################################
################### SAVE NEW LOOKUP ##############
##################################################

fwrite(pop_by_LSOA_agg, file = paste0(rawdatadir,"Other data/Mid-year population estimates/CCG/pop_by_LSOA_agg.csv"), sep = ",")
fwrite(IMD2019_LSOA_agg, file = paste0(rawdatadir,"Other data/Lookups/CCG_to_IMD_2020_lookup.csv"), sep = ",")
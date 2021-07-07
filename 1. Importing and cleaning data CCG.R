##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

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

###########################################################
################### LOOKUP: CCG TO HIGHER #################
###########################################################

##############################################
################### POPULATION ###############
##############################################

#CCGs in England

pop_by_CCG <- read_excel("M:/Analytics/Networked Data Lab/Shielding/Other data/Mid-year population estimates/CCG/sape22dt6amid2019ccg2020estimatesunformatted/SAPE22DT6a-mid-2019-ccg-2020-estimates-unformatted.xlsx",
                         sheet="Mid-2019 Persons",skip=6) %>%
  select(.,`CCG Code`,`CCG Name`,`STP20 Code`,`STP20 Name`,`All Ages`) %>%
  filter(.,!is.na(`CCG Code`)) %>%
  rename(.,pop19=`All Ages`)

#Scottish Health Boards
#Grampian
#Other source: https://www.srr.scot.nhs.uk/publications/docs/2020-10-13-SRR-Report.pdf?4

pop_by_SHB <- fread("M:/Analytics/Networked Data Lab/Shielding/Other data/Mid-year population estimates/Scotland/population-estimates-2011-datazone-linked-dataset.csv", header=TRUE, sep=",", check.names=T,skip=8)

#Wales
#Other source: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/annualmidyearpopulationestimates/mid2019estimates

pop_Wales <- 3152879

##############################################
################### DEPRIVATION ##############
##############################################

IMD_by_CCG <- fread(paste0(rawdatadir,"Other data/Lookups/CCG_to_IMD_2020_lookup.csv"), header=TRUE, sep=",", check.names=T)

##########################################################################
################### NUMBER OF SHIELDERS BY CCG: England ##################
##########################################################################

#March 2021
SPL_by_CCG_20March <- fread(paste0(rawdatadir,"SPL/England/March 2021/CCG/Coronavirus Shielded Patient List, England - Open Data with CMO DG - CCG - 2021-03-20.csv"), header=TRUE, sep=",", check.names=T)
SPL_by_CCG_12March <- fread(paste0(rawdatadir,"SPL/England/March 2021/CCG/Coronavirus Shielded Patient List, England - Open Data with CMO DG - CCG - 2021-03-12.csv"), header=TRUE, sep=",", check.names=T)
SPL_by_CCG_06March <- fread(paste0(rawdatadir,"SPL/England/March 2021/CCG/Coronavirus Shielded Patient List, England - Open Data with CMO DG - CCG - 2021-03-06.csv"), header=TRUE, sep=",", check.names=T)

#February 2021
SPL_by_CCG_26Feb <- fread(paste0(rawdatadir,"SPL/England/February 2021/CCG/Coronavirus Shielded Patient List, England - Open Data with CMO DG - CCG - 2021-02-26.csv"), header=TRUE, sep=",", check.names=T)
SPL_by_CCG_22Feb <- fread(paste0(rawdatadir,"SPL/England/February 2021/CCG/Coronavirus Shielded Patient List, England - Open Data with CMO DG - CCG - 2021-02-22.csv"), header=TRUE, sep=",", check.names=T)

#15 Feb is the first dataset after QCOVID
SPL_by_CCG_15Feb <- fread(paste0(rawdatadir,"SPL/England/February 2021/CCG/Coronavirus Shielded Patient List, England - Open Data with CMO DG - CCG - 2021-02-15.csv"), header=TRUE, sep=",", check.names=T)

#10 Feb is the last dataset before QCOVID  
SPL_by_CCG_10Feb <- fread(paste0(rawdatadir,"SPL/England/February 2021/CCG/Coronavirus Shielded Patient List, England - Open Data with CMO DG - CCG - 2021-02-10.csv"), header=TRUE, sep=",", check.names=T)

#Append datasets from different dates
SPL_by_CCG <- plyr::rbind.fill(SPL_by_CCG_20March,
                              SPL_by_CCG_12March,
                              SPL_by_CCG_06March,
                              SPL_by_CCG_26Feb,
                              SPL_by_CCG_22Feb,
                              SPL_by_CCG_15Feb,
                              SPL_by_CCG_10Feb)

rm(SPL_by_CCG_20March,SPL_by_CCG_12March,SPL_by_CCG_06March,SPL_by_CCG_26Feb,SPL_by_CCG_22Feb,SPL_by_CCG_15Feb,SPL_by_CCG_10Feb)

############ Merge in population numbers

length(unique(SPL_by_CCG$CCG.Code))

#Merge in population numbers

nrow(SPL_by_CCG)
SPL_by_CCG <- left_join(SPL_by_CCG,pop_by_CCG,by=c("CCG.Code"="CCG20CDH"))
nrow(SPL_by_CCG)

#Manually fix pop19 for England

SPL_by_CCG$pop19[which(SPL_by_CCG$CCG.Name=="ENGLAND")] <- sum(pop_by_CCG$pop19)

############ Save

fwrite(SPL_by_CCG, file = here::here("Clean data","SPL_by_CCG.csv"), sep = ",")

############ Reduced dataset for all shielders

SPL_by_CCG_All <- dplyr::filter(SPL_by_CCG,Breakdown.Field=="ALL")

#Compute % shielding in each CCG

SPL_by_CCG_All <- SPL_by_CCG_All %>%
  mutate(.,Patient.Count=as.numeric(Patient.Count)) %>%
  mutate(.,Shielders_pct=Patient.Count/pop19*100) %>%
  arrange(.,Extract.Date,desc(Shielders_pct)) %>% as.data.table()

############ Find how many added by QCOVID and merge into 'ALL' dataset

QCOVID_totals <- SPL_by_CCG %>%
  filter(.,Breakdown.Field=="Disease Group",
         Breakdown.Value=="Added by COVID-19 Population Risk Assessment") %>%
  select(.,Extract.Date,CCG.Code,Patient.Count) %>%
  dplyr::rename(.,Patient.Count.QCOVID=Patient.Count)

SPL_by_CCG_All <- left_join(SPL_by_CCG_All,QCOVID_totals,by=c("Extract.Date","CCG.Code")) %>%
  mutate(.,Patient.Count.QCOVID=as.numeric(Patient.Count.QCOVID),
         Patient.Count.nonQCOVID=ifelse(is.na(Patient.Count.QCOVID),Patient.Count,Patient.Count-Patient.Count.QCOVID),
         Shielders_from_QCOVID_pct=Patient.Count.QCOVID/Patient.Count*100)

rm(QCOVID_totals)

############ Merge in deprivation

nrow(SPL_by_CCG_All)
SPL_by_CCG_All <- left_join(SPL_by_CCG_All,select(IMD_by_CCG,CCG20CDH,IMD19_decile,IMD19_quintile),by=c("CCG.Code"="CCG20CDH"))
nrow(SPL_by_CCG_All)

############ Save

fwrite(SPL_by_CCG_All, file = here::here("Clean data","SPL_by_CCG_All.csv"), sep = ",")

############ Before/After dataset - long (Feb 10 vs. March 20)

before_after_data_long <- SPL_by_CCG_All %>%
  filter(.,Extract.Date %in% c("10/02/2021","20/03/2021")) %>%
  select(.,Extract.Date,CCG.Code,CCG.Name,Patient.Count,Patient.Count.QCOVID,
         Shielders_pct,Shielders_from_QCOVID_pct,IMD19_decile,IMD19_quintile,
         pop19) %>%
  mutate(.,before_after=ifelse(Extract.Date=="20/03/2021","after","before")) %>%
  select(.,-"Extract.Date")

fwrite(before_after_data_long, file = here::here("Clean data","CCG_before_after_data_long.csv"), sep = ",")

############ Before/After dataset - wide (Feb 10 vs. March 20)

before_after_data <- SPL_by_CCG_All %>%
  filter(.,Extract.Date %in% c("10/02/2021","20/03/2021")) %>%
  select(.,Extract.Date,CCG.Code,CCG.Name,Patient.Count,Patient.Count.QCOVID,
         Shielders_pct,Shielders_from_QCOVID_pct,IMD19_decile,IMD19_quintile,
         pop19) %>%
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

fwrite(before_after_data, file = here::here("Clean data","CCG_before_after_data.csv"), sep = ",")
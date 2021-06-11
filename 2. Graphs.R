##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

##############################################
################### SETUP ####################
##############################################

#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,stringr,ggplot2,easycsv,
               gmodels,Rmisc,DescTools,data.table,
               tibble,leaflet,raster,plotly,
               here,RColorBrewer,ggthemes,hrbrthemes,
               ggchicklet,tidyverse,showtext)

#Clean up the global environment
rm(list = ls())

#Set directory where inputs are saved
rawdatadir <- "M:/Analytics/Networked Data Lab/Shielding/"

#Git directory
gitdir <- dirname(rstudioapi::getSourceEditorContext()$path)

#One Drive directory for graphs
onedrivegraphs <- "C:/Users/SebastienP/OneDrive - The Health Foundation/Open Data QCOVID/"

#Projection codes
# ukgrid = "+init=epsg:27700"
# latlong="+init=epsg:4326"

##################################################
################### Load clean data ##############
##################################################

fread_folder(paste0(gitdir,"/Clean data/"), extension = "CSV")

# SPL_by_LA <- fread(here::here("Clean data","SPL_by_LA.csv"), header=TRUE, sep=",", check.names=T)
# SPL_by_LA_All <- fread(here::here("Clean data","SPL_by_LA_All.csv"), header=TRUE, sep=",", check.names=T)
# SPL_by_LA_All_incl_ENG <- fread(here::here("Clean data","SPL_by_LA_All_incl_ENG.csv"), header=TRUE, sep=",", check.names=T)
# before_after_data <- fread(here::here("Clean data","before_after_data.csv"), header=TRUE, sep=",", check.names=T)
# before_after_data_long <- fread(here::here("Clean data","before_after_data_long.csv"), header=TRUE, sep=",", check.names=T)

#######################################################
################### UK regions shapefile ##############
#######################################################

# setwd(paste0(rawdatadir,"/Shapefiles"))
# 
# UK_shp <- readOGR(dsn="NUTS_Level_1__January_2018__Boundaries-shp",layer="NUTS_Level_1__January_2018__Boundaries")
# UK_shp <- spTransform(UK_shp, CRS(latlong))

#################################################
############## Reason for shielding #############
#################################################

filter(SPL_by_CCG_All,CCG.Code=='ENG',Extract.Date=='20/03/2021')

reasons.data <- SPL_by_CCG %>%
  filter(.,Breakdown.Field=='Disease Group',CCG.Code=='ENG',Extract.Date=='20/03/2021')

fwrite(reasons.data, file = here::here("Clean data","reasons.data.csv"), sep = ",")

# 546601/3803374*100
# 3920/100878

#######################################################
############## Millions of people by week #############
#######################################################

QCOVID_numbers_data <- SPL_by_LA_All_incl_ENG %>%
  filter(.,LA.Name=="ENGLAND") %>%
  mutate(.,Patient.Count.QCOVID=ifelse(is.na(Patient.Count.QCOVID),0,Patient.Count.QCOVID)) %>%
  select(.,Extract.Date,Patient.Count,Shielders_pct,Patient.Count.QCOVID,Patient.Count.nonQCOVID) %>%
  gather(., QCOVID, number.people, c("Patient.Count.QCOVID","Patient.Count.nonQCOVID"), factor_key=FALSE) %>%
  mutate(.,QCOVID=ifelse(QCOVID=="Patient.Count.QCOVID","QCOVID",QCOVID),
           QCOVID=ifelse(QCOVID=="Patient.Count.nonQCOVID","National methodology",QCOVID)) %>%
  mutate(.,Extract.Date=lubridate::dmy(Extract.Date)) %>%
  mutate(., QCOVID = factor(QCOVID, levels=c("QCOVID",
                                           "National methodology")))

QCOVID_numbers_chart <- QCOVID_numbers_data %>%
  ggplot(., aes(fill=QCOVID, y=number.people, x=Extract.Date)) + 
  geom_bar(position="stack", stat="identity") +
  theme_ipsum() +
  guides(fill=guide_legend(title="Addition method")) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%b %d %Y", breaks = unique(QCOVID_numbers_data$Extract.Date)) +
  labs(title="Number of people identified as CEV in England",
       x ="Extract date", y = "Number of people") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        legend.text=element_text(size=11),
        text = element_text(size = 11),
        axis.text = element_text(size =11),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size =11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(COVID_HRGS_MH1) %>%
  layout(title = list(text = paste0('Number of people identified as CEV in England',
                                    '<br>',
                                    '<sup>',
                                    'Data from NHS Digital between 26 Feb 21 and 20 March 21',
                                    '</sup>')))

###########################################################
############## Range of pct difference by CCG #############
###########################################################

#Percentage from QCOVID
CCG_before_after_data %>%
  slice_max(.,Shielders_from_QCOVID_pct_after,n=5)

CCG_before_after_data %>%
  slice_min(.,Shielders_from_QCOVID_pct_after,n=5)

#Difference after QCOVID implemented
CCG_before_after_data %>%
  slice_max(.,Shielders_pct_diff,n=5)

CCG_before_after_data %>%
  slice_min(.,Shielders_pct_diff,n=5)


################################################################
############## Pct shielding by IMD quintile (CGG) #############
################################################################

desc.stat.by.imd.ccg <- SPL_by_CCG_All %>%
  filter(.,(Extract.Date %in% c("10/02/2021","20/03/2021")),CCG.Name!="ENGLAND") %>%
  select(.,Extract.Date,CCG.Name,Patient.Count,Shielders_pct,pop19,IMD19_quintile) %>%
  group_by(Extract.Date,IMD19_quintile) %>%
  summarise(.,pop19=sum(pop19,na.rm=TRUE),
            Patient.Count=sum(Patient.Count,na.rm=TRUE),
            std.dev.pct.shielding=sd(Shielders_pct)) %>%
  mutate(.,Shielders_pct_agg=Patient.Count/pop19*100) %>%
  ungroup() %>%
  arrange(.,IMD19_quintile,Extract.Date)

fwrite(desc.stat.by.imd.ccg, file = here::here("Clean data","desc.stat.by.imd.ccg.csv"), sep = ",")

# SPL_by_CCG_All %>%
#   filter(.,Extract.Date %in% c("10/02/2021"),IMD19_quintile==1) %>%
#   select(.,Extract.Date,CCG.Name,Patient.Count,Shielders_pct,pop19,IMD19_quintile) %>%
#   select(.,Shielders_pct) %>%
#   unlist(.) %>%
#   sd(.)

###########################################################
############## Range of pct difference by CCG #############
###########################################################

CCG_before_after_data %>%
  filter(.,!is.na(IMD19_quintile)) %>%
  group_by(IMD19_quintile) %>%
  summarise(.,std.dev=sd(Shielders_pct_before)) %>%
  ungroup()

# variance_most_deprived/variance_least_deprived
# 1.32/0.485
  
###########################################################
############## Range of pct difference by CCG #############
###########################################################

CCG_before_after_data %>%
  filter(.,!is.na(IMD19_quintile)) %>%
  group_by(IMD19_quintile) %>%
  summarise(.,Patient.Count_before=sum(Patient.Count_before,na.rm=TRUE),
            Patient.Count_after=sum(Patient.Count_after,na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(.,pct_diff=(Patient.Count_after-Patient.Count_before)/Patient.Count_before*100)
  
####################################################################################
############## Change in rate of CEV by IMD quintile (aggregated, CCG) #############
####################################################################################

agg_by_dep_data_CCG <- CCG_before_after_data_long %>%
  filter(.,!is.na(IMD19_quintile)) %>%
  select(.,before_after,IMD19_quintile,CCG.Code,CCG.Name,Patient.Count,pop19) %>%
  group_by(before_after,IMD19_quintile) %>%
  summarise(Patient.Count=sum(Patient.Count,na.rm=TRUE),
            pop19=sum(pop19,na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(.,pct_shielding=Patient.Count/pop19*100) %>%
  arrange(.,IMD19_quintile,before_after) %>%
  mutate(., IMD19_quintile=ifelse(IMD19_quintile=="1","1 (most deprived)",IMD19_quintile)) %>%
  mutate(., IMD19_quintile=ifelse(IMD19_quintile=="5","5 (least deprived)",IMD19_quintile)) %>%
  mutate(., IMD19_quintile = factor(IMD19_quintile, levels=c("1 (most deprived)","2","3","4","5 (least deprived)")))

agg_by_dep_data_chart_CCG <- agg_by_dep_data_CCG %>%
  ggplot(.,aes(fill=before_after,x = IMD19_quintile,y = pct_shielding)) +
  geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(name="% of people who are CEV",limits=c(0,10)) +
  theme_ipsum() +
  scale_fill_brewer(palette = "Set1") +
  guides(fill=guide_legend(title="Before/after QCOVID")) +
  labs(title="Impact of QCOVID on shielding rates",
       x ="IMD quintile", y = "% of CEV people")  +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        legend.text=element_text(size=11),
        text = element_text(size = 11),
        axis.text = element_text(size =11),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size =11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(agg_by_dep_data_chart) %>%
  layout(title = list(text = paste0('Impact of QCOVID on shielding rates',
                                    '<br>',
                                    '<sup>',
                                    'Source: NHS England',
                                    '</sup>')))
  
###############################################################################
############## Change in rate of CEV by IMD quintile (aggregated) #############
###############################################################################

agg_by_dep_data <- before_after_data_long %>%
  filter(.,!is.na(quintile_income)) %>%
  select(.,before_after,quintile_income,LA.Code,LA.Name,Patient.Count,pop19) %>%
  group_by(before_after,quintile_income) %>%
  summarise(Patient.Count=sum(Patient.Count,na.rm=TRUE),
            pop19=sum(pop19,na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(.,pct_shielding=Patient.Count/pop19*100) %>%
  arrange(.,quintile_income,before_after) %>%
  mutate(., quintile_income=ifelse(quintile_income=="1","1 (most deprived)",quintile_income)) %>%
  mutate(., quintile_income=ifelse(quintile_income=="5","5 (least deprived)",quintile_income)) %>%
  mutate(., quintile_income = factor(quintile_income, levels=c("1 (most deprived)","2","3","4","5 (least deprived)")))

agg_by_dep_data_chart <- agg_by_dep_data %>%
  ggplot(.,aes(fill=before_after,x = quintile_income,y = pct_shielding)) +
  geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(name="% of people who are CEV",limits=c(0,10)) +
  theme_ipsum() +
  scale_fill_brewer(palette = "Set1") +
  guides(fill=guide_legend(title="Before/after QCOVID")) +
  labs(title="Impact of QCOVID on shielding rates",
       x ="IMD income quintile", y = "% of CEV people")  +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        legend.text=element_text(size=11),
        text = element_text(size = 11),
        axis.text = element_text(size =11),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size =11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(agg_by_dep_data_chart) %>%
  layout(title = list(text = paste0('Impact of QCOVID on shielding rates',
                                    '<br>',
                                    '<sup>',
                                    'Source: NHS England',
                                    '</sup>')))


##################################################################
############## Change in rate of CEV by IMD quintile #############
##################################################################

change_pct_points_by_dep_data <- before_after_data %>%
  select(.,LA.Code,LA.Name,quintile_income,Shielders_pct_diff) %>%
  filter(.,!(is.na(Shielders_pct_diff)|is.na(quintile_income))) %>%
  arrange(.,desc(Shielders_pct_diff))

m <- list(   l = 80,   r = 300,   b = 80,   t = 100,   pad = 0 )

change_pct_points_by_dep <- change_pct_points_by_dep_data %>%
  ggplot(.) +
  geom_col(aes(x=reorder(LA.Name, Shielders_pct_diff), y = Shielders_pct_diff,fill = factor(quintile_income))) +
  ylim(0, 8) +
  scale_fill_brewer(palette="Spectral",name = "IMD income\nquintile",direction = 1) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(1,2,1,1), "cm")) +
  labs(title="Change in rate of CEV people after QCOVID",y = "Change in % points",x="Local authority")

ggplotly(change_pct_points_by_dep) %>%
  layout(autosize = F, width = 650, height = 350, margin = m)

#########################################################################
############## Change in rate of CEV by region (aggregated) #############
#########################################################################

agg_by_region_data <- before_after_data_long %>%
  filter(.,!(is.na(RGN19NM)|RGN19NM=="")) %>%
  select(.,before_after,RGN19NM,LA.Code,LA.Name,Patient.Count,pop19) %>%
  group_by(before_after,RGN19NM) %>%
  summarise(Patient.Count=sum(Patient.Count,na.rm=TRUE),
            pop19=sum(pop19,na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(.,pct_shielding=Patient.Count/pop19*100) %>%
  arrange(.,RGN19NM,before_after)

after_data  <- agg_by_region_data %>%
  filter(.,before_after=="after") %>%
  arrange(.,pct_shielding) %>%
  mutate(.,order=1:nrow(.)) %>%
  select(.,RGN19NM,order)

agg_by_region_data <- left_join(agg_by_region_data,after_data,by="RGN19NM")

agg_by_region_data_chart <- agg_by_region_data %>%
  ggplot(.,aes(fill=before_after,x = reorder(RGN19NM,order),y = pct_shielding)) +
  geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(name="% of people who are CEV",limits=c(0,10)) +
  theme_ipsum() +
  scale_fill_brewer(palette = "Set1") +
  guides(fill=guide_legend(title="Before/after QCOVID")) +
  labs(title="Impact of QCOVID on shielding rates",
       x ="Region", y = "% of CEV people")  +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        legend.text=element_text(size=11),
        text = element_text(size = 11),
        axis.text = element_text(size =11),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size =11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(agg_by_dep_data_chart) %>%
  layout(title = list(text = paste0('Impact of QCOVID on shielding rates',
                                    '<br>',
                                    '<sup>',
                                    'Source: NHS England',
                                    '</sup>')))

########################################
############## Scatterplot #############
########################################

scatter_data <- before_after_data %>%
  select(.,LA.Code,LA.Name,quintile_income,Shielders_pct_before,Shielders_pct_after,Shielders_pct_diff) %>%
  filter(.,!(is.na(Shielders_pct_diff)|is.na(quintile_income))) %>%
  arrange(.,desc(Shielders_pct_diff)) %>%
  mutate(., quintile_income=ifelse(quintile_income=="1","1 (most deprived)",quintile_income)) %>%
  mutate(., quintile_income=ifelse(quintile_income=="5","5 (least deprived)",quintile_income)) %>%
  mutate(., quintile_income = factor(quintile_income, levels=c("1 (most deprived)","2","3","4","5 (least deprived)")))

scatter_chart <- scatter_data %>%
  ggplot(.,aes(x=Shielders_pct_before, y = Shielders_pct_diff)) +
  geom_point(aes(fill=factor(quintile_income)),colour="darkgrey",pch=21, size=3) +
  geom_smooth(method='lm',se=FALSE,col="black",linetype="dashed") +
  ylim(0, 8) +
  facet_wrap(~ quintile_income) +
  scale_fill_brewer(palette="Spectral",name = "IMD income\nquintile",direction = 1) +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(1,2,1,1), "cm")) +
  labs(title="Change in rate of CEV people after QCOVID",y = "Change in % points",x="% CEV before QCOVID")

ggplotly(scatter_chart) %>%
  layout(autosize = F, width = 650, height = 350, margin = m)

#######################################################
############## Dumbbell plot for Flourish #############
#######################################################

after_data  <- CCG_before_after_data_long %>%
  filter(.,before_after=="after") %>%
  arrange(.,Shielders_pct) %>%
  mutate(.,order=1:nrow(.)) %>%
  select(.,CCG.Code,order)

CCG_before_after_data_long <- left_join(CCG_before_after_data_long,after_data,by="CCG.Code")

dumbbell_ccg_data <- CCG_before_after_data_long %>%
  select(.,CCG.Name,before_after,Shielders_pct,IMD19_quintile,order) %>%
  filter(.,CCG.Name!="ENGLAND") %>%
  mutate(.,CCG.Name=str_replace_all(CCG.Name,"NHS ","")) %>%
  mutate(.,CCG.Name=str_replace_all(CCG.Name," CCG","")) %>%
  mutate(.,before_after=str_replace_all(before_after,"before","Before QCOVID")) %>%
  mutate(.,before_after=str_replace_all(before_after,"after","After QCOVID")) %>%
  arrange(.,IMD19_quintile,order) %>%
  mutate(.,IMD19_quintile=str_replace_all(IMD19_quintile,"1","1 (most deprived)")) %>%
  mutate(.,IMD19_quintile=str_replace_all(IMD19_quintile,"5","5 (least deprived)"))

fwrite(dumbbell_ccg_data, file = paste0(onedrivegraphs,"dumbbell_ccg_data.csv"), sep = ",")

##########################################
############## Dumbbell plot #############
##########################################

library(ggalt)

df <- data.frame(trt=LETTERS[1:5], l=c(20, 40, 10, 30, 50), r=c(70, 50, 30, 60, 80))

ggplot(df, aes(y=trt, x=l, xend=r)) +
  geom_dumbbell(size=3, color="#e3e2e1",
                colour_x = "#5b8124", colour_xend = "#bad744",
                dot_guide=TRUE, dot_guide_size=0.25) +
  labs(x=NULL, y=NULL, title="ggplot2 geom_dumbbell with dot guide") +
  theme_minimal() +
  theme(panel.grid.major.x=element_line(size=0.05))


df <- data.frame(trt=LETTERS[1:5], l=c(20, 40, 10, 30, 50), r=c(70, 50, 30, 60, 80))
df2 = tidyr::gather(df, group, value, -trt)

ggplot(df, aes(y = trt)) + 
  geom_point(data = df2, aes(x = value, color = group), size = 3) +
  geom_dumbbell(aes(x = l, xend = r), size=3, color="#e3e2e1", 
                colour_x = "red", colour_xend = "blue",
                dot_guide=TRUE, dot_guide_size=0.25) +
  theme_bw() +
  scale_color_manual(name = "", values = c("red", "blue") )



ggplot(df, aes(y = trt)) + 
  geom_point(data = df2, aes(x = value, color = group), size = 3) +
  geom_dumbbell(aes(x = l, xend = r), size=3, color="#e3e2e1", 
                colour_x = "red", colour_xend = "blue",
                dot_guide=TRUE, dot_guide_size=0.25) +
  theme_bw() +
  scale_color_manual(name = "", values = c("red", "blue") )

#Dumbbell plot (one quintile)

filter(filter(scatter_data,quintile_income=="1 (most deprived)")) %>%
  ggplot(., aes(y=reorder(LA.Name,Shielders_pct_after),
                x=Shielders_pct_before,
                xend=Shielders_pct_after)) +
  geom_dumbbell(size=3, color="#e3e2e1",
                colour_x = "#5b8124", colour_xend = "#bad744",
                dot_guide=FALSE, dot_guide_size=0.25,show.legend=TRUE) +
  facet_wrap(~ quintile_income) +
  labs(title="Change in rate of CEV people after QCOVID",y = "Local authority",x="% CEV") +
  theme(panel.border = element_blank(),
        plot.margin = unit(c(1,2,1,1), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

filter(filter(scatter_data,quintile_income=="5 (least deprived)")) %>%
  ggplot(., aes(y=reorder(LA.Name,Shielders_pct_after),
                x=Shielders_pct_before,
                xend=Shielders_pct_after)) +
  geom_dumbbell(size=3, color="#e3e2e1",
                colour_x = "#5b8124", colour_xend = "#bad744",
                dot_guide=FALSE, dot_guide_size=0.25,show.legend=TRUE) +
  facet_wrap(~ quintile_income) +
  labs(title="Change in rate of CEV people after QCOVID",y = "Local authority",x="% CEV") +
  theme(panel.border = element_blank(),
        plot.margin = unit(c(1,2,1,1), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


#Dumbbell plot (all quintiles)

scatter_data <- scatter_data[sample(1:nrow(scatter_data),135,replace = FALSE),]

scatter_data %>%
  ggplot(., aes(y=reorder(LA.Name,Shielders_pct_after),group=LA.Name)) +
  geom_dumbbell(aes(x=Shielders_pct_before,xend=Shielders_pct_after,colour=Shielders_pct_diff),size=1,
                colour_x = "black", colour_xend = "blue") +
  facet_grid(rows = vars(quintile_income)) +
  scale_color_gradient(low="green", high="red") +
  labs(title="Change in rate of CEV people after QCOVID",y = "Local authority",x="% CEV") +
  theme(panel.border = element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(1,2,1,1), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
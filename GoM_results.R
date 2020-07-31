library(doBy)
library(reshape)
library(dplyr)
library(data.table)
library(ggplot2)
library(cowplot)
library(plotly)
library(ggpubr) 
library(gridExtra)

#Read data SOURCE MBON--------
# Source = https://coralnet.ucsd.edu/source/1997/

#Set the working directory to the folder "Source_GoM Rocky Shore" and read percent_cover.csv and metdata.csv files (downloaded "as it" from Coralnet source)
library(here)
GoM.photoquadrat.cover <- read.csv(here("Source_GoM Rocky Shore", "percent_covers.csv"))
GoM.photoquadrat.metadata <- read.csv(here("Source_GoM Rocky Shore", "metadata.csv"))

#Merge photoquadrat.metadata and photoquadrat.cover
GoM.photoquadrat<- merge(GoM.photoquadrat.metadata,GoM.photoquadrat.cover, by = "Name", all.x = TRUE) 

#CHECK wich date format you have in the files. Sometimes if you open the file with excel it may change the date format 
#transform to date format
GoM.photoquadrat$Date <- as.Date(GoM.photoquadrat$Date,'%Y-%m-%d')
#GoM.photoquadrat$Date <- as.Date(GoM.photoquadrat$Date,'%d/%m/%Y')

#Eliminate colums with taxa categories (CATAMI) that were not used for the photoquadrats
#as this codes search colsum= 0 in all the data frame , and removo those columns, we need to set lat ang long that have negative numbers in order to avoid being removed
GoM.photoquadrat$Latitude <- as.character(GoM.photoquadrat$Latitude)
GoM.photoquadrat$Longitude <- as.character(GoM.photoquadrat$Longitude)
library(tidyverse) 
GoM.photoquadrat <- GoM.photoquadrat %>% select_if(negate(function(col) is.numeric(col) && sum(col) <= 0))

#visual data
GoM.visual <- read.csv(here("Visual_quadrats","gom_visual.csv"))
GoM.visual$Date <- as.Date(GoM.visual$Date,'%m/%d/%Y')
GoM.visual$Latitude <- as.character(GoM.visual$Latitude)
GoM.visual$Longitude <- as.character(GoM.visual$Longitude)

#PREPARE DATA FOR ANALYSIS---------------------------------------------------------
#Joint photo and visual dataframes
photoquadrat_visual <- dplyr::bind_rows(GoM.photoquadrat, GoM.visual)
#replace NA from unc to cero values
photoquadrat_visual[is.na(photoquadrat_visual)] <- 0


#Annotation (types of analysis) in comments column 
photoquadrat_visual$Comments <- as.factor(photoquadrat_visual$Annotation.status)
levels(photoquadrat_visual$Comments) <-  c("Human","Robot","Visualquadrat")


#Create long type dataframe 
photoquadrat_visual_long = melt(photoquadrat_visual, id.vars = 1:20, measure.vars = 21:ncol(photoquadrat_visual), variable_name = "CATAMI", value_name ="cover", na.rm = T)
#rename columns because the ontop command is not working 
colnames(photoquadrat_visual_long)[21] <- "CATAMI"
colnames(photoquadrat_visual_long)[22] <- "cover"

#Calculate mean, SD, SE for cover data by factors (species=Shortname,site, strata,) 
Coverdata <- summaryBy(cover ~ CATAMI + country + locality + strata + Comments,data=photoquadrat_visual_long, FUN = function(x) { c(mean = mean(x),SD=sd(x),SE = sqrt(var(x)/length(x)))})


#Abundant CATAMI categories-------
# Subset more abundant TAXA
#SC,MAEC, MOB,CRB, MAEN
Coverdata_abundant <- subset(Coverdata,CATAMI=="SC"|CATAMI=="MAEC"|CATAMI=="MOB"|CATAMI=="CRB"|CATAMI=="MAEN")


#PLOTS-------

#PLOT type of analysis by country for abundant categories 
CATAMI_1 <- ggplot(Coverdata_abundant,aes(x=strata,y=cover.mean,fill=CATAMI)) + geom_bar(alpha=0.7,stat="identity",color="black",position=position_dodge()) + scale_color_grey() + geom_errorbar(aes(ymin=cover.mean-cover.SE, ymax=cover.mean+cover.SE), width=.2,position=position_dodge(.9)) + theme_bw() + scale_y_continuous(limits = c(0,100))+ scale_x_discrete(limits=c("HIGHTIDE","LOWTIDE"),labels=c("HIGH","LOW"))+ labs(fill = "CATAMI CODE",x = "", y = "Cover (%)", title = "",caption ="Mean + SE") + facet_grid(country~Comments) + theme(legend.position = "bottom",panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + scale_fill_brewer(palette="BrBG")

ggplot(Coverdata_abundant,aes(x=strata,y=cover.mean,fill=CATAMI)) + geom_bar(alpha=0.7,stat="identity",color="black",position=position_dodge()) + scale_color_grey() + geom_errorbar(aes(ymin=cover.mean-cover.SE, ymax=cover.mean+cover.SE), width=.2,position=position_dodge(.9)) + theme_bw() + scale_y_continuous(limits = c(0,100))+ scale_x_discrete(limits=c("HIGHTIDE","LOWTIDE"),labels=c("HIGH","LOW"))+ labs(fill = "CATAMI CODE",x = "", y = "Cover (%)", title = "",caption ="Mean + SE") + facet_grid(locality~Comments) + theme(legend.position = "bottom",panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + scale_fill_brewer(palette="BrBG")
#ggsave(here("Figures", "CATAMI_1.png"),width =12,height =10)


CATAMI_2 <- ggplot(Coverdata_abundant,aes(x=strata,y=cover.mean,fill=Comments)) + geom_bar(alpha=0.7,stat="identity",color="black",position=position_dodge()) + scale_color_grey() + geom_errorbar(aes(ymin=cover.mean-cover.SE, ymax=cover.mean+cover.SE), width=.2,position=position_dodge(.9)) + theme_bw() + scale_y_continuous(limits = c(0,100))+ scale_x_discrete(limits=c("HIGHTIDE","LOWTIDE"),labels=c("HIGH","LOW")) + facet_grid(country~CATAMI) + labs(fill = "Method",x = "", y = "Cover (%)", title = "",caption ="Mean + SE") + theme(legend.position = "bottom",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank())  + scale_fill_brewer(palette="BrBG")

ggsave(here("Figures", "CATAMI_2.png"),width =12,height =10)




#BOXPLOT
FG_boxplot <- ggplot(data=photoquadrat_visual_long, mapping=aes(x=strata, y=cover,fill=Comments)) +geom_boxplot()   + theme_bw() + scale_y_continuous(limits = c(0,100))+ scale_x_discrete(limits=c("HIGHTIDE","LOWTIDE"),labels=c("HIGH","LOW"))  + labs(fill = "Method",x = "", y = "Cover (%)", title = "",caption ="Mean + SE") + theme(legend.position = "bottom",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank()) + scale_fill_brewer(palette="BrBG") + facet_grid(.~CATAMI)

ggsave(here("Figures", "FG_boxplot.png"),width =12,height =10)

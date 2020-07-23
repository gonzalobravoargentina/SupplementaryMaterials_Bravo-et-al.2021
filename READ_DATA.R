#CORALNET Source folders contains files with results:
# annotations.csv
# metadata.csv
# percent_cover.csv

#Visual quadrats folder has results from insitu "visual_quadrats"
# argentina_visual.csd
# colombia_visual.csv
# galapagos_visula.csv

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

library(doBy)
library(reshape)
library(dplyr)
library(data.table)


#Read data SOURCE MBON--------
# Source = https://coralnet.ucsd.edu/source/1972/

#Set the working directory to the folder "Source_MBON_P2P" and read percent_cover.csv and metdata.csv files (downloaded "as it" from Coralnet source)
library(here)
MBON.photoquadrat.cover <- read.csv(here("Source_MBON_P2P", "percent_covers.csv"))
MBON.photoquadrat.metadata <- read.csv(here("Source_MBON_P2P", "metadata.csv"))

#Merge photoquadrat.metadata and photoquadrat.cover
MBON.photoquadrat<- merge(MBON.photoquadrat.metadata,MBON.photoquadrat.cover, by = "Name", all.x = TRUE) 

#CHECK wich date format you have in the files. Sometimes if you open the file with excel it may change the date format 
#transform to date format
MBON.photoquadrat$Date <- as.Date(MBON.photoquadrat$Date,'%Y-%m-%d')
#MBON.photoquadrat$Date <- as.Date(MBON.photoquadrat$Date,'%d/%m/%Y')

#Eliminate colums with taxa categories (CATAMI) that were not used for the photoquadrats
#as this codes search colsum= 0 in all the data frame , and removo those columns, we need to set lat ang long that have negative numbers in order to avoid being removed
MBON.photoquadrat$Latitude <- as.character(MBON.photoquadrat$Latitude)
MBON.photoquadrat$Longitude <- as.character(MBON.photoquadrat$Longitude)
library(tidyverse) 
MBON.photoquadrat <- MBON.photoquadrat %>% select_if(negate(function(col) is.numeric(col) && sum(col) <= 0))



#Read photoquadrats data from individual sources in COUNTRIES

#Source_Galapagos ECUADOR(EC)------
#https://coralnet.ucsd.edu/source/1949/

#Set the working directory to the folder "Source_Galapagos" and read percent_cover.csv and metdata.csv files (downloaded "as it" from Coralnet source)
library(here)
EC.photoquadrat.cover <- read.csv(here("Source_Galapagos","percent_covers.csv"))
EC.photoquadrat.metadata <- read.csv(here("Source_Galapagos","metadata.csv"))

#Merge photoquadrat.metadata and photoquadrat.cover
EC.photoquadrat<- merge(EC.photoquadrat.metadata,EC.photoquadrat.cover, by = "Name", all.x = TRUE) 

#CHECK wich date format you have in the files. Sometimes if you open the file with excel it may change the date format 
#transform to date format
EC.photoquadrat$Date <- as.Date(EC.photoquadrat$Date,'%Y-%m-%d')
#EC.photoquadrat$Date <- as.Date(EC.photoquadrat$Date,'%d/%m/%Y')

#Eliminate colums with taxa categories (CATAMI) that were not used for the photoquadrats
#as this codes search colsum= 0 in all the data frame , and removo those columns, we need to set lat ang long that have negative numbers in order to avoid being removed
EC.photoquadrat$Latitude <- as.character(EC.photoquadrat$Latitude)
EC.photoquadrat$Longitude <- as.character(EC.photoquadrat$Longitude)
library(tidyverse) 
EC.photoquadrat <- EC.photoquadrat %>% select_if(negate(function(col) is.numeric(col) && sum(col) <= 0))



#Source_Colombia (CO)---------
#https://coralnet.ucsd.edu/source/1983/

#Set the working directory to the folder "Source_Colombia" and read percent_cover.csv and metdata.csv files (downloaded "as it" from Coralnet source)
library(here)
CO.photoquadrat.cover <- read.csv(here("Source_Colombia","percent_covers.csv"))
CO.photoquadrat.metadata <- read.csv(here("Source_Colombia","metadata.csv"))


#Merge photoquadrat.metadata and photoquadrat.cover
CO.photoquadrat<- merge(CO.photoquadrat.metadata,CO.photoquadrat.cover, by = "Name", all.x = TRUE) 

#CHCOK wich date format you have in the files. Sometimes if you open the file with excel it may change the date format 
#transform to date format
CO.photoquadrat$Date <- as.Date(CO.photoquadrat$Date,'%Y-%m-%d')
#CO.photoquadrat$Date <- as.Date(CO.photoquadrat$Date,'%d/%m/%Y')

#Eliminate colums with taxa categories (CATAMI) that were not used for the photoquadrats
#as this codes search colsum= 0 in all the data frame , and removo those columns, we need to set lat ang long that have negative numbers in order to avoid being removed
CO.photoquadrat$Latitude <- as.character(CO.photoquadrat$Latitude)
CO.photoquadrat$Longitude <- as.character(CO.photoquadrat$Longitude)
library(tidyverse) 
CO.photoquadrat <- CO.photoquadrat %>% select_if(negate(function(col) is.numeric(col) && sum(col) <= 0))



#Source_Argentina (AR)---------
#https://coralnet.ucsd.edu/source/1988/

#Set the working directory to the folder "Source_Argentina" and read percent_cover.csv and metdata.csv files (downloaded "as it" from Coralnet source)
library(here)
AR.photoquadrat.cover <- read.csv(here("Source_Argentina","percent_covers.csv"))
AR.photoquadrat.metadata <- read.csv(here("Source_Argentina","metadata.csv"))


#Merge photoquadrat.metadata and photoquadrat.cover
AR.photoquadrat<- merge(AR.photoquadrat.metadata,AR.photoquadrat.cover, by = "Name", all.x = TRUE) 

#CHARK wich date format you have in the files. Sometimes if you open the file with excel it may change the date format 
#transform to date format
AR.photoquadrat$Date <- as.Date(AR.photoquadrat$Date,'%Y-%m-%d')
#AR.photoquadrat$Date <- as.Date(AR.photoquadrat$Date,'%d/%m/%Y')

#Eliminate colums with taxa categories (CATAMI) that were not used for the photoquadrats
#as this codes search colsum= 0 in all the data frame , and removo those columns, we need to set lat ang long that have negative numbers in order to avoid being removed
AR.photoquadrat$Latitude <- as.character(AR.photoquadrat$Latitude)
AR.photoquadrat$Longitude <- as.character(AR.photoquadrat$Longitude)
library(tidyverse) 
AR.photoquadrat <- AR.photoquadrat %>% select_if(negate(function(col) is.numeric(col) && sum(col) <= 0))



#READ VISUAL QUADRAT DATA--------------------------------------------------------

#Set the working directory to the folder "Visual quadrats"
library(here)
#These file were prepared to have the metadata on it. date format must be as  YYYY-mm-dd. 
AR.visual <- read.csv(here("Visual_quadrats","argentina_visual.csv"))
AR.visual$Date <- as.Date(AR.visual$Date,'%d/%m/%Y')
CO.visual <- read.csv(here("Visual_quadrats","colombia_visual.csv"))
CO.visual$Date <- as.Date(CO.visual$Date,'%d/%m/%Y')
EC.visual <- read.csv(here("Visual_quadrats","galapagos_visual.csv"))
EC.visual$Date <- as.Date(EC.visual$Date,'%d/%m/%Y')

#joint all visula quadrat dataframes
visual.ALL <- dplyr::bind_rows(AR.visual, CO.visual, EC.visual)
#eliminated NA and add 0 
visual.ALL[is.na(visual.ALL)] <- 0




#PREPARE DATA FOR ANALYSIS---------------------------------------------------------
#Joint photo and visual dataframes
photoquadrat_visual <- rbindlist(list(visual.ALL, MBON.photoquadrat,AR.photoquadrat,CO.photoquadrat,EC.photoquadrat), fill = TRUE)
#replace NA from unc to cero values
photoquadrat_visual[is.na(photoquadrat_visual)] <- 0


#write.csv(photoquadrat_visual,file="photoquadrat_visual.csv")


#Annotation (types of analysis) in comments column 
photoquadrat_visual$Comments <- as.factor(photoquadrat_visual$Comments)
levels(photoquadrat_visual$Comments) <-  c("Alleviate 50%","Human", "Photoquadrat.R","Robot","Robot.bycountry","Visualquadrat")

#change the  order of factor levels
photoquadrat_visual$Comments <- factor(photoquadrat_visual$Comments, levels =c("Photoquadrat.R","Human","Robot","Robot.bycountry","Alleviate 50%","Visualquadrat"))


#Create long type dataframe 
photoquadrat_visual_long = melt(photoquadrat_visual, id.vars = 1:20, measure.vars = 21:ncol(photoquadrat_visual), variable_name = "CATAMI", value_name ="cover", na.rm = T)
#rename columns because the ontop command is not working 
colnames(photoquadrat_visual_long)[21] <- "CATAMI"
colnames(photoquadrat_visual_long)[22] <- "cover"

#Calculate mean, SD, SE for cover data by factors (species=Shortname,site, strata,) 
Coverdata <- summaryBy(cover ~ CATAMI + country + strata + Comments,data=photoquadrat_visual_long, FUN = function(x) { c(mean = mean(x),SD=sd(x),SE = sqrt(var(x)/length(x)))})


#Abundant CATAMI categories-------
# Subset more abundant TAXA
#SC,MAEN, MOB, MAF, MAS, MAA
Coverdata_abundant <- subset(Coverdata,CATAMI=="SC"|CATAMI=="MAEN"|CATAMI=="MOB"|CATAMI=="MAF"|CATAMI=="MAS"|CATAMI=="MAA")

#we take out photoquadrats analysed by 100 random points
Coverdata_abundant <- subset(Coverdata_abundant,Comments!="Photoquadrat.R")

#Functional Groups-----------
#Create a dataframe with functional groups ALGAE, SUSTRATE and INVERTEBRATES
photoquadrat_visual.FG <- photoquadrat_visual
photoquadrat_visual.FG$ALGAE <- as.numeric(paste(photoquadrat_visual.FG$MOG+photoquadrat_visual.FG$MAEN+photoquadrat_visual.FG$MAA+photoquadrat_visual.FG$MAF+photoquadrat_visual.FG$MAG+photoquadrat_visual.FG$MALCB+photoquadrat_visual.FG$MAS))
photoquadrat_visual.FG$SUSTRATE <- photoquadrat_visual.FG$SC
photoquadrat_visual.FG$INVERTEBRATES <- as.numeric(paste(photoquadrat_visual.FG$MOB+photoquadrat_visual.FG$CNCA+photoquadrat_visual.FG$CNTR+photoquadrat_visual.FG$CRB+photoquadrat_visual.FG$WPOT))

#Create long type dataframe for FG
photoquadrat_visual_long.FG = melt(photoquadrat_visual.FG, id.vars = 1:20, measure.vars = 35:37, variable_name = "CATAMI", cover_name ="cover", na.rm = T)
colnames(photoquadrat_visual_long.FG)[21] <- "CATAMI"
colnames(photoquadrat_visual_long.FG)[22] <- "cover"

#calculate avg, SD and SE
Coverdata.FG <- summaryBy(cover ~ CATAMI + country + strata + Comments,data=photoquadrat_visual_long.FG , FUN = function(x) { c(mean = mean(x),SD=sd(x),SE = sqrt(var(x)/length(x)))})

#we take out photoquadrats analysed by 100 random points
Coverdata.FG <- subset(Coverdata.FG,Comments!="Photoquadrat.R")

#we take out photoquadrats analysed by 100 random points
photoquadrat_visual_long.FG2 <- subset(photoquadrat_visual_long.FG,Comments!="Photoquadrat.R")

#READ LABELSET

labelset <- read.csv(here("Labelset","labelset_used.csv"))

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

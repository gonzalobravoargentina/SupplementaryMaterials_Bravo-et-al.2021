#CORALNET Source folders contains files with results:
# annotations.csv
# metadata.csv
# percent_cover.csv

#Visual quadrats folder has results from insitu "visual_quadrats"
# argentina_visual.csd
# colombia_visual.csv
# galapagos_visual.csv
# usa_visual.csv

# In all the files the Comments column will include FACTORS:
# Human.random (Photos annotated by human using 100 random points)
# Human (Photos annotated by human using 100 grid)
# Robot (Photos annotated by robot)
# Robot.SOURCE (Photos annotated by robot in the country source)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

library(doBy)
library(reshape)
library(dplyr)
library(data.table)

#Read data MBON_AR_CO_EC_US_Robot--------
#Photos random and robot annotations from 4 countries 
# Source = https://coralnet.ucsd.edu/source/2048/

#Set the working directory to the folder "Source_MBON_AR_CO_EC_US_Robot" and read percent_cover.csv and metdata.csv files (downloaded "as it" from Coralnet source)
library(here)
PQ.ALL.cover <- read.csv(here("Source_MBON_AR_CO_EC_US_Robot", "percent_covers.csv"))
PQ.ALL.metadata <- read.csv(here("Source_MBON_AR_CO_EC_US_Robot", "metadata.csv"))

#Merge photoquadrat.metadata and photoquadrat.cover
PQ.ALL<- merge(PQ.ALL.metadata,PQ.ALL.cover, by = "Name", all.x = TRUE) 

#Remove original data frames from enviroment
rm(PQ.ALL.cover)
rm(PQ.ALL.metadata)

#CHECK wich date format you have in the files. Sometimes if you open the file with excel it may change the date format 
#transform to date format
PQ.ALL$Date <- as.Date(PQ.ALL$Date,'%Y-%m-%d')
#PQ.ALL$Date <- as.Date(PQ.ALL$Date,'%d/%m/%Y')

#Eliminate colums with taxa categories (CATAMI) that were not used for the photoquadrats
#this codes search colsum= 0 in all the data frame , and remove those columns, we need to set lat ang long that have negative numbers in order to avoid being removed
PQ.ALL$Water.quality <- "INTERTIDAL"
PQ.ALL$Depth <- "INTERTIDAL"
PQ.ALL$White.balance.card <- "NONE"
PQ.ALL$Strobes <- "NONE"
PQ.ALL$Height..cm. <- as.character(PQ.ALL$Height..cm.)
PQ.ALL$Latitude <- as.character(PQ.ALL$Latitude)
PQ.ALL$Longitude <- as.character(PQ.ALL$Longitude)
library(tidyverse) 
PQ.ALL <- PQ.ALL %>% select_if(negate(function(col) is.numeric(col) && sum(col) <= 0))

PQ.ALL$Comments <- as.factor(PQ.ALL$Comments)
levels(PQ.ALL$Comments) <-  c("PQ.Human.Random", "PQ.Robot")


#Read photoquadrats data from individual sources of each COUNTRY (random and robot)
#Source_MBON_Galapagos ECUADOR(EC)-----
#https://coralnet.ucsd.edu/source/1949/

#Set the working directory to the folder "Source_MBON_Galapagos" and read percent_cover.csv and metdata.csv files (downloaded "as it" from Coralnet source)
library(here)
PQ.EC.cover <- read.csv(here("Source_MBON_Galapagos","percent_covers.csv"))
PQ.EC.metadata <- read.csv(here("Source_MBON_Galapagos","metadata.csv"))

#Merge photoquadrat.metadata and photoquadrat.cover
PQ.EC<- merge(PQ.EC.metadata,PQ.EC.cover, by = "Name", all.x = TRUE) 

#Remove original data frames from enviroment
rm(PQ.EC.cover)
rm(PQ.EC.metadata)

#CHECK wich date format you have in the files. Sometimes if you open the file with excel it may change the date format 
#transform to date format
PQ.EC$Date <- as.Date(PQ.EC$Date,'%Y-%m-%d')
#PQ.EC$Date <- as.Date(PQ.EC$Date,'%d/%m/%Y')

#Eliminate colums with taxa categories (CATAMI) that were not used for the photoquadrats
#as this codes search colsum= 0 in all the data frame , and removo those columns, we need to set lat ang long that have negative numbers in order to avoid being removed
PQ.EC$Latitude <- as.character(PQ.EC$Latitude)
PQ.EC$Longitude <- as.character(PQ.EC$Longitude)
PQ.EC$Depth <- "INTERTIDAL"
PQ.EC$Water.quality <- "INTERTIDAL"
PQ.EC$Strobes <- "NONE"
PQ.EC$White.balance.card <- "NONE"
library(tidyverse) 
PQ.EC <- PQ.EC %>% select_if(negate(function(col) is.numeric(col) && sum(col) <= 0))

PQ.EC$Comments <- as.factor(PQ.EC$Comments)
levels(PQ.EC$Comments) <-  c("PQ.Human.Random", "PQ.Robot.bycountry")


#Source_Colombia (CO)---------
#https://coralnet.ucsd.edu/source/1983/

#Set the working directory to the folder "Source_Colombia" and read percent_cover.csv and metdata.csv files (downloaded "as it" from Coralnet source)
library(here)
PQ.CO.cover <- read.csv(here("Source_Colombia","percent_covers.csv"))
PQ.CO.metadata <- read.csv(here("Source_Colombia","metadata.csv"))


#Merge photoquadrat.metadata and photoquadrat.cover
PQ.CO<- merge(PQ.CO.metadata,PQ.CO.cover, by = "Name", all.x = TRUE) 

#Remove original data frames from enviroment
rm(PQ.CO.cover)
rm(PQ.CO.metadata)

#Check wich date format you have in the files. Sometimes if you open the file with excel it may change the date format 
#transform to date format
PQ.CO$Date <- as.Date(PQ.CO$Date,'%Y-%m-%d')
#PQ.CO$Date <- as.Date(PQ.CO$Date,'%d/%m/%Y')

#Eliminate colums with taxa categories (CATAMI) that were not used for the photoquadrats
#as this codes search colsum= 0 in all the data frame , and removo those columns, we need to set lat ang long that have negative numbers in order to avoid being removed
PQ.CO$Latitude <- as.character(PQ.CO$Latitude)
PQ.CO$Longitude <- as.character(PQ.CO$Longitude)
PQ.CO$Water.quality <- "INTERTIDAL"
PQ.CO$Depth <- "INTERTIDAL"
PQ.CO$Strobes <- "NONE"
PQ.CO$White.balance.card <- "NONE"
library(tidyverse) 
PQ.CO <- PQ.CO %>% select_if(negate(function(col) is.numeric(col) && sum(col) <= 0))

PQ.CO$Comments <- as.factor(PQ.CO$Comments)
levels(PQ.CO$Comments) <-  c("PQ.Human.Random", "PQ.Robot.bycountry")


#Source_Argentina (AR)---------
#https://coralnet.ucsd.edu/source/1988/

#Set the working directory to the folder "Source_Argentina" and read percent_cover.csv and metdata.csv files (downloaded "as it" from Coralnet source)
library(here)
PQ.AR.cover <- read.csv(here("Source_Argentina","percent_covers.csv"))
PQ.AR.metadata <- read.csv(here("Source_Argentina","metadata.csv"))


#Merge photoquadrat.metadata and photoquadrat.cover
PQ.AR<- merge(PQ.AR.metadata,PQ.AR.cover, by = "Name", all.x = TRUE) 

#Remove original data frames from enviroment
rm(PQ.AR.cover)
rm(PQ.AR.metadata)

#CHECK wich date format you have in the files. Sometimes if you open the file with excel it may change the date format 
#transform to date format
PQ.AR$Date <- as.Date(PQ.AR$Date,'%Y-%m-%d')
#PQ.AR$Date <- as.Date(PQ.AR$Date,'%d/%m/%Y')

#Eliminate colums with taxa categories (CATAMI) that were not used for the photoquadrats
#as this codes search colsum= 0 in all the data frame , and removo those columns, we need to set lat ang long that have negative numbers in order to avoid being removed
PQ.AR$Latitude <- as.character(PQ.AR$Latitude)
PQ.AR$Longitude <- as.character(PQ.AR$Longitude)
PQ.AR$Depth<- "INTERTIDAL"
PQ.AR$Strobes<- "NONE"
PQ.AR$White.balance.card<- "NONE"
library(tidyverse) 
PQ.AR <- PQ.AR %>% select_if(negate(function(col) is.numeric(col) && sum(col) <= 0))

PQ.AR$Comments <- as.factor(PQ.AR$Comments)
levels(PQ.AR$Comments) <-  c("PQ.Human.Random", "PQ.Robot.bycountry")


#Source_USA (US)---------
#https://coralnet.ucsd.edu/source/1997/

#Set the working directory to the folder "Source_Argentina" and read percent_cover.csv and metdata.csv files (downloaded "as it" from Coralnet source)
library(here)
PQ.US.cover <- read.csv(here("Source_USA/Source_MBON_USA","percent_covers.csv"))
PQ.US.metadata <- read.csv(here("Source_USA/Source_MBON_USA","metadata.csv"))


#Merge photoquadrat.metadata and photoquadrat.cover
PQ.US<- merge(PQ.US.metadata,PQ.US.cover, by = "Name", all.x = TRUE) 

#Remove original data frames from enviroment
rm(PQ.US.cover)
rm(PQ.US.metadata)

#CHECK wich date format you have in the files. Sometimes if you open the file with excel it may change the date format 
#transform to date format
PQ.US$Date <- as.Date(PQ.US$Date,'%Y-%m-%d')
#PQ.US$Date <- as.Date(PQ.US$Date,'%d/%m/%Y')

#Eliminate colums with taxa categories (CATAMI) that were not used for the photoquadrats
#as this codes search colsum= 0 in all the data frame , and removo those columns, we need to set lat ang long that have negative numbers in order to avoid being removed
PQ.US$Depth <- "INTERTIDAL"
PQ.US$White.balance.card <- "NONE"
PQ.US$Strobes <- "NONE"
PQ.US$Latitude <- as.character(PQ.US$Latitude)
PQ.US$Longitude <- as.character(PQ.US$Longitude)
library(tidyverse) 
PQ.US <- PQ.US %>% select_if(negate(function(col) is.numeric(col) && sum(col) <= 0))

PQ.US$Comments <- as.factor(PQ.US$Comments)
levels(PQ.US$Comments) <-  c("PQ.Human.Random", "PQ.Robot.bycountry")      


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
US.visual <- read.csv(here("Visual_quadrats","usa_visual.csv"))
US.visual$Date <- as.Date(EC.visual$Date,'%m/%d/%Y')

#joint all visual quadrat dataframes
VQ.Human.ALL <- dplyr::bind_rows(AR.visual, CO.visual, EC.visual,US.visual)
#eliminated NA and add 0 
VQ.Human.ALL[is.na(VQ.Human.ALL)] <- 0

#remove data frames with visual data from each country 
rm(AR.visual,CO.visual,EC.visual,US.visual)

VQ.Human.ALL$Comments <- as.factor(VQ.Human.ALL$Comments)
levels(VQ.Human.ALL$Comments) <-  c("VQ.Human")



#READ Photoquadrats by HUMAN-------- 
PQ.Human.ALL <- read.csv(here("Human_photoquadrats","photoquadrat_human_AR_EC_CO_US.csv"))
#PQ.Human.ALL$Date <- as.Date(PQ.Human.ALL$Date,'%Y-%m-%d')
PQ.Human.ALL$Date <- as.Date(PQ.Human.ALL$Date,'%d/%m/%Y')
PQ.Human.ALL$Comments <- as.factor(PQ.Human.ALL$Comments)
levels(PQ.Human.ALL$Comments) <-  c("PQ.Human")

#write.csv(PQ.Human.ALL,file="PQ.Human.ALL.csv", row.names=FALSE)


#PREPARE DATA FOR ANALYSIS---------------------------------------------------------
#Joint photo and visual dataframes
PQ_VQ <- rbindlist(list(VQ.Human.ALL,PQ.Human.ALL,PQ.ALL,PQ.AR,PQ.CO,PQ.EC,PQ.US), fill = TRUE)



#replace NA with cero values
PQ_VQ[is.na(PQ_VQ)] <- 0

# We take out PQ.Human.Random level , because was used only for training the robot
PQ_VQ <- droplevels(PQ_VQ[!PQ_VQ$Comments == 'PQ.Human.Random',])


#Annotation (types of analysis) in comments column 
PQ_VQ$Comments <- as.factor(PQ_VQ$Comments)


#change the  order of factor levels
PQ_VQ$Comments <- factor(PQ_VQ$Comments, levels =c("PQ.Human", "PQ.Robot","PQ.Robot.bycountry","VQ.Human"))



#unc + SHAD = NODATA
PQ_VQ$NODATA <- as.numeric(paste(PQ_VQ$Unc + PQ_VQ$SHAD))

PQ_VQ <- PQ_VQ %>% 
  select(-Unc,-SHAD,-NODATA)




#Create long type dataframe 
PQ_VQ_long = melt(PQ_VQ, id.vars = 1:20, measure.vars = 21:ncol(PQ_VQ), variable_name = "CATAMI", value_name ="cover", na.rm = T)
#rename columns because the ontop command is not working 
colnames(PQ_VQ_long)[21] <- "CATAMI"
colnames(PQ_VQ_long)[22] <- "cover"


## calculate number of points used for % (100- NODATA points)
nIntersecciones = PQ_VQ_long %>% group_by(Name,Comments) %>% 
  summarise(totalIntersecciones = sum(cover, na.rm=T))

PQ_VQ_long = left_join(PQ_VQ_long, nIntersecciones)
PQ_VQ_long$cover.rel = round(100*PQ_VQ_long$cover/PQ_VQ_long$totalIntersecciones, 1)


#Calculate mean, SD, SE for cover data by factors 
Coverdata <- summaryBy(cover + cover.rel ~ CATAMI + country + strata + Comments,data=PQ_VQ_long, FUN = function(x) { c(mean = mean(x),SD=sd(x),SE = sqrt(var(x)/length(x)))})


#Abundant CATAMI categories-------
# Subset more abundant TAXA from all sources 
#SC,MAEN, MOB, MAF, MAS, MAA,MAEC,CRB
Coverdata_abundant <- subset(Coverdata,CATAMI=="SC"|CATAMI=="MAEN"|CATAMI=="MOB"|CATAMI=="MAF"|CATAMI=="MAS"|CATAMI=="MAA"|CATAMI=="MAEC"|CATAMI=="CRB")


#Functional Groups-----------
#Create a dataframe with functional groups ALGAE, SUSTRATE and INVERTEBRATES

PQ_VQ.FG <- PQ_VQ
#MAF,MAEN,MAA,MAG,MAS,MALCB,MAEC,MAEF,MALA
PQ_VQ.FG$ALGAE <- as.numeric(paste(PQ_VQ.FG$MAF+PQ_VQ.FG$MAEN+PQ_VQ.FG$MAA+PQ_VQ.FG$MAG+PQ_VQ.FG$MAS+PQ_VQ.FG$MALCB+PQ_VQ.FG$MAEC+PQ_VQ.FG$MAEF+PQ_VQ.FG$MALA))
PQ_VQ.FG$SUBSTRATE <- PQ_VQ.FG$SC
#CRB,MOB,CNTR,WPOT,MOG,CNCA,BRY,CR,MOCH
PQ_VQ.FG$INVERTEBRATES <- as.numeric(paste(PQ_VQ.FG$CRB+PQ_VQ.FG$MOB+PQ_VQ.FG$CNTR+PQ_VQ.FG$WPOT+PQ_VQ.FG$MOG+ PQ_VQ.FG$CNCA+PQ_VQ.FG$BRY+PQ_VQ.FG$CR+PQ_VQ.FG$MOCH))

#Create long type dataframe for FG
PQ_VQ_long.FG = melt(PQ_VQ.FG, id.vars = 1:20, measure.vars = 40:42, variable_name = "CATAMI", cover_name ="cover", na.rm = T)
colnames(PQ_VQ_long.FG)[21] <- "CATAMI"
colnames(PQ_VQ_long.FG)[22] <- "cover"

##number of points in PHOTO (100-Unc)
nIntersecciones = PQ_VQ_long.FG %>% group_by(Name,Comments) %>% 
  summarise(totalIntersecciones = sum(cover, na.rm=T))
## y calculo la cobertura relativa al total de intersecciones en la cuadrata
PQ_VQ_long.FG = left_join(PQ_VQ_long.FG, nIntersecciones)
PQ_VQ_long.FG$cover.rel = round(100*PQ_VQ_long.FG$cover/PQ_VQ_long.FG$totalIntersecciones, 1)

rm(nIntersecciones)

#calculate avg, SD and SE
Coverdata.FG <- summaryBy(cover + cover.rel ~ CATAMI + country + strata + Comments,data=PQ_VQ_long.FG , FUN = function(x) { c(mean = mean(x),SD=sd(x),SE = sqrt(var(x)/length(x)))})


#READ LABELSET

labelset <- read.csv(here("Labelset","labelset_used.csv"))
#knitr::kable(labelset)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


#create matrix for correlation 
correlations<- merge(PQ.ALL,VQ.Human.ALL, by = "Name",suffixes = c(".A",".H")) 
#A= photo H = visual


#Read file with differences between VQ.HUMAN and PQ.ROBOT for Bland-Altman plots
#FG <- read.csv("FG_diff.csv")


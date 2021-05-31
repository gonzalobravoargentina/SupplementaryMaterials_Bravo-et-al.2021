#the source GoM Rocky shore has 203 photos with 100 random points 
#https://coralnet.ucsd.edu/source/1997/

#In order to be able to compare with orther sources we selected 92 photos . This scrip is to extract annotations, metadata an % cover for those 92 photos

# SELECT 92 RANDOM ANNOTATIONS PHOTOS (HUMAN)-----

#Read list of photos 92 selected
selectedphotos <- read.csv(here("Source_USA","selected_photos_92.csv"))

#Read metadata of 204 photos
library(here)
metadata204 <- read.csv(here("Source_USA/204random", "metadata_204.csv"))

#Merge and keep only the 92 selected photos
library(reshape)
metadataselectedphotos<- merge(selectedphotos,metadata204, by = "Name", all.x = TRUE) 
#add info to metadata
metadataselectedphotos$Comments <- "Human.random"
metadataselectedphotos$Water.quality <- "INTERTIDAL"
metadataselectedphotos$Date <- as.Date(metadataselectedphotos$Date,'%d/%m/%Y')

#write.csv(metadataselectedphotos,file = "metadata.csv")

#read metadata from others countries
metadataothers <- read.csv("metadata_AR_CO_EC.csv")
metadataothers$Comments <- "random"
metadataothers$Date <- as.Date(metadataothers$Date,'%m/%d/%Y')

# all metadata together for source MBON (~ 90 photos with random pints of each country)
library(dplyr)
metadata <- dplyr::bind_rows(metadataothers, metadataselectedphotos)
#metadata$Date <- as.Date(metadata$Date,'%Y-%m-%d')

#write csv 
write.csv(metadata,file = "metadata_MBONsource.csv")


#annotatios 204 photos source GoM
library(here)
annotations204 <- read.csv(here("Source_USA/204random", "annotations204.csv"))
#subset anly annotaions for 92 selected photos
annotations92<- merge(selectedphotos,annotations204, by = "Name") 
write.csv(annotations92,file = "annotations.csv")

#annotatios others countries
annotationsothers <- read.csv("annotations_AR_CO_EC-corrected.csv")

#merge annotation AR CO EC and GoM
annotaions_MBONsource <- dplyr::bind_rows(annotations92, annotationsothers)
write.csv(annotaions_MBONsource,file = "anntotaions_MBONsource.csv")


#% cover
library(here)
percentcover204 <- read.csv(here("Source_USA/204random", "percent_covers204.csv"))
#Merge and keep only the 92 selected photos
library(reshape)
percentcoverselectedphotos<- merge(selectedphotos,percentcover204, by = "Name", all.x = TRUE)
write.csv(percentcoverselectedphotos,file = "percent_covers.csv")



#SELECT 92 GRID ANNOTATIONS PHOTOS (HUMAN)-----
#source https://coralnet.ucsd.edu/source/2028/
#Read list of photos 92 selected
selectedphotos <- read.csv(here("Source_USA","selected_photos_92.csv"))
colnames(selectedphotos) <- "Name2"

#Read metadata of 203 photos human
library(here)
metadata203 <- read.csv(here("Source_USA/203human", "metadata.csv"))

#Merge and keep only the 92 selected photos
library(reshape)
metadataselectedphotos<- merge(selectedphotos,metadata203, by = "Name2", all.x = TRUE) 
#add info to metadata
metadataselectedphotos$Comments <- "Human"
metadataselectedphotos$Water.quality <- "INTERTIDAL"
metadataselectedphotos$Date <- as.Date(metadataselectedphotos$Date,'%d/%m/%Y')

#elimanate NAME1 
metadataselectedphotos <- metadataselectedphotos[,2:19]
colnames(metadataselectedphotos)[1] <- "Name"

#Write metadata file 
#write.csv(metadataselectedphotos,file = "metadata.csv",row.names=FALSE)


#% cover
library(here)
percentcover203 <- read.csv(here("Source_USA/203human", "percent_covers.csv"))
#Merge and keep only the 92 selected photos
library(reshape)
percentcoverselectedphotos<- merge(selectedphotos,percentcover203, by = "Name2", all.x = TRUE)

percentcoverselectedphotos <- percentcoverselectedphotos[,2:32]
colnames(percentcoverselectedphotos)[1] <- "Name"

#write.csv(percentcoverselectedphotos,file = "percent_covers.csv",row.names=FALSE)





#subset 184 (.jpg and _robot.jpg) from 406 photos from source: https://coralnet.ucsd.edu/source/1997/

#Read list of 184 photos  selected
selectedphotos <- read.csv(here("Source_USA","selected_photos_184.csv"))

#Read metadata of 406 photos
library(here)
metadata406 <- read.csv(here("Source_USA/406_random_robot", "metadata.csv"))

#Merge and keep only the 184 selected photos
library(reshape)
metadataselectedphotos<- merge(selectedphotos,metadata406, by = "Name", all.x = TRUE) 
#add info to metadata
metadataselectedphotos$Comments <- c("Robot.SOURCE","Human.random")
metadataselectedphotos$Water.quality <- "INTERTIDAL"
metadataselectedphotos$Depth <- "INTERTIDAL"
metadataselectedphotos$Strobes <- "NONE"
metadataselectedphotos$White.balance.card <- "NONE"
metadataselectedphotos$Date <- as.Date(metadataselectedphotos$Date,'%Y-%m-%d')

write.csv(metadataselectedphotos,file = "metadata.csv",row.names=FALSE)

#annotations 406 photos source GoM
library(here)
annotations406 <- read.csv(here("Source_USA/406_random_robot", "annotations.csv"))
#subset anly annotaions for 92 selected photos
annotations184<- merge(selectedphotos,annotations406, by = "Name") 


write.csv(annotations184,file = "annotations.csv",row.names=FALSE)


#% cover
library(here)
percentcover406 <- read.csv(here("Source_USA/406_random_robot", "percent_covers.csv"))
#Merge and keep only the 184 selected photos
library(reshape)
percentcoverselectedphotos<- merge(selectedphotos,percentcover406, by = "Name", all.x = TRUE)
write.csv(percentcoverselectedphotos,file = "percent_covers.csv",row.names=FALSE)




#HUMAN QUADRATS------
#Create Human_photoquadrats data with those photos analysed by human using 100 points grid
#Set the working directory to the folder "Source_MBON_AR_CO_EC" and read percent_cover.csv and metdata.csv files (downloaded "as it" from Coralnet source)
#https://coralnet.ucsd.edu/source/1972
library(here)
photoquadrat_human.cover <- read.csv(here("Source_MBON_AR_CO_EC", "percent_covers.csv"))
photoquadrat_human.metadata <- read.csv(here("Source_MBON_AR_CO_EC", "metadata.csv"))

#Merge photoquadrat.metadata and photoquadrat.cover
photoquadrat_human<- merge(photoquadrat_human.metadata,photoquadrat_human.cover, by = "Name", all.x = TRUE) 

#CHECK wich date format you have in the files. Sometimes if you open the file with excel it may change the date format 
#transform to date format
photoquadrat_human$Date <- as.Date(photoquadrat_human$Date,'%Y-%m-%d')
#photoquadrat_human$Date <- as.Date(photoquadrat_human$Date,'%d/%m/%Y')

#Eliminate colums with taxa categories (CATAMI) that were not used for the photoquadrats
#as this codes search colsum= 0 in all the data frame , and remove those columns, we need to set lat ang long that have negative numbers in order to avoid being removed
photoquadrat_human$Latitude <- as.character(photoquadrat_human$Latitude)
photoquadrat_human$Longitude <- as.character(photoquadrat_human$Longitude)
library(tidyverse) 
photoquadrat_human <- photoquadrat_human %>% select_if(negate(function(col) is.numeric(col) && sum(col) <= 0))

photoquadrat_human_AR_EC_CO <- subset(photoquadrat_human,Comments=="Human")

rm(photoquadrat_human)
rm(photoquadrat_human.metadata)
rm(photoquadrat_human.cover)

#USA
library(here)
photoquadrat_human.cover.US <- read.csv(here("Source_USA/92human", "percent_covers.csv"))
photoquadrat_human.metadata.US <- read.csv(here("Source_USA/92human", "metadata.csv"))

#Merge photoquadrat.metadata and photoquadrat.cover
photoquadrat_human.US<- merge(photoquadrat_human.metadata.US,photoquadrat_human.cover.US, by = "Name", all.x = TRUE) 

rm(photoquadrat_human.cover.US)
rm(photoquadrat_human.metadata.US)


#CHECK wich date format you have in the files. Sometimes if you open the file with excel it may change the date format 
#transform to date format
#photoquadrat_human.US$Date <- as.Date(photoquadrat_human.US$Date,'%Y-%m-%d')
photoquadrat_human.US$Date <- as.Date(photoquadrat_human.US$Date,'%d/%m/%Y')

#Eliminate colums with taxa categories (CATAMI) that were not used for the photoquadrats
#as this codes search colsum= 0 in all the data frame , and remove those columns, we need to set lat ang long that have negative numbers in order to avoid being removed
photoquadrat_human.US$Latitude <- as.character(photoquadrat_human.US$Latitude)
photoquadrat_human.US$Longitude <- as.character(photoquadrat_human.US$Longitude)
library(tidyverse) 
photoquadrat_human.US <- photoquadrat_human.US %>% select_if(negate(function(col) is.numeric(col) && sum(col) <= 0))


photoquadrat_human <- dplyr::bind_rows(photoquadrat_human_AR_EC_CO, photoquadrat_human.US)
#eliminated NA and add 0 
photoquadrat_human[is.na(photoquadrat_human)] <- 0

write.csv(photoquadrat_human, file="photoquadrat_human_AR_EC_CO_US.csv",row.names=FALSE)
          

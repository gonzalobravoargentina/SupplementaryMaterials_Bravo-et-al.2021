#the source GoM Rocky shore has 203 photos with 100 random points 
#https://coralnet.ucsd.edu/source/1997/

#In order to be able to compare with orther sources we selected 92 photos . This scrip is to extract annotations, metadata an % cover for those 92 photos
 
#Read list of photos 92 selected
selectedphotos <- read.csv("selected_photos_92.csv")

#Read metadata of 204 photos
library(here)
metadata204 <- read.csv(here("Source_GoM Rocky Shore/204random", "metadata_204.csv"))

#Merge and keep only the 92 selected photos
library(reshape)
metadataselectedphotos<- merge(selectedphotos,metadata204, by = "Name", all.x = TRUE) 
#add info to metadata
metadataselectedphotos$Comments <- "random"
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
annotations204 <- read.csv(here("Source_GoM Rocky Shore/204random", "annotations204.csv"))
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
percentcover204 <- read.csv(here("Source_GoM Rocky Shore/204random", "percent_covers204.csv"))
#Merge and keep only the 92 selected photos
library(reshape)
percentcoverselectedphotos<- merge(selectedphotos,percentcover204, by = "Name", all.x = TRUE)
write.csv(percentcoverselectedphotos,file = "percent_covers.csv")

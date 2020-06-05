library(ggplot2)
library(cowplot)
library(doBy)
library(reshape)
library(dplyr)
library(plotly)
library(ggpubr) 


photo.cover <- read.csv("percent_covers_photoquadrats.csv")
photo.metadata <- read.csv("metadata_photquadrants.csv")

#merge photo.metadata and photo.cover
photo<- merge(photo.metadata,photo.cover, by = "Name", all.x = TRUE) 

#Visual data 
visual <- read.csv("colombia_visual.csv") 

#Joint the two dataframes
photo_visual <- dplyr::bind_rows(photo, visual)
#photo_visual <- rbind(photo, visual)


#Change names of Annotation Status (types of analysis)
photo_visual$Annotation.status <- as.factor(photo_visual$Annotation.status)
levels(photo_visual$Annotation.status) <-  c("Photoquadrat Human", "Photoquadrat Robot","Visual quadrat")

write.csv(photo_visual,file= "photo_visual_dataframe_COLOMBIA.csv")

#Create long type dataframe 
library(reshape)
photo_visual_long = melt(photo_visual, id.vars = 1:20, measure.vars = 21:ncol(photo_visual), variable_name = "CATAMI", value_name ="cover", na.rm = T)


#Script objetives
#- Read data from CoralNet photoquadrats (Confirmed by human and Robot annotations)
#- Read data from Visual quadrats

# Data files were download from CoralNet and the original name was modify adding the source name:
# MBON_Argentina_metadata.csv (#Date,country,locality,site,strata info)
# MBON_Argentina_percent_covers.csv (percent cover)
# MBON_Argentina_annotations.cvs (info of the robot estimations)
# percent_cover_visual_2018.csv (data from visual quadrats)


library(ggplot2)
library(cowplot)
library(doBy)
library(reshape)
library(dplyr)
library(plotly)
library(ggpubr)

#read data
photo.cover <- read.csv("MBON_Argentina_percent_covers.csv")
photo.metadata <- read.csv("MBON_Argentina_metadata.csv")

#merge photo.metadata and photo.cover
photo<- merge(photo.metadata,photo.cover, by = "Name", all.x = TRUE) 

#Visual data 
visual <- read.csv("percent_cover_visual_2018.csv") 

#Joint the two dataframes
photo_visual <- rbind(photo, visual)

#Change names of Annotation Status (types of analysis)
photo_visual$Annotation.status <- as.factor(photo_visual$Annotation.status)
levels(photo_visual$Annotation.status) <-  c("Photoquadrat Human", "Photoquadrat Robot","Visual quadrat")


#nMDS
library(vegan)

#Select site
photo_visual.Site <- subset(photo_visual, site=="PUNTA ESTE")
photo_visual.Site <- photo_visual
#MDS by strata
#create a list with dataframe of strata 
l<-list(subset(photo_visual.Site,strata=="HIGHTIDE"),subset(photo_visual.Site,strata=="MIDTIDE"),subset(photo_visual.Site,strata=="LOWTIDE"))

#nMDS plot
nMDSbystrata <- lapply(l, function (j) { NMDS=metaMDS(sqrt(sqrt(j[,-(1:20)])),k=2,trymax=10,try = 10,distance ="bray",autotransform = FALSE)
NMDS1 <-NMDS$points[,1] 
NMDS2 <- NMDS$points[,2]
MDS.plot<-cbind(j[,-(1:20)], NMDS1, NMDS2,j$Annotation.status) 
ggplot(MDS.plot, aes(NMDS1, NMDS2, color=j$Annotation.status,shape=j$Annotation.status))+geom_point(position=position_jitter(.1),size=3)+stat_ellipse(type='t',size =2) +theme_bw() + theme(legend.position = "bottom",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank()) + annotate("text", x=max(NMDS1)-0.5, y=min(NMDS2)-0.5, label=paste('Stress =',round(NMDS$stress,3)))+ggtitle("") + theme(legend.key.size = unit(4,"line")) + scale_color_grey(name = "Analysis")+ scale_shape_manual(name = "Analysis",values = c(16,17,15,3))+ggtitle(unique(j$strata))
})

#PERMANOVA with transformation
PERMANOVAbystrata_transformed <- lapply(l, function (j) { 
  library(pairwiseAdonis)
  #data transformed
  pairwise.adonis(sqrt(sqrt(j[,-(1:20)])),j$Annotation.status,sim.method ="bray")
})

#PERMANOVA without transformation
PERMANOVAbystrata_NOtransformed <- lapply(l, function (j) { 
  library(pairwiseAdonis)
  #data no transformed
  pairwise.adonis(j[,-(1:20)],j$Annotation.status,sim.method ="bray")
})





#Create long type matrix 
library(reshape)
photo_visual_long = melt(photo_visual, id.vars = 1:20, measure.vars = 21:ncol(photo_visual), variable_name = "CATAMI", value_name ="cover", na.rm = T)


#Calculate mean, SD, SE for cover data by factors (species=Shortname,site, strata,) 
library(doBy)
Coverdata <- summaryBy(value ~ CATAMI + site + strata + Annotation.status,data=photo_visual_long, FUN = function(x) { c(mean = mean(x),SD=sd(x),SE = sqrt(var(x)/length(x)))})


#plot
library(ggplot2)
p <- ggplot(Coverdata,aes(x=strata,y=value.mean,fill=CATAMI)) + geom_bar(alpha=0.7,stat="identity",color="black",position=position_dodge()) + scale_color_grey() + geom_errorbar(aes(ymin=value.mean-value.SE, ymax=value.mean+value.SE), width=.2,position=position_dodge(.9)) + theme_bw() + scale_y_continuous(limits = c(0,100))+ scale_x_discrete(limits=c("HIGHTIDE","MIDTIDE","LOWTIDE"),labels=c("HIGHT","MID","LOW"))+ labs(fill = "CATAMI CODE",x = "", y = "Cover (%)", title = "",caption ="Mean + SE") + facet_grid(site~Annotation.status)

library(plotly)
ggplotly(p)

# Subset more abundant 
#MOB, MAFG,MAFR, MASG, SC, MAA, 
Coverdata_abundant <- subset(Coverdata,CATAMI=="MOB"|CATAMI=="MAFG"|CATAMI=="MASG"|CATAMI=="MAFR"|CATAMI=="MAFG"|CATAMI=="SC"|CATAMI=="MAA")

ggplot(Coverdata_abundant,aes(x=strata,y=value.mean,fill=CATAMI)) + geom_bar(alpha=0.7,stat="identity",color="black",position=position_dodge()) + scale_color_grey() + geom_errorbar(aes(ymin=value.mean-value.SE, ymax=value.mean+value.SE), width=.2,position=position_dodge(.9)) + theme_bw() + scale_y_continuous(limits = c(0,100))+ scale_x_discrete(limits=c("HIGHTIDE","MIDTIDE","LOWTIDE"),labels=c("HIGHT","MID","LOW"))+ labs(fill = "CATAMI CODE",x = "", y = "Cover (%)", title = "",caption ="Mean + SE") + scale_fill_brewer(palette="Greys")+ facet_grid(site~Annotation.status) + theme(legend.position = "bottom",panel.grid.major = element_blank(),panel.grid.minor = element_blank())

library(plotly)
ggplotly(p)


ggplot(Coverdata_abundant,aes(x=strata,y=value.mean,fill=Annotation.status)) + geom_bar(alpha=0.7,stat="identity",color="black",position=position_dodge()) + scale_color_grey() + geom_errorbar(aes(ymin=value.mean-value.SE, ymax=value.mean+value.SE), width=.2,position=position_dodge(.9)) + theme_bw() + scale_y_continuous(limits = c(0,100))+ scale_x_discrete(limits=c("HIGHTIDE","MIDTIDE","LOWTIDE"),labels=c("HIGHT","MID","LOW")) + facet_grid(site~CATAMI) + labs(fill = "Method",x = "", y = "Cover (%)", title = "",caption ="Mean + SE") + theme(legend.position = "bottom",panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +scale_fill_brewer(palette="Greys",name = "Method")


#list with most abundant CATAMI categories
#filter by stata
photo_visual_long_strata <- subset(photo_visual_long,strata=="MIDTIDE" & site=="PUNTA CUEVAS")
listCATAMI <- list(subset(photo_visual_long_strata,CATAMI=="MOB"),subset(photo_visual_long_strata,CATAMI=="MAFG"), subset(photo_visual_long_strata,CATAMI=="MASG"),subset(photo_visual_long_strata, CATAMI=="MAFR"),subset(photo_visual_long_strata,CATAMI=="MAFG"),subset(photo_visual_long_strata, CATAMI=="SC"), subset(photo_visual_long_strata, CATAMI=="MAA"))

names(listCATAMI) <- c("MOB","MAFG","MASG","MAFR","MAFG","SC","MAA")

#Kruskal-Wallis chi-squared 
krusk <- lapply(listCATAMI, function (j) c(kruskal.test(value ~ Annotation.status, data = j),kruskalmc(value ~ Annotation.status, data = j)))

pairwise.wilcox <- lapply(listCATAMI, function (j) (pairwise.wilcox.test(j$value,j$Annotation.status,p.adjust.method = "BH")))

pairwise.wilcox[[1]]$p.adjust.method

pairwise.wilcox$MOB$p.value

library(knitr)
kable(pairwise.wilcox$MOB$p.value,align="l",format = "pandoc",row.names =FALSE,booktabs = T, caption =c("Pairwise comparisons using Wilcoxon rank sum test with continuity correction"))



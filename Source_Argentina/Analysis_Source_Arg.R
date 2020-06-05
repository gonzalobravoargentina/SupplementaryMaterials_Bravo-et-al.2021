#Script objetives
#- Read data from CoralNet photoquadrats (Confirmed by human and Robot annotations)
#- Read data from Visual quadrats

# Data files were download from CoralNet and the original name was modify adding the source name:
# MBON_Argentina_metadata_2018.csv (#Date,country,locality,site,strata info)
# MBON_Argentina_percent_covers_2018.csv (percent cover)
# MBON_Argentina_annotations_2018.csv (info of the robot estimations)
# MBON_Argentina_percent_cover_visual_2018.csv (data from visual quadrats)
# correation_matrix_visualVSrobot.csv (data visual vs robot estimations)

library(ggplot2)
library(cowplot)
library(doBy)
library(reshape)
library(dplyr)
library(plotly)
library(ggpubr) 


#Set the working directory to the folder "Source_Argentina"


#read data
photo.cover <- read.csv("MBON_Argentina_percent_covers_2018.csv")
photo.metadata <- read.csv("MBON_Argentina_metadata_2018.csv")


#merge photo.metadata and photo.cover
photo<- merge(photo.metadata,photo.cover, by = "Name", all.x = TRUE) 

#Visual data 
visual <- read.csv("MBON_Argentina_percent_cover_visual_2018.csv") 

#Joint the two dataframes
photo_visual <- rbind(photo, visual)


#Change names of Annotation Status (types of analysis)
photo_visual$Annotation.status <- as.factor(photo_visual$Annotation.status)
levels(photo_visual$Annotation.status) <-  c("Photoquadrat Human", "Photoquadrat Robot","Visual quadrat")


#Create long type dataframe 
library(reshape)
photo_visual_long = melt(photo_visual, id.vars = 1:20, measure.vars = 21:ncol(photo_visual), variable_name = "CATAMI", value_name ="cover", na.rm = T)


#Calculate mean, SD, SE for cover data by factors (species=Shortname,site, strata,) 
library(doBy)
Coverdata <- summaryBy(value ~ CATAMI + site + strata + Annotation.status,data=photo_visual_long, FUN = function(x) { c(mean = mean(x),SD=sd(x),SE = sqrt(var(x)/length(x)))})


#table
tableobservations <- summaryBy(value ~ CATAMI,data=photo_visual_long, FUN = function(x) {sum(x)})
names(tableobservations) <- c("CATAMI CODE", "Frequency")
tableobservations <- tableobservations[order(-tableobservations$Frequency),]

knitr::kable(tableobservations)

#read labelset MBON
labelset <- read.csv("labelset_MBON.csv")

knitr::kable(labelset)


#nMDS
library(vegan)

#Select site
#photo_visual.Site <- subset(photo_visual, site=="PUNTA CUEVAS")
photo_visual.Site <- photo_visual
#MDS by strata
#create a list with dataframe of strata 
l<-list(subset(photo_visual.Site,strata=="HIGHTIDE"),subset(photo_visual.Site,strata=="MIDTIDE"),subset(photo_visual.Site,strata=="LOWTIDE"))

#nMDS plot
nMDSbystrata <- lapply(l, function (j) { NMDS=metaMDS(sqrt(sqrt(j[,-(1:20)])),k=2,trymax=10,try = 10,distance ="bray",autotransform = FALSE)
NMDS1 <-NMDS$points[,1] 
NMDS2 <- NMDS$points[,2]
MDS.plot<-cbind(j[,-(1:20)], NMDS1, NMDS2,j$Annotation.status) 
ggplot(MDS.plot, aes(NMDS1, NMDS2, color=j$Annotation.status,shape=j$Annotation.status))+geom_point(position=position_jitter(.1),size=3)+stat_ellipse(type='t',size =2) +theme_bw() + theme(legend.position = "none",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank()) + annotate("text", x=max(NMDS1)-0.5, y=min(NMDS2)-0.5, label=paste('Stress =',round(NMDS$stress,3)))+ggtitle("") + theme(legend.key.size = unit(4,"line")) + scale_color_grey(name = "Analysis")+ scale_shape_manual(name = "Analysis",values = c(16,17,15,3))+ggtitle(unique(j$strata))
})

#Plot for legend 
nMDSlegend=metaMDS(photo_visual.Site[,-(1:20)],k=2,trymax=10,try = 10,distance ="bray",autotransform = FALSE)
NMDS1.legend <-nMDSlegend$points[,1] 
NMDS2.legend <- nMDSlegend$points[,2]
MDS.plot.legend<-cbind(photo_visual.Site[,-(1:20)], NMDS1.legend, NMDS2.legend,photo_visual.Site$strata) 
legend <- ggplot(MDS.plot.legend, aes(NMDS1.legend, NMDS2.legend, color=photo_visual.Site$Annotation.status,shape=photo_visual.Site$Annotation.status))+geom_point(position=position_jitter(.1),size=3)+stat_ellipse(type='t',size =2) +theme_bw() + theme(legend.position = "bottom",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank())+ theme(legend.key.size = unit(4,"line")) + scale_color_grey(name = "")+ scale_shape_manual(name = "",values = c(16,17,15,3))


#Create plot grid and save
library(cowplot)
legend_plot <- get_legend(legend)#take legend
plotnMDS <- plot_grid(nMDSbystrata[[1]],nMDSbystrata[[2]],nMDSbystrata[[3]],ncol = 1, align = "v")
plotnMDS_legend <- plot_grid(plotnMDS, legend_plot, ncol = 1, rel_heights = c(1,.2))



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


knitr::kable(PERMANOVAbystrata_NOtransformed)


#plot with all CATAMI CATEGORIES
library(ggplot2)
ggplot(Coverdata,aes(x=strata,y=value.mean,fill=CATAMI)) + geom_bar(alpha=0.7,stat="identity",color="black",position=position_dodge()) + scale_color_grey() + geom_errorbar(aes(ymin=value.mean-value.SE, ymax=value.mean+value.SE), width=.2,position=position_dodge(.9)) + theme_bw() + scale_y_continuous(limits = c(0,100))+ scale_x_discrete(limits=c("HIGHTIDE","MIDTIDE","LOWTIDE"),labels=c("HIGHT","MID","LOW"))+ labs(fill = "CATAMI CODE",x = "", y = "Cover (%)", title = "",caption ="Mean + SE") + facet_grid(site~Annotation.status)

# Subset more abundant TAXA
#MOB, MAFG,MAFR, MASG, SC, MAA, 
Coverdata_abundant <- subset(Coverdata,CATAMI=="MOB"|CATAMI=="MAFG"|CATAMI=="MASG"|CATAMI=="MAFR"|CATAMI=="MAFG"|CATAMI=="SC"|CATAMI=="MAA")

ggplot(Coverdata_abundant,aes(x=strata,y=value.mean,fill=CATAMI)) + geom_bar(alpha=0.7,stat="identity",color="black",position=position_dodge()) + scale_color_grey() + geom_errorbar(aes(ymin=value.mean-value.SE, ymax=value.mean+value.SE), width=.2,position=position_dodge(.9)) + theme_bw() + scale_y_continuous(limits = c(0,100))+ scale_x_discrete(limits=c("HIGHTIDE","MIDTIDE","LOWTIDE"),labels=c("HIGHT","MID","LOW"))+ labs(fill = "CATAMI CODE",x = "", y = "Cover (%)", title = "",caption ="Mean + SE") + scale_fill_brewer(palette="Greys")+ facet_grid(site~Annotation.status) + theme(legend.position = "bottom",panel.grid.major = element_blank(),panel.grid.minor = element_blank())


plot1 <- ggplot(Coverdata_abundant,aes(x=strata,y=value.mean,fill=Annotation.status)) + geom_bar(alpha=0.7,stat="identity",color="black",position=position_dodge()) + scale_color_grey() + geom_errorbar(aes(ymin=value.mean-value.SE, ymax=value.mean+value.SE), width=.2,position=position_dodge(.9)) + theme_bw() + scale_y_continuous(limits = c(0,100))+ scale_x_discrete(limits=c("HIGHTIDE","MIDTIDE","LOWTIDE"),labels=c("HIGHT","MID","LOW")) + facet_grid(site~CATAMI) + labs(fill = "Method",x = "", y = "Cover (%)", title = "",caption ="Mean + SE") + theme(legend.position = "bottom",panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +scale_fill_brewer(palette="Greys",name = "Method")

ggsave(filename = "plot.1.pdf",plot =plot1, device="pdf",width =10,height =8)



#list with most abundant CATAMI categories
#filter by stata and site
photo_visual_long_strata <- subset(photo_visual_long,strata=="MIDTIDE" & site=="PUNTA CUEVAS")
listCATAMI <- list(subset(photo_visual_long_strata,CATAMI=="MOB"),subset(photo_visual_long_strata,CATAMI=="MAFG"), subset(photo_visual_long_strata,CATAMI=="MASG"),subset(photo_visual_long_strata, CATAMI=="MAFR"),subset(photo_visual_long_strata,CATAMI=="MAFG"),subset(photo_visual_long_strata, CATAMI=="SC"), subset(photo_visual_long_strata, CATAMI=="MAA"))

names(listCATAMI) <- c("MOB","MAFG","MASG","MAFR","MAFG","SC","MAA")

#Kruskal-Wallis chi-squared 
pairwise.wilcox <- lapply(listCATAMI, function (j) (pairwise.wilcox.test(j$value,j$Annotation.status,p.adjust.method = "BH")))

pairwise.wilcox



#Correlations
correlations <- read.csv("correation_matrix_visualVSrobot.csv",stringsAsFactors=T)

correlations$algae <- as.numeric(paste(correlations$MAA+correlations$MAEN+correlations$MAENRC+correlations$MAFG+correlations$MAFR+correlations$MAG+correlations$MALCB+correlations$MASB+correlations$MASG))

correlations$algae.A <- as.numeric(paste(correlations$MAA.A+correlations$MAEN.A+correlations$MAENRC.A+correlations$MAFG.A+correlations$MAFR.A+correlations$MAG.A+correlations$MALCB.A+correlations$MASB.A+correlations$MASG.A))

correlations$MAG_MAFG <- as.numeric(paste(correlations$MAFG+correlations$MASG))
correlations$MAG_MAFG.A <- as.numeric(paste(correlations$MAFG.A+correlations$MASG))

ggscatter(correlations, x = "MAG_MAFG", y = "MAG_MAFG.A", add = "reg.line",conf.int=T) +
  labs(title="MAG_MAFG", y="CoralNet",x="Visual")+ ylim(0,100)+ xlim(0,100)+
  stat_cor(label.x = 3, label.y = 100) 

ggscatter(correlations, x = "MAEN", y = "MAEN.A", add = "reg.line",conf.int=T) +
  labs(title="Encrusting Algae", y="CoralNet",x="Visual")+ ylim(0,25)+ xlim(0,25)+
  stat_cor(label.x = 3, label.y =25) 


ggscatter(correlations, x = "algae", y = "algae.A", add = "reg.line",conf.int=T) +
  labs(title="Algae Total", y="CoralNet",x="Visual")+ ylim(0,100)+ xlim(0,100)+
  stat_cor(label.x = 3, label.y = 100) 

ggscatter(correlations, x = "SC", y = "SC.A", add = "reg.line",conf.int=T) +
  labs(title="Substrate", y="CoralNet",x="Visual")+ ylim(0,100)+ xlim(0,100)+
  stat_cor(label.x = 3, label.y = 100) 

ggscatter(correlations, x = "MOB", y = "MOB.A", add = "reg.line",conf.int=T) +
  labs(title="Molluscs: Bivalves", y="CoralNet",x="Visual")+ ylim(0,100)+ xlim(0,100)+
  stat_cor(label.x = 3, label.y = 100) 


ggscatter(correlations, x = "MAFG", y = "MAFG.A", add = "reg.line",conf.int=T) +
  labs(title="Macroalgae:filamentous Green", y="CoralNet",x="Visual")+ ylim(0,100)+ xlim(0,100)+
  stat_cor(label.x = 3, label.y = 100) 

ggscatter(correlations, x = "MAFR", y = "MAFR.A", add = "reg.line",conf.int=T) +
  labs(title="Macroalgae:filamentous Red", y="CoralNet",x="Visual")+ ylim(0,100)+ xlim(0,100)+
  stat_cor(label.x = 3, label.y = 100) 


ggscatter(correlations, x = "MASG", y = "MASG.A", add = "reg.line",conf.int=T) +
  labs(title="Macroalgae:sheet-like Green", y="CoralNet",x="Visual")+ ylim(0,100)+ xlim(0,100)+
  stat_cor(label.x = 3, label.y = 100) 


ggscatter(correlations, x = "MAA", y = "MAA.A", add = "reg.line",conf.int=T) +
  labs(title="Macroalgae:Articulated calcareous", y="CoralNet",x="Visual")+ ylim(0,100)+ xlim(0,100)+
  stat_cor(label.x = 3, label.y = 100) 



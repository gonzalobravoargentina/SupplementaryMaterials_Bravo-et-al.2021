source('./READ_DATA.R')

#Packages used:
library(ggplot2)
library(cowplot)
library(plotly)
library(ggpubr) 
library(gridExtra)
library(gridGraphics)
library(doBy)
library(reshape)
library(dplyr)
library(plotly)
library(ggpubr)


#PLOTS-------

#PLOT type of analysis by country for abundant categories 
CATAMI_1 <- ggplot(Coverdata_abundant,aes(x=strata,y=cover.mean,fill=CATAMI)) + geom_bar(alpha=0.7,stat="identity",color="black",position=position_dodge()) + scale_color_grey() + geom_errorbar(aes(ymin=cover.mean-cover.SE, ymax=cover.mean+cover.SE), width=.2,position=position_dodge(.9)) + theme_bw() + scale_y_continuous(limits = c(0,100))+ scale_x_discrete(limits=c("HIGHTIDE","MIDTIDE","LOWTIDE"),labels=c("HIGH","MID","LOW"))+ labs(fill = "CATAMI CODE",x = "", y = "Cover (%)", title = "",caption ="Mean + SE") + facet_grid(country~Comments) + theme(legend.position = "bottom",panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + scale_fill_brewer(palette="BrBG")

ggsave(here("Figures", "CATAMI_1.png"),width =12,height =10)


CATAMI_2 <- ggplot(Coverdata_abundant,aes(x=strata,y=cover.mean,fill=Comments)) + geom_bar(alpha=0.7,stat="identity",color="black",position=position_dodge()) + scale_color_grey() + geom_errorbar(aes(ymin=cover.mean-cover.SE, ymax=cover.mean+cover.SE), width=.2,position=position_dodge(.9)) + theme_bw() + scale_y_continuous(limits = c(0,100))+ scale_x_discrete(limits=c("HIGHTIDE","MIDTIDE","LOWTIDE"),labels=c("HIGH","MID","LOW")) + facet_grid(country~CATAMI) + labs(fill = "Method",x = "", y = "Cover (%)", title = "",caption ="Mean + SE") + theme(legend.position = "bottom",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank())  + scale_fill_brewer(palette="BrBG")

ggsave(here("Figures", "CATAMI_2.png"),width =12,height =10)


FG_1 <- ggplot(Coverdata.FG,aes(x=strata,y=cover.mean,fill=CATAMI)) + geom_bar(alpha=0.7,stat="identity",color="black",position=position_dodge()) + scale_color_grey() + geom_errorbar(aes(ymin=cover.mean-cover.SE, ymax=cover.mean+cover.SE), width=.2,position=position_dodge(.9)) + theme_bw() + scale_y_continuous(limits = c(0,100))+ scale_x_discrete(limits=c("HIGHTIDE","MIDTIDE","LOWTIDE"),labels=c("HIGH","MID","LOW"))+ labs(fill = "CATAMI CODE",x = "", y = "Cover (%)", title = "",caption ="Mean + SE") + facet_grid(country~Comments) + theme(legend.position = "bottom",panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + scale_fill_brewer(palette="BrBG")

ggsave(here("Figures", "FG_1.png"),width =12,height =10)


FG_2 <- ggplot(Coverdata.FG,aes(x=strata,y=cover.mean,fill=Comments)) + geom_bar(alpha=0.7,stat="identity",color="black",position=position_dodge()) + scale_color_grey() + geom_errorbar(aes(ymin=cover.mean-cover.SE, ymax=cover.mean+cover.SE), width=.2,position=position_dodge(.9)) + theme_bw() + scale_y_continuous(limits = c(0,100))+ scale_x_discrete(limits=c("HIGHTIDE","MIDTIDE","LOWTIDE"),labels=c("HIGH","MID","LOW")) + facet_grid(country~CATAMI) + labs(fill = "Method",x = "", y = "Cover (%)", title = "",caption ="Mean + SE") + theme(legend.position = "bottom",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank()) + scale_fill_brewer(palette="BrBG")
#LOWTIDE-SUSTRATE SD

ggsave(here("Figures", "FG_2.png"),width =12,height =10)


#BOXPLOT
FG_boxplot <- ggplot(data=photoquadrat_visual_long.FG2, mapping=aes(x=strata, y=cover,fill=Comments)) +geom_boxplot()   + theme_bw() + scale_y_continuous(limits = c(0,100))+ scale_x_discrete(limits=c("HIGHTIDE","MIDTIDE","LOWTIDE"),labels=c("HIGH","MID","LOW")) + facet_grid(country~CATAMI) + labs(fill = "Method",x = "", y = "Cover (%)", title = "",caption ="Mean + SE") + theme(legend.position = "bottom",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank()) + scale_fill_brewer(palette="BrBG")

ggsave(here("Figures", "FG_boxplot.png"),width =12,height =10)


#By country
ggplot(subset(Coverdata_abundant,country=="ARGENTINA"),aes(x=strata,y=cover.mean,fill=CATAMI)) + geom_bar(alpha=0.7,stat="identity",color="black",position=position_dodge()) + scale_color_grey() + geom_errorbar(aes(ymin=cover.mean-cover.SE, ymax=cover.mean+cover.SE), width=.2,position=position_dodge(.9)) + theme_bw() + scale_y_continuous(limits = c(0,100))+ scale_x_discrete(limits=c("HIGHTIDE","MIDTIDE","LOWTIDE"),labels=c("HIGH","MID","LOW"))+ labs(fill = "CATAMI CODE",x = "", y = "Cover (%)", title = "",caption ="Mean + SE") + facet_grid(country~Comments) + theme(legend.position = "bottom",panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + scale_fill_brewer(palette="BrBG")

ggplot(subset(Coverdata_abundant,country=="ARGENTINA"),aes(x=strata,y=cover.mean,fill=Comments)) + geom_bar(alpha=0.7,stat="identity",color="black",position=position_dodge()) + scale_color_grey() + geom_errorbar(aes(ymin=cover.mean-cover.SE, ymax=cover.mean+cover.SE), width=.2,position=position_dodge(.9)) + theme_bw() + scale_y_continuous(limits = c(0,100))+ scale_x_discrete(limits=c("HIGHTIDE","MIDTIDE","LOWTIDE"),labels=c("HIGH","MID","LOW")) + facet_grid(country~CATAMI) + labs(fill = "Method",x = "", y = "Cover (%)", title = "",caption ="Mean + SE") + theme(legend.position = "bottom",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank())  + scale_fill_brewer(palette="BrBG")



#CORRELATIONS PLOTS 

library(ggpubr) 
plotSC<- ggscatter(correlations, x = "SC.H", y = "SC.A", add = "reg.line",conf.int=T) +
  labs(title="SC", y="Photo",x="Visual")+ ylim(0,100)+ xlim(0,100)+
  stat_cor(label.x = 3, label.y = 100)



#BAre substrate
site <- correlations$country.A
#correlations <- subset(correlations,strata=="HIGHTIDE")
#site <- correlations.hightide$site

#by photo
SC<-  summaryBy(SC.A + SC.H  ~ Name ,data=correlations , FUN = mean)
SC$site <- site
#differenced_data= Method_A - Method_B
SC$diff <- SC$SC.A.mean-SC$SC.H.mean
SC$avg <- (SC$SC.A.mean + SC$SC.H.mean) / 2

#by site
SC2<-  summaryBy(SC.A + SC.H ~ country.A ,data=correlations, FUN = mean)
SC2$diff <- SC2$SC.A.mean-SC2$SC.H.mean
SC2$avg <- (SC2$SC.A.mean + SC2$SC.H.mean) / 2

#plot by photo
g1 <- ggplot(SC, aes(x=avg, y=diff,colour=site))+geom_hline(yintercept=0) + geom_point()+ theme_bw()+labs(y="Difference in % cover between methods", x="",title = "Substrate Consolidate")+theme(legend.position="none",plot.margin=unit(c(0,0,1,1), "cm"))
#plot by site

#Create CI using boot
library(boot)
SC$site
SC.AR <- subset(SC,site=="ARGENTINA")
SC.CO <- subset(SC,site=="COLOMBIA")
SC.EC <- subset(SC,site=="ECUADOR")

MEAN<- function(d, w) sum(d * w)
Output=boot(SC.EC$diff, MEAN, R = 9999, stype = "w")
boot.ci(Output, type="bca")

MEAN<- function(d, w) sum(d * w)
Output=boot(SC.EC$avg, MEAN, R = 9999, stype = "w")
boot.ci(Output, type="bca")

#ARdiif -9.211 , -2.978
#ARavg 21.56, 34.56 
#COdiff 2.567, 10.746
#COavg 70.47, 81.63
#ECdiff 5.622, 12.367
#ECavg 80.63, 89.20


g2 <- ggplot(SC2, aes(x=avg, y=diff,colour=country.A)) + geom_point()+theme_bw() + labs(y="", x="Avg % cover of both methods (± 95%CI)") + theme(axis.title.y =element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+geom_hline(yintercept=0)+theme(legend.position="none",plot.margin=unit(c(0,1,1,-0.1), "cm"))+geom_errorbar(aes(ymin =c(-9.211,2.567,5.622),ymax =c(-2.978,10.746,12.367)),width = 0.2) + geom_errorbarh(aes(xmin = c(21.56,70.47,80.63),xmax = c(34.56,81.63,89.20)),height =-.2)


#set y limits and brecks
limits <- c(-30, 90)
breaks <- seq(limits[1], limits[2], by=8)

# assign common axis to both plots
p1.common.y <- g1 + scale_y_continuous(limits=limits, breaks=breaks)
p2.common.y <- g2 + scale_y_continuous(limits=limits, breaks=breaks)

# At this point, they have the same axis, but the axis lengths are unequal, so ...
# build the plots 
p1.common.y <- ggplot_gtable(ggplot_build(p1.common.y))
p2.common.y <- ggplot_gtable(ggplot_build(p2.common.y))

# copy the plot height from p1 to p2
p2.common.y$heights <- p1.common.y$heights
#plot grid
grid <- grid.arrange(p1.common.y,p2.common.y,ncol=2,widths=c(1,1))

#add legend
legend <- ggplot(SC, aes(x=avg, y=diff,colour=site))+geom_hline(yintercept=0) + geom_point()+ theme_bw()+labs(y="Difference in % cover between methods", x="% cover visual",title = "Bare Substrate")+theme(legend.position="top",plot.margin=unit(c(0,0,1,1), "cm"))
legend_plot <- get_legend(legend)#take legend
plot3 <- plot_grid(legend_plot,grid, ncol = 1, rel_heights = c(.1,1))





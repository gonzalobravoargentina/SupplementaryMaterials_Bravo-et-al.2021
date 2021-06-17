#Bland-Altman plots, also called difference plots

#The Bland-Altman plot determines differences between the two estimations against the observer estimations, or reference sensu (Krouwer JS (2008) Why Bland-Altman plots should use X, not (Y + X)/2 when X is a reference method. Stat Med 27:778–780) 

source('./READ_DATA.R')
#PREPARATION OF DATA for Bland-Altman plots

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Mean for groups (Algae,substrate and Invertebrates) by sites
library(doBy)
FG_DATA <- summaryBy(ALGAE+SUBSTRATE+INVERTEBRATES ~ site + country + strata +Comments,data=PQ_VQ.FG,FUN = mean)

#add columns names
colnames(FG_DATA) <- c("site","country","strata","Comments","ALGAE","SUBSTRATE","INVERTEBRATES")

#take only VQ.HUMAN and PQ.ROBOT
#FG_DATA <- subset(FG_DATA, Comments=="VQ.Human"|Comments=="PQ.Robot")

#take only PQ.ROBOT
FG_DATA_Robot <-  subset(FG_DATA, Comments=="PQ.Robot")
colnames(FG_DATA_Robot) <- c("site","country","strata","Comments","ALGAE.R","SUBSTRATE.R","INVERTEBRATES.R")

#take only VQ.HUMAN 
FG_DATA_Human <-  subset(FG_DATA, Comments=="VQ.Human")
colnames(FG_DATA_Human) <- c("site","country","strata","Comments","ALGAE.H","SUBSTRATE.H","INVERTEBRATES.H")


#MERGE FG_DATA_Robot and FG_DATA_Human
#chosse cmbination (VQ vs PQ or PQ.human vs PQ.robot)
FG <- dplyr::full_join(FG_DATA_Human, FG_DATA_Robot,by = c("site", "country", "strata"))

rm(FG_DATA,FG_DATA_Human,FG_DATA_Robot)


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#AlL observations for Arg, COl y EC 
#FG_DATA <- summaryBy(ALGAE+SUBSTRATE+INVERTEBRATES ~ site + country + strata +Name +Comments,data=PQ_VQ.FG,FUN = mean)

#take only VQ.HUMAN and PQ.ROBOT
#FG_DATA <- subset(FG_DATA, Comments=="VQ.Human"|Comments=="PQ.Robot")

#take only PQ.ROBOT
#FG_DATA_Robot <-  subset(FG_DATA, Comments=="PQ.Robot")
#colnames(FG_DATA_Robot) <- c("site","country","strata","Name","Comments","ALGAE.R","SUBSTRATE.R","INVERTEBRATES.R")

#take only VQ.HUMAN 
#FG_DATA_Human <-  subset(FG_DATA, Comments=="VQ.Human")
#colnames(FG_DATA_Human) <- c("site","country","strata","Name","Comments","ALGAE.H","SUBSTRATE.H","INVERTEBRATES.H")

#MERGE FG_DATA_Robot and FG_DATA_Human
#FG <- dplyr::full_join(FG_DATA_Human, FG_DATA_Robot,by = c("site", "country", "strata"))
#FG <-  subset(FG, country!="USA")

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


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



#Plot Supplementary materials------
#PQ.Robot vs VQ (Human)
#SUBSTRATE

SC<-  FG
#differenced_data= Method_A - Method_B
SC$diff <- SC$SUBSTRATE.R-SC$SUBSTRATE.H
SC$avg <- SC$SUBSTRATE.H


#by country
SC2 <- SC %>% 
  group_by(country) %>% 
  summarise_at(vars(SUBSTRATE.R,SUBSTRATE.H,diff,avg), 
               list(mean, sd)) 
colnames(SC2) <- c("country","SUBSTRATE.R.mean","SUBSTRATE.H.mean","diff","avg","SUBSTRATE.R.SD","SUBSTRATE.H.SD","diff.SD","avg.SD")


#plot by site and strata 
g1 <- ggplot(SC, aes(x=avg, y=diff,colour=country,shape=strata))+geom_hline(yintercept=0) + geom_point()+ theme_bw()+labs(y="Difference in % cover between methods", x="Average % cover by VQ.Human",title = "SUBSTRATE")+theme(legend.position="none",plot.margin=unit(c(0,0,1,1), "cm"))
#plot by country
g2 <- ggplot(SC2, aes(x=avg, y=diff,colour=country)) + geom_point()+theme_bw() + labs(y="", x="Average % cover by VQ.Human (± SD)") + theme(axis.title.y =element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+geom_hline(yintercept=0)+theme(legend.position="none",plot.margin=unit(c(0,1,1,-0.1), "cm"))+geom_errorbar(aes(ymin =diff-diff.SD,ymax =diff+diff.SD),width = 0.2) + geom_errorbarh(aes(xmin =avg-avg.SD ,xmax = avg+avg.SD),height =-.2)

#set y limits and brecks
limits <- c(-40, 40)#see plot to set 
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
grid1 <- grid.arrange(p1.common.y,p2.common.y,ncol=2,widths=c(1,1))

#add legend
legend <- ggplot(SC, aes(x=avg, y=diff,colour=country,shape=strata))+geom_hline(yintercept=0) + geom_point()+ theme_bw()+labs(y="Difference in % cover between methods", x="",title = "Substrate")+theme(legend.position="bottom",legend.title = element_blank(),legend.box="vertical",plot.margin=unit(c(0,0,1,1), "cm"))
legend_plot <- get_legend(legend)#take legend
#plot.SUBSTRATE <- plot_grid(legend_plot,grid1, ncol = 1, rel_heights = c(.25,1))

#ggsave(here("Figures", "Fig_3.pdf"),width =10,height =5)
#ggsave(here("Figures", "Fig_3.png"),width =10,height =5)

#ALGAE
AL<-  FG
#differenced_data= Method_A - Method_B
AL$diff <- AL$ALGAE.R-AL$ALGAE.H
AL$avg <- AL$ALGAE.H


#by country
AL2 <- AL %>% 
  group_by(country) %>% 
  summarise_at(vars(ALGAE.R,ALGAE.H,diff,avg), 
               list(mean, sd)) 
colnames(AL2) <- c("country","ALGAE.R.mean","ALGAE.H.mean","diff","avg","ALGAE.R.SD","ALGAE.H.SD","diff.SD","avg.SD")


#plot by site and strata
P.AL1 <- ggplot(AL, aes(x=avg, y=diff,colour=country,shape=strata))+geom_hline(yintercept=0) + geom_point()+ theme_bw()+labs(y="Difference in % cover between methods", x="Average % cover by VQ.Human",title = "ALGAE")+theme(legend.position="none",plot.margin=unit(c(0,0,1,1), "cm"))
#plot by country
P.AL2 <- ggplot(AL2, aes(x=avg, y=diff,colour=country)) + geom_point()+theme_bw() + labs(y="", x="Average % cover by VQ.Human (± SD)") + theme(axis.title.y =element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+geom_hline(yintercept=0)+theme(legend.position="none",plot.margin=unit(c(0,1,1,-0.1), "cm"))+geom_errorbar(aes(ymin =diff-diff.SD,ymax =diff+diff.SD),width = 0.2) + geom_errorbarh(aes(xmin =avg-avg.SD ,xmax = avg+avg.SD),height =-.2)

#set y limits and brecks
limits <- c(-40, 40)#see plot to set 
breaks <- seq(limits[1], limits[2], by=8)

# assign common axis to both plots
p1.common.y <- P.AL1 + scale_y_continuous(limits=limits, breaks=breaks)
p2.common.y <- P.AL2 + scale_y_continuous(limits=limits, breaks=breaks)

# At this point, they have the same axis, but the axis lengths are unequal, so ...
# build the plots 
p1.common.y <- ggplot_gtable(ggplot_build(p1.common.y))
p2.common.y <- ggplot_gtable(ggplot_build(p2.common.y))

# copy the plot height from p1 to p2
p2.common.y$heights <- p1.common.y$heights
#plot grid
grid2 <- grid.arrange(p1.common.y,p2.common.y,ncol=2,widths=c(1,1))

#add legend
legend <- ggplot(AL, aes(x=avg, y=diff,colour=country,shape=strata))+geom_hline(yintercept=0) + geom_point()+ theme_bw()+labs(y="Difference in % cover between methods", x="",title = "ALGAE")+theme(legend.position="bottom",legend.title = element_blank(),legend.box="vertical",plot.margin=unit(c(0,0,1,1), "cm"))
legend_plot <- get_legend(legend)#take legend
plot.ALGAE <- plot_grid(legend_plot,grid2, ncol = 1, rel_heights = c(.25,1))

#ggsave(here("Figures", "Fig_4.pdf"),width =5,height =5)
#ggsave(here("Figures", "Fig_4.png"),width =5,height =5)

#INVERTEBRATES

IN<-  FG
#differenced_data= Method_A - Method_B
IN$diff <- IN$INVERTEBRATES.R-IN$INVERTEBRATES.H
IN$avg <- IN$INVERTEBRATES.H


#by country
IN2 <- IN %>% 
  group_by(country) %>% 
  summarise_at(vars(INVERTEBRATES.R,INVERTEBRATES.H,diff,avg), 
               list(mean, sd)) 
colnames(IN2) <- c("country","INVERTEBRATES.R.mean","INVERTEBRATES.H.mean","diff","avg","INVERTEBRATES.R.SD","INVERTEBRATES.H.SD","diff.SD","avg.SD")


#plot by site and strata
P.IN1 <- ggplot(IN, aes(x=avg, y=diff,colour=country,shape=strata))+geom_hline(yintercept=0) + geom_point()+ theme_bw()+labs(y="Difference in % cover between methods", x="Average % cover by VQ.Human",title = "INVERTEBRATES")+theme(legend.position="none",plot.margin=unit(c(0,0,1,1), "cm"))
#plot by country
P.IN2 <- ggplot(IN2, aes(x=avg, y=diff,colour=country)) + geom_point()+theme_bw() + labs(y="", x="Average % cover by VQ.Human (± SD)") + theme(axis.title.y =element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+geom_hline(yintercept=0)+theme(legend.position="none",plot.margin=unit(c(0,1,1,-0.1), "cm"))+geom_errorbar(aes(ymin =diff-diff.SD,ymax =diff+diff.SD),width = 0.2) + geom_errorbarh(aes(xmin =avg-avg.SD ,xmax = avg+avg.SD),height =-.2)

#set y limits and brecks
limits <- c(-40, 40)#see plot to set 
breaks <- seq(limits[1], limits[2], by=8)

# assign common axis to both plots
p1.common.y <- P.IN1 + scale_y_continuous(limits=limits, breaks=breaks)
p2.common.y <- P.IN2 + scale_y_continuous(limits=limits, breaks=breaks)

# At this point, they have the same axis, but the axis lengths are unequal, so ...
# build the plots 
p1.common.y <- ggplot_gtable(ggplot_build(p1.common.y))
p2.common.y <- ggplot_gtable(ggplot_build(p2.common.y))

# copy the plot height from p1 to p2
p2.common.y$heights <- p1.common.y$heights
#plot grid
grid3 <- grid.arrange(p1.common.y,p2.common.y,ncol=2,widths=c(1,1))

#add legend
legend <- ggplot(IN, aes(x=avg, y=diff,colour=country,shape=strata))+geom_hline(yintercept=0) + geom_point()+ theme_bw()+labs(y="Difference in % cover between methods", x="",title = "INVERTEBRATES")+theme(legend.position="bottom",legend.title = element_blank(),legend.box="vertical",plot.margin=unit(c(0,0,1,1), "cm"))
legend_plot <- get_legend(legend)#take legend
plot.INVERTEBRATES <- plot_grid(legend_plot,grid3, ncol = 1, rel_heights = c(.25,1))


#Plot Substrate , Algae and invertebrates in the same grid
plot.SC_ALGAE_INV <- plot_grid(legend_plot,grid1,grid2,grid3, ncol = 1, rel_heights = c(.25,1,1,1))
#ggsave(here("Figures", "Fig_4.png"),width =8,height =12)
#ggsave(here("Figures", "Fig_4.pdf"),width =8,height =12)


#Plot figure 4 A------
#(run first line on top)

#ALGAE,SUBSTRATE AND INVERTEBRATES
#PQ.Robot vs VQ
#https://cran.r-project.org/web/packages/BlandAltmanLeh/vignettes/Intro.html
library(ggplot2)
library(BlandAltmanLeh) 
A <- c(IN$INVERTEBRATES.R,AL$ALGAE.R,SC$SUBSTRATE.R)
B <- c(IN$INVERTEBRATES.H,AL$ALGAE.H,SC$SUBSTRATE.H)
C <- c(IN$country,AL$country,SC$country)
Color <- as.factor(C)
levels(Color) <- c("#F8766D","#7CAE00","#00BFC4","#C77CFF")
ba.stats <- bland.altman.stats(A, B)

pl <- bland.altman.plot(A, B,graph.sys = "ggplot2")+ theme_light()
pl <- pl + geom_point(color=Color,size=3) + ylab("Difference in % cover (PQ.Robot vs VQ)") + xlab("Average % cover of both methods (PQ.Robot vs VQ)")+ theme(text = element_text(size = 15))

ggsave(here("Figures", "Fig_4.A.PDF"),width =8,height =6) 




#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Plot Figure 4 B----
#COMPARISON PQ.Human vs PQ.Robot
#Mean for groups (Algae,substrate and Invertebrates) by sites
library(doBy)
FG_DATA <- summaryBy(ALGAE+SUBSTRATE+INVERTEBRATES ~ site + country + strata +Comments,data=PQ_VQ.FG,FUN = mean)

#add columns names
colnames(FG_DATA) <- c("site","country","strata","Comments","ALGAE","SUBSTRATE","INVERTEBRATES")


#take only PQ.ROBOT
FG_DATA_Robot <-  subset(FG_DATA, Comments=="PQ.Robot")
colnames(FG_DATA_Robot) <- c("site","country","strata","Comments","ALGAE.R","SUBSTRATE.R","INVERTEBRATES.R")

#take only PQ.HUMAN 
FG_DATA_Human <-  subset(FG_DATA, Comments=="PQ.Human")
colnames(FG_DATA_Human) <- c("site","country","strata","Comments","ALGAE.H","SUBSTRATE.H","INVERTEBRATES.H")

#MERGE FG_DATA_Robot and FG_DATA_Human
#chosse cmbination (VQ vs PQ or PQ.human vs PQ.robot)
FG <- dplyr::full_join(FG_DATA_Human, FG_DATA_Robot,by = c("site", "country", "strata"))


#SUBSTRATE
SC<-  FG
#differenced_data= Method_A - Method_B
SC$diff <- SC$SUBSTRATE.R-SC$SUBSTRATE.H
SC$avg <- (SC$SUBSTRATE.H+SC$SUBSTRATE.R)/2
#by country
SC2 <- SC %>% 
  group_by(country) %>% 
  summarise_at(vars(SUBSTRATE.R,SUBSTRATE.H,diff,avg), 
               list(mean, sd)) 
colnames(SC2) <- c("country","SUBSTRATE.R.mean","SUBSTRATE.H.mean","diff","avg","SUBSTRATE.R.SD","SUBSTRATE.H.SD","diff.SD","avg.SD")

#ALGAE
AL<-  FG
#differenced_data= Method_A - Method_B
AL$diff <- AL$ALGAE.R-AL$ALGAE.H
AL$avg <- (AL$ALGAE.H+AL$ALGAE.R)/2

#by country
AL2 <- AL %>% 
  group_by(country) %>% 
  summarise_at(vars(ALGAE.R,ALGAE.H,diff,avg), 
               list(mean, sd)) 
colnames(AL2) <- c("country","ALGAE.R.mean","ALGAE.H.mean","diff","avg","ALGAE.R.SD","ALGAE.H.SD","diff.SD","avg.SD")


#INVERTEBRATES------
IN<-  FG
#differenced_data= Method_A - Method_B
IN$diff <- IN$INVERTEBRATES.R-IN$INVERTEBRATES.H
IN$avg <- (IN$INVERTEBRATES.H+IN$INVERTEBRATES.R)/2
#by country
IN2 <- IN %>% 
  group_by(country) %>% 
  summarise_at(vars(INVERTEBRATES.R,INVERTEBRATES.H,diff,avg), 
               list(mean, sd)) 
colnames(IN2) <- c("country","INVERTEBRATES.R.mean","INVERTEBRATES.H.mean","diff","avg","INVERTEBRATES.R.SD","INVERTEBRATES.H.SD","diff.SD","avg.SD")


library(ggplot2)
library(BlandAltmanLeh) 
A <- c(IN$INVERTEBRATES.R,AL$ALGAE.R,SC$SUBSTRATE.R)
B <- c(IN$INVERTEBRATES.H,AL$ALGAE.H,SC$SUBSTRATE.H)
C <- c(IN$country,AL$country,SC$country)
Color <- as.factor(C)
levels(Color) <- c("#F8766D","#7CAE00","#00BFC4","#C77CFF")
ba.stats <- bland.altman.stats(A, B)

pl <- bland.altman.plot(A, B,graph.sys = "ggplot2")+ theme_light()
pl <- pl + geom_point(color=Color,size=3) + ylab("Difference in % cover (PQ.Robot vs PQ.Human)") + xlab("Average % cover of both methods (PQ.Robot vs PQ.Human)")+ theme(text = element_text(size = 15))


ggsave(here("Figures", "Fig_4.B.pdf"),width =8,height =6) 

















#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#COMPARISON PQ.Human vs VQ.Human
#Mean for groups (Algae,substrate and Invertebrates) by sites
library(doBy)
FG_DATA <- summaryBy(ALGAE+SUBSTRATE+INVERTEBRATES ~ site + country + strata +Comments,data=PQ_VQ.FG,FUN = mean)

#add columns names
colnames(FG_DATA) <- c("site","country","strata","Comments","ALGAE","SUBSTRATE","INVERTEBRATES")


#take only PQ.ROBOT
FG_DATA_Robot <-  subset(FG_DATA, Comments=="PQ.Human")
colnames(FG_DATA_Robot) <- c("site","country","strata","Comments","ALGAE.R","SUBSTRATE.R","INVERTEBRATES.R")

#take only PQ.HUMAN 
FG_DATA_Human <-  subset(FG_DATA, Comments=="VQ.Human")
colnames(FG_DATA_Human) <- c("site","country","strata","Comments","ALGAE.H","SUBSTRATE.H","INVERTEBRATES.H")

#MERGE FG_DATA_Robot and FG_DATA_Human
#chosse cmbination (VQ vs PQ or PQ.human vs PQ.robot)
FG <- dplyr::full_join(FG_DATA_Human, FG_DATA_Robot,by = c("site", "country", "strata"))


#SUBSTRATE------
SC<-  FG
#differenced_data= Method_A - Method_B
SC$diff <- SC$SUBSTRATE.R-SC$SUBSTRATE.H
SC$avg <- (SC$SUBSTRATE.H+SC$SUBSTRATE.R)/2
#by country
SC2 <- SC %>% 
  group_by(country) %>% 
  summarise_at(vars(SUBSTRATE.R,SUBSTRATE.H,diff,avg), 
               list(mean, sd)) 
colnames(SC2) <- c("country","SUBSTRATE.R.mean","SUBSTRATE.H.mean","diff","avg","SUBSTRATE.R.SD","SUBSTRATE.H.SD","diff.SD","avg.SD")

#ALGAE------
AL<-  FG
#differenced_data= Method_A - Method_B
AL$diff <- AL$ALGAE.R-AL$ALGAE.H
AL$avg <- (AL$ALGAE.H+AL$ALGAE.R)/2

#by country
AL2 <- AL %>% 
  group_by(country) %>% 
  summarise_at(vars(ALGAE.R,ALGAE.H,diff,avg), 
               list(mean, sd)) 
colnames(AL2) <- c("country","ALGAE.R.mean","ALGAE.H.mean","diff","avg","ALGAE.R.SD","ALGAE.H.SD","diff.SD","avg.SD")


#INVERTEBRATES------
IN<-  FG
#differenced_data= Method_A - Method_B
IN$diff <- IN$INVERTEBRATES.R-IN$INVERTEBRATES.H
IN$avg <- (IN$INVERTEBRATES.H+IN$INVERTEBRATES.R)/2
#by country
IN2 <- IN %>% 
  group_by(country) %>% 
  summarise_at(vars(INVERTEBRATES.R,INVERTEBRATES.H,diff,avg), 
               list(mean, sd)) 
colnames(IN2) <- c("country","INVERTEBRATES.R.mean","INVERTEBRATES.H.mean","diff","avg","INVERTEBRATES.R.SD","INVERTEBRATES.H.SD","diff.SD","avg.SD")



library(ggplot2)
library(BlandAltmanLeh) 
A <- c(IN$INVERTEBRATES.R,AL$ALGAE.R,SC$SUBSTRATE.R)
B <- c(IN$INVERTEBRATES.H,AL$ALGAE.H,SC$SUBSTRATE.H)
C <- c(IN$country,AL$country,SC$country)
Color <- as.factor(C)
levels(Color) <- c("#F8766D","#7CAE00","#00BFC4","#C77CFF")
ba.stats <- bland.altman.stats(A, B)

pl <- bland.altman.plot(A, B,graph.sys = "ggplot2")+ theme_light()
pl <- pl + geom_point(color=Color) + ylab("Difference in % cover (PQ.Human vs VQ.Human)") + xlab("Average % cover of both methods (PQ.Human vs VQ.Human)")

#ggsave(here("Figures", "Fig_4.2_Bis.PDF"),width =8,height =6) 






library(scales)
show_col(hue_pal()(4))
show_col(hue_pal()(5))

#CORRELATIONS PLOTS 
library(ggpubr) 
plotSC<- ggscatter(correlations, x = "SC.H", y = "SC.A", add = "reg.line",conf.int=T) +
  labs(title="SC", y="Photo",x="Visual")+ ylim(0,100)+ xlim(0,100)+
  stat_cor(label.x = 3, label.y = 100)



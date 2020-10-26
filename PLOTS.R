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
CATAMI <- ggplot(Coverdata_abundant,aes(x=strata,y=cover.rel.mean,fill=Comments)) + geom_bar(alpha=0.7,stat="identity",color="black",position=position_dodge()) + scale_color_grey() + geom_errorbar(aes(ymin=cover.rel.mean-cover.rel.SE, ymax=cover.rel.mean+cover.rel.SE), width=.2,position=position_dodge(.9)) + theme_bw() + scale_y_continuous(limits = c(0,100))+ scale_x_discrete(limits=c("HIGHTIDE","MIDTIDE","LOWTIDE"),labels=c("HIGH","MID","LOW")) + labs(fill = "Method",x = "", y = "Cover (%)", title = "") + theme(legend.position = "bottom",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank()) + scale_fill_brewer(palette="BrBG")+ facet_grid(country~CATAMI)+theme(strip.text.x = element_text(size = 10),strip.text.y = element_text(size = 10))

ggsave(here("Figures", "Fig_2.pdf"),width =10,height =8)
ggsave(here("Figures", "Fig_2.png"),width =10,height =8)

#BOXPLOT FG
FG_boxplot <- ggplot(data=PQ_VQ_long.FG, mapping=aes(x=strata, y=cover.rel,fill=Comments)) +geom_boxplot()   + theme_bw() + scale_y_continuous(limits = c(0,100))+ scale_x_discrete(limits=c("HIGHTIDE","MIDTIDE","LOWTIDE"),labels=c("HIGH","MID","LOW")) + facet_grid(country~CATAMI) + labs(fill = "Method",x = "", y = "Cover (%)", title = "") + theme(legend.position = "bottom",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank()) + scale_fill_brewer(palette="BrBG")+theme(strip.text.x = element_text(size = 10),strip.text.y = element_text(size = 10))

ggsave(here("Figures", "Fig_3.png"),width =10,height =8)
ggsave(here("Figures", "Fig_3.pdf"),width =10,height =8)












FG_1 <- ggplot(Coverdata.FG,aes(x=strata,y=cover.mean,fill=CATAMI)) + geom_bar(alpha=0.7,stat="identity",color="black",position=position_dodge()) + scale_color_grey() + geom_errorbar(aes(ymin=cover.mean-cover.SE, ymax=cover.mean+cover.SE), width=.2,position=position_dodge(.9)) + theme_bw() + scale_y_continuous(limits = c(0,100))+ scale_x_discrete(limits=c("HIGHTIDE","MIDTIDE","LOWTIDE"),labels=c("HIGH","MID","LOW"))+ labs(fill = "CATAMI CODE",x = "", y = "Cover (%)", title = "",caption ="Mean + SE") + facet_grid(country~Comments) + theme(legend.position = "bottom",panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + scale_fill_brewer(palette="BrBG")

ggsave(here("Figures", "FG_1.pdf"),width =12,height =10)
ggsave(here("Figures", "FG_1.png"),width =12,height =10)

FG_2 <- ggplot(Coverdata.FG,aes(x=strata,y=cover.rel.mean,fill=Comments)) + geom_bar(alpha=0.7,stat="identity",color="black",position=position_dodge()) + scale_color_grey() + geom_errorbar(aes(ymin=cover.rel.mean-cover.rel.SE, ymax=cover.rel.mean+cover.rel.SE), width=.2,position=position_dodge(.9)) + theme_bw() + scale_y_continuous(limits = c(0,100))+ scale_x_discrete(limits=c("HIGHTIDE","MIDTIDE","LOWTIDE"),labels=c("HIGH","MID","LOW")) + facet_grid(country~CATAMI) + labs(fill = "Method",x = "", y = "Cover (%)", title = "",caption ="Mean + SE") + theme(legend.position = "bottom",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank()) + scale_fill_brewer(palette="BrBG")


ggsave(here("Figures", "FG_2.png"),width =12,height =10)









#############################################################################################################################
dev.off()
rm(list=ls())
library(tidyverse)
library(dplyr)
library(eurostat)
library(pals)
library(ggrepel)
library(ggpubr)
library(ggplot2)


data.dir <- "C:\\Users\\Magdalena\\demography\\education\\data\\shareedu"
data.out <- "C:\\Users\\Magdalena\\demography\\education\\data\\ready"
setwd(data.dir)

femall <- read.table(file="BL_F.csv", sep=",", header=TRUE)
maleall <- read.table(file="BL_M.csv", sep=",", header=TRUE)

setwd(data.out)
countrylist <- read.table(file="countrylist.csv", sep=",")

edu3544 <- femall %>%
  filter(country %in% t(countrylist), agefrom==35) %>%
  mutate(high=lh, sex="f") %>%
  select(sex,country,year,high) %>%
  bind_rows(maleall %>%
              filter(country %in% t(countrylist), agefrom==35) %>%
              mutate(high=lh, sex="m") %>%
              select(sex,country,year,high))
mean3544 <- edu3544 %>%
  group_by(sex,year) %>%
  dplyr::summarize(country=1,high=mean(high))

dev.off()
pdf(file = "C:/Users/Magdalena/demography/education/figures/eduexpansion.pdf",  width = 6, height = 8, onefile=FALSE) 

ggarrange(ggplot(NULL, aes(x=year, y=high, group=country))  + 
            theme(legend.position="none") +   
            geom_line(data=(edu3544 %>% filter(sex=="f")), col="grey90") + 
            geom_line(data=(mean3544%>% filter(sex=="f")),col="black")+ 
            geom_line(data=(edu3544%>% filter(sex=="f",country=="Italy")),col="black", lty=3)+
            geom_line(data=(edu3544%>% filter(sex=="f",country=="Canada")),col="black", lty=3)+
            annotate("text", x=2005, y=65, label= "Canada") + 
            annotate("text", x=2005, y=10, label= "Italy") + 
  theme(plot.margin = unit(c(0.5,0.5,1,1), "lines"))+
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    axis.line = element_line(colour = "black", size = 0.2, linetype = "solid"))+
    scale_x_continuous(name="Year", limits=c(1950,2015), breaks=c(seq(from=1950,to=2010,by=10),2015), labels=c(seq(from=1950,to=2010,by=10),2015))+
    scale_y_continuous(name="in %", limits=c(0,68))+
  ggtitle("Female") + theme(plot.title = element_text(face="bold")),
ggplot(NULL, aes(x=year, y=high, group=country))  + 
         theme(legend.position="none") +   
         geom_line(data=(edu3544 %>% filter(sex=="m")), col="grey90") + 
         geom_line(data=(mean3544%>% filter(sex=="m")),col="black")+
         geom_line(data=(edu3544 %>% filter(sex=="m", country=="Italy")),col="black", lty=3)+
         geom_line(data=(edu3544 %>% filter(sex=="m", country=="Canada")),col="black", lty=3)+
         annotate("text", x=2005, y=60, label= "Canada") + 
         annotate("text", x=2005, y=10, label= "Italy") +
         theme(plot.margin = unit(c(0.5,0.5,1,1), "lines"))+
         theme(
           panel.background = element_rect(fill = "transparent",colour = NA),
           plot.background = element_rect(fill = "transparent",colour = NA),
           axis.line = element_line(colour = "black", size = 0.2, linetype = "solid"))+
         scale_x_continuous(name="Year", limits=c(1950,2015), breaks=c(seq(from=1950,to=2010,by=10),2015), labels=c(seq(from=1950,to=2010,by=10),2015))+
         scale_y_continuous(name="in %", limits=c(0,68))+
  ggtitle("Male") + theme(plot.title = element_text(face="bold")),
nrow=2,ncol=1,common.legend = TRUE, legend="bottom")

dev.off()


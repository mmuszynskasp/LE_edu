#############################################################################################################################
dev.off()
rm(list=ls())
library(tidyverse)
library(dplyr)
library(HMDHFDplus)
library(MortalityLaws)
library(ggrepel)
library(ggpubr)


data.dir <- "C:\\Users\\Magdalena\\demography\\education\\data\\LE"
data.out <- "C:\\Users\\Magdalena\\demography\\education\\data\\ready"
setwd(data.dir)

################# LE by edu, LE_new
nemeth <- read.table(file="nemeth.txt", sep="\t", header=TRUE)
nemeth <- nemeth %>%
  dplyr::select(Country,Period,Sex,Education,AgeGrp,ex) %>%
  mutate(edu=recode(Education, "High"="3","Low"="1","Middle"="2")) %>%
  rename_with(tolower) %>%
  dplyr::rename(age=agegrp,LE=ex) %>%
  mutate(sex=recode(sex, "Female"="f", "Male"="m"))
  
###read-in population
setwd(data.out)
popall <- read.table(file="popedu.csv",sep=",",header=TRUE)
popall <- popall %>%
  mutate(age=recode(age,"From 25 to 29 years"="25-29", "From 30 to 34 years"="30-34","From 35 to 39 years"="35-39",
           "From 40 to 44 years"="40-44", "From 45 to 49 years"="45-49", "From 50 to 54 years"="50-54",
           "From 55 to 59 years"="55-59", "From 60 to 64 years"="60-64", "From 65 to 69 years"="65-69"),
       year=as.character(year), edu=as.character(edu))

##
byedu <-nemeth %>%
  filter((country=="Denmark"|country=="Sweden"), (period=="2001-2005"| period=="2011-2015")) %>%
           mutate(year=substr(period,1,4)) %>%
  left_join(popall %>% filter((country=="Denmark"|country=="Sweden"), (year==2001| year==2011)) %>%
              mutate(edu=as.character(edu)), 
            by=c("country","sex","edu","age", "year"))%>%
  filter(!is.na(prev)) %>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev),
         age=substr(age,1,2)) %>%
  group_by(country,period,sex,age) %>%
  dplyr::summarise(edu=edu,LE=LE,LEnew=sum(LEprev)) %>%
  rename(year=period)

#############LE total from HMD
newcountries <- c("Denmark","Sweden")
newHMDcou <- c("DNK", "SWE")
ages <- seq(from=30, to=65, by=5)
years1 <- c(2001,2011)
years2 <- c(2005,2015)

#read-in data
i=1
country <- newcountries[i]
countryname <- newHMDcou[i]
DxHMD <- readHMDweb(CNTRY=countryname, item="Deaths_1x1", username = "mmuszynska@gmail.com", password = "D8YiaVNkjSJubs4")
NxHMD <- readHMDweb(CNTRY=countryname, item="Exposures_1x1", username = "mmuszynska@gmail.com", password = "D8YiaVNkjSJubs4")

DNKall <- DxHMD %>%
  dplyr::select(Year,Age,Female,Male) %>%
  rename_with(tolower) %>%
  dplyr::rename(fem.Dx=female,male.Dx=male) %>%
  left_join(NxHMD %>%
              dplyr::select(Year,Age,Female,Male) %>%
              mutate(country=country,countryHMD=countryname) %>%
              rename_with(tolower) %>%
              dplyr::rename(fem.Nx=female,male.Nx=male)) 

####
j=1
agej <- ages[j]
k=1  #2009-2010
years <- seq(from=years1[k],to=years2[k],by=1)

DNK <- DNKall %>%
  filter(year %in%years, age>=agej) %>%
  group_by(age) %>%
  dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
  dplyr::summarise(fem.ex=LifeTable(x=age,Dx=femDx,Ex=femNx)$lt$ex[1],
                   male.ex=LifeTable(x=age,Dx=maleDx,Ex=maleNx)$lt$ex[1]) %>%
  mutate(country=country, countryHMD=countryname,  year=paste(years1[1],years2[1],sep="-"),age=agej)
write.table(DNK, file="denswe.csv", sep=",", row.names=FALSE)

for (j in 2:length(ages)){
  agej <- ages[j]
  DNK <- DNKall %>%
    filter(year %in%years, age>=agej) %>%
    group_by(age) %>%
    dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
    dplyr::summarise(fem.ex=LifeTable(x=age,Dx=femDx,Ex=femNx)$lt$ex[1],
                     male.ex=LifeTable(x=age,Dx=maleDx,Ex=maleNx)$lt$ex[1]) %>%
    mutate(country=country, countryHMD=countryname,  year=paste(years1[k],years2[k],sep="-"),age=agej)
  write.table(DNK, file="denswe.csv", sep=",", row.names=FALSE,col.names=FALSE, append=TRUE)
}


k=2
for (j in 1:length(ages)){
  agej <- ages[j]
  years <- seq(from=years1[k],to=years2[k],by=1)
  DNK <- DNKall %>%
      filter(year %in%years, age>=agej) %>%
      group_by(age) %>%
      dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
      dplyr::summarise(fem.ex=LifeTable(x=age,Dx=femDx,Ex=femNx)$lt$ex[1],
                     male.ex=LifeTable(x=age,Dx=maleDx,Ex=maleNx)$lt$ex[1]) %>%
  mutate(country=country, countryHMD=countryname,  year=paste(years1[k],years2[k],sep="-"),age=agej)
  write.table(DNK, file="denswe.csv", sep=",", row.names=FALSE,col.names=FALSE, append=TRUE)
}



i=2
country <- newcountries[i]
countryname <- newHMDcou[i]

DxHMD <- readHMDweb(CNTRY=countryname, item="Deaths_1x1", username = "mmuszynska@gmail.com", password = "D8YiaVNkjSJubs4")
NxHMD <- readHMDweb(CNTRY=countryname, item="Exposures_1x1", username = "mmuszynska@gmail.com", password = "D8YiaVNkjSJubs4")

for (k in 1:2){
  for (j in 1:length(ages)){
        agej <- ages[j]
        years <- seq(from=years1[k],to=years2[k],by=1)
        all <- DxHMD %>%
        dplyr::select(Year,Age,Female,Male) %>%
        rename_with(tolower) %>%
        dplyr::rename(fem.Dx=female,male.Dx=male) %>%
        left_join(NxHMD %>%
                    dplyr::select(Year,Age,Female,Male) %>%
                    mutate(country=country,countryHMD=countryname) %>%
                    rename_with(tolower) %>%
                    dplyr::rename(fem.Nx=female,male.Nx=male)) 
        
      new <- all %>%
      filter(year %in%years, age>=agej) %>%
      group_by(age) %>%
      dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
      dplyr::summarise(fem.ex=LifeTable(x=age,Dx=femDx,Ex=femNx)$lt$ex[1],
                       male.ex=LifeTable(x=age,Dx=maleDx,Ex=maleNx)$lt$ex[1]) %>%
      mutate(country=country, countryHMD=countryname,  year=paste(years1[k],years2[k],sep="-"),age=agej)
      write.table(new, file="denswe.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
  }
}

#####################
DNKSWE <- read.table(file="denswe.csv", sep=",", header=TRUE)

LEall <- DNKSWE %>%
  pivot_longer(fem.ex:male.ex, names_to="sex", values_to="LE_total") %>%
  mutate(sex=substr(sex,1,1)) %>%
  left_join(byedu %>% mutate(age=as.numeric(age)), by=c("country","year","age","sex")) %>%
  pivot_longer(c(LE_total,LE,LEnew), values_to="LE", names_to="type") %>%
  distinct(LE, .keep_all= TRUE) %>%
  mutate(edu= recode(edu,"1"="low","2"="medium", "3"="high")) %>%
  mutate(type=ifelse(type=="LE", paste(type,edu,sep="_"),type),
         type=recode(type,"LEnew"="LE_new"))

 

dev.off()
pdf(file = "C:/Users/Magdalena/demography/education/figures/femdnkswe.pdf",  width = 8, height = 8, onefile=FALSE) 

ggarrange(ggplot(data=LEall
                 %>% filter(country=="Denmark", year=="2001-2005", sex=="f"), aes(x=age, y=LE, group=type))  + 
            geom_line(aes(linetype=type, col=type))+
            scale_linetype_manual(values=c(2,3,4,1,1))+
            scale_color_manual(values=c(rep("black",4),"red"))+
            theme(legend.position="none") + 
            theme(plot.margin = unit(c(0.5,0.5,1,1), "lines"))+
            theme(
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              axis.line = element_line(colour = "black", size = 0.2, linetype = "solid"))+
            scale_x_continuous(name="Age")+ scale_y_continuous(name="Life expectancy")+
            ggtitle("Denmark, 2001-2005") + theme(plot.title = element_text(face="bold")),
          
          ggplot(data=LEall
                 %>% filter(country=="Denmark", year=="2011-2015", sex=="f"), aes(x=age, y=LE, group=type))  + 
            geom_line(aes(linetype=type, col=type))+
            scale_linetype_manual(values=c(2,3,4,1,1))+
            scale_color_manual(values=c(rep("black",4),"red"))+
            theme(legend.position="none") + 
            theme(plot.margin = unit(c(0.5,0.5,1,1), "lines"))+
            theme(
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              axis.line = element_line(colour = "black", size = 0.2, linetype = "solid"))+
            scale_x_continuous(name="Age")+ scale_y_continuous(name="Life expectancy")+
            ggtitle("Denmark, 2011-2015") + theme(plot.title = element_text(face="bold")),
          
          ggplot(data=LEall
                 %>% filter(country=="Sweden", year=="2001-2005", sex=="f"), aes(x=age, y=LE, group=type))  + 
            geom_line(aes(linetype=type, col=type))+
            scale_linetype_manual(values=c(2,3,4,1,1))+
            scale_color_manual(values=c(rep("black",4),"red"))+
            theme(legend.position="none") + 
            theme(plot.margin = unit(c(0.5,0.5,1,1), "lines"))+
            theme(
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              axis.line = element_line(colour = "black", size = 0.2, linetype = "solid"))+
            scale_x_continuous(name="Age")+ scale_y_continuous(name="Life expectancy")+
            ggtitle("Sweden, 2001-2005") + theme(plot.title = element_text(face="bold")),
          
          ggplot(data=LEall
                 %>% filter(country=="Sweden", year=="2011-2015", sex=="f"), aes(x=age, y=LE, group=type))  + 
            geom_line(aes(linetype=type, col=type))+
            scale_linetype_manual(values=c(2,3,4,1,1))+
            scale_color_manual(values=c(rep("black",4),"red"))+
            theme(legend.position="none") + 
            theme(plot.margin = unit(c(0.5,0.5,1,1), "lines"))+
            theme(
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              axis.line = element_line(colour = "black", size = 0.2, linetype = "solid"))+
            scale_x_continuous(name="Age")+ scale_y_continuous(name="Life expectancy")+
            ggtitle("Sweden, 2011-2015") + theme(plot.title = element_text(face="bold")),
      nrow=2,ncol=2,common.legend = TRUE, legend="bottom")

dev.off()



dev.off()
pdf(file = "C:/Users/Magdalena/demography/education/figures/malednkswe.pdf",  width = 8, height = 8, onefile=FALSE) 

ggarrange(ggplot(data=LEall
                 %>% filter(country=="Denmark", year=="2001-2005", sex=="m"), aes(x=age, y=LE, group=type))  + 
            geom_line(aes(linetype=type, col=type))+
            scale_linetype_manual(values=c(2,3,4,1,1))+
            scale_color_manual(values=c(rep("black",4),"red"))+
            theme(legend.position="none") + 
            theme(plot.margin = unit(c(0.5,0.5,1,1), "lines"))+
            theme(
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              axis.line = element_line(colour = "black", size = 0.2, linetype = "solid"))+
            scale_x_continuous(name="Age")+ scale_y_continuous(name="Life expectancy")+
            ggtitle("Denmark, 2001-2005") + theme(plot.title = element_text(face="bold")),
          
          ggplot(data=LEall
                 %>% filter(country=="Denmark", year=="2011-2015", sex=="m"), aes(x=age, y=LE, group=type))  + 
            geom_line(aes(linetype=type, col=type))+
            scale_linetype_manual(values=c(2,3,4,1,1))+
            scale_color_manual(values=c(rep("black",4),"red"))+
            theme(legend.position="none") + 
            theme(plot.margin = unit(c(0.5,0.5,1,1), "lines"))+
            theme(
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              axis.line = element_line(colour = "black", size = 0.2, linetype = "solid"))+
            scale_x_continuous(name="Age")+ scale_y_continuous(name="Life expectancy")+
            ggtitle("Denmark, 2011-2015") + theme(plot.title = element_text(face="bold")),
          
          ggplot(data=LEall
                 %>% filter(country=="Sweden", year=="2001-2005", sex=="m"), aes(x=age, y=LE, group=type))  + 
            geom_line(aes(linetype=type, col=type))+
            scale_linetype_manual(values=c(2,3,4,1,1))+
            scale_color_manual(values=c(rep("black",4),"red"))+
            theme(legend.position="none") + 
            theme(plot.margin = unit(c(0.5,0.5,1,1), "lines"))+
            theme(
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              axis.line = element_line(colour = "black", size = 0.2, linetype = "solid"))+
            scale_x_continuous(name="Age")+ scale_y_continuous(name="Life expectancy")+
            ggtitle("Sweden, 2001-2005") + theme(plot.title = element_text(face="bold")),
          
          ggplot(data=LEall
                 %>% filter(country=="Sweden", year=="2011-2015", sex=="m"), aes(x=age, y=LE, group=type))  + 
            geom_line(aes(linetype=type, col=type))+
            scale_linetype_manual(values=c(2,3,4,1,1))+
            scale_color_manual(values=c(rep("black",4),"red"))+
            theme(legend.position="none") + 
            theme(plot.margin = unit(c(0.5,0.5,1,1), "lines"))+
            theme(
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              axis.line = element_line(colour = "black", size = 0.2, linetype = "solid"))+
            scale_x_continuous(name="Age")+ scale_y_continuous(name="Life expectancy")+
            ggtitle("Sweden, 2011-2015") + theme(plot.title = element_text(face="bold")),
          nrow=2,ncol=2,common.legend = TRUE, legend="bottom")

dev.off()



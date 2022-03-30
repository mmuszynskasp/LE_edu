#############################################################################################################################
dev.off()
rm(list=ls())
library(tidyverse)
library(dplyr)
library(HMDHFDplus)
library(MortalityLaws)
library(xtable)
library(ggrepel)
library(ggpubr)

############################################################################################################################
##all observation except from van Raalte
###########################################################################################################################
data.dir <- "C:\\Users\\Magdalena\\demography\\education\\data\\ready"
setwd(data.dir)

allLE <- read.table(file="allLE.csv", sep=",",header=TRUE)
HMDcountries <- c("AUS","AUT","BEL", "CAN", "DNK", "FIN", "FRATNP", "ISR","ITA","LVA", "NOR",  
                  "SVN","ESP","SWE","USA")
countrynames <- unique(allLE$country[str_sub(allLE$country,-3)!="van"]) 
allLE <- allLE %>% 
  filter(str_sub(country,-3)!="van") # out the vanraalte observations  

years <- unique(allLE$year)
ages <- unique(substr(allLE$age,1,2))

###years for LE by education single and in between years range
yearseasy <- years[nchar(years)==4]
yearsdiff <- years[nchar(years)>4]

##read-in data from HMD for LE all edu levels
i=1
countryHMD <- HMDcountries[i]
countryname <- countrynames[i]

femHMDle1 <- readHMDweb(CNTRY=countryHMD, item="fltper_1x1", username = "mmuszynska@gmail.com", password = "D8YiaVNkjSJubs4")
maleHMDle1 <- readHMDweb(CNTRY=countryHMD, item="mltper_1x1", username = "mmuszynska@gmail.com", password = "D8YiaVNkjSJubs4")
le <- femHMDle1 %>%
  mutate(sex="f") %>%
  bind_rows(maleHMDle1 %>% mutate(sex="m")) %>%
  #  filter(Year %in% as.numeric(yearssel),Age %in% as.numeric(agessel)) %>%
  dplyr::select(Year,Age,sex,ex) %>%
  rename_with(tolower)%>%
  mutate(country=countryname,countryHMD=countryHMD) 
write.table(le, file="LEalledu.csv", sep=",", row.names=FALSE)

for (i in 2:length(HMDcountries)){
  countryHMD <- HMDcountries[i]
  countryname <- countrynames[i]
  femHMDle1 <- readHMDweb(CNTRY=countryHMD, item="fltper_1x1", username = "mmuszynska@gmail.com", password = "D8YiaVNkjSJubs4")
  maleHMDle1 <- readHMDweb(CNTRY=countryHMD, item="mltper_1x1", username = "mmuszynska@gmail.com", password = "D8YiaVNkjSJubs4")
  le <- femHMDle1 %>%
    mutate(sex="f") %>%
    bind_rows(maleHMDle1 %>% mutate(sex="m")) %>%
    #    filter(Year %in% as.numeric(yearseasy),Age %in% as.numeric(ages)) %>%
    dplyr::select(Year,Age,sex,ex) %>%
    rename_with(tolower) %>%
    mutate(country=countryname,countryHMD=countryHMD) 
  write.table(le, file="LEalledu.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
}

############################################################################################################################
LEalledu <- read.table(file="LEalledu.csv", sep=",",header=TRUE)

allLEtab1 <- allLE %>%
  mutate(age=substr(age,1,2)) %>%
  left_join(LEalledu %>% #add those with single years
              mutate(year=as.character(year), age=as.character(age))) %>%
  mutate(diffLE=as.numeric(LEnew)-as.numeric(ex)) %>%
  mutate(diffrel=100*diffLE/as.numeric(ex))

allLEtabsin <- allLEtab1 %>%
  filter(!is.na(ex))


###############################################################################################################################
##### ex to be estimated for multiple years, here country by country, too complex for general formulas
alltabmore <- allLEtab1 %>%
  filter(is.na(ex)) %>%
  dplyr::select(sex,year,LEnew,age,country)

newcountries <- c("Denmark", "Spain", "Sweden")
newHMDcou <- c("DNK", "ESP", "SWE")

####ESP
i=2
country <- newcountries[i]
countryname <- newHMDcou[i]
#years and age in LE by edu
ages <- unique(as.numeric((alltabmore$age[alltabmore$country==country])))
years1 <- unique(as.numeric(substr(alltabmore$year[alltabmore$country==country],1,4)))
years2 <- unique(as.numeric(substr(alltabmore$year[alltabmore$country==country],6,9)))

DxHMD <- readHMDweb(CNTRY=countryname, item="Deaths_1x1", username = "mmuszynska@gmail.com", password = "D8YiaVNkjSJubs4")
NxHMD <- readHMDweb(CNTRY=countryname, item="Exposures_1x1", username = "mmuszynska@gmail.com", password = "D8YiaVNkjSJubs4")

ex <- DxHMD %>%
  dplyr::select(Year,Age,Female,Male) %>%
  rename_with(tolower) %>%
  dplyr::rename(fem.Dx=female,male.Dx=male) %>%
  left_join(NxHMD %>%
              dplyr::select(Year,Age,Female,Male) %>%
              mutate(country=country,countryHMD=countryname) %>%
              rename_with(tolower) %>%
              dplyr::rename(fem.Nx=female,male.Nx=male)) 


yearsa <- seq(from=as.numeric(years1[1]),to=as.numeric(years2[1]),by=1)
yearsb <- seq(from=as.numeric(years1[2]),to=as.numeric(years2[2]),by=1)


ESPex1 <- ex%>%
  filter(year %in%yearsa, age>=ages) %>%
  group_by(age) %>%
  dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
  dplyr::summarise(fem.ex=LifeTable(x=age,Dx=femDx,Ex=femNx)$lt$ex[1],
                   male.ex=LifeTable(x=age,Dx=maleDx,Ex=maleNx)$lt$ex[1]) %>%
  mutate(country=country, countryHMD=countryname, year=paste(years1[1],years2[1],sep="-"),age=ages)

ESPex2 <- ex%>%
  filter(year %in%yearsb, age>=ages) %>%
  group_by(age) %>%
  dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
  dplyr::summarise(fem.ex=LifeTable(x=age,Dx=femDx,Ex=femNx)$lt$ex[1],
                   male.ex=LifeTable(x=age,Dx=maleDx,Ex=maleNx)$lt$ex[1]) %>%
  mutate(country=country, countryHMD=countryname, year=paste(years1[2],years2[2],sep="-"),age=ages) %>%
  bind_rows(ESPex1)


write.table(ESPex2,file="LEalledu2.csv", sep=",", row.names=FALSE)

###DNK  
i=1
country <- newcountries[i]
countryname <- newHMDcou[i]
#years and age in LE by edu
ages <- unique(as.numeric((alltabmore$age[alltabmore$country==country])))
years1 <- unique(as.numeric(substr(alltabmore$year[alltabmore$country==country],1,4)))
years2 <- unique(as.numeric(substr(alltabmore$year[alltabmore$country==country],6,9)))

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

###2009-2010
years <- seq(from=years1[1],to=years2[1],by=1)
agej <- ages[1]
DNK11 <- DNKall %>%
  filter(year %in%years, age>=agej) %>%
  group_by(age) %>%
  dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
  dplyr::summarise(fem.ex=LifeTable(x=age,Dx=femDx,Ex=femNx)$lt$ex[1],
                   male.ex=LifeTable(x=age,Dx=maleDx,Ex=maleNx)$lt$ex[1]) %>%
  mutate(country=country, countryHMD=countryname,  year=paste(years1[1],years2[1],sep="-"),age=agej)

agej <- ages[2]
DNK12 <- DNKall %>%
  filter(year %in%years, age>=agej) %>%
  group_by(age) %>%
  dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
  dplyr::summarise(fem.ex=LifeTable(x=age,Dx=femDx,Ex=femNx)$lt$ex[1],
                   male.ex=LifeTable(x=age,Dx=maleDx,Ex=maleNx)$lt$ex[1]) %>%
  mutate(country=country, countryHMD=countryname,  year=paste(years1[1],years2[1],sep="-"),age=agej)

agej <- ages[3]
DNK13 <- DNKall %>%
  filter(year %in%years, age>=agej) %>%
  group_by(age) %>%
  dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
  dplyr::summarise(fem.ex=LifeTable(x=age,Dx=femDx,Ex=femNx)$lt$ex[1],
                   male.ex=LifeTable(x=age,Dx=maleDx,Ex=maleNx)$lt$ex[1]) %>%
  mutate(country=country, countryHMD=countryname,  year=paste(years1[1],years2[1],sep="-"),age=agej)

###2011-2015
years <- seq(from=years1[2],to=years2[2],by=1)
agej <- ages[1]
DNK21 <- DNKall %>%
  filter(year %in%years, age>=agej) %>%
  group_by(age) %>%
  dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
  dplyr::summarise(fem.ex=LifeTable(x=age,Dx=femDx,Ex=femNx)$lt$ex[1],
                   male.ex=LifeTable(x=age,Dx=maleDx,Ex=maleNx)$lt$ex[1]) %>%
  mutate(country=country, countryHMD=countryname, year=paste(years1[2],years2[2],sep="-"),age=agej)

agej <- ages[2]
DNK22 <- DNKall %>%
  filter(year %in%years, age>=agej) %>%
  group_by(age) %>%
  dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
  dplyr::summarise(fem.ex=LifeTable(x=age,Dx=femDx,Ex=femNx)$lt$ex[1],
                   male.ex=LifeTable(x=age,Dx=maleDx,Ex=maleNx)$lt$ex[1]) %>%
  mutate(country=country, countryHMD=countryname,  year=paste(years1[2],years2[2],sep="-"),age=agej)

agej <- ages[3]
DNK23 <- DNKall %>%
  filter(year %in%years, age>=agej) %>%
  group_by(age) %>%
  dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
  dplyr::summarise(fem.ex=LifeTable(x=age,Dx=femDx,Ex=femNx)$lt$ex[1],
                   male.ex=LifeTable(x=age,Dx=maleDx,Ex=maleNx)$lt$ex[1]) %>%
  mutate(country=country, countryHMD=countryname,  year=paste(years1[2],years2[2],sep="-"),age=agej)

DNK <- DNK11 %>%
  bind_rows(DNK12,DNK13,DNK21,DNK22,DNK23)

write.table(DNK,file="LEalledu2.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)

###SWE  
i=3
country <- newcountries[i]
countryname <- newHMDcou[i]
#years and age in LE by edu
ages <- unique(as.numeric((alltabmore$age[alltabmore$country==country])))
years1 <- unique(as.numeric(substr(alltabmore$year[alltabmore$country==country],1,4)))
years2 <- unique(as.numeric(substr(alltabmore$year[alltabmore$country==country],6,9)))

DxHMD <- readHMDweb(CNTRY=countryname, item="Deaths_1x1", username = "mmuszynska@gmail.com", password = "D8YiaVNkjSJubs4")
NxHMD <- readHMDweb(CNTRY=countryname, item="Exposures_1x1", username = "mmuszynska@gmail.com", password = "D8YiaVNkjSJubs4")

SWEall <- DxHMD %>%
  dplyr::select(Year,Age,Female,Male) %>%
  rename_with(tolower) %>%
  dplyr::rename(fem.Dx=female,male.Dx=male) %>%
  left_join(NxHMD %>%
              dplyr::select(Year,Age,Female,Male) %>%
              mutate(country=country,countryHMD=countryname) %>%
              rename_with(tolower) %>%
              dplyr::rename(fem.Nx=female,male.Nx=male)) 

###2001-2005
years <- seq(from=years1[1],to=years2[1],by=1)
agej <- ages[1]
SWE11 <- SWEall %>%
  filter(year %in%years, age>=agej) %>%
  group_by(age) %>%
  dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
  dplyr::summarise(fem.ex=LifeTable(x=age,Dx=femDx,Ex=femNx)$lt$ex[1],
                   male.ex=LifeTable(x=age,Dx=maleDx,Ex=maleNx)$lt$ex[1]) %>%
  mutate(country=country, countryHMD=countryname,  year=paste(years1[1],years2[1],sep="-"),age=agej)

agej <- ages[2]
SWE12 <- SWEall %>%
  filter(year %in%years, age>=agej) %>%
  group_by(age) %>%
  dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
  dplyr::summarise(fem.ex=LifeTable(x=age,Dx=femDx,Ex=femNx)$lt$ex[1],
                   male.ex=LifeTable(x=age,Dx=maleDx,Ex=maleNx)$lt$ex[1]) %>%
  mutate(country=country, countryHMD=countryname,  year=paste(years1[1],years2[1],sep="-"),age=agej)

agej <- ages[3]
SWE13 <- SWEall %>%
  filter(year %in%years, age>=agej) %>%
  group_by(age) %>%
  dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
  dplyr::summarise(fem.ex=LifeTable(x=age,Dx=femDx,Ex=femNx)$lt$ex[1],
                   male.ex=LifeTable(x=age,Dx=maleDx,Ex=maleNx)$lt$ex[1]) %>%
  mutate(country=country, countryHMD=countryname,  year=paste(years1[1],years2[1],sep="-"),age=agej)

###2011-2015
years <- seq(from=years1[2],to=years2[2],by=1)
agej <- ages[1]
SWE21 <- SWEall %>%
  filter(year %in%years, age>=agej) %>%
  group_by(age) %>%
  dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
  dplyr::summarise(fem.ex=LifeTable(x=age,Dx=femDx,Ex=femNx)$lt$ex[1],
                   male.ex=LifeTable(x=age,Dx=maleDx,Ex=maleNx)$lt$ex[1]) %>%
  mutate(country=country, countryHMD=countryname, year=paste(years1[2],years2[2],sep="-"),age=agej)

agej <- ages[2]
SWE22 <- SWEall %>%
  filter(year %in%years, age>=agej) %>%
  group_by(age) %>%
  dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
  dplyr::summarise(fem.ex=LifeTable(x=age,Dx=femDx,Ex=femNx)$lt$ex[1],
                   male.ex=LifeTable(x=age,Dx=maleDx,Ex=maleNx)$lt$ex[1]) %>%
  mutate(country=country, countryHMD=countryname,  year=paste(years1[2],years2[2],sep="-"),age=agej)

agej <- ages[3]
SWE23 <- SWEall %>%
  filter(year %in%years, age>=agej) %>%
  group_by(age) %>%
  dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
  dplyr::summarise(fem.ex=LifeTable(x=age,Dx=femDx,Ex=femNx)$lt$ex[1],
                   male.ex=LifeTable(x=age,Dx=maleDx,Ex=maleNx)$lt$ex[1]) %>%
  mutate(country=country, countryHMD=countryname,  year=paste(years1[2],years2[2],sep="-"),age=agej)

SWE <- SWE11 %>%
  bind_rows(SWE12,SWE13,SWE21,SWE22,SWE23)

write.table(SWE,file="LEalledu2.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)

#################
LEalledu2 <- read.table(file="LEalledu2.csv", sep=",",header=TRUE)

allLEtab2 <- allLE %>%
  mutate(age=substr(age,1,2)) %>%
  filter(nchar(year)>4) %>%
  left_join(LEalledu2 %>% #add those with single years
              mutate(year=as.character(year), age=as.character(age)) %>%
              pivot_longer(fem.ex:male.ex, names_to="sex", values_to="ex") %>%
              mutate(sex=substr(sex,1,1))) %>%
  filter(!is.na(ex)) %>%
  mutate(diffLE=as.numeric(LEnew)-as.numeric(ex)) %>%
  mutate(diffrel=100*diffLE/as.numeric(ex))

#############################################################################################################################
#### together
diffLE <- allLEtab1 %>%
  filter(!is.na(ex)) %>%
  bind_rows(allLEtab2) %>%
  dplyr::select(sex,LEnew,ex,age,year,country,countryHMD,diffLE) %>%
  pivot_wider(
    names_from = sex,
    values_from = c(ex,LEnew,diffLE))%>%
  dplyr::select(age,country,countryHMD,year,LEnew_f,ex_f,diffLE_f,LEnew_m,ex_m, diffLE_m) %>%
  arrange(age,country,year)  


write.table(diffLE,file="diffLE1.csv", sep=",", row.names=FALSE)



############################################################################################################################
#####van raalte
###########################################################################################################################
dev.off()
rm(list=ls())
library(tidyverse)
library(dplyr)
library(HMDHFDplus)
library(MortalityLaws)
library(xtable)
library(ggrepel)
library(ggpubr)

data.dir <- "C:\\Users\\Magdalena\\demography\\education\\data\\ready"
setwd(data.dir)

allLE <- read.table(file="allLE.csv", sep=",",header=TRUE)

HMDcountries.van <- c("CZE","EST","FIN", "FRATNP","LTU", "NOR","POL","CHE")
countrynames.van <- unique(allLE$country[str_sub(allLE$country,-3)=="van"]) 

allLE <- allLE %>% 
  filter(str_sub(country,-3)=="van") %>%
  mutate(country=substr(country,1,nchar(country)-4)) %>%
  mutate(countryHMD=dplyr::recode(country,"Czechia"="CZE","Switzerland"="CHE", "Estonia"="EST",
                                  "Finland"="FIN","France"="FRATNP","Lithuania"="LTU", 
                                  "Norway"="NOR","Poland"="POL"))# out the vanraalte observations  

data.dir <- "C:\\Users\\Magdalena\\demography\\education\\data\\LE"
setwd("C:\\Users\\Magdalena\\demography\\education\\data\\LE")
vanraalte2 <- read.table(file="vanraalte.txt", sep=",", header=TRUE)  
vanraalte <- vanraalte2 %>%
  mutate(ex_m=men_Total-35,ex_f=fem_Total-35,
         country=dplyr::recode(country,"Czech Rep."="Czechia")) %>%
  select(country,ex_m,ex_f) %>%
  left_join(allLE %>% pivot_wider(names_from=sex,values_from=LEnew), by="country") %>%
  filter(!is.na(age)) %>%
  rename(LEnew_f=f,LEnew_m=m) %>%
  mutate(LEnew_f=LEnew_f-35, LEnew_m=LEnew_m-35,
    diffLE_f=LEnew_f-ex_f,diffLE_m=LEnew_m-ex_m,
         age=substr(age,1,2))


####################################################################################################################################
###############bring all together
data.dir <- "C:\\Users\\Magdalena\\demography\\education\\data\\ready"
setwd(data.dir)

diff1 <- read.table(file="diffLE1.csv", sep=",", header=TRUE)


diffLE <- diff1 %>%
  mutate(age=as.character(age)) %>%
  bind_rows(vanraalte) %>%
  dplyr::select(age,country, countryHMD, year,LEnew_f,ex_f,diffLE_f,LEnew_m,ex_m, diffLE_m) %>%
  arrange(age,country,year)  

print(xtable(diffLE[,-c(1,3)], digits=c(0,0,1,1,1,1,1,1,1)),include.rownames=FALSE)

countrylist <- unique(diffLE$country)
write.table(countrylist, file="countrylist.csv", sep=",")
##############################################################################################################################
###############plot
diffLEplot <- diffLE %>%
  mutate(LEx_f=as.numeric(ex_f), LEx_m=as.numeric(ex_m)) %>%
  pivot_longer(c(diffLE_m,diffLE_f), values_to="diff", names_to="sex") %>%
    mutate(sex=substr(sex,8,8),
           Lex=ifelse(sex=="m",LEx_m, LEx_f),
           newlab=paste(countryHMD,year,sep=".")) 

difff <- diffLEplot %>% filter(sex=="f")
diffm <- diffLEplot %>% filter(sex=="m")


dev.off()
pdf(file = "C:/Users/Magdalena/demography/education/figures/difffem.pdf",  width = 10, height = 10) 

ggarrange(ggplot(difff %>% filter(age==25), aes(x=Lex, y=diff, label=newlab)) + geom_point()+
            geom_label_repel(aes(label = newlab),
                             box.padding   = 0.35, point.padding = 0.5,segment.color = 'grey50', direction = "both") +
            theme(plot.margin = unit(c(0.5,0.5,1,1), "lines"))+
            theme(
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              axis.line = element_line(colour = "black", size = 0.2, linetype = "solid"))+
            scale_x_continuous(name="LE")+
            scale_y_continuous(name="LE_new-LE", limits=c(0,4))+
            ggtitle("At 25 years") + theme(plot.title = element_text(face="bold")),
          ggplot(difff %>% filter(age==30), aes(x=Lex, y=diff, label=newlab)) + geom_point()+
            geom_label_repel(aes(label = newlab),
                             box.padding   = 0.35, point.padding = 0.5,segment.color = 'grey50', direction = "both") +
            theme(plot.margin = unit(c(0.5,0.5,1,1), "lines"))+
            theme(
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              axis.line = element_line(colour = "black", size = 0.2, linetype = "solid"))+
            scale_x_continuous(name="LE")+
            scale_y_continuous(name="LE_new-LE", limits=c(0,4))+
            ggtitle("At 30 years") + theme(plot.title = element_text(face="bold")),
          ggplot(difff %>% filter(age==50), aes(x=Lex, y=diff, label=newlab)) + geom_point()+
            geom_label_repel(aes(label = newlab),
                             box.padding   = 0.35, point.padding = 0.5,segment.color = 'grey50', direction = "both") +
            theme(plot.margin = unit(c(0.5,0.5,1,1), "lines"))+
            theme(
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              axis.line = element_line(colour = "black", size = 0.2, linetype = "solid"))+
            scale_x_continuous(name="LE")+
            scale_y_continuous(name="LE_new-LE", limits=c(0,4))+
            ggtitle("At 50 years") + theme(plot.title = element_text(face="bold")),
          ggplot(difff %>% filter(age==65), aes(x=Lex, y=diff, label=newlab)) + geom_point()+
            geom_label_repel(aes(label = newlab),
                             box.padding   = 0.35, point.padding = 0.5,segment.color = 'grey50', direction = "both") +
            theme(plot.margin = unit(c(0.5,0.5,1,1), "lines"))+
            theme(
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              axis.line = element_line(colour = "black", size = 0.2, linetype = "solid"))+
            scale_x_continuous(name="LE")+
            scale_y_continuous(name="LE_new-LE", limits=c(0,4))+
            ggtitle("At 65 years") + theme(plot.title = element_text(face="bold")), nrow=2, ncol=2)
dev.off()




dev.off()
pdf(file = "C:/Users/Magdalena/demography/education/figures/diffmale.pdf",  width = 10, height = 10) 

ggarrange(ggplot(diffm %>% filter(age==25), aes(x=Lex, y=diff, label=newlab)) + geom_point()+ 
            geom_label_repel(aes(label = newlab),
                             box.padding   = 0.35, point.padding = 0.5,segment.color = 'grey50', direction = "both") +
            theme(plot.margin = unit(c(0.5,0.5,1,1), "lines"))+
            theme(
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              axis.line = element_line(colour = "black", size = 0.2, linetype = "solid"))+
            scale_x_continuous(name="LE")+
            scale_y_continuous(name="LE_new-LE", limits=c(0,4))+
            ggtitle("At 25 years") + theme(plot.title = element_text(face="bold")),
          ggplot(diffm %>% filter(age==30), aes(x=Lex, y=diff, label=newlab)) + geom_point()+ 
            geom_label_repel(aes(label = newlab),
                             box.padding   = 0.35, point.padding = 0.5,segment.color = 'grey50', direction = "both")+ 
            theme(plot.margin = unit(c(0.5,0.5,1,1), "lines"))+
            theme(
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              axis.line = element_line(colour = "black", size = 0.2, linetype = "solid"))+
            scale_x_continuous(name="LE")+
            scale_y_continuous(name="LE_new-LE", limits=c(0,4))+
            ggtitle("At 30 years") + theme(plot.title = element_text(face="bold")),
          ggplot(diffm %>% filter(age==50), aes(x=Lex, y=diff, label=newlab)) + geom_point()+ 
            geom_label_repel(aes(label = newlab),
                             box.padding   = 0.35, point.padding = 0.5,segment.color = 'grey50', direction = "both") +
            theme(plot.margin = unit(c(0.5,0.5,1,1), "lines"))+
            theme(
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              axis.line = element_line(colour = "black", size = 0.2, linetype = "solid"))+
            scale_x_continuous(name="LE")+
            scale_y_continuous(name="LE_new-LE", limits=c(0,4))+
            ggtitle("At 50 years") + theme(plot.title = element_text(face="bold")),
          ggplot(diffm %>% filter(age==65), aes(x=Lex, y=diff, label=newlab)) + geom_point()+
            geom_label_repel(aes(label = newlab),
                             box.padding   = 0.35, point.padding = 0.5,segment.color = 'grey50', direction = "both") +
            theme(plot.margin = unit(c(0.5,0.5,1,1), "lines"))+
            theme(
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              axis.line = element_line(colour = "black", size = 0.2, linetype = "solid"))+
            scale_x_continuous(name="LE")+
            scale_y_continuous(name="LE_new-LE", limits=c(0,4))+
            ggtitle("At 65 years") + theme(plot.title = element_text(face="bold")), nrow=2, ncol=2)
dev.off()



dev.off()
pdf(file = "C:/Users/Magdalena/demography/education/figures/both35.pdf",  width = 10, height = 5) 

ggarrange(ggplot(difff %>% filter(age==35), aes(x=Lex, y=diff, label=newlab)) + geom_point()+
            geom_label_repel(aes(label = newlab),
                             box.padding   = 0.35, point.padding = 0.5,segment.color = 'grey50', direction = "both") +
            theme(plot.margin = unit(c(0.5,0.5,1,1), "lines"))+
            theme(
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              axis.line = element_line(colour = "black", size = 0.2, linetype = "solid"))+
            scale_x_continuous(name="LE")+
            scale_y_continuous(name="LE_new-LE", limits=c(0,4))+
            ggtitle("Female") + theme(plot.title = element_text(face="bold")),
          ggplot(diffm %>% filter(age==35), aes(x=Lex, y=diff, label=newlab)) + geom_point()+
            geom_label_repel(aes(label = newlab),
                             box.padding   = 0.35, point.padding = 0.5,segment.color = 'grey50', direction = "both") +
            theme(plot.margin = unit(c(0.5,0.5,1,1), "lines"))+
            theme(
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              axis.line = element_line(colour = "black", size = 0.2, linetype = "solid"))+
            scale_x_continuous(name="LE")+
            scale_y_continuous(name="LE_new-LE", limits=c(0,4))+
            ggtitle("Male") + theme(plot.title = element_text(face="bold")), nrow=1, ncol=2)
dev.off()





diffLEstat <- diffLE %>%
  mutate(diffLE_f=ifelse(diffLE_f<0,NA,diffLE_f),
         diffLE_m=ifelse(diffLE_m<0,NA,diffLE_m),
         diffrm=100*diffLE_m/ex_m, diffrf=100*diffLE_f/ex_f) %>%
  group_by(age) %>%
  dplyr::summarise(obs=n(),femm=mean(diffLE_f, na.rm=TRUE),
                   femmin=min(diffLE_f, na.rm=TRUE),femmax=max(diffLE_f, na.rm=TRUE),
                  # femmrel=mean(100*diffLE_f/ex_f, na.rm=TRUE),
                   malem=mean(diffLE_m, na.rm=TRUE), malemin=min(diffLE_m, na.rm=TRUE),malemax=max(diffLE_m, na.rm=TRUE))
                  # malemrel=mean(100*diffLE_m/ex_m, na.rm=TRUE))

print(xtable(diffLEstat, digits=c(0,0,0,1,1,1,1,1,1)),include.rownames=FALSE)



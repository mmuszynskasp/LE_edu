#############################################################################################################################
dev.off()
rm(list=ls())
library(tidyverse)
library(dplyr)
library(HMDHFDplus)
library(MortalityLaws)
library(xtable)

data.dir <- "C:\\Users\\Magdalena\\demography\\education\\data\\ready"
setwd(data.dir)

allLE <- read.table(file="allLE.csv", sep=",",header=TRUE)

############################################################################################################################
##read-in data from HMD for LE all edu levels
###########################################################################################################################
HMDcountries <- c("AUS","AUT","BEL", "CAN","CZE", "DNK", "EST","FIN", "FRATNP", "ISR","ITA","LVA", "LTU","NOR", "POL", 
                  "SVN","ESP","SWE","CHE","USA")
countrynames <- unique(allLE$country)
years <- unique(allLE$year)
ages <- unique(substr(allLE$age,1,2))

###years for LE by education single and in between years range
yearseasy <- years[nchar(years)==4]
yearsdiff <- years[nchar(years)>4]


i=1
country <- HMDcountries[i]
countryname <- countrynames[i]

femHMDle1 <- readHMDweb(CNTRY=country, item="fltper_1x1", username = "mmuszynska@gmail.com", password = "D8YiaVNkjSJubs4")
maleHMDle1 <- readHMDweb(CNTRY=country, item="mltper_1x1", username = "mmuszynska@gmail.com", password = "D8YiaVNkjSJubs4")
le <- femHMDle1 %>%
  mutate(sex="f") %>%
  bind_rows(maleHMDle1 %>% mutate(sex="m")) %>%
  #  filter(Year %in% as.numeric(yearssel),Age %in% as.numeric(agessel)) %>%
  dplyr::select(Year,Age,sex,ex) %>%
  mutate(country=countryname) %>%
  rename_with(tolower)
write.table(le, file="LEalledu.csv", sep=",", row.names=FALSE)

for (i in 2:length(HMDcountries)){
  country <- HMDcountries[i]
  countryname <- countrynames[i]
  femHMDle1 <- readHMDweb(CNTRY=country, item="fltper_1x1", username = "mmuszynska@gmail.com", password = "D8YiaVNkjSJubs4")
  maleHMDle1 <- readHMDweb(CNTRY=country, item="mltper_1x1", username = "mmuszynska@gmail.com", password = "D8YiaVNkjSJubs4")
  le <- femHMDle1 %>%
    mutate(sex="f") %>%
    bind_rows(maleHMDle1 %>% mutate(sex="m")) %>%
    #    filter(Year %in% as.numeric(yearseasy),Age %in% as.numeric(ages)) %>%
    dplyr::select(Year,Age,sex,ex) %>%
    mutate(country=countryname) %>%
    rename_with(tolower)
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

newcountries <- unique(alltabmore$country)
newHMDcou <- c("CZE", "DNK", "EST","FIN", "FRATNP", "NOR", "ESP","CHE")

####CZE
i=1
country <- newcountries[i]
countryname <- newHMDcou[i]
#years and age in LE by edu
ages <- unique(as.numeric((alltabmore$age[alltabmore$country==country])))
years1 <- unique(as.numeric(substr(alltabmore$year[alltabmore$country==country],1,4)))
years2 <- unique(as.numeric(substr(alltabmore$year[alltabmore$country==country],6,9)))
years <- seq(from=years1,to=years2,by=1)

DxHMD <- readHMDweb(CNTRY=countryname, item="Deaths_1x1", username = "mmuszynska@gmail.com", password = "D8YiaVNkjSJubs4")
NxHMD <- readHMDweb(CNTRY=countryname, item="Exposures_1x1", username = "mmuszynska@gmail.com", password = "D8YiaVNkjSJubs4")

ex <- DxHMD %>%
  dplyr::select(Year,Age,Female,Male) %>%
  rename_with(tolower) %>%
  dplyr::rename(fem.Dx=female,male.Dx=male) %>%
  left_join(NxHMD %>%
              dplyr::select(Year,Age,Female,Male) %>%
              mutate(country=countryname) %>%
              rename_with(tolower) %>%
              dplyr::rename(fem.Nx=female,male.Nx=male)) %>%
  filter(year %in%years, age>=ages) %>%
  group_by(age) %>%
  dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
  dplyr::summarise(fem.ex=LifeTable(x=age,Dx=femDx,Ex=femNx)$lt$ex[1],
                   male.ex=LifeTable(x=age,Dx=maleDx,Ex=maleNx)$lt$ex[1]) %>%
  mutate(country=country, year=paste(years1,years2,sep="-"),age=ages)

write.table(ex,file="LEalledu2.csv", sep=",", row.names=FALSE)

###other except DNK
for (i in 3:length(newcountries)){
  country <- newcountries[i]
  countryname <- newHMDcou[i]
  #years and age in LE by edu
  ages <- unique(as.numeric((alltabmore$age[alltabmore$country==country])))
  years1 <- unique(as.numeric(substr(alltabmore$year[alltabmore$country==country],1,4)))
  years2 <- unique(as.numeric(substr(alltabmore$year[alltabmore$country==country],6,9)))
  years <- seq(from=years1,to=years2,by=1)
  
  DxHMD <- readHMDweb(CNTRY=countryname, item="Deaths_1x1", username = "mmuszynska@gmail.com", password = "D8YiaVNkjSJubs4")
  NxHMD <- readHMDweb(CNTRY=countryname, item="Exposures_1x1", username = "mmuszynska@gmail.com", password = "D8YiaVNkjSJubs4")
  
  ex <- DxHMD %>%
    dplyr::select(Year,Age,Female,Male) %>%
    rename_with(tolower) %>%
    dplyr::rename(fem.Dx=female,male.Dx=male) %>%
    left_join(NxHMD %>%
                dplyr::select(Year,Age,Female,Male) %>%
                mutate(country=countryname) %>%
                rename_with(tolower) %>%
                dplyr::rename(fem.Nx=female,male.Nx=male)) %>%
    filter(year %in%years, age>=ages) %>%
    group_by(age) %>%
    dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
    dplyr::summarise(fem.ex=LifeTable(x=age,Dx=femDx,Ex=femNx)$lt$ex[1],
                     male.ex=LifeTable(x=age,Dx=maleDx,Ex=maleNx)$lt$ex[1]) %>%
    mutate(country=country, year=paste(years1,years2,sep="-"),age=ages)
  
  write.table(ex,file="LEalledu2.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
}


#########################################################################################################################
###DNK  
i=2
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
              mutate(country=countryname) %>%
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
  mutate(country=country, year=paste(years1[1],years2[1],sep="-"),age=agej)

agej <- ages[2]
DNK12 <- DNKall %>%
  filter(year %in%years, age>=agej) %>%
  group_by(age) %>%
  dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
  dplyr::summarise(fem.ex=LifeTable(x=age,Dx=femDx,Ex=femNx)$lt$ex[1],
                   male.ex=LifeTable(x=age,Dx=maleDx,Ex=maleNx)$lt$ex[1]) %>%
  mutate(country=country, year=paste(years1[1],years2[1],sep="-"),age=agej)

agej <- ages[3]
DNK13 <- DNKall %>%
  filter(year %in%years, age>=agej) %>%
  group_by(age) %>%
  dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
  dplyr::summarise(fem.ex=LifeTable(x=age,Dx=femDx,Ex=femNx)$lt$ex[1],
                   male.ex=LifeTable(x=age,Dx=maleDx,Ex=maleNx)$lt$ex[1]) %>%
  mutate(country=country, year=paste(years1[1],years2[1],sep="-"),age=agej)

###2011-2015
years <- seq(from=years1[2],to=years2[2],by=1)
agej <- ages[1]
DNK21 <- DNKall %>%
  filter(year %in%years, age>=agej) %>%
  group_by(age) %>%
  dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
  dplyr::summarise(fem.ex=LifeTable(x=age,Dx=femDx,Ex=femNx)$lt$ex[1],
                   male.ex=LifeTable(x=age,Dx=maleDx,Ex=maleNx)$lt$ex[1]) %>%
  mutate(country=country, year=paste(years1[2],years2[2],sep="-"),age=agej)

agej <- ages[2]
DNK22 <- DNKall %>%
  filter(year %in%years, age>=agej) %>%
  group_by(age) %>%
  dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
  dplyr::summarise(fem.ex=LifeTable(x=age,Dx=femDx,Ex=femNx)$lt$ex[1],
                   male.ex=LifeTable(x=age,Dx=maleDx,Ex=maleNx)$lt$ex[1]) %>%
  mutate(country=country, year=paste(years1[2],years2[2],sep="-"),age=agej)

agej <- ages[3]
DNK23 <- DNKall %>%
  filter(year %in%years, age>=agej) %>%
  group_by(age) %>%
  dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
  dplyr::summarise(fem.ex=LifeTable(x=age,Dx=femDx,Ex=femNx)$lt$ex[1],
                   male.ex=LifeTable(x=age,Dx=maleDx,Ex=maleNx)$lt$ex[1]) %>%
  mutate(country=country, year=paste(years1[2],years2[2],sep="-"),age=agej)

DNK <- DNK11 %>%
  bind_rows(DNK12,DNK13,DNK21,DNK22,DNK23)

write.table(DNK,file="LEalledu2.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)

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
  dplyr::select(sex,LEnew,ex,age,year,country,diffLE) %>%
  pivot_wider(
    names_from = sex,
    values_from = c(ex,LEnew,diffLE))%>%
  dplyr::select(age,country,year,LEnew_f,ex_f,diffLE_f,LEnew_m,ex_m, diffLE_m) %>%
  arrange(age,country,year)  

print(xtable(diffLE, digits=c(0,0,0,0,1,1,1,1,1,1)),include.rownames=FALSE)



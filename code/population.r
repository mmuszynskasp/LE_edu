#############################################################################################################################
dev.off()
rm(list=ls())
library(tidyverse)
library(dplyr)
library(eurostat)
library(ipumsr)

data.dir <- "C:\\Users\\Magdalena\\demography\\education\\data\\population"
data.out <- "C:\\Users\\Magdalena\\demography\\education\\data\\ready"
setwd(data.dir)

ddi <- read_ipums_ddi("ipumsi_00008.xml")
all<- read_ipums_micro(ddi)
israel <- read_ipums_ddi("israel.xml")
israel<- read_ipums_micro(israel)
canada <- read_ipums_ddi("ipumsi_00009.xml")
canada <- read_ipums_micro(canada)

memory.limit(400000)
allIPUMS <- all %>%
  filter(COUNTRY!=840, SEX!=9, EDATTAIN!=0) %>%
  select(COUNTRY,YEAR,SAMPLE,AGE,SEX,EDATTAIN) %>% 
  bind_rows(israel %>%  select(COUNTRY,YEAR,SAMPLE,AGE,SEX,EDATTAIN)) %>%
  bind_rows(canada %>%  select(COUNTRY,YEAR,SAMPLE,AGE,SEX,EDATTAIN)) %>%
  filter(AGE>=25, AGE<=70) %>%
  mutate(age="25-29", 
         age=ifelse(as.numeric(AGE)>29,"30-34",age),
         age=ifelse(as.numeric(AGE)>34,"35-39",age),
         age=ifelse(as.numeric(AGE)>39,"40-44",age),
         age=ifelse(as.numeric(AGE)>44,"45-49",age),
         age=ifelse(as.numeric(AGE)>49,"50-54",age),
         age=ifelse(as.numeric(AGE)>54,"35-39",age),
         age=ifelse(as.numeric(AGE)>59,"60-64",age),
         age=ifelse(as.numeric(AGE)>64,"65-69",age),
         sex=recode(as.double(SEX),"2"="f", "1"="m")) %>%
  dplyr::select("COUNTRY", "YEAR","sex", "age","EDATTAIN") %>%
  rename_with(tolower) %>%
  dplyr::rename(edu=edattain) %>%
  mutate(edu= ifelse((edu==9 & year==2011),4,edu)) %>% #in 2011 PL edu=9 is high, otherwise missing, remove missings then
  filter(edu!=9) %>%
  mutate(edu=recode(edu, "1"="1","2"="1","3"="2","4"="3"),
         year=as.character(year), 
         edu=as.character(edu),
         source="IPUMS", 
         country=recode(as.double(country), "124"="CA", "376"="IL", "40"="AT", "250"="FR", "616"="PL", "724"="ES", "756"="CH", "705"="SI"))

IPUMS <- allIPUMS %>%
  group_by(year,country,sex,edu,age) %>%
  filter(sex!=9) %>%
  dplyr::summarise(n=n()) %>%
  left_join(allIPUMS %>%
              group_by(year,country,sex,age) %>%
              dplyr::summarise(alln=n())) %>%
  mutate(prev=n/alln, 
         source="IPUMS")
rm(allIPUMS)

memory.limit(400000)
US <- all %>%
  filter(COUNTRY==840, SEX!=9, EDATTAIN!=0, YEAR>1980) %>%
  select(YEAR,SAMPLE,AGE,SEX,EDATTAIN) %>% 
  filter(AGE>=25, AGE<=70) %>%
  mutate(age="25-29", 
         age=ifelse(as.numeric(AGE)>29,"30-34",age),
         age=ifelse(as.numeric(AGE)>34,"35-39",age),
         age=ifelse(as.numeric(AGE)>39,"40-44",age),
         age=ifelse(as.numeric(AGE)>44,"45-49",age),
         age=ifelse(as.numeric(AGE)>49,"50-54",age),
         age=ifelse(as.numeric(AGE)>54,"35-39",age),
         age=ifelse(as.numeric(AGE)>59,"60-64",age),
         age=ifelse(as.numeric(AGE)>64,"65-69",age),
         sex=recode(as.double(SEX),"2"="f", "1"="m")) %>%
  dplyr::select(YEAR,sex, age,EDATTAIN) %>%
  rename_with(tolower) %>%
  dplyr::rename(edu=edattain) %>%
  filter(edu!=9) %>% # remove missing edu
  mutate(edu=recode(as.double(edu), "1"="1","2"="1","3"="2","4"="3"),
         year=as.character(year), 
         edu=as.character(edu),
         source="IPUMS", 
         country="US")

USpop <- US %>%
  group_by(year,sex,edu,age) %>%
  filter(sex!=9) %>%
  dplyr::summarise(n=n()) %>%
  left_join(US %>%
              group_by(year,sex,age) %>%
              dplyr::summarise(alln=n())) %>%
  mutate(prev=n/alln, 
         source="IPUMS", country="US")

rm(US)
setwd(data.out)
write.table(USpop, file="us.csv",sep=",", row.names=FALSE)


####spain, no edu
memory.limit(100000)
allspain <- all %>%
  filter(COUNTRY==724, SEX!=9, EDATTAIN!=0) %>%
  select(COUNTRY,YEAR,SAMPLE,AGE,SEX,EDATTAIN) %>% 
  bind_rows(israel %>%  select(COUNTRY,YEAR,SAMPLE,AGE,SEX,EDATTAIN)) %>%
  filter(AGE>=25, AGE<=70) %>%
  mutate(age="25-29", 
         age=ifelse(as.numeric(AGE)>29,"30-34",age),
         age=ifelse(as.numeric(AGE)>34,"35-39",age),
         age=ifelse(as.numeric(AGE)>39,"40-44",age),
         age=ifelse(as.numeric(AGE)>44,"45-49",age),
         age=ifelse(as.numeric(AGE)>49,"50-54",age),
         age=ifelse(as.numeric(AGE)>54,"35-39",age),
         age=ifelse(as.numeric(AGE)>59,"60-64",age),
         age=ifelse(as.numeric(AGE)>64,"65-69",age),
         sex=recode(as.double(SEX),"2"="f", "1"="m")) %>%
  dplyr::select("COUNTRY", "YEAR","sex", "age","EDATTAIN") %>%
  rename_with(tolower) %>%
  dplyr::rename(edu=edattain) %>%
  filter(edu!=9) %>%
  mutate(edu=recode(as.double(edu),"1"="0","2"="1","3"="2","4"="3"),
         year=as.character(year), 
         edu=as.character(edu)) 
 
spain <- allspain %>%
  group_by(year,sex,edu,age) %>%
  filter(sex!=9) %>%
  dplyr::summarise(n=n()) %>%
  left_join(allspain %>%
              group_by(year,sex,age) %>%
              dplyr::summarise(alln=n())) %>%
  mutate(prev=n/alln, 
         source="IPUMS")

setwd(data.out)
write.table(spain, file="spain.csv",sep=",", row.names=FALSE)

#### eurostat, census
setwd(data.dir)
cens9101 <- read.table(file="census9101b.csv", sep=",", header=TRUE)
cens11 <- read.table(file="census11b.csv", sep=",", header=TRUE)
cens01 <- read.table(file="census01.csv", sep=",", header=TRUE) #more edu groups than in other census datafiles
cens91a <- read.table(file="active91.csv", sep=",", header=TRUE) #more edu groups than in other census datafiles
cens91b <- read.table(file="inactive91.csv", sep=",", header=TRUE) #more edu groups than in other census datafiles



allcensus <- cens9101 %>%
  bind_rows(cens11 %>% mutate(TIME=2011)) %>%
  filter(Value!=":", ISCED97!="Unknown") %>%
  mutate(Value=str_replace(Value,",", "")) %>%
  mutate(Value=str_replace(Value,",", "")) %>%
  mutate(edu=str_sub(ISCED97,-2,-2),
         edu=recode(edu, "o"="1","2"="1","3"="2","4"="2", "5"="3","6"="3"),
         sex=recode(SEX,"Females"="f","Males"="m"))

popcensus <- allcensus %>%
  group_by(GEO,TIME,sex,edu,AGE) %>%
  dplyr::summarise(n=sum(as.numeric(as.character(Value)),na.rm=TRUE)) %>%
  filter(n!=0) %>%
  left_join(allcensus %>%
              group_by(GEO,TIME,sex,AGE) %>%
              dplyr::summarise(nall=sum(as.numeric(as.character(Value)),na.rm=TRUE)) %>%
              filter(nall!=0)) %>%
  mutate(prev=n/nall,
         source="EUROSTATcensus") %>%
  rename_with(tolower) %>%
  dplyr::select(time,geo,sex,edu,age,n,prev,source)%>%
  dplyr::rename(country=geo, year=time) 
  
### two population sources together
newpop <- popcensus %>%
  mutate(year=as.character(year)) %>% 
  bind_rows(IPUMS %>%  mutate(sex=as.character(sex), 
                                 year=as.character(year),
                                 edu=as.character(edu))) %>%
  bind_rows(USpop %>%  mutate(sex=as.character(sex), 
                              year=as.character(year),
                              edu=as.character(edu)))

setwd(data.out)
write.table(newpop, "popedu.csv", sep=",", row.names = FALSE)


####2 middle edu levels
allcensus2 <- cens01 %>%
  bind_rows(cens11 %>% mutate(TIME=2011)) %>%
  filter(Value!=":", ISCED97!="Unknown") %>%
  mutate(Value=str_replace(Value, ",", "")) %>%
  mutate(Value=str_replace(Value,",", "")) %>%
  mutate(edu=str_sub(ISCED97,-2,-2),
         edu=recode(edu, "A"="3","B"="3","e"="2a","o"="1","2"="2a","3"="2b","4"="2b", "5"="3","6"="3","0"="1"),
         sex=recode(SEX,"Females"="f","Males"="m"))

popcensus2 <- allcensus2 %>%
  group_by(GEO,TIME,sex,edu,AGE) %>%
  dplyr::summarise(n=sum(as.numeric(as.character(Value)),na.rm=TRUE)) %>%
  filter(n!=0) %>%
  left_join(allcensus2 %>%
              group_by(GEO,TIME,sex,AGE) %>%
              dplyr::summarise(nall=sum(as.numeric(as.character(Value)),na.rm=TRUE)) %>%
              filter(nall!=0)) %>%
  mutate(prev=n/nall,
         source="EUROSTATcensus") %>%
  rename_with(tolower) %>%
  dplyr::select(time,geo,sex,edu,age,n,prev,source)%>%
  dplyr::rename(country=geo, year=time) 

setwd(data.out)
write.table(popcensus2, "popedu2mid.csv", sep=",", row.names = FALSE)


pop91new <- cens91a %>%
  bind_rows(cens91b)%>%
  filter(Value!=":", ISCED97!="Unknown", ISCED97!="Primary and lower secondary education (levels 1 and 2)") %>%
  mutate(Value=str_replace(Value, ",", "")) %>%
  mutate(Value=str_replace(Value,",", "")) %>%
  mutate(edu=str_sub(ISCED97,-2,-2),
         edu=recode(edu, "0"="1", "1"="1","2"="2a", "3"="2b","6"="3"),
                    sex=recode(SEX,"Females"="f","Males"="m"))

popcensus91new <- pop91new %>%
  group_by(GEO,TIME,sex,edu,AGE) %>%
  dplyr::summarise(n=sum(as.numeric(as.character(Value)),na.rm=TRUE)) %>%
  filter(n!=0) %>%
  left_join(pop91new %>%
              group_by(GEO,TIME,sex,AGE) %>%
              dplyr::summarise(nall=sum(as.numeric(as.character(Value)),na.rm=TRUE)) %>%
              filter(nall!=0)) %>%
  mutate(prev=n/nall,
         source="EUROSTATcensus") %>%
  rename_with(tolower) %>%
  dplyr::select(time,geo,sex,edu,age,n,prev,source)%>%
  dplyr::rename(country=geo, year=time) 
setwd(data.out)
write.table(popcensus91new, "popedu2mid91.csv", sep=",", row.names = FALSE)

###denmark0910
setwd(data.dir)
denmark0910 <- read.table(file="denmark0910.csv", sep="\t",header=TRUE)

denmark <- denmark0910 %>%
  mutate(edu=substr(edu,2,2),
         edu=recode(edu,"1"="1","2"="2","3"="2","4"="3","5"="3","6"="3","7"="3","8"="3"),
         sex=recode(sex,"Women"="f","Men"="m"),
         value=X2009+X2010)
denmarkpop <- denmark %>%
  group_by(sex,edu,age) %>%
  dplyr::summarise(n=sum(as.numeric(as.character(value)),na.rm=TRUE)) %>%
  filter(n!=0) %>%  
  left_join(denmark%>%
              group_by(sex,age) %>%
              dplyr::summarise(nall=sum(as.numeric(as.character(value)),na.rm=TRUE)) %>%
              filter(nall!=0)) %>%
  mutate(prev=n/nall) 
  
setwd(data.out)
write.table(denmarkpop, "denmarkpop0910.csv", sep=",", row.names = FALSE)  

###
setwd(data.dir)
denmark0910 <- read.table(file="denmark0910.csv", sep="\t",header=TRUE)

denmark <- denmark0910 %>%
  mutate(edu=substr(edu,2,2),
         edu=recode(edu,"1"="1","2"="2","3"="2","4"="3","5"="3","6"="3","7"="3","8"="3"),
         sex=recode(sex,"Women"="f","Men"="m"),
         value=X2009+X2010)
denmarkpop <- denmark %>%
  group_by(sex,edu,age) %>%
  dplyr::summarise(n=sum(as.numeric(as.character(value)),na.rm=TRUE)) %>%
  filter(n!=0) %>%  
  left_join(denmark%>%
              group_by(sex,age) %>%
              dplyr::summarise(nall=sum(as.numeric(as.character(value)),na.rm=TRUE)) %>%
              filter(nall!=0)) %>%
  mutate(prev=n/nall) 

setwd(data.out)
write.table(denmarkpop, "denmarkpop0910.csv", sep=",", row.names = FALSE)  

###
setwd(data.dir)
australia <- read.table(file="australia11.txt", sep=",",header=TRUE)
australia <- australia%>%
  pivot_longer(male:female, names_to="sex", values_to="value")

australiapop <- australia %>%
  group_by(sex,edu) %>%
  dplyr::summarise(n=sum(as.numeric(as.character(value)),na.rm=TRUE)) %>%
  left_join(australia %>% 
              group_by(sex) %>%
              dplyr::summarise(alln=sum(as.numeric(as.character(value)),na.rm=TRUE)), by=c("sex")) %>%
  mutate(prev=n/alln) 

setwd(data.out)
write.table(australiapop, "australia11.csv", sep=",", row.names = FALSE)  


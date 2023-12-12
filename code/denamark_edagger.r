#############################################################################################################################
rm(list=ls())
library(tidyverse)
library(dplyr)
library(HMDHFDplus)
library(MortalityLaws)
library(ggrepel)
library(ggpubr)
library(forcats)
library(directlabels)
library(svglite)

data.dir <- "C:\\Users\\mmusz\\Dropbox\\DR_Vaupel\\education\\education\\code\\LE_edu\\LE_edu\\data"
data.out <- "C:\\Users\\mmusz\\Dropbox\\DR_Vaupel\\education\\education\\code\\LE_edu\\LE_edu\\data\\ready"
pop.dir <- "C:\\Users\\mmusz\\Dropbox\\DR_Vaupel\\education\\education\\code\\LE_edu\\LE_edu\\data"
figures.dir <- "C:\\Users\\mmusz\\Dropbox\\DR_Vaupel\\education\\figures"

###read-in population
setwd(pop.dir)
#2011-2015
pop1115 <- read.table(file="denmark1115.csv", sep=";")[,-c(1,2)]
colnames(pop1115) <- c("sex","edu","age","y2011","y2012","y2013","y2014","y2015")

pop1115raw <- pop1115 %>%
  mutate(edu=substr(edu,1,2),
         age=substr(age,1,5),
         edu=recode(edu,"10"="Low","20"="Medium","25"="Medium","35"="Medium","35"="High","40"="High","50"="High","60"="High","65"="High"),
         y1115=y2011+y2012+y2013+y2014+y2015) %>%
  filter(edu!=90) %>%
  group_by(sex,edu,age) %>%
  summarize(y1115=sum(y1115))

##1991-1995
pop9195 <- read.table(file="denmark9195.csv", sep=";")[,-c(1,2)]
colnames(pop9195) <- c("sex","age","edu","y1991","y1992","y1993","y1994","y1995")

pop9195raw <- pop9195 %>%
   mutate(edu=substr(edu,1,2),
         age=substr(age,1,5),
         edu=recode(edu,"10"="Low","20"="Medium","25"="Medium","35"="Medium","35"="High","40"="High","50"="High","60"="High","65"="High"),
         y9195=y1991+y1992+y1993+y1994+y1995) %>%
  filter(edu!=90) %>%
  group_by(sex,edu,age) %>%
  summarize(y9195=sum(y9195)) 

####together both years, estimate share of each edu group at each age >=30
popshare <- pop9195raw %>%  
  left_join(pop9195raw %>%
               group_by(sex,age) %>%
               summarize(sum9195=sum(y9195))) %>%
  mutate(share=y9195/sum9195,
         sex=recode(sex,"Men"="m","Women"="f"),
         period="1991-1995") %>%
  select(sex,edu,age,period,share) %>%
  bind_rows(pop1115raw %>%
              left_join(pop1115raw %>%
                          group_by(sex,age) %>%
                          summarize(sum1115=sum(y1115))) %>%
              mutate(share=y1115/sum1115,
                     sex=recode(sex,"Men"="m","Women"="f"),
                     period="2011-2015") %>%
              select(sex,edu,age,period,share)) %>%
  mutate(age2=substr(age,1,2)) %>%
  filter(as.numeric(age2)>=30)


################# read in LE by edu, estimate share at each age of edu groups according to current risk conditions, LE_edu
setwd(data.dir)


############# read in LE total from HMD - need to be calculated since these years are not grouped together in HMD
country <- "Denmark"
countryname <- "DNK"
ages <- seq(from=30, to=110, by=5)
years1 <- c(1991,2011)
years2 <- c(1995,2015)

#read-in data
DxHMD <- readHMDweb(CNTRY=countryname, item="Deaths_5x1")   #provide your username and password
NxHMD <- readHMDweb(CNTRY=countryname, item="Exposures_5x1") #provide your username and password

DNKall <- DxHMD %>%
  dplyr::select(Year,Age,Female,Male) %>%
  rename_with(tolower) %>%
  dplyr::rename(fem.Dx=female,male.Dx=male) %>%
  left_join(NxHMD %>%
              dplyr::select(Year,Age,Female,Male) %>%
              mutate(country=country,countryHMD=countryname) %>%
              rename_with(tolower) %>%
              dplyr::rename(fem.Nx=female,male.Nx=male)) 


##### ax from 5x5 lt for 1985-94 and 2010-2014
axfem <- readHMDweb(CNTRY=countryname, item="fltper_5x5") #provide your username and password
axmale <- readHMDweb(CNTRY=countryname, item="mltper_5x5") #provide your username and password


DNKax <- axfem %>%
  filter((Year==1995|Year==2015), Age<=110) %>%
  mutate(ax=ifelse(Age==110,ex,ax)) %>%
  dplyr::rename(ax.fem=ax) %>%
  left_join(axmale %>% filter(Year==1995|Year==2015) %>% 
              mutate(ax=ifelse(Age==110,ex,ax)) %>%
              dplyr::rename(ax.male=ax), 
            by=c("Year","Age")) %>%
  select(Year,Age,ax.fem,ax.male)
  

mylt <- function(Mx,ax){
  px <- 1-(5*Mx/(1+(5-ax)*Mx))
  px[length(Mx)] <- 0
  lx <- matrix(100000,nrow=length(Mx),ncol=1)
  dx <- matrix(0,nrow=length(Mx),ncol=1)
  for (i in 2:length(lx)){
    lx[i] <- lx[i-1]*px[i-1]
    dx[i-1] <- lx[i-1]-lx[i]
  }
  Lx <- 5*lx-(5-ax)*dx
  Lx[length(Lx)] <- lx[length(Lx)]*ax[length(Lx)]
  Tx <- matrix(0,nrow=length(Mx),ncol=1)
  for (i in 1:length(lx)){
    Tx[i] <- sum(Lx[i:length(Lx)])
  }
  ex= Tx/lx
  return(as.vector(ex))
}


myedagger <- function(Mx,ax){
  px <- 1-(5*Mx/(1+(5-ax)*Mx))
  px[length(Mx)] <- 0
  lx <- matrix(100000,nrow=length(Mx),ncol=1)
  dx <- matrix(0,nrow=length(Mx),ncol=1)
  for (i in 2:length(lx)){
    lx[i] <- lx[i-1]*px[i-1]
    dx[i-1] <- lx[i-1]-lx[i]
  }
  Lx <- 5*lx-(5-ax)*dx
  Lx[length(Lx)] <- lx[length(Lx)]*ax[length(Lx)]
  Tx <- matrix(0,nrow=length(Mx),ncol=1)
  for (i in 1:length(lx)){
    Tx[i] <- sum(Lx[i:length(Lx)])
  }
  ex= Tx/lx
  
  
  lostex=(ex-ax)*dx
  Lostx <- matrix(0,nrow=length(Mx),ncol=1)
  for (i in 1:length(lx)){
    Lostx[i] <- sum(lostex[i:length(Lx)])
  }
  exdagger=Lostx/lx
  return(cbind(exdagger,lx))
}




k=1  #1991-1995
years <- seq(from=years1[k],to=years2[k],by=1)

DNK1 <- DNKall %>%
  filter(year %in%years, age>=30) %>%
  group_by(age) %>%
  dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
  inner_join(DNKax %>% filter(Year==(years2[k])) %>% rename_with(tolower)) %>%
  mutate(Mx.fem=femDx/femNx, Mx.male=maleDx/maleNx,
         fem.edaggerx=myedagger(Mx=Mx.fem,ax=ax.fem)[,1], male.edaggerx=myedagger(Mx=Mx.male,ax=ax.male)[,1],
         fem.lx=myedagger(Mx=Mx.fem,ax=ax.fem)[,2], male.lx=myedagger(Mx=Mx.male,ax=ax.male)[,2])
  
k=2  #1991-1995
years <- seq(from=years1[k],to=years2[k],by=1)

DNK2 <- DNKall %>%
  filter(year %in%years, age>=30) %>%
  group_by(age) %>%
  dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
  inner_join(DNKax %>% filter(Year==(years2[k])) %>% rename_with(tolower)) %>%
  mutate(Mx.fem=femDx/femNx, Mx.male=maleDx/maleNx,
         fem.edaggerx=myedagger(Mx=Mx.fem,ax=ax.fem)[,1], male.edaggerx=myedagger(Mx=Mx.male,ax=ax.male)[,1],
         fem.lx=myedagger(Mx=Mx.fem,ax=ax.fem)[,2], male.lx=myedagger(Mx=Mx.male,ax=ax.male)[,2])


DNKedaggerx <- DNK1 %>%
  select(fem.edaggerx, male.edaggerx,fem.lx,male.lx,age) %>%
  mutate(year=paste("1991","1995", sep="-")) %>%
  bind_rows(DNK2 %>%
              select(fem.edaggerx, male.edaggerx,fem.lx,male.lx,age) %>%
              mutate(year=paste("2011","2015", sep="-")))

DNKedaggerx90 <- DNKedaggerx %>% 
  filter(age==90) %>%
  mutate(add90.fem=fem.edaggerx*fem.lx/100000, 
         add90.male=male.edaggerx*male.lx/100000) %>%
  select(year,add90.fem,add90.male) %>%
  pivot_longer(c(add90.fem,add90.male),names_to="sex", values_to="ed90",names_prefix = "add90.") %>%
  mutate(sex=ifelse(sex=="fem","f","m")) %>%
  mutate(period=year)
  

###by edu

educomp <- read.table(file="nemethall.txt", sep="\t", header=TRUE) %>% # read-in data on LE by edu
  dplyr::select(Country,Period,Sex,Education,AgeGrp,lx,ex,dx) %>%
  mutate(edu=recode(Education, "Middle"="Medium"),
         Sex=recode(Sex, "Female"="f", "Male"="m"),
         year=substr(Period,1,4)) %>%
  rename_with(tolower) %>%
  dplyr::rename(age=agegrp,LE=ex) %>%
  filter((period=="1991-1995"|period=="2011-2015"), country=="Denmark") %>%
  right_join(popshare%>%
               filter(as.numeric(age2)==30),by=c("sex","edu","period"))%>%        #add the share at age 30 estimated in previous step
  mutate(age=substr(age.x,1,2),
         sharenew=share*lx/100000)




edagger <- function(ax,dx,ex,lx0){           #e-dagger only at one age group
  lostx <- (ex-ax)*dx
  TLostx <- sum(lostx)
  exdagger=TLostx/lx0
  return(as.vector(exdagger))
}

educomp2 <- educomp %>%                  #select only to age 89, 90+ from the main life table
  filter(age<90) %>%
  left_join(DNKax %>%
              pivot_longer(ax.fem:ax.male, values_to="ax", names_to="sex", names_prefix="ax.") %>%
              mutate(sex=substr(sex,1,1), 
                     age=as.character(Age), 
                     year=as.character(ifelse(Year==1995,1991,2011))), by=c("year","age","sex")) %>%
  group_by(period,sex,education) %>%
  summarise(edagger=edagger(dx=dx,ex=LE,lx=lx[1], ax=ax)) %>%
  left_join(educomp %>%
              filter(age==30) %>%
              select(period,sex,education,share)) 


byeduto90 <- educomp2 %>%
  group_by(period,sex) %>%
  summarize(edagger_new=sum(share*edagger)) %>%
  left_join(educomp2)


byedu <- byeduto90 %>%
  left_join(DNKedaggerx90, by=c("period","sex")) %>%
  mutate(edagger=edagger+ed90,
         edagger_new=edagger_new+ed90) 

edaggerall <- DNKedaggerx %>%
  filter(age==30) %>%
  pivot_longer(fem.edaggerx:male.edaggerx, names_to="sex", values_to="edagger_total") %>%
  mutate(sex=substr(sex,1,1), period=year) %>%
  full_join(byedu, by=c("period","sex"))%>%
  pivot_longer(c(edagger_total,edagger,edagger_new), values_to="edagger", names_to="type") %>%
  mutate(type=ifelse(type=="edagger", paste("edagger_",education,sep=""),type),
         type=recode(type,"edagger_new"="New", "edagger_total"="Total",
                     "edagger_High"="High","edagger_Low"="Low","edagger_Middle"="Medium")) %>%
  rename("Type"="type") %>%
  select(sex,period,Type,edagger) %>%
  distinct() 

tableedagger <- edaggerall %>%
  filter(Type=="Total"|Type=="New", sex=="m")


############ decompose differences between two edaggers
basediff <- edaggerall %>%
  rename("education"="Type") %>%
  left_join(byedu %>%
              select(sex,period,education,share) %>%
              mutate(education=ifelse(education=="Middle","Medium",education))) %>%
  filter(!is.na(share)) %>%
  mutate(period=substr(period,1,4))


differ <- basediff %>%
  pivot_wider(names_from = "period", values_from = c("edagger","share")) %>%
  mutate(all= edagger_2011*share_2011-edagger_1991*share_1991,
         part1=(edagger_2011+edagger_1991)*(share_2011-share_1991)/2, 
         part2=(edagger_2011-edagger_1991)*(share_2011+share_1991)/2) %>%
  group_by(sex) %>%
  summarise(all=sum(all),part1=sum(part1),part2=sum(part2))


sexgap <- basediff %>%
  pivot_wider(names_from = "sex", values_from = c("edagger","share")) %>%
  mutate(all= edagger_m*share_m-edagger_f*share_f,
         part1=(edagger_m+edagger_f)*(share_m-share_f)/2, 
         part2=(edagger_m-edagger_f)*(share_m+share_f)/2) %>%
  group_by(period) %>%
  summarise(all=sum(all),part1=sum(part1),part2=sum(part2))


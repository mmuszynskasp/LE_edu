rm(list=ls())

library(dplyr)
library(tidyverse)
library(purrr)
library(ggplot2)
library(HMDHFDplus)
library(devtools)
#install_github("mpascariu/MortalityLaws")
library(MortalityLaws)
library(xtable)

cohorts.out <- "C:\\Users\\Magdalena\\demography\\education\\data\\ready\\cohorts"
periods.out <- "C:\\Users\\Magdalena\\demography\\education\\data\\ready\\periods"
plots.out <- "C:\\Users\\Magdalena\\demography\\education\\figures"


countryname <- "DNK"

####check out Denmark first
Dx <- readHMDweb(CNTRY=countryname, item="Deaths_lexis", username="mmuszynska@gmail.com",password="123Mucha123!")
Ex <- readHMDweb(CNTRY=countryname, item="Exposures_lexis", username="mmuszynska@gmail.com",password="123Mucha123!")
flt <-   readHMDweb(CNTRY=countryname, item="fltper_1x1", username="mmuszynska@gmail.com",password="123Mucha123!")
#mlt <-    readHMDweb(CNTRY=countryname, item="mltper_1x1", username="mmuszynska@gmail.com",password="123Mucha123!")
  
####################################################################################################
########### females first

##########cohort
Dxc <- Dx %>%
  select(Year,Age,Cohort,Female) %>%
  group_by(Age,Cohort) %>%
  summarise(Dx=sum(Female))
  
mxc <- Ex %>%
  select(Year,Age,Cohort,Female) %>%
  group_by(Age,Cohort) %>%
  summarise(Nx=sum(Female)) %>%
  left_join(Dxc) %>%
  mutate(mx=Dx/Nx,
         Cohort=as.numeric(Cohort))


ltc <- mxc %>%
  filter(Cohort>=1870 & Cohort<=2015) %>%
  arrange(as.numeric(Age))%>%
  group_by(Cohort) %>%
  group_map(~(LifeTable(x=as.numeric(.x$Age),mx=as.numeric(.x$mx)))$lt) 

#ltc <- mx %>%
#  filter(Cohort>=1800 & Cohort<=2020) %>%
#  arrange(as.numeric(Age))%>%
#  group_by(Cohort) %>%
#  group_map(~(LifeTable(x=as.numeric(.x$Age),mx=as.numeric(.x$mx)))$lt) 

#cohorts <- unique(mx$Cohort[mx$Cohort<=2000])
#cohorts <- sort(unique(as.numeric(mx$Cohort)))
cohorts <- 1870:2015

setwd(cohorts.out)

i=1
newtab <- ltc[[i]] %>%
  mutate(cohort=cohorts[i]) %>%
  select(x,lx,cohort) %>%
  slice(1:n()-1)
  
setwd(cohorts.out)  
write.table(newtab, file="cohorts.csv", row.names=FALSE, sep=",")


for(i in 2:length(cohorts)){
  newtab <- ltc[[i]] %>%
  mutate(cohort=cohorts[i]) %>%
  select(x,lx,cohort) %>%
  slice(1:n()-1)
  write.table(newtab, file="cohorts.csv", row.names=FALSE, col.names=FALSE, append=TRUE, sep=",")
 }

##period
Dxp <- Dx%>%
  select(Year,Age,Cohort,Female) %>%
#  filter(Year>1870, as.numeric(Age)<=86) %>%
  group_by(Age,Year) %>%
  summarise(Dx=sum(Female))

mxp <- Ex %>%
  select(Year,Age,Cohort,Female) %>%
#  filter(Year>1870, as.numeric(Age)<=86) %>%
  group_by(Age,Year) %>%
  summarise(Nx=sum(Female)) %>%
  left_join(Dxp) %>%
  mutate(mx=Dx/Nx)

ltcp <- mxp %>%
  #filter(Year<2001) %>%
  arrange(as.numeric(Age))%>%
  group_by(Year) %>%
  group_map(~(LifeTable(x=as.numeric(.x$Age),mx=as.numeric(.x$mx)))$lt) 

years <- unique(mxp$Year)

i=1
newtab <- ltcp[[i]] %>%
  mutate(year=years[i]) %>%
  select(x,mx,lx,year) %>%
  slice(1:n()-1)

setwd(periods.out)  
write.table(newtab, file="periods.csv", row.names=FALSE, sep=",")


for(i in 2:length(years)){
  newtab <- ltcp[[i]] %>%
    mutate(year=years[i]) %>%
    select(x,mx,lx,year) %>%
    slice(1:n()-1)
  write.table(newtab, file="periods.csv", row.names=FALSE, col.names=FALSE, append=TRUE, sep=",")
}


###############################  
####make the coefficient to multiply
setwd(cohorts.out)  
allcoh <- read.table(file="cohorts.csv", sep=",", header=TRUE)
setwd(periods.out) 
allper <- read.table(file="periods.csv", sep=",", header=TRUE)

mycoeff <- allcoh %>%
  mutate(year=cohort+x,
         lx.coh=lx) %>%
  left_join(allper, by=c("x","year")) %>%
  mutate(ourcoeff=lx.y/lx.coh) 
####
check <- tapply(X=mycoeff$ourcoeff, INDEX=list(age=mycoeff$x, year=mycoeff$year), FUN=sum)
###plot
check2 <- as_tibble(check) %>%
  mutate(age=rownames(check)) %>%
  tidyr::pivot_longer(cols=-age,names_to = "year", values_to = "rel_surv") %>%
  na.omit %>%
  arrange(as.numeric(age)) %>%
  filter(as.numeric(age)>=30)

######################################################################################################################
##################### LE 
newdata <- check2 %>%
  mutate(age=as.numeric(age),
         year=as.numeric(year)) %>%
  left_join(flt %>% 
              mutate(age=as.numeric(Age),
                     year=as.numeric(Year))%>%
              select(age,ax,year,mx)) 
  
years <- unique(newdata$year)


mylt <- function(mx,ax){
  px <- 1-mx/(2+(1-ax)*mx)
  px[length(mx)] <- 0
  lx <- matrix(100000,nrow=length(mx),ncol=1)
  dx <- matrix(0,nrow=length(mx),ncol=1)
  for (i in 2:length(lx)){
    lx[i] <- lx[i-1]*px[i-1]
    dx[i-1] <- lx[i-1]-lx[i]
  }
  Lx <- lx-(1-ax)*dx
  Lx[length(Lx)] <- lx[length(Lx)]*ax[length(Lx)]
  Tx <- matrix(0,nrow=length(mx),ncol=1)
  for (i in 1:length(lx)){
    Tx[i] <- sum(Lx[i:length(Lx)])
  }
  ex= Tx/lx
  return(ex[1])
}

new_mx <-  function(age,mx,sigma,rel_surv){
  new_mx <- mx*(rel_surv)^sigma
#  new_mx <- ifelse(age==30,mx,new_mx)
  new_mx <- ifelse(age==110,1,new_mx)
  return(as.vector(new_mx))
}


LEdata <- newdata %>%
#    add_row(flt %>%
#            filter(Age==109, Year %in%years ) %>%
#            mutate(age=as.numeric(Age),
#                   year=as.numeric(Year),
#                   rel_surv=1,
#                   ax=ifelse(Age==108,ex,ax))%>%
#              select(age,ax,year,mx)) %>%
  mutate(new_mx_3=new_mx(age=age,mx=mx,rel_surv=rel_surv,sigma=0.3),
         new_mx_7=new_mx(age=age,mx=mx,rel_surv=rel_surv,sigma=0.7),
         new_mx_15=new_mx(age=age,mx=mx,rel_surv=rel_surv,sigma=1.5)) %>%
  arrange(as.numeric(age)) %>%
  filter(year>=1991) %>%
  group_by(year) %>%
  summarize(ex=mylt(mx=mx,ax=ax),
            new_ex_3=mylt(mx=new_mx_3,ax=ax),
            new_ex_7=mylt(mx=new_mx_7,ax=ax),
            new_ex_15=mylt(mx=new_mx_15,ax=ax)) 

femgap <- LEdata %>%
  mutate(gap3=ex-new_ex_3,
         gap7=ex-new_ex_7,
         gap15=ex-new_ex_15) %>%
  tidyr::pivot_longer(cols=gap3:gap15,names_to="Type",values_to="gap") %>%
  mutate(sigma=ifelse(Type=="gap3","0.3","0.7"),
         sigma=ifelse(Type=="gap15","1.5",sigma)) %>%
  filter(year %in% c(1991:1995,2011:2015)) %>%
  mutate(yearag=ifelse(year %in% c(1991:1995), "1991-1995","2011-2015")) %>%
  group_by(yearag, Type) %>%
  summarise(gap=mean(gap))
  


########################################################################################################################################################################################
########### males
##########cohort
Dxc <- Dx %>%
  select(Year,Age,Cohort,Male) %>%
  group_by(Age,Cohort) %>%
  summarise(Dx=sum(Male))

mxc <- Ex %>%
  select(Year,Age,Cohort,Male) %>%
  group_by(Age,Cohort) %>%
  summarise(Nx=sum(Male)) %>%
  left_join(Dxc) %>%
  mutate(mx=Dx/Nx,
         Cohort=as.numeric(Cohort))


ltc <- mxc %>%
  filter(Cohort>=1870 & Cohort<=2015) %>%
  arrange(as.numeric(Age))%>%
  group_by(Cohort) %>%
  group_map(~(LifeTable(x=as.numeric(.x$Age),mx=as.numeric(.x$mx)))$lt) 

#ltc <- mx %>%
#  filter(Cohort>=1800 & Cohort<=2020) %>%
#  arrange(as.numeric(Age))%>%
#  group_by(Cohort) %>%
#  group_map(~(LifeTable(x=as.numeric(.x$Age),mx=as.numeric(.x$mx)))$lt) 

#cohorts <- unique(mx$Cohort[mx$Cohort<=2000])
#cohorts <- sort(unique(as.numeric(mx$Cohort)))
cohorts <- 1870:2015

setwd(cohorts.out)

i=1
newtab <- ltc[[i]] %>%
  mutate(cohort=cohorts[i]) %>%
  select(x,lx,cohort) %>%
  slice(1:n()-1)

setwd(cohorts.out)  
write.table(newtab, file="cohorts.csv", row.names=FALSE, sep=",")


for(i in 2:length(cohorts)){
  newtab <- ltc[[i]] %>%
    mutate(cohort=cohorts[i]) %>%
    select(x,lx,cohort) %>%
    slice(1:n()-1)
  write.table(newtab, file="cohorts.csv", row.names=FALSE, col.names=FALSE, append=TRUE, sep=",")
}

##period
Dxp <- Dx%>%
  select(Year,Age,Cohort,Male) %>%
  #  filter(Year>1870, as.numeric(Age)<=86) %>%
  group_by(Age,Year) %>%
  summarise(Dx=sum(Male))

mxp <- Ex %>%
  select(Year,Age,Cohort,Male) %>%
  #  filter(Year>1870, as.numeric(Age)<=86) %>%
  group_by(Age,Year) %>%
  summarise(Nx=sum(Male)) %>%
  left_join(Dxp) %>%
  mutate(mx=Dx/Nx)

ltcp <- mxp %>%
  #filter(Year<2001) %>%
  arrange(as.numeric(Age))%>%
  group_by(Year) %>%
  group_map(~(LifeTable(x=as.numeric(.x$Age),mx=as.numeric(.x$mx)))$lt) 

years <- unique(mxp$Year)

i=1
newtab <- ltcp[[i]] %>%
  mutate(year=years[i]) %>%
  select(x,mx,lx,year) %>%
  slice(1:n()-1)

setwd(periods.out)  
write.table(newtab, file="periods.csv", row.names=FALSE, sep=",")


for(i in 2:length(years)){
  newtab <- ltcp[[i]] %>%
    mutate(year=years[i]) %>%
    select(x,mx,lx,year) %>%
    slice(1:n()-1)
  write.table(newtab, file="periods.csv", row.names=FALSE, col.names=FALSE, append=TRUE, sep=",")
}


###############################  
####make the coefficient to multiply
setwd(cohorts.out)  
allcoh <- read.table(file="cohorts.csv", sep=",", header=TRUE)
setwd(periods.out) 
allper <- read.table(file="periods.csv", sep=",", header=TRUE)

mycoeff <- allcoh %>%
  mutate(year=cohort+x,
         lx.coh=lx) %>%
  left_join(allper, by=c("x","year")) %>%
  mutate(ourcoeff=lx.y/lx.coh) 
####
check <- tapply(X=mycoeff$ourcoeff, INDEX=list(age=mycoeff$x, year=mycoeff$year), FUN=sum)
###plot
check2 <- as_tibble(check) %>%
  mutate(age=rownames(check)) %>%
  tidyr::pivot_longer(cols=-age,names_to = "year", values_to = "rel_surv") %>%
  na.omit %>%
  arrange(as.numeric(age)) %>%
  filter(as.numeric(age)>=30)

######################################################################################################################
##################### LE 
newdata <- check2 %>%
  mutate(age=as.numeric(age),
         year=as.numeric(year)) %>%
  left_join(flt %>% 
              mutate(age=as.numeric(Age),
                     year=as.numeric(Year))%>%
              select(age,ax,year,mx)) 

years <- unique(newdata$year)


mylt <- function(mx,ax){
  px <- 1-mx/(2+(1-ax)*mx)
  px[length(mx)] <- 0
  lx <- matrix(100000,nrow=length(mx),ncol=1)
  dx <- matrix(0,nrow=length(mx),ncol=1)
  for (i in 2:length(lx)){
    lx[i] <- lx[i-1]*px[i-1]
    dx[i-1] <- lx[i-1]-lx[i]
  }
  Lx <- lx-(1-ax)*dx
  Lx[length(Lx)] <- lx[length(Lx)]*ax[length(Lx)]
  Tx <- matrix(0,nrow=length(mx),ncol=1)
  for (i in 1:length(lx)){
    Tx[i] <- sum(Lx[i:length(Lx)])
  }
  ex= Tx/lx
  return(ex[1])
}

new_mx <-  function(age,mx,sigma,rel_surv){
  new_mx <- mx*(rel_surv)^sigma
  #  new_mx <- ifelse(age==30,mx,new_mx)
  new_mx <- ifelse(age==110,1,new_mx)
  return(as.vector(new_mx))
}


LEdata <- newdata %>%
  #    add_row(flt %>%
  #            filter(Age==109, Year %in%years ) %>%
  #            mutate(age=as.numeric(Age),
  #                   year=as.numeric(Year),
  #                   rel_surv=1,
  #                   ax=ifelse(Age==108,ex,ax))%>%
  #              select(age,ax,year,mx)) %>%
  mutate(new_mx_3=new_mx(age=age,mx=mx,rel_surv=rel_surv,sigma=0.3),
         new_mx_7=new_mx(age=age,mx=mx,rel_surv=rel_surv,sigma=0.7),
         new_mx_15=new_mx(age=age,mx=mx,rel_surv=rel_surv,sigma=1.5)) %>%
  arrange(as.numeric(age)) %>%
  filter(year>=1991) %>%
  group_by(year) %>%
  summarize(ex=mylt(mx=mx,ax=ax),
            new_ex_3=mylt(mx=new_mx_3,ax=ax),
            new_ex_7=mylt(mx=new_mx_7,ax=ax),
            new_ex_15=mylt(mx=new_mx_15,ax=ax)) 

malegap <- LEdata %>%
  mutate(gap3=ex-new_ex_3,
         gap7=ex-new_ex_7,
         gap15=ex-new_ex_15) %>%
  tidyr::pivot_longer(cols=gap3:gap15,names_to="Type",values_to="gap") %>%
  mutate(sigma=ifelse(Type=="gap3","0.3","0.7"),
         sigma=ifelse(Type=="gap15","1.5",sigma)) %>%
  filter(year %in% c(1991:1995,2011:2015)) %>%
  mutate(yearag=ifelse(year %in% c(1991:1995), "1991-1995","2011-2015")) %>%
  group_by(yearag, Type) %>%
 summarise(gap=mean(gap))

bothgap <- femgap %>%
  rename(femgap=gap) %>%
  left_join(malegap %>%
              rename(malegap=gap)) %>%
  mutate(Type=ifelse(Type=="gap15","hap15",Type)) %>%
  arrange(yearag,Type)%>%
  pivot_wider(names_from = Type, values_from = femgap:malegap)



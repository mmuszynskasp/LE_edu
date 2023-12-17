rm(list=ls())

library(dplyr)
library(tidyverse)
library(purrr)
library(ggplot2)
library(HMDHFDplus)
library(devtools)
library(MortalityLaws)
library(xtable)

cohorts.out <- "C:\\Users\\mmusz\\Dropbox\\DR_Vaupel\\education\\education\\code\\LE_edu\\LE_edu\\data\\ready\\DNK\\cohorts"
periods.out <- "C:\\Users\\mmusz\\Dropbox\\DR_Vaupel\\education\\education\\code\\LE_edu\\LE_edu\\data\\ready\\DNK\\periods"
plots.out <- "C:\\Users\\mmusz\\Dropbox\\DR_Vaupel\\education\\education\\code\\LE_edu\\LE_edu\\figures"

countryname <- "DNK"


####################################################################################################
########### females #########################################################################
flt <-   readHMDweb(CNTRY=countryname, item="fltper_1x1")#, username="",password="")

setwd(periods.out) 
check2 <- read.table(file="femmultiplier.csv", sep=",", header=TRUE)


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
  
  edax= dx*(ex-ax)
  Edax <- matrix(0,nrow=length(mx),ncol=1)
  for (i in 1:length(lx)){
    Edax[i] <- sum(edax[i:length(edax)])
  }
  edaggerx=Edax/lx
  
  return(edaggerx[1])}

new_mx <-  function(age,mx,sigma,rel_surv){
  new_mx <- mx*(rel_surv)^sigma
  new_mx <- ifelse(age==110,1,new_mx)
  return(as.vector(new_mx))}


edaggerdata <- newdata %>%
  mutate(new_mx_06=new_mx(age=age,mx=mx,rel_surv=rel_surv,sigma=1/0.6),
         new_mx_2=new_mx(age=age,mx=mx,rel_surv=rel_surv,sigma=1/2),
         new_mx_4=new_mx(age=age,mx=mx,rel_surv=rel_surv,sigma=1/4),
         new_mx_8=new_mx(age=age,mx=mx,rel_surv=rel_surv,sigma=1/8)) %>%
  arrange(as.numeric(age)) %>%
  filter(year>=1991) %>%
  group_by(year) %>%
  summarize(edagger=mylt(mx=mx,ax=ax),
            edagger_06=mylt(mx=new_mx_06,ax=ax),
            edagger_2=mylt(mx=new_mx_2,ax=ax),
            edagger_4=mylt(mx=new_mx_4,ax=ax),
            edagger_8=mylt(mx=new_mx_8,ax=ax)) 


femdagger <- edaggerdata %>%
  tidyr::pivot_longer(cols="edagger_06":"edagger_8",names_to="Type",values_to="edag") %>%
  mutate(k=gsub('gap',"",x=Type),
         k=ifelse(k=="06","0.6",k))%>%
  filter(year %in% c(1991:1995,2011:2015)) %>%
  mutate(yearag=ifelse(year %in% c(1991:1995), "1991-1995","2011-2015")) %>%
  group_by(yearag, k) %>%
  summarise(edagger=mean(edag))


####################################################################################################
########### males #########################################################################
mlt <-   readHMDweb(CNTRY=countryname, item="mltper_1x1")#, username="",password="")

setwd(periods.out) 
check2 <- read.table(file="malemultiplier.csv", sep=",", header=TRUE)


newdatam <- check2 %>%
  mutate(age=as.numeric(age),
         year=as.numeric(year)) %>%
  left_join(mlt %>% 
              mutate(age=as.numeric(Age),
                     year=as.numeric(Year))%>%
              select(age,ax,year,mx)) 

years <- unique(newdatam$year)



edaggerdata <- newdata %>%
  mutate(new_mx_06=new_mx(age=age,mx=mx,rel_surv=rel_surv,sigma=1/0.6),
         new_mx_2=new_mx(age=age,mx=mx,rel_surv=rel_surv,sigma=1/2),
         new_mx_4=new_mx(age=age,mx=mx,rel_surv=rel_surv,sigma=1/4),
         new_mx_8=new_mx(age=age,mx=mx,rel_surv=rel_surv,sigma=1/8)) %>%
  arrange(as.numeric(age)) %>%
  filter(year>=1991) %>%
  group_by(year) %>%
  summarize(edagger=mylt(mx=mx,ax=ax),
            edagger_06=mylt(mx=new_mx_06,ax=ax),
            edagger_2=mylt(mx=new_mx_2,ax=ax),
            edagger_4=mylt(mx=new_mx_4,ax=ax),
            edagger_8=mylt(mx=new_mx_8,ax=ax)) 


maledagger <- edaggerdata %>%
  tidyr::pivot_longer(cols="edagger_06":"edagger_8",names_to="Type",values_to="edag") %>%
  mutate(k=gsub('gap',"",x=Type),
         k=ifelse(k=="06","0.6",k))%>%
  filter(year %in% c(1991:1995,2011:2015)) %>%
  mutate(yearag=ifelse(year %in% c(1991:1995), "1991-1995","2011-2015")) %>%
  group_by(yearag, k) %>%
  summarise(edagger=mean(edag))


###########################################################################################################
####### table with edagger for both sexes
bothedagger <- femdagger %>%
  rename(femedagger=edagger) %>%
  left_join(maledagger %>%
              rename(maleedagger=edagger)) %>%
  arrange(yearag,k)%>%
  pivot_wider(names_from = k, values_from = femedagger:maleedagger)

print(xtable(bothedagger, digits=1), include.rownames = FALSE)




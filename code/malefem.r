#############################################################################################################################
rm(list=ls())
library(tidyverse)
library(dplyr)
library(HMDHFDplus)
library(ggrepel)
library(ggpubr)
library(forcats)
library(directlabels)
library(svglite)

data.out <- "C:\\Users\\Magdalena\\demography\\education\\data\\ready"

##############################################
##### read-in data from HMD for men and women for Sweden

mydata <- readHMDweb(CNTRY="DNK", item="fltper_1x1",username="mmuszynska@gmail.com", password="123Mucha123!") %>%
  mutate(femex=ex) %>%
  select(Year,Age,femex) %>%
  left_join(readHMDweb(CNTRY="DNK", item="mltper_1x1",username="mmuszynska@gmail.com", password="123Mucha123!") %>%
  mutate(memex=ex) %>% 
  select(Year,Age,memex)) %>%
  left_join(readHMDweb(CNTRY="DNK", item="bltper_1x1",username="mmuszynska@gmail.com", password="123Mucha123!") %>%
              mutate(totalex=ex) %>% 
              select(Year,Age,totalex)) %>%
  filter(Age==0) %>%
  left_join(ungroup(readHMDweb(CNTRY="DNK", item="Population",username="mmuszynska@gmail.com", password="123Mucha123!") %>%
    group_by(Year) %>%
    summarise(Fem_pop=sum(Female1),Male_pop=sum(Male1)))) %>%
  mutate(totalnew=(femex*Fem_pop+memex*Male_pop)/(Fem_pop+Male_pop),
         gapex=totalnew-totalex)


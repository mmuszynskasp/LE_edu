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


data.dir <- #customize
data.out <-  #customize
pop.dir <-  #customize
figures.dir <- #customize

  
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

educomp1 <- read.table(file="nemethall.txt", sep="\t", header=TRUE) %>% # read-in data on LE by edu
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

educomp <- educomp1 %>%
  left_join(educomp1 %>%
  group_by(period,sex,age) %>%
  summarise(all=sum(sharenew)))%>%
  mutate(sharenew=sharenew/all)


byedu <-educomp %>%                                                #LE according to current edu composition, each age as threshold age
  left_join(educomp %>%
              group_by(period,sex,age) %>%
              summarize(LE_new=sum(sharenew*LE)), 
            by=c("sex","age","period"))


############# read in LE total from HMD - need to be calculated since these years are not grouped together in HMD
country <- "Denmark"
countryname <- "DNK"
ages <- seq(from=30, to=110, by=5)
years1 <- c(1991,2011)
years2 <- c(1995,2015)

#read-in data
DxHMD <- readHMDweb(CNTRY=countryname, item="Deaths_5x1", username="", password = "")   #provide your username and password
NxHMD <- readHMDweb(CNTRY=countryname, item="Exposures_5x1", username="", password = "") #provide your username and password

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
axfem <- readHMDweb(CNTRY=countryname, item="fltper_5x5", username="", password = "") #provide your username and password
axmale <- readHMDweb(CNTRY=countryname, item="mltper_5x5", username="", password = "") #provide your username and password


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


k=1  #1991-1995
years <- seq(from=years1[k],to=years2[k],by=1)

DNK1 <- DNKall %>%
  filter(year %in%years, age>=30) %>%
  group_by(age) %>%
  dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
  inner_join(DNKax %>% filter(Year==(years2[k])) %>% rename_with(tolower)) %>%
  mutate(Mx.fem=femDx/femNx, Mx.male=maleDx/maleNx,
         fem.ex=mylt(Mx=Mx.fem,ax=ax.fem), male.ex=mylt(Mx=Mx.male,ax=ax.male))
  
k=2  #1991-1995
years <- seq(from=years1[k],to=years2[k],by=1)

DNK2 <- DNKall %>%
  filter(year %in%years, age>=30) %>%
  group_by(age) %>%
  dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
  inner_join(DNKax %>% filter(Year==(years2[k])) %>% rename_with(tolower)) %>%
  mutate(Mx.fem=femDx/femNx, Mx.male=maleDx/maleNx,
         fem.ex=mylt(Mx=Mx.fem,ax=ax.fem), male.ex=mylt(Mx=Mx.male,ax=ax.male))


DNKex <- DNK1 %>%
  select(fem.ex, male.ex,age) %>%
  mutate(year=paste("1991","1995", sep="-")) %>%
  bind_rows(DNK2 %>%
              select(fem.ex, male.ex,age) %>%
              mutate(year=paste("2011","2015", sep="-")))
  
  
LEall <- DNKex %>%
  pivot_longer(fem.ex:male.ex, names_to="sex", values_to="LE_total") %>%
  mutate(sex=substr(sex,1,1), period=year) %>%
  full_join(byedu %>%
              mutate(age=as.numeric(age)), 
            by=c("period","age","sex")) %>%
  pivot_longer(c(LE_total,LE,LE_new), values_to="LE", names_to="type") %>%
  mutate(edu= recode(edu,"1"="low","2"="medium", "3"="high"),
         type=ifelse(type=="LE", paste("LE_",edu,sep=""),type),
         type=recode(type,"LEnew"="New", "LE_total"="Total","LE_high"="High","LE_low"="Low","LE_medium"="Medium"),
         LEage =LE+age) %>%
  rename("Type"="type") %>%
  filter(!is.na(LE))%>% 
  distinct() 

########################################################plot absolute and relative diff as percentage of remaining e(x) for total (LE_total==1)
LEperc <- LEall %>%
  left_join(LEall %>% filter(Type=="Total") %>%
              mutate(total=LE) %>%
              select(period,age,sex,total)%>%
              distinct(), 
            by=c("period","age","sex")) %>%
  mutate(diff_total=(LE-total)) %>%
  mutate(diff_perc=100*diff_total/total) %>%
  filter(Type=="LE_new") %>%
  mutate(Sex=recode(sex,"f"="Women","m"="Men"),
         Sex=factor(Sex, levels = c("Women", "Men")),
         Year=period,
         Type=paste(sex,Year,sep=",")) %>%
  distinct(diff_total, .keep_all= TRUE) %>%
 filter(age<90)   #at 90 some problem with data probably

Leplot <- ggarrange(LEperc %>%
  filter(age<90) %>%
  mutate(Sex=factor(Sex, levels = c("Women", "Men"))) %>%
  ggplot(aes(x=age, y=diff_total, color=Sex, linetype =Year)) +
  geom_line()+
  scale_color_manual(values=c("red","black")) +
  theme_minimal() +
  theme(legend.position="none")+
  scale_x_continuous(name="Age", limits=c(30,85), breaks=c(seq(from=30, to=80, by=10)), labels=c(seq(from=30, to=80, by=10)))+
  scale_y_continuous(name="Years")+  
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        strip.text.y = element_text(angle=0))+
    labs(title = "(a) Absolute Difference"),
  LEperc %>%
    filter(age<90) %>%
    mutate(Sex=factor(Sex, levels = c("Women", "Men"))) %>%
    ggplot(aes(x=age, y=diff_perc, color=Sex, linetype =Year)) +
    geom_line()+
    scale_color_manual(values=c("red","black")) +
    theme_minimal() +
    theme(legend.position="none")+
    scale_x_continuous(name="Age", limits=c(30,85), breaks=c(seq(from=30, to=80, by=10)), labels=c(seq(from=30, to=80, by=10)))+
    scale_y_continuous(name="Percent")+  
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          strip.text = element_text(size = 14),
          strip.text.y = element_text(angle=0))+
    labs(title = "(b) Relative to Lífe Expectancy"),
  nrow=1, ncol=2, common.legend=TRUE, legend="bottom")

setwd(figures.dir)
ggsave(Leplot, file="plotLE.pdf",width = 8, height = 4)    



################ plot with the distribution of population by age and educational attainment, normal and according to current conditions
shareplot <- popshare %>%
  mutate(agegr=age,
         age=substr(agegr,1,2),
         Observed=share) %>%
  left_join(byedu%>%
              select(period,sex,edu,age,sharenew)) %>%
  mutate(sex=ifelse(sex=="f", "Women", "Men"),
         Current=sharenew) %>%
  pivot_longer(c(Observed,Current), names_to="Type", values_to="prevb") %>%
  mutate(variant=Type,
         Type=paste(Type,edu,sep="_")) %>%
   mutate(sex=factor(sex, levels = c("Women", "Men"))) %>%
  ggplot(aes(x=age, y=prevb, group=Type, color = edu, linetype = variant)) +
  geom_line(size = 0.7)+
  facet_grid(period~sex) +
  scale_color_manual(values=c("#0F8B8D","#A8201A","#EC9A29"),name = "Education") +
  scale_linetype_manual(values = c("solid", "dashed"), name = "Type") + 
  theme_minimal() +
  theme(legend.position="bottom")+
  labs(y = "Share of Population",
       x = "Age") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        strip.text.y = element_text(angle=0))
setwd(figures.dir)
ggsave(plot=shareplot, filename="comp30.pdf",  width = 7.5, height = 7)


############ decompose differences between LE_new
LEall2 <- DNKex %>%
  pivot_longer(fem.ex:male.ex, names_to="sex", values_to="LE_total") %>%
  mutate(sex=substr(sex,1,1)) %>%
  left_join(byedu %>% 
              mutate(year=period,
                     age=as.numeric(age)),
            by=c("year","age","sex")) 

yearchange <- LEall2 %>%
  filter(year=="1991-1995") %>%
  select(sex,age,edu,LE,sharenew,LE_new)%>%
  rename("LE_91"="LE","share_91"="sharenew","LE_new_91"="LE_new") %>%
  left_join(LEall2 %>%filter(year=="2011-2015") %>% 
              select(sex,age,edu,LE,sharenew,LE_new)%>% 
              rename("LE_11"="LE","share_11"="sharenew","LE_new_11"="LE_new")) %>%
  mutate(EL1= (share_11-share_91)*(LE_11+LE_91)/2,
         EL2= (share_11+share_91)*(LE_11-LE_91)/2) %>%
  group_by(sex,age) %>%
  mutate(Total=LE_new_11-LE_new_91,
         Total2=Total,
         Composition=sum(EL1), LE=sum(EL2),
         Ratio=100*LE_new_11/LE_new_91) %>%
  pivot_longer(c(Total,Composition,LE), names_to="Component", values_to="decopart") %>%
  filter(edu=="High") %>%
  mutate(partsrel=100*decopart/LE_new_91,
         type2=paste(Component,sex, sep=",")) %>%
  mutate(sex=ifelse(sex=="f", "Women", "Men")) %>%
  select(sex,age,Component,decopart,partsrel)


changeplot1 <- yearchange %>%
  mutate(sex=factor(sex, levels = c("Women", "Men"))) %>%
  filter(age<90) %>%
  ggplot(aes(x=age, y=decopart, color=Component)) +
  geom_line()+
  scale_color_manual(values=c("red","blue","black")) +
  theme_minimal() +
  theme(legend.position="none")+
  scale_x_continuous(name="Age", limits=c(30,85), breaks=c(seq(from=30, to=80, by=10)), labels=c(seq(from=30, to=80, by=10)))+
  scale_y_continuous(name="Years", limits=c(0,5.5), breaks=c(seq(from=0, to=5, by=1)), labels=c(seq(from=0, to=5, by=1)))+  
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        strip.text.y = element_text(angle=0))+
  facet_grid(~sex)
ggsave(changeplot1, file="changey1.pdf",width = 7, height = 3.5)    



changeplot2 <- yearchange %>% 
  mutate(sex=factor(sex, levels = c("Women", "Men"))) %>%
  filter(age<90) %>%
  ggplot(aes(x=age, y=partsrel, color=Component))  + 
  geom_line()+
  scale_color_manual(values=c("red","blue","black")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        strip.text.y = element_text(angle=0))+
  theme(legend.position="bottom")+
  labs(x = "Age") +
  scale_y_continuous(name="Percent", limits=c(0,36), breaks=c(seq(from=0, to=35, by=5)), labels=c(seq(from=0, to=35, by=5)))+
  facet_grid(~sex)

ggsave(changeplot2, file="changey2.pdf", width = 7, height = 5)



#####decompose gap between sexes

sexdiff <- LEall2 %>%
  filter(sex=="f") %>%
  select(year,sex,age,edu,LE,sharenew,LE_new)%>%
  rename("LE_f"="LE","share_f"="sharenew","LE_new_f"="LE_new") %>%
  left_join(LEall2 %>%
              filter(sex=="m") %>% 
              select(year,sex,age,edu,LE,sharenew,LE_new)%>% 
              rename("LE_m"="LE","share_m"="sharenew","LE_new_m"="LE_new"), 
            by=c("year","age","edu")) %>%
  mutate(Total=LE_new_f-LE_new_m,
         EL1= (share_f-share_m)*(LE_f + LE_m)/2,
         EL2= (share_f+share_m)*(LE_f-LE_m)/2) %>%
  group_by(year,age) %>%
  mutate(Total2=Total,
         Composition=sum(EL1), LE=sum(EL2))%>%
  pivot_longer(c(Total,Composition,LE), names_to="Component", values_to="decopart") %>%
  filter(edu=="High") %>%
  mutate(partsrel=100*decopart/LE_new_m,
         type2=paste(Component,year, sep=",")) %>%
  select(year,age,Component,decopart, partsrel)



sexplot1 <- ggarrange(
   ggplot(data=sexdiff %>% filter(year=="1991-1995")  , aes(x=age, y=decopart, Group = Component,  color=Component)) +
    geom_line(size = 0.7)+
    scale_color_manual(values=c("red","blue","black")) +
  #  scale_linetype_manual(values=c(1,1,1))+
    theme_minimal() +
    theme(legend.position="none")+
    labs(x = "Age") +
    scale_y_continuous(name="Years")+ 
    scale_x_continuous(name="Age", limits=c(30,85), breaks=c(seq(from=30, to=80, by=10)), labels=c(seq(from=30, to=80, by=10)))+
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          strip.text = element_text(size = 14),
          strip.text.y = element_text(angle=0))+
     ggtitle("1991-1995") ,
   ggplot(data=sexdiff %>% filter(year=="2011-2015")  , aes(x=age, y=decopart, Group = Component,  color=Component)) +
     geom_line(size = 0.7)+
     scale_color_manual(values=c("red","blue","black")) +
     theme_minimal() +
     theme(legend.position="none")+
     labs(x = "Age") +
     scale_y_continuous(name="Years")+  
     scale_x_continuous(name="Age", limits=c(30,85), breaks=c(seq(from=30, to=80, by=10)), labels=c(seq(from=30, to=80, by=10)))+
     theme(axis.title = element_text(size = 14),
           axis.text = element_text(size = 12),
           strip.text = element_text(size = 14),
           strip.text.y = element_text(angle=0))+
      ggtitle("2011-2015"),nrow=1,ncol=2, common.legend = TRUE,legend="bottom")
ggsave(sexplot1, file="sexdiff1.pdf", width = 7, height = 3.5)

 
 
sexplot2 <- ggarrange(
   ggplot(data=sexdiff %>% filter(year=="1991-1995")  , aes(x=age, y=partsrel, Group = Component, color=Component)) +
     geom_line(size = 0.7)+
     scale_color_manual(values=c("red","blue","black")) +
     theme_minimal() +
     theme(legend.position="none")+
     labs(x = "Age") +
     scale_y_continuous(name="Percent", limits=c(-1,35), breaks=c(seq(from=0, to=30, by=10)), labels=c(seq(from=0, to=30, by=10)))+   
     scale_x_continuous(name="Age", limits=c(30,85), breaks=c(seq(from=30, to=80, by=10)), labels=c(seq(from=30, to=80, by=10)))+
     theme(axis.title = element_text(size = 14),
           axis.text = element_text(size = 12),
           strip.text = element_text(size = 14),
           strip.text.y = element_text(angle=0))+
     ggtitle("1991-1995"),
   
   ggplot(data=sexdiff %>% filter(year=="2011-2015")  , aes(x=age, y=partsrel, Group = Component, color=Component)) +
     geom_line(size = 0.7)+
     scale_color_manual(values=c("red","blue","black")) +
     theme_minimal() +
     theme(legend.position="none")+
     labs(x = "Age") +
     scale_y_continuous(name="Percent", limits=c(-1,35), breaks=c(seq(from=0, to=30, by=10)), labels=c(seq(from=0, to=30, by=10)))+
     scale_x_continuous(name="Age", limits=c(30,85), breaks=c(seq(from=30, to=80, by=10)), labels=c(seq(from=30, to=80, by=10)))+
     theme(axis.title = element_text(size = 14),
           axis.text = element_text(size = 12),
           strip.text = element_text(size = 14),
           strip.text.y = element_text(angle=0))+
     ggtitle("2011-2015"), nrow=1,ncol=2, common.legend = TRUE,legend="bottom")
ggsave(sexplot2, file="sexdiff2.pdf", width = 7, height = 4)



  




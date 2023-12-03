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

####together both years and share at the life table threshold age
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
  filter(as.numeric(age2)==30)  
#######################################################################################
################# LE by edu, LE_new
setwd(data.dir)

educomp <- read.table(file="nemethall.txt", sep="\t", header=TRUE) %>%
  dplyr::select(Country,Period,Sex,Education,AgeGrp,lx,ex,dx) %>%
  mutate(edu=recode(Education, "Middle"="Medium"),
         Sex=recode(Sex, "Female"="f", "Male"="m"),
         year=substr(Period,1,4)) %>%
  rename_with(tolower) %>%
  dplyr::rename(age=agegrp,LE=ex) %>%
  filter((period=="1991-1995"|period=="2011-2015"), country=="Denmark") %>%
  right_join(popshare,by=c("sex","edu","period"))%>%
  filter(age2>=30)


#write.table(educomp, file="educomp.csv",sep=",",row.names=FALSE)
#educomp <- read.table("educomp.csv",sep=",", header=TRUE)

  
sumsynth <- educomp %>%
  group_by(period,sex,age,agei) %>%
  summarize(comp=sum(pithr),LE_new=sum(pithr*LEi)/sum(pithr))

byedu <-educomp %>%
  left_join(sumsynth, by=c("sex","age","agei","period"))

#######################################################################################################
#############LE total from HMD
newcountries <- c("Denmark")
newHMDcou <- c("DNK")
ages <- seq(from=30, to=65, by=5)
years1 <- c(1991,2011)
years2 <- c(1995,2015)

#read-in data
i=1
country <- newcountries[i]
countryname <- newHMDcou[i]
DxHMD <- readHMDweb(CNTRY=countryname, item="Deaths_5x1")
NxHMD <- readHMDweb(CNTRY=countryname, item="Exposures_5x1")

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
##### ax from 5x5 lt for 1990-94 and 2010-2014
axfem <- readHMDweb(CNTRY=countryname, item="fltper_5x5")
axmale <- readHMDweb(CNTRY=countryname, item="mltper_5x5")


DNKax <- axfem %>%
  filter((Year==1990|Year==2010), Age<=90) %>%
  mutate(ax=ifelse(Age==90,ex,ax)) %>%
  dplyr::rename(ax.fem=ax) %>%
  left_join(axmale %>% filter(Year==1990|Year==2010) %>% 
              mutate(ax=ifelse(Age==90,ex,ax)) %>%
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
  inner_join(DNKax %>% filter(Year==(years1[k]-1)) %>% rename_with(tolower)) %>%
  mutate(Mx.fem=femDx/femNx, Mx.male=maleDx/maleNx,
         fem.ex=mylt(Mx=Mx.fem,ax=ax.fem), male.ex=mylt(Mx=Mx.male,ax=ax.male))
  
k=2  #1991-1995
years <- seq(from=years1[k],to=years2[k],by=1)

DNK2 <- DNKall %>%
  filter(year %in%years, age>=30) %>%
  group_by(age) %>%
  dplyr::summarise(femDx=sum(fem.Dx), maleDx=sum(male.Dx),femNx=sum(fem.Nx), maleNx=sum(male.Nx)) %>%
  inner_join(DNKax %>% filter(Year==(years1[k]-1)) %>% rename_with(tolower)) %>%
  mutate(Mx.fem=femDx/femNx, Mx.male=maleDx/maleNx,
         fem.ex=mylt(Mx=Mx.fem,ax=ax.fem), male.ex=mylt(Mx=Mx.male,ax=ax.male))


#####################
DNKSWEex <- DNK1 %>%
#  filter(age<=65) %>%
  select(fem.ex, male.ex,age) %>%
  mutate(year=paste("1991","1995", sep="-")) %>%
  bind_rows(DNK2 %>%
 #             filter(age<=65) %>%
              select(fem.ex, male.ex,age) %>%
              mutate(year=paste("2011","2015", sep="-")))
  
  
LEall <- DNKSWEex %>%
  pivot_longer(fem.ex:male.ex, names_to="sex", values_to="LE_total") %>%
  mutate(sex=substr(sex,1,1), period=year) %>%
  full_join(byedu, by=c("period","age","sex")) %>%
  pivot_longer(c(LE_total,LEi,LE_new), values_to="LE", names_to="type") %>%
#  distinct(LE, .keep_all= TRUE) %>%
  mutate(edu= recode(edu,"1"="low","2"="medium", "3"="high"),
         type=ifelse(type=="LEi", paste("LE_",edu,sep=""),type),
         type=recode(type,"LEnew"="New", "LE_total"="Total","LE_high"="High","LE_low"="Low","LE_medium"="Medium"),
         LEage =LE+age) %>%
  rename("Type"="type") %>%
  filter(!is.na(LE)) 

########################################################plot as percentage of remaining e(x) for total (LE_total==1)
LEperc <- LEall %>%
 # filter(age==agei) %>%
  left_join(LEall %>% filter(Type=="Total") %>%
              mutate(total=LE) %>%
              select(period,age,sex,total), by=c("period","age","sex")) %>%
  mutate(diff_total=(LE-total)) %>%
  mutate(diff_perc=100*diff_total/total) %>%
  filter(Type=="LE_new") %>%
  mutate(sex=recode(sex,"f"="Female","m"="Male"),
         year=period,
         Type=paste(sex,year,sep=",")) %>%
  distinct(diff_total, .keep_all= TRUE) 

aaa <- ggarrange(ggplot(data=LEperc,aes(x=age, y=diff_total, group=Type))  + 
            geom_line(aes(linetype=Type, col=Type), size=0.7)+
            scale_linetype_manual(values=c(2,1,2,1))+
            scale_color_manual(values=c(rep("black",2),rep("red",2)))+
            theme_minimal() +
            theme(axis.title = element_text(size = 14),
                    axis.text = element_text(size = 12),
                    strip.text = element_text(size = 14),
                    strip.text.y = element_text(angle=0))+
            theme(legend.position="none")+
            scale_x_continuous(name="Age",limits=c(30,65), breaks=seq(from=30, to=65, by=10), labels=seq(from=30, to=65, by=10))+ 
            scale_y_continuous(name="Years")+#, limits=c(-0.1,2), breaks=c(-0.1, seq(from=0, to=2, by=0.5)), labels=c(-0.1,seq(from=0, to=2, by=0.5)))+  
            geom_hline(yintercept=0, lty=2, col="grey")+
            ggtitle("Absolute Gap"),
      
          ggplot(data=LEperc,aes(x=age, y=diff_perc, group=Type))  + 
            geom_line(aes(linetype=Type, col=Type), size=0.7)+
            scale_linetype_manual(values=c(2,1,2,1))+
            scale_color_manual(values=c(rep("black",2),rep("red",2)))+
            theme_minimal() +
            theme(legend.position="none")+
            scale_x_continuous(name="Age",limits=c(30,65), breaks=seq(from=30, to=65, by=10), labels=seq(from=30, to=65, by=10))+ 
            #scale_y_continuous(name="Years", limits=c(-0.1,2), breaks=c(-0.1, seq(from=0, to=2, by=0.5)), labels=c(-0.1,seq(from=0, to=2, by=0.5)))+  
            scale_y_continuous(name="Percent")+
            #theme(axis.title.y=element_blank())+
            theme(axis.title = element_text(size = 14),
                  axis.text = element_text(size = 12),
                  strip.text = element_text(size = 14),
                  strip.text.y = element_text(angle=0))+
            ggtitle("Relative Gap"),
          ggplot(data=LEperc,aes(x=age, y=diff_perc, group=Type)) + #to be cut out
            geom_line(aes(linetype=Type, col=Type), size=0.7)+
            scale_linetype_manual(values=c(2,1,2,1))+
            scale_color_manual(values=c(rep("black",2),rep("red",2)))+
            theme_minimal() +
            theme(legend.position="none")+
            scale_x_continuous(name="Age",limits=c(30,65), breaks=seq(from=30, to=65, by=10), labels=seq(from=30, to=65, by=10))+ 
            #scale_y_continuous(name="Years", limits=c(-0.1,2), breaks=c(-0.1, seq(from=0, to=2, by=0.5)), labels=c(-0.1,seq(from=0, to=2, by=0.5)))+  
            scale_y_continuous(name="Percent")+
            #theme(axis.title.y=element_blank())+
            theme(axis.title = element_text(size = 14),
                  axis.text = element_text(size = 12),
                  strip.text = element_text(size = 14),
                  strip.text.y = element_text(angle=0))+
            ggtitle("Relative Gap"),
          ggplot(data=LEperc,aes(x=age, y=diff_perc, group=Type))  + #to be cut out
            geom_line(aes(linetype=Type, col=Type), size=0.7)+
            scale_linetype_manual(values=c(2,1,2,1))+
            scale_color_manual(values=c(rep("black",2),rep("red",2)))+
            theme_minimal() +
            theme(legend.position="none")+
            scale_x_continuous(name="Age",limits=c(30,65), breaks=seq(from=30, to=65, by=10), labels=seq(from=30, to=65, by=10))+ 
            #scale_y_continuous(name="Years", limits=c(-0.1,2), breaks=c(-0.1, seq(from=0, to=2, by=0.5)), labels=c(-0.1,seq(from=0, to=2, by=0.5)))+  
            scale_y_continuous(name="Percent")+
            #theme(axis.title.y=element_blank())+
            theme(axis.title = element_text(size = 14),
                  axis.text = element_text(size = 12),
                  strip.text = element_text(size = 14),
                  strip.text.y = element_text(angle=0))+
            ggtitle("Relative Gap"),
         #+ geom_hline(yintercept=0, lty=2, col="grey"),
          nrow=2,ncol=2)
         #,common.legend = TRUE, legend="right", values=c(rep("black",2),rep("red",2)))
ggsave(aaa, file="C:/Users/Magdalena/demography/education/figures/diffDen.svg", width = 7.2, height = 7.1)





LEdenplot <- LEall %>%
  mutate(Type=recode(Type,"Total"="LE","LE_new"="LEsec","LE_High"="High","LE_Low"="Low", "LE_Medium"="Medium"),
         Type = fct_relevel(Type,c("High","Medium","Low","LE","LEsec")),
         sex = if_else(sex == "m","Males","Females")) %>%
  ggplot(aes(x=age, y=LEage, group=Type))  + 
       geom_line(aes(linetype=Type, col=Type), size=0.7)+
       scale_linetype_manual(values=c(1,1,1,1,2))+
       scale_color_manual(values=c("#0F8B8D","#EC9A29","#A8201A", "black","black"))+
       facet_grid(vars(sex),vars(period))+
       theme_minimal() +
       labs(y = "Average Age at Death",
            x = "Age") +
        theme(legend.position="none")+
       theme(axis.title = element_text(size = 14),
       axis.text = element_text(size = 12),
       strip.text = element_text(size = 14),
       strip.text.y = element_text(angle=0))

ggsave(plot=LEdenplot, filename="C:/Users/Magdalena/demography/education/figures/LEden3.svg",  width = 7.4, height = 7)

            



###################################################################################################################################################################
################ plot with the distribution of population by age and educational attainment, normal and synthetic cohort
i=1
ages <- ageall[i]

popshare2 <- pop9195 %>%
  filter(edu!=90) %>%
  left_join(pop9195all) %>%
  mutate(share=y9195/sum9195,
         sex=recode(sex,"Men"="m","Women"="f"),
         period="1991-1995") %>%
  select(sex,edu,age,period,share) %>%
  bind_rows(pop1115) %>%
  mutate(age2=substr(age,1,2)) %>%
  filter(as.numeric(age2)>=30) 


educomp <- nemethall %>%
  dplyr::select(Country,Period,Sex,Education,AgeGrp,lx,ex,dx) %>%
  mutate(edu=recode(Education, "Middle"="Medium"),
         Sex=recode(Sex, "Female"="f", "Male"="m"),
         year=substr(Period,1,4)) %>%
  rename_with(tolower) %>%
  dplyr::rename(age=agegrp,LE=ex) %>%
  filter((period=="1991-1995"|period=="2011-2015"), country=="Denmark", age<70) %>%
  left_join(popshare2, by=c("sex","edu","age", "period"))%>%
  group_by(period,sex,edu) %>%
  summarize(age=age,lx=lx,lx30=lx[i],prev30=share[i],prev=share) %>%
  mutate(sx=lx/lx30,
         total=prev,
         Current_30=prev30*sx,
         age=substr(age,1,2)) 

  
sumsynth <- educomp %>%
  group_by(period,sex,age) %>%
  summarize(sum1=sum(prev),sum2=sum(Current_30))


educomp <- educomp %>%
   left_join(sumsynth) %>%
   mutate(Current_30=Current_30/sum2)%>%
   pivot_longer(total:Current_30, names_to="Type", values_to="prevb") %>%
   mutate(Type=paste(Type,edu,sep="_")) %>%
   mutate(Level=recode(Type, "total_High"="High","total_Low"="Low","total_Medium"="Medium", "Current_30_High"="30_High", "Current_30_Medium"="30_Medium", "Current_30_Low"="30_Low"))

redux <-
  educomp %>%
  mutate(variant = if_else(grepl(Type, pattern = "Current"), "Current","Total"),
         edu = fct_relevel(edu,c("High","Medium","Low")),
         sex = if_else(sex == "m","Males","Females")) %>%
  ggplot(aes(x=age, y=prevb, group=Level, color = edu, linetype = variant)) +
  geom_line(size = 0.7)+
  facet_grid(vars(sex),vars(period)) +
  scale_color_manual(values=c("#0F8B8D","#A8201A","#EC9A29")) +
  theme_minimal() +
  theme(legend.position="none")+
  labs(y = "Share of Population",
       x = "Age") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        strip.text.y = element_text(angle=0))
ggsave(plot=redux, filename="C:/Users/Magdalena/demography/education/figures/comp30.svg",  width = 7.5, height = 7)

######################################################################################################################################################
############ decompose differences between LE_new
LEall2 <- DNKSWEex %>%
  pivot_longer(fem.ex:male.ex, names_to="sex", values_to="LE_total") %>%
  mutate(sex=substr(sex,1,1)) %>%
  left_join(byedu %>% mutate(year=period), by=c("year","age","sex")) %>%
  filter(agei==30)

yearchange <- LEall2 %>%
  filter(year=="1991-1995",agei==30) %>%
  select(sex,age,edu,LEi,pithr,LE_new)%>%
  rename("LEi_91"="LEi","pi_91"="pithr","LE_new_91"="LE_new") %>%
  left_join(LEall2 %>%filter(year=="2011-2015",agei==30) %>% 
              select(sex,age,edu,LEi,pithr,LE_new)%>% 
              rename("LEi_11"="LEi","pi_11"="pithr","LE_new_11"="LE_new")) %>%
  mutate(EL1= (pi_11-pi_91)*(LEi_11 + LEi_91)/2,
         EL2= (pi_11+pi_91)*(LEi_11-LEi_91)/2) %>%
  group_by(sex,age) %>%
  mutate(Total=LE_new_11-LE_new_91,
         Total2=Total,
         Composition=sum(EL1), LE=sum(EL2),
         Ratio=100*LE_new_11/LE_new_91) %>%
  pivot_longer(c(Total,Composition,LE), names_to="Component", values_to="decopart") %>%
  filter(edu=="High") %>%
  mutate(partsrel=100*decopart/LE_new_91,
         type2=paste(Component,sex, sep=","))

sexdiff <- LEall2 %>%
  filter(sex=="f",agei==30) %>%
  select(year,sex,age,edu,LEi,pithr,LE_new)%>%
  rename("LEi_f"="LEi","pi_f"="pithr","LE_new_f"="LE_new") %>%
  left_join(LEall2 %>%filter(sex=="m",agei==30) %>% 
              select(year,sex,age,edu,LEi,pithr,LE_new)%>% 
              rename("LEi_m"="LEi","pi_m"="pithr","LE_new_m"="LE_new"), by=c("year","age","edu")) %>%
  mutate(Total=LE_new_f-LE_new_m,
         EL1= (pi_f-pi_m)*(LEi_f + LEi_m)/2,
         EL2= (pi_f+pi_m)*(LEi_f-LEi_m)/2) %>%
  group_by(year,age) %>%
  mutate(Total2=Total,
         Composition=sum(EL1), LE=sum(EL2))%>%
  pivot_longer(c(Total,Composition,LE), names_to="Component", values_to="decopart") %>%
  filter(edu=="High") %>%
  mutate(partsrel=100*decopart/LE_new_m,
         type2=paste(Component,year, sep=","))


sexplot2 <- ggarrange(
   ggplot(data=sexdiff %>% filter(year=="1991-1995")  , aes(x=age, y=decopart, Group = Component, linetype = Component, color=Component)) +
    geom_line(size = 0.7)+
    scale_color_manual(values=c("red","blue","black")) +
    scale_linetype_manual(values=c(1,1,1))+
    theme_minimal() +
    theme(legend.position="none")+
    labs(x = "Age") +
    scale_y_continuous(name="Years", limits=c(-0.1,5), breaks=c(-0.1, seq(from=0, to=5, by=1)), labels=c(-0.1,seq(from=0, to=5, by=1)))+  
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          strip.text = element_text(size = 14),
          strip.text.y = element_text(angle=0))+
     ggtitle("1991-1995") ,
   ggplot(data=sexdiff %>% filter(year=="2011-2015")  , aes(x=age, y=decopart, Group = Component, linetype = Component, color=Component)) +
     geom_line(size = 0.7)+
     scale_color_manual(values=c("red","blue","black")) +
     scale_linetype_manual(values=c(2,2,2))+
     theme_minimal() +
     theme(legend.position="none")+
     labs(x = "Age") +
     scale_y_continuous(name="Years", limits=c(-0.1,5), breaks=c(-0.1, seq(from=0, to=5, by=1)), labels=c(-0.1,seq(from=0, to=5, by=1)))+  
     theme(axis.title = element_text(size = 14),
           axis.text = element_text(size = 12),
           strip.text = element_text(size = 14),
           strip.text.y = element_text(angle=0))+
      ggtitle("2011-2015"),
  
  ggplot(data=sexdiff %>% filter(Component=="Total"),aes(x=age, y=partsrel, group=year))  + 
                   geom_line(aes(linetype=year), size=0.7)+
                   scale_linetype_manual(values=c(1,2))+
                   scale_color_manual(values=c("black","black"))+
                   theme_minimal() +
                   theme(axis.title = element_text(size = 14),
                         axis.text = element_text(size = 12),
                         strip.text = element_text(size = 14),
                         strip.text.y = element_text(angle=0))+
                   theme(legend.position="none")+
                  labs(x = "Age") +
                  scale_y_continuous(name="Percent", limits=c(-0.1,25), breaks=c(seq(from=0, to=25, by=5)), labels=c(seq(from=0, to=25, by=5)))+  
                   ggtitle("Total"),
    ggplot(data=sexdiff %>% filter(Component!="Total"),aes(x=age, y=partsrel, group=type2))  + 
    geom_line(aes(col=Component, linetype=year), size=0.7)+
    scale_linetype_manual(values=c(1,2,1,2))+
    scale_color_manual(values=c("red","blue", "red","blue"))+
    theme_minimal() +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          strip.text = element_text(size = 14),
          strip.text.y = element_text(angle=0))+
    theme(legend.position="none")+
    labs(x = "Age") +
    scale_y_continuous(name="Percent", limits=c(-0.1,25), breaks=c(seq(from=0, to=25, by=5)), labels=c(seq(from=0, to=25, by=5)))+  
    ggtitle("Elements"), nrow=2,ncol=2)
ggsave(sexplot2, file="C:/Users/Magdalena/demography/education/figures/sexdiffcorr.svg", width = 7.3, height = 7.1)


changeplot2 <- ggarrange(
    ggplot(data=yearchange %>% filter(sex=="f")  , aes(x=age, y=decopart, Group = Component, linetype = Component, color=Component)) +
      geom_line(size = 0.7)+
      scale_color_manual(values=c("red","blue","black")) +
      scale_linetype_manual(values=c(1,1,1))+
      theme_minimal() +
      theme(legend.position="none")+
      labs(x = "Age") +
      scale_y_continuous(name="Years", limits=c(0,5.4), breaks=c(seq(from=0, to=5, by=1)), labels=c(seq(from=0, to=5, by=1)))+  
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            strip.text = element_text(size = 14),
            strip.text.y = element_text(angle=0))+
      geom_hline(yintercept=0, size=0.05, col="grey")+
      ggtitle("Females") ,
    ggplot(data=yearchange %>% filter(sex=="m")  , aes(x=age, y=decopart, Group = Component, linetype = Component, color=Component)) +
      geom_line(size = 0.7)+
      scale_color_manual(values=c("red","blue","black")) +
      scale_linetype_manual(values=c(2,2,2))+
      theme_minimal() +
      theme(legend.position="none")+
      labs(x = "Age") +
      scale_y_continuous(name="Years", limits=c(0,5.4), breaks=c(seq(from=0, to=5, by=1)), labels=c(seq(from=0, to=5, by=1)))+  
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            strip.text = element_text(size = 14),
            strip.text.y = element_text(angle=0))+
      geom_hline(yintercept=0, size=0.05, col="grey")+
      ggtitle("Males"),
    
    ggplot(data=yearchange %>% filter(Component=="Total"),aes(x=age, y=partsrel, group=sex))  + 
      geom_line(aes(linetype=sex), size=0.7)+
      scale_linetype_manual(values=c(1,2))+
      scale_color_manual(values=c("black","black"))+
      theme_minimal() +
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            strip.text = element_text(size = 14),
            strip.text.y = element_text(angle=0))+
      theme(legend.position="none")+
      labs(x = "Age") +
      scale_y_continuous(name="Percent", limits=c(0,27), breaks=c(seq(from=0, to=25, by=5)), labels=c(seq(from=0, to=25, by=5)))+  
      ggtitle("Total"),
    
    ggplot(data=yearchange %>% filter(Component!="Total"),aes(x=age, y=partsrel, group=type2))  + 
      geom_line(aes(col=Component, linetype=sex), size=0.7)+
      scale_linetype_manual(values=c(1,2,1,2))+
      scale_color_manual(values=c("red","blue", "red","blue"))+
      theme_minimal() +
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            strip.text = element_text(size = 14),
            strip.text.y = element_text(angle=0))+
      theme(legend.position="none")+
      labs(x = "Age") +
      scale_y_continuous(name="Percent", limits=c(0,27), breaks=c(seq(from=0, to=25, by=5)), labels=c(seq(from=0, to=25, by=5)))+  
      ggtitle("Elements"), nrow=2,ncol=2)
ggsave(changeplot2, file="C:/Users/Magdalena/demography/education/figures/yeardiff.svg", width = 7.0, height = 7.1)

  




#############################################################################################################################
dev.off()
rm(list=ls())
library(tidyverse)
library(dplyr)
library(eurostat)


data.dir <- "C:\\Users\\Magdalena\\demography\\education\\data\\LE"
data.out <- "C:\\Users\\Magdalena\\demography\\education\\data\\ready"
setwd(data.dir)

###LE data
murtin <- read.table(file="oecdwp.txt", sep=",") # LE at 25
colnames(murtin) <- c("country","year","source", paste("men", c("low","lowci","middle","middleci","high","highci"), sep=""),
                    paste("fem", c("low","lowci","middle","middleci","high","highci"),sep=""))
murtinLE <- murtin %>%
  filter(source=="OECD") %>%
  dplyr::select(country,year,menlow,menmiddle,menhigh,femlow,femmiddle,femhigh) %>%
  pivot_longer(cols=menlow:femhigh,names_to = "edu", values_to="LE") %>%
  mutate(sex=substr(edu,1,1),
         edu=substring(edu,4),
         edu=recode(edu,"low"="1","middle"="2","high"="3"))

vanraalte2 <- read.table(file="vanraalte.txt", sep=",", header=TRUE)  
vanraalte <- vanraalte2 %>%
  pivot_longer(cols=-country,names_to = "sexedu", values_to="LE") %>%
  mutate(sex=substr(sexedu,1,1)) %>%
  mutate(edu=substring(sexedu,5)) %>%
  filter(sexedu!="men_Total" & sexedu!="fem_Total") %>%
  mutate(edu=recode(edu, "low"="1","lower_sec"="2a", "upper_sec"="2b","tert"="3"))



nemeth <- read.table(file="nemeth.txt", sep="\t", header=TRUE)
nemeth <- nemeth %>%
  dplyr::select(Country,Period,Sex,Education,AgeGrp,ex) %>%
  filter(AgeGrp=="30-34" | AgeGrp=="50-54" | AgeGrp=="65-69") %>%
  mutate(edu=recode(Education, "High"="3","Low"="1","Middle"="2")) %>%
  rename_with(tolower) %>%
  dplyr::rename(age=agegrp,LE=ex) %>%
  mutate(sex=recode(sex, "Female"="f", "Male"="m"))
  

austria <- read.table(file="austriaklotz.txt", sep=",", header=TRUE)
austriaLE <- austria %>%
  dplyr::select(Educational,m1981,m1991,m2001,m2006,f1981,f1991,f2001,f2006) %>%
  filter(Educational!="Total") %>%
  pivot_longer(cols=m1981:f2006,names_to = "edu", values_to="LE") %>%
  mutate(sex=substr(edu,1,1)) %>%
  mutate(year=substring(edu,2))
  

belgium <- read.table(file="belgium11.txt", sep=",", header=TRUE)
belgiumLE11 <- belgium %>%
  pivot_longer(cols=Low:Missing,names_to = "edu", values_to="LE") %>%
  filter(edu!="Missing") %>%
  mutate(sex=substr(sexage,1,1),
         age=substring(sexage,2),
         edu=recode(edu,"Low"="1","Mid"="2","High"="3")) 


belgium <- read.table(file="belgium9101.txt", sep=",", header=TRUE)
belgiumLE01 <- belgium %>%
  dplyr::select(education,men,women) %>%
  pivot_longer(cols=men:women,names_to = "sex", values_to="LE") %>%
  filter(education!="all")


pernmayer <- read.table(file="spaineduLE.txt", sep=",", header=TRUE)
spain <- pernmayer %>%
  pivot_longer(cols=low:high,names_to = "edu", values_to="LE") %>%
  mutate(edu=recode(edu,"low"="0","primary"="1","secondary"="2", high="3"),
         sex=substr(sex,1,1))
  

luy <- read.table(file="luydata.csv",sep="\t",header=TRUE) %>%
  dplyr::select(Country:Age, ex_Low:ex_High) %>%
  pivot_longer(ex_Low:ex_High,names_to = "edu",
               values_to = "LE") %>%
  mutate(edu, edu=recode(edu,"ex_Low"="1","ex_Medium"="2","ex_High"="3"),
         Sex=tolower(substr(Sex,1,1))) %>%
  rename_with(tolower)
  
  

###read-in population
setwd(data.out)
popall <- read.table(file="popedu.csv",sep=",",header=TRUE)
popall2 <- read.table(file="popedu2mid.csv", sep=",", header=TRUE)
popall912 <- read.table(file="popedu2mid91.csv", sep=",", header=TRUE)
popspain <- read.table(file="spain.csv", sep=",", header=TRUE)
popdenmark <- read.table(file="denmarkpop0910.csv", sep=",",header=TRUE)
popaustralia <- read.table(file="australia11.csv", sep=",",header=TRUE)

popall <- popall %>%
  mutate(age=recode(age,"From 25 to 29 years"="25-29", "From 30 to 34 years"="30-34","From 35 to 39 years"="35-39",
           "From 40 to 44 years"="40-44", "From 45 to 49 years"="45-49", "From 50 to 54 years"="50-54",
           "From 55 to 59 years"="55-59", "From 60 to 64 years"="60-64", "From 65 to 69 years"="65-69"),
       year=as.character(year), edu=as.character(edu))

popall2 <- popall2 %>%
  mutate(age=recode(age,"From 25 to 29 years"="25-29", "From 30 to 34 years"="30-34","From 35 to 39 years"="35-39",
                    "From 40 to 44 years"="40-44", "From 45 to 49 years"="45-49", "From 50 to 54 years"="50-54",
                    "From 55 to 59 years"="55-59", "From 60 to 64 years"="60-64", "From 65 to 69 years"="65-69"),
         year=as.character(year), edu=as.character(edu))
popall912 <- popall912 %>%
  mutate(age=recode(age,"From 25 to 29 years"="25-29", "From 30 to 34 years"="30-34","From 35 to 39 years"="35-39",
                    "From 40 to 44 years"="40-44", "From 45 to 49 years"="45-49", "From 50 to 54 years"="50-54",
                    "From 55 to 59 years"="55-59", "From 60 to 64 years"="60-64", "From 65 to 69 years"="65-69"),
         year=as.character(year), edu=as.character(edu))


##########################################################################################################################
##########new and old LE
#Austria & 2011 & 25 & \citet{murtin17} \\ 
australia <- murtinLE %>%
  mutate(year=as.character(year), edu=as.character(edu))%>%
  filter(country=="AUS") %>%
  left_join(popaustralia%>% 
              mutate(edu=as.character(edu), sex=substr(sex,1,1)), 
            by=c("sex","edu"))%>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev)) %>%
  group_by(sex) %>%
  dplyr::summarise(year="2011",LEnew=sum(LEprev)) %>%
  mutate(age="25-29", country="Australia")


# Austria & 1981,1991,2001 & 25 & \citet{klotz10} &  1981,1991, 2001 & 25-29 & \citet{IPUMS0322} \\
# & 2011 & 25 & \citet{murtin17} & 2011 & 25-29 &  \citet{Eurostatcensus}\\

austria8101 <- austriaLE %>%
  mutate(edu=recode(Educational,"High"="3","Medium"="2","Low"="1")) %>%
  filter(year<2006) %>%
  left_join(popall %>% filter(country=="AT", age=="25-29", source=="IPUMS")%>% 
              mutate(year=as.character(year), edu=as.character(edu)), 
            by=c("year","sex","edu"))%>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev)) %>%
  group_by(year,sex) %>%
  dplyr::summarise(LEnew=sum(LEprev))

austria <- murtinLE %>%
  mutate(year=as.character(year), edu=as.character(edu))%>%
  filter(country=="AUT") %>%
  left_join(popall %>% filter(country=="Austria", age=="25-29", year=="2011"),
            by=c("sex","edu"))%>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev)) %>%
  group_by(sex) %>%
  dplyr::summarise(year="2012",LEnew=sum(LEprev)) %>%
  bind_rows(austria8101) %>%
  mutate(age="25-29", country="Austria")

# Belgium  & 2001 & 25 &\citet{deboosere09} &  &????  & \\
# & 2011 & 25, 30,50,65 &\citet{renard19} & 2011 & 25-29+  &  \citet{Eurostatcensus}\\
belgium <- belgiumLE11 %>%
  left_join(popall %>% filter(country=="Belgium", year==2011) %>%
              mutate(edu=as.character(edu),
                     age=recode(age,"25-29"="25", "30-34"="30","50-54"="50","65-69"="65")), 
            by=c("sex","edu","age"))%>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev)) %>%
  group_by(year,sex,age) %>%
  dplyr::summarise(LEnew=sum(LEprev))%>%
  mutate(country="Belgium")

#Canada & 2011 & 25 & \citet{murtin17} & 2011  & 25-29 & \citet{IPUMS0322}\\ 
canada <- murtinLE %>%
  mutate(year=as.character(year), edu=as.character(edu))%>%
  filter(country=="CAN", year=="2011") %>%
  left_join(popall %>% filter(country=="CA", age=="25-29", source=="IPUMS", year=="2011")%>% 
              mutate(edu=as.character(edu)), 
            by=c("sex","edu"))%>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev)) %>%
  group_by(sex) %>%
  dplyr::summarise(year="2011",LEnew=sum(LEprev)) %>%
  mutate(age="25-29", country="Canada")


# Czechia &  1999-2003 & 35 & \citet{vanraalte12} & 2001 & 35-39 & \citet{Eurostatcensus}  \\
czechia <- vanraalte %>%
  filter(country=="Czech Rep.") %>%
  left_join(popall2 %>% filter(country=="Czechia", age=="35-39", year==2001), 
            by=c("sex","edu")) %>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev)) %>%
  group_by(year,sex,age) %>%
  dplyr::summarise(LEnew=sum(LEprev)-35)%>%
  mutate(age="35-39", country="Czechia", year="1999-2003")
  
# Denmark & 2001-2005 &  30,50,65 & \citet{nemeth21} & 2001 & 30-34+  &  \citet{Eurostatcensus}\\
# & 2011-2015 &  30,50,65  &\citet{nemeth21} &  2011 & 30-34+ & \citet{Eurostatcensus} \\
denmark0105 <-nemeth %>%
  filter(country=="Denmark", period=="2001-2005") %>%
  left_join(popall %>% filter(country=="Denmark", year==2001) %>%
              mutate(edu=as.character(edu)), 
            by=c("sex","edu","age"))%>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev)) %>%
  group_by(year,sex,age) %>%
  dplyr::summarise(LEnew=sum(LEprev)) %>%
  mutate(country="Denmark",year="2001-2005")

denmark1 <-nemeth %>%
  filter(country=="Denmark", period=="2011-2015") %>%
  left_join(popall %>% filter(country=="Denmark", year==2011) %>%
              mutate(edu=as.character(edu)), 
            by=c("sex","edu","age"))%>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev)) %>%
  group_by(year,sex,age) %>%
  dplyr::summarise(LEnew=sum(LEprev)) %>%
  mutate(country="Denmark", year="2011-2015") %>%
  bind_rows(denmark0105) 


denmark <- luy %>%
  filter(sex!="a",country=="Denmark", (age=="30-34"|age=="50-54"|age=="65-69"), year=="2009/10") %>%
  left_join(popdenmark %>%
              mutate(edu=as.character(edu)),
            by=c("sex","edu","age"))%>%
  mutate(LEprev=as.numeric(le)*as.numeric(prev)) %>%
  group_by(year,sex,age) %>%
  dplyr::summarise(LEnew=sum(LEprev))%>%
  mutate(country="Denmark", year="2009-2010") %>%
  bind_rows(denmark1)

# Estonia &  1998-2002 & 35 & \citet{vanraalte12} & 2001 & 35-39 & \citet{Eurostatcensus}\\
estonia <- vanraalte %>%
  filter(country=="Estonia") %>%
  left_join(popall2 %>% filter(country=="Estonia", age=="35-39", year==2001), 
            by=c("sex","edu")) %>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev)) %>%
  group_by(year,sex,age) %>%
  dplyr::summarise(LEnew=sum(LEprev)-35)%>%
  mutate(age="35-39", country="Estonia", year="1988-2002")

#France &  1991-1999 & 35 & \citet{vanraalte12} & 1991* & 35-39 & \citet{Eurostatcensus}  \\
#& 2011 & 25 & \citet{murtin17} & 2011 & 25-29 & \citet{Eurostatcensus} \\    
france2 <- murtinLE %>%
  mutate(year=as.character(year), edu=as.character(edu))%>%
  filter(country=="FRA") %>%
  left_join(popall %>% filter(country=="France", age=="25-29", year==2011), 
            by=c("sex","edu")) %>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev)) %>%
  group_by(sex) %>%
  dplyr::summarise(year="2012",LEnew=sum(LEprev)) %>%
  mutate(age="25-29", country="France")

france <- vanraalte %>%
  filter(country=="France") %>%
  left_join(popall912 %>% filter(country=="France", age=="35-39", year==1991), 
            by=c("sex","edu")) %>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev)) %>%
  group_by(year,sex,age) %>%
  dplyr::summarise(LEnew=sum(LEprev)-35)%>%
  mutate(age="35-39", country="France", year="1991-1999") %>%
  bind_rows(france2)


# finland &  1991-2000 & 35 & \citet{vanraalte12} & 1991, 2001 & 35-39 & \citet{Eurostatcensus}  \\
finlandpop <- popall2 %>% 
  filter(country=="Finland", age=="35-39", year==2001) %>%
  mutate(edu=recode(edu,"1"="2a")) %>%
  bind_rows(switzerlandpop91 <- popall912 %>%
              filter(country=="Finland", age=="35-39") %>%
              mutate(edu=recode(edu,"1"="2a"))) 

finlandpop2 <- finlandpop %>%
  group_by(sex,edu)%>% 
  dplyr::summarise(n=sum(n)) %>%
  left_join(finlandpop %>% group_by(sex)%>% 
              dplyr::summarise(nall=sum(n)),by="sex") %>%
  mutate(prev=n/nall)

finland2 <- vanraalte %>%
  filter(country=="Finland", sexedu!="men_low", sexedu!="fem_low") %>%
  left_join(finlandpop2, by=c("sex","edu")) %>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev)) %>%
  group_by(sex) %>%
  dplyr::summarise(LEnew=sum(LEprev)-35) %>%
  mutate(year="1991-2001", age="35-39", country="Finland")



#Finland & 2010 & 25 & \citet{murtin17} & 2011 & 25-29 & \citet{Eurostatcensus} \\
finland <- murtinLE %>%
  mutate(year=as.character(year), edu=as.character(edu))%>%
  filter(country=="FIN", year=="2010") %>%
  left_join(popall %>% filter(country=="Finland", age=="25-29",year=="2011"),
            by=c("sex","edu"))%>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev)) %>%
  group_by(sex) %>%
  dplyr::summarise(LEnew=sum(LEprev))%>%
  mutate(year="2010", age="25-29", country="Finland") %>%
  bind_rows(finland2)

# Israel & 2008 & 25 & \citet{murtin17} & 2008 & 25-29 &  \citet{IPUMS0322} \\
israel <- murtinLE %>%
  mutate(year=as.character(year), edu=as.character(edu))%>%
  filter(country=="ISR", year=="2008") %>%
  left_join(popall %>% filter(country=="IL", age=="25-29",year=="2008"),
            by=c("sex","edu"))%>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev)) %>%
  group_by(sex) %>%
  dplyr::summarise(LEnew=sum(LEprev))%>%
  mutate(age="25-29", country="Israel", year="2008")

#Italy & 1991, 2011 & 30,50,65 & \citet{luy19} & 1991,2011 & 30-34+ &   \citet{Eurostatcensus}  \\
italy2 <- murtinLE %>%
  mutate(year=as.character(year), edu=as.character(edu))%>%
  filter(country=="ITA") %>%
  left_join(popall %>% filter(country=="Italy", age=="25-29", year==2011), 
            by=c("sex","edu")) %>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev)) %>%
  group_by(sex) %>%
  dplyr::summarise(year="2012",LEnew=sum(LEprev)) %>%
  mutate(age="25-29", country="Italy")


italy <- luy %>%
  filter(sex!="a",country=="Italy", (age=="30-34"|age=="50-54"|age=="65-69")) %>%
  left_join(popall %>% filter(country=="Italy",(age=="30-34"|age=="50-54"|age=="65-69"),(year=="1991"|year=="2001"|year=="2011")),
            by=c("sex","edu","year","age"))%>%
  mutate(LEprev=as.numeric(le)*as.numeric(prev)) %>%
  group_by(year,sex,age) %>%
  dplyr::summarise(LEnew=sum(LEprev))%>%
  mutate(country="Italy") %>%
  bind_rows(italy2)


# Latvia & 2011 & 25 & \citet{murtin17} & 2011 & 25-29 &  \citet{Eurostatcensus} \\
latvia <- murtinLE %>%
  mutate(year=as.character(year), edu=as.character(edu))%>%
  filter(country=="LVA", year=="2011") %>%
  left_join(popall %>% filter(country=="Latvia", age=="25-29",year=="2011"),
            by=c("sex","edu"))%>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev)) %>%
  group_by(sex) %>%
  dplyr::summarise(LEnew=sum(LEprev))%>%
  mutate(age="25-29", country="Latvia", year="2011")


#Lithuania &  2000-2002 & 35 & \citet{vanraalte12} & 2001* & 35-39 & \citet{Eurostatcensus}\\
lithuania <- vanraalte %>%
  filter(country=="Lithuania") %>%
  left_join(popall2 %>% filter(country=="Lithuania", age=="35-39", year==2001), 
            by=c("sex","edu")) %>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev)) %>%
  group_by(year,sex,age) %>%
  dplyr::summarise(LEnew=sum(LEprev)-35) %>%
  mutate(year="2001", age="35-39", country="Lithuania")


# norway &  1991-2000 & 35 & \citet{vanraalte12} & 1991, 2001 & 35-39 & \citet{Eurostatcensus}  \\
norwaypop <- popall2 %>% 
  filter(country=="norway", age=="35-39", year==2001) %>%
  mutate(edu=recode(edu,"1"="2a")) %>%
  bind_rows(switzerlandpop91 <- popall912 %>%
              filter(country=="Norway", age=="35-39") %>%
              mutate(edu=recode(edu,"1"="2a"))) 

norwaypop2 <- norwaypop %>%
  group_by(sex,edu)%>% 
  dplyr::summarise(n=sum(n)) %>%
  left_join(norwaypop %>% group_by(sex)%>% 
              dplyr::summarise(nall=sum(n)),by="sex") %>%
  mutate(prev=n/nall)

norway2 <- vanraalte %>%
  filter(country=="Norway", sexedu!="men_low", sexedu!="fem_low") %>%
  left_join(norwaypop2, by=c("sex","edu")) %>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev)) %>%
  group_by(sex) %>%
  dplyr::summarise(LEnew=sum(LEprev)-35) %>%
  mutate(year="1991-2001", age="35-39", country="Norway")

# Norway &  1991,2001,2011 & 25 & \citet{murtin17} & 1991, 2001,2011 & 25-29 & \citet{Eurostatcensus}  \\
norway <- murtinLE %>%
  mutate(year=as.character(year), edu=as.character(edu))%>%
  filter(country=="NOR", (year==1991|year==2001|year==2011)) %>%
  left_join(popall %>% filter(country=="Norway", age=="25-29"),
            by=c("sex","edu","year"))%>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev)) %>%
  group_by(sex,year) %>%
  dplyr::summarise(LEnew=sum(LEprev))%>%
  mutate(age="25-29", country="Norway") %>%
  bind_rows(norway2)

# Poland &  2001-2003 & 35 & \citet{vanraalte12} & 2001 & 35-39 & \citet{Eurostatcensus}  \\
poland <- vanraalte %>%
  filter(country=="Poland") %>%
  left_join(popall2 %>% filter(country=="Poland", age=="35-39", year==2001), 
            by=c("sex","edu")) %>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev)) %>%
  group_by(year,sex,age) %>%
  dplyr::summarise(LEnew=sum(LEprev)-35) %>%
  mutate(year="2001", age="35-39", country="Poland")

# Slovenia &  2011 & 25 & \citet{murtin17} & 2011 & 25-29 & \citet{Eurostatcensus}  \\
slovenia <- murtinLE %>%
  mutate(year=as.character(year), edu=as.character(edu))%>%
  filter(country=="SVN", year==2011) %>%
  left_join(popall %>% filter(country=="Slovenia", age=="25-29", year=="2011"),
            by=c("sex","edu"))%>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev)) %>%
  group_by(sex) %>%
  dplyr::summarise(LEnew=sum(LEprev)) %>%
  mutate(year="2011", age="25-29", country="Slovenia")

# Spain & 1980-1989 & 35 &  \citet{pernmayer18} &  1981,1991 &  35-39 & \citet{IPUMS0322}\\
spain8089 <- spain %>%
  filter(year=="1980-89") %>%
  left_join(popspain %>% filter(age=="35-39", year=="1981") %>% 
              mutate(edu=as.character(edu)),
            by=c("sex","edu"))%>%
  left_join(popspain %>% filter(age=="35-39", year=="1991") %>% 
              mutate(edu=as.character(edu)),
            by=c("sex","edu")) %>%
  mutate(prev=(n.x+n.y)/(alln.x+alln.y)) %>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev)) %>%
  group_by(sex) %>%
  dplyr::summarise(LEnew=sum(LEprev)) %>%
  mutate(year="1980-1989")

# & 2012-2015 & 35 &  \citet{pernmayer18} &  2011 & 35-39 & \citet{IPUMS0322} \\
spain <- spain %>%
  filter(year=="2012-15") %>%
  left_join(popspain %>% filter(age=="35-39", year=="2011") %>% 
              mutate(edu=as.character(edu)),
            by=c("sex","edu"))%>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev)) %>%
  group_by(sex) %>%
  dplyr::summarise(LEnew=sum(LEprev)) %>%
  mutate(year="2012-2015") %>%
  bind_rows(spain8089) %>%
  mutate(age="35-39", country="Spain")
  
  

# Sweden &  2001-2005 & 30+ & \citet{nemeth21} & 2001 & 30-34+ & \citet{Eurostatcensus}  \\
sweden0105 <-nemeth %>%
  filter(country=="Sweden", period=="2001-2005") %>%
  left_join(popall %>% filter(country=="Sweden", year==2001) %>%
              mutate(edu=as.character(edu)), 
            by=c("sex","edu","age"))%>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev)) %>%
  group_by(year,sex,age) %>%
  dplyr::summarise(LEnew=sum(LEprev))
# &  2011-2015 &  30+ & \citet{nemeth21}  &  2011 & 30-34+ & \citet{Eurostatcensus}\\
sweden1115 <-nemeth %>%
  filter(country=="Sweden", period=="2011-2015") %>%
  left_join(popall %>% filter(country=="Sweden", year==2011) %>%
              mutate(edu=as.character(edu)), 
            by=c("sex","edu","age"))%>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev)) %>%
  group_by(year,sex,age) %>%
  dplyr::summarise(LEnew=sum(LEprev))

# &  2011 & 25 & \citet{murtin17} & 2011 & 25-29 & \citet{Eurostatcensus}  \\
sweden <- murtinLE %>%
  mutate(year=as.character(year), edu=as.character(edu))%>%
  filter(country=="SWE", year==2011) %>%
  left_join(popall %>% filter(country=="Sweden", age=="25-29", year=="2011"),
            by=c("sex","edu"))%>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev)) %>%
  group_by(sex) %>%
  dplyr::summarise(LEnew=sum(LEprev)) %>%
  mutate(year="2011",age="25-29") %>%
  bind_rows(sweden0105,sweden1115) %>%
  mutate(country="Sweden")

# Switzerland &  1991-2000 & 35 & \citet{vanraalte12} & 1991, 2001 & 35-39 & \citet{Eurostatcensus}  \\
switzerlandpop <- popall2 %>% 
  filter(country=="Switzerland", age=="35-39", year==2001) %>%
  mutate(edu=recode(edu,"1"="2a")) %>%
  bind_rows(switzerlandpop91 <- popall912 %>%
              filter(country=="Switzerland", age=="35-39") %>%
              mutate(edu=recode(edu,"1"="2a"))) 

switzerlandpop2 <- switzerlandpop %>%
  group_by(sex,edu)%>% 
  dplyr::summarise(n=sum(n)) %>%
  left_join(switzerlandpop %>% group_by(sex)%>% 
              dplyr::summarise(nall=sum(n)),by="sex") %>%
  mutate(prev=n/nall)

switzerland <- vanraalte %>%
  filter(country=="Switzerland", sexedu!="men_low", sexedu!="fem_low") %>%
  left_join(switzerlandpop2, by=c("sex","edu")) %>%
  mutate(LEprev=as.numeric(LE)*as.numeric(prev)) %>%
  group_by(sex) %>%
  dplyr::summarise(LEnew=sum(LEprev)-35) %>%
  mutate(year="1991-2001", age="35-39", country="Switzerland")


#US & 1990, 2010 & 30,50,65 & \citet{luy19} & 1990,2010 & 30-34+ &   \citet{Eurostatcensus}  \\
us <- luy %>%
  filter(sex!="a",country=="United", (age=="30-34"|age=="50-54"|age=="65-69")) %>%
  left_join(popall %>% filter(country=="US",(age=="30-34"|age=="50-54"|age=="65-69"),(year=="1990"|year=="2010")),
            by=c("sex","edu","year","age"))%>%
  mutate(LEprev=as.numeric(le)*as.numeric(prev)) %>%
  group_by(year,sex,age) %>%
  dplyr::summarise(LEnew=sum(LEprev))%>%
  mutate(country="US")

allLE <- australia %>%
  bind_rows(austria,belgium,canada,czechia,denmark,estonia,finland,france, israel,italy,latvia,lithuania, norway,poland,slovenia,spain,sweden,switzerland,us)

setwd(data.out)
write.table(allLE, file="allLE.csv", sep=",",row.names=FALSE)


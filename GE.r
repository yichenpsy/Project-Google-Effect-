## Pakete laden
library(tidyverse) ## Alternative: library(dplyr); library(ggplot2)
library(plotrix) # f??r std.error()
library(dplyr)
library(readxl)
library(ez)
library(xlsx)

## Arbeitsspeicher leeren
rm(list=ls())
## Vorbereitung 
# set working directory: Arbeitsverzeichnis setzen (wo liegen die Daten?)
setwd("/Users/yichenzhong/Desktop/GE Daten/")
# welche files (Dateien) sollen eingelesen werden?
logfiles <- list.files(pattern = "GE")
# eine leere Datentabelle anlegen
data.df <- data.frame()
# Daten aller VPs einlesen
for(file in logfiles){
  vplogfile <- read_excel(file)
  vplogfile.1 <- select(vplogfile,Subject,ExperimentName,Session,Session,relevant,StroopSlide1.ACC,StroopSlide1.RT)
  vplogfile.2 <- filter(vplogfile.1,vplogfile.1$relevant!= "NA" )
  vplogfile.3 <- rename(vplogfile.2 , Vp=Subject, Stroop=ExperimentName,Frage=Session,Re=relevant, Acc=StroopSlide1.ACC, Rt=StroopSlide1.RT)
  data.df <- rbind(data.df,vplogfile.3)
}
# UV recoding
data.df$Frage <- factor(data.df$Frage, levels=1:2,labels=c("l","s"))


###Stroop Genauigkeit
## Analyse der summe der Fehler Anzahl pro Bedingung, pro Person
test <- data.df %>% 
  group_by(Vp,Re,Frage) %>% 
  summarise(N=sum(Acc==0)) 
# mean Genauigkeit pro Bedingungen
test1<- data.df %>% 
  group_by(Vp,Re,Frage) %>% 
  summarise(Co = mean(Acc))
test2<- test1 %>% 
  group_by(Re,Frage) %>% 
  summarise(Co1 = mean(Co),SE=std.error(Co))

# ANOVA Erro, Ob Nach schwer Frage mehr Fehler?  ob re W??rter mehr Fehler?
# kein Signifiktant unterschiede! 
Acc.anova <- ezANOVA(data = test1
                     , dv = Co
                     , wid = Vp
                     , within = .(Re,Frage)
)
Acc.anova

## Daten filtern: Falsch, zu schnell, zu langsam
# Ausschluss von Trials, in denen die VP falsch reagiert hat
data1.df <- filter(data.df,Acc==1 )
# zu schnelle Reaktionszeiten ausschliessen 
data1.df %>% 
  filter(Rt <= 200) %>% 
  summarise(N = n())
data2.df <- filter(data1.df, Rt >= 200)   
# zu langsame RTs ausschliessen 
# wieviele Trials?
data2.df %>% 
  group_by(Vp,Re,Frage) %>%
  filter(Rt > mean(Rt) + 3*sd(Rt)) %>% 
  summarise(N = n())
# Trials mit zu langsame RTs ausschliessen
data3.df <- data2.df %>% 
  group_by(Vp,Re,Frage) %>%
  filter(Rt < mean(Rt) + 3*sd(Rt))



## Mittlere Reaktionszeiten pro Bedingung pro VP
data4.df <- data3.df %>% 
  group_by(Vp,Re,Stroop,Frage) %>%
  summarise(Mrt=mean(Rt))
# mittlere Reaktionszeiten pro Bedingung
vp.rt <- data4.df %>% 
  group_by(Re,Frage,Stroop) %>%
  summarise(mean(Mrt))

vp.rt1 <- data4.df %>% 
  group_by(Re,Frage) %>%
  summarise(M=mean(Mrt),SE=std.error(Mrt))

# Bar Plot, y=Rt, UV1=Frage, UV2=RE 
ggplot(vp.rt1, aes(x = vp.rt1$Frage, y = vp.rt1$M, fill = vp.rt1$Re))+ 
  geom_bar(position = "dodge", stat = "identity", colour = "black", width = 0.6)+
  theme_bw()+
  scale_fill_brewer(palette="Pastel1")+
  ylab("Reaction Time (ms)") +
  xlab ("Question difficulty") +
coord_cartesian(ylim=c(400,675))+
  scale_x_discrete(breaks=c("l", "s"),
                   labels=c("easy", "hard"))+
  scale_fill_discrete(name="Words Relevance")+
scale_fill_discrete(name="Words Relevance",
                         breaks=c("ir", "re"),
                         labels=c("Computer irrelevant", "Computer relevant")) + 
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width=.1, position=position_dodge(.6))+
  theme(text=element_text(size=20), axis.text.x=element_text(size=20, colour = "black"),    # Beschriftung x-Achse
             axis.text.y=element_text(size=20, colour = "black"))



vp.rt2 <- data4.df %>% 
  group_by(Re,Stroop) %>%
  summarise(M=mean(Mrt),SE=std.error(Mrt))


##Google Effekt
#1, Rt schwere-re > Rt schwere-ir? (nicht signifikant)
schwierig <- subset(data4.df, data4.df$Frage=="s")
t.test(schwierig$Mrt~schwierig$Re,paired=T)

#2, Rt leichte-re > Rt leichte-ir ? (nicht signifikant)
leicht <- subset(data4.df, data4.df$Frage=="l")
t.test(leicht$Mrt~leicht$Re,paired=T)

#3, Rt re > Rt ir ?  ANOVA ,RE, Frage
#Frage Effekt : bei ANOVA Haupt Effekt , keine Interaktion mt Relevant
rt.anova1 <- ezANOVA(data = data4.df
                     , dv = Mrt
                     , wid = Vp
                     , within = .(Re,Frage))
rt.anova1

 
## Stroop1, Stroop2 Analyse
# Haupt Effekt Stroop, interaktion mit Re
rt.anova2<- ezANOVA(data = data4.df
                     , dv = Mrt
                     , wid = Vp
                     , within = .(Re,Stroop)
)
rt.anova2

#ANOVA Re mit Frage, bei Stroop1, 
# signifikant HE bei RE, interaktion 
stroop1 <- subset(data4.df, data4.df$Stroop=="GE1")
rt.anova3 <- ezANOVA(data = stroop1
                     , dv = Mrt
                     , wid = Vp
                     , within = .(Re)
                     , between = .(Frage)
)
rt.anova3

# Google Effekt bei Stroop1
# bei Stroop1, nach schwere Frage, Rt_re > Rt_ir?
stroop1s<- subset(schwierig, schwierig$Stroop=="GE1")
t.test(stroop1s$Mrt~stroop1s$Re,paired=T)
## bei Stroop1, nach leichte Frage, Rt_re > Rt_ir?
stroop1l<- subset(leicht, leicht$Stroop=="GE1")
t.test(stroop1l$Mrt~stroop1l$Re,paired=T)

# ANOVA Re mit Frage, bei Stroop2, 
stroop2 <- subset(data4.df, data4.df$Stroop=="GE2")
rt.anova4 <- ezANOVA(data = stroop2
                     , dv = Mrt
                     , wid = Vp
                     , within = .(Re)
                     , between = .(Frage)
)
rt.anova4


#Grafikf??r Stroop1
vp.rt.stroop1 <- stroop1 %>% 
  group_by(Re,Frage) %>%
  summarise(M=mean(Mrt),SE=std.error(Mrt))
# Bar Plot F??r Stroop1, y=Rt, UV1=Frage, UV2=RE 
ggplot(vp.rt.stroop1, aes(x = vp.rt.stroop1$Frage, y = vp.rt.stroop1$M, fill = vp.rt.stroop1$Re))+ 
  geom_bar(position = "dodge", stat = "identity", colour = "black", width = 0.6)+
  theme_bw()+
  scale_fill_brewer(palette="Pastel1")+
  ylab("Reaction Time (ms)") +
  xlab ("Question difficulty") +
  coord_cartesian(ylim=c(400,675))+
  scale_x_discrete(breaks=c("l", "s"),
                   labels=c("easy", "hard"))+
  scale_fill_discrete(name="Words Relevance")+
  scale_fill_discrete(name="Words Relevance",
                      breaks=c("ir", "re"),
                      labels=c("Computer irrelevant", "Computer relevant")) + 
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width=.1, position=position_dodge(.6))+
  theme(text=element_text(size=20), axis.text.x=element_text(size=20, colour = "black"),    # Beschriftung x-Achse
        axis.text.y=element_text(size=20, colour = "black"))




#Grafik f??r Stroop2
vp.rt.stroop2 <- stroop2 %>% 
  group_by(Re,Frage) %>%
  summarise(M=mean(Mrt),SE=std.error(Mrt))
# Bar Plot f??r Stroop2, y=Rt, UV1=Frage, UV2=RE 
ggplot(vp.rt.stroop2, aes(x = vp.rt.stroop2$Frage, y = vp.rt.stroop2$M, fill = vp.rt.stroop2$Re))+ 
  geom_bar(position = "dodge", stat = "identity", colour = "black", width = 0.6)+
  theme_bw()+
  scale_fill_brewer(palette="Pastel1")+
  ylab("Reaction Time (ms)") +
  xlab ("Question difficulty") +
  coord_cartesian(ylim=c(400,675))+
  scale_x_discrete(breaks=c("l", "s"),
                   labels=c("easy", "hard"))+
  scale_fill_discrete(name="Words Relevance")+
  scale_fill_discrete(name="Words Relevance",
                      breaks=c("ir", "re"),
                      labels=c("Computer irrelevant", "Computer relevant")) + 
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width=.1, position=position_dodge(.6))+
  theme(text=element_text(size=20), axis.text.x=element_text(size=20, colour = "black"),    # Beschriftung x-Achse
        axis.text.y=element_text(size=20, colour = "black"))


##########################

## Bio Data

# neue Variablen hinzuf??gen
data5.df <- read_excel("~/Desktop/data5.xlsx")


# Anzahl Geschlecht
anzahl.Geschl<-table(data5.df$Gesch)/4
anzahl.Geschl

# Alter
age.data <- data5.df %>% 
  group_by(Vp) %>% 
  summarise(age=mean(Alter)) 
summary(age.data$age)
sd(age.data$age)

# Bildung
anzahl.Bildung<-table(data5.df$Bildung)/4
anzahl.Bildung

#Benutzung H??ufigkeit
H??ufig.data <- data5.df %>% 
  group_by(Vp) %>% 
  summarise(H??ufig=mean(H??ufig)) 
summary(H??ufig.data$H??ufig)
sd(H??ufig.data$H??ufig)


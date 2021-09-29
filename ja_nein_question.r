library(tidyverse)
library(readxl)

## clear workspace
rm(list=ls())

## directories
home = ("/Users/yichenzhong/Desktop")
setwd(home)

## import data
Ja_nein <- read_excel("Ja_nein.xls", col_types = c( "text", rep("numeric", 45)))

## rename columns
Ja_nein <- rename(Ja_nein, frage = X__1, vp01 = '1', vp02 = '2', vp03 = '3', vp04 = '4', vp05 = '5', 
                  vp06 = '6', vp07 = '7', vp08 = '8', vp09 = '9', vp10 = '10',
                  vp11 = '11', vp12 = '12', vp13 = '13', vp14 = '14', vp15 = '15',
                  vp16 = '16',vp17 = '17',vp18 = '18',vp19 = '19',vp20 = '20',
                  vp21 = '21',vp22 = '22',vp23 = '23',vp24 = '24',vp25 = '25',
                  vp26 = '26',vp27 = '27',vp28 = '28',vp29 = '29',vp30 = '30',
                  vp31 = '31',vp32 = '32',vp33 = '33',vp34 = '34',vp35 = '35',
                  vp36 = '36',vp37 = '37',vp38 = '38',vp39 = '39',vp40 = '40',
                  vp41 = '41',vp42 = '42',vp43 = '43',vp44 = '44')


## select(korrekt, vp01:vp16) ---- select columns; 
## mutate_all(funs(c = . + korrekt))  ---- calculate: L1 + korrekt (for all columns, i.e. all questions)
## possible results: 0 + 0 = 0; 0 + 1 = 1; 1 + 0 = 1; 1 + 1 = 2

## result 0 and 2 are the correct answers
## recode the answers: 0 and 2 into 0 (correct); 1 into 0 (incorrect)
daten <- Ja_nein %>%
  select(korrekt, vp01:vp44) %>%       
  mutate_all(funs(c = . + korrekt)) %>%
  select(vp01_c:vp44_c) %>%
  mutate_all(funs(recode(., '0' = 0L, '1' = 1L, '2' = 0L)))

### transpose data
datenT <- t(daten)
datenT <- data.frame(datenT)
colnames(datenT)=Ja_nein$frage

## add column VP
daten.final <- datenT %>%
  mutate(Vp = 1:44)
rownames(daten.final) <- c()

## add columns sumLF ("leichte Fragen") and sumSF ("schwere Fragen"): each contains the sum of errors for this vp
daten.final <- daten.final %>% 
  mutate(sumLF = rowSums(.[1:16]),
         sumSF = rowSums(.[17:32]))
## mean
Accl<-(1-(mean(daten.final$sumLF)/16))*100
Accl
Accs<-(1-(mean(daten.final$sumSF)/16))*100
Accs
## SE
daten.final1<- daten.final %>% 
  summarise(Co1 = mean(1-sumLF/16),SE=std.error(1-sumLF/16))
daten.final2<- daten.final %>% 
  summarise(Co1 = mean(1-sumSF/16),SE=std.error(1-sumSF/16))


t.test(daten.final$sumSF, daten.final$sumLF,paired=T)

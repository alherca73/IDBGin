library(sf)
library(dplyr)
setwd("/Users/alexhernandez/Desktop/Gines")
library(here)

### Read in the ETA Crops file
Data.raw<-read.csv(here("Crops/Eta_Crops.csv"))
### Read in the Deptos Names
Deptos<-read.csv(here("Crops/deptonames.csv"))
Data.work<-full_join(Data.raw, Deptos, by = c("DEPTOS"="CodDepto"))

## Leaving "Pastos/Cultivos" as the difference from the RISOMA fraction

target<-c("No Cultivos","Pastos/Cultivos")
Data.work1<- Data.work%>%
  filter(!CLCROP %in% target & ETA !=10 )


# Replacing "Agricultura Tecnificada" with "Banana" in the following provinces
target2<-c("COLON", "CORTES", "ATLANTIDA","YORO")
Data.work2<-Data.work1 %>% 
  mutate(CLCROP = replace(CLCROP, CLCROP == "Agricultura Tecnificada" & Name %in% target2, "Banana"))

#Data.work2<-Data.work2 %>% 
#  mutate(CLCROP = replace(CLCROP, CLCROP == "Pastos/Cultivos", "Pastos"))

target3<-c("Pastos/Cultivos")
Data.work3<- Data.work%>%
  filter(CLCROP %in% target3 & ETA !=10 )

Data.work3$INVRISOMA<-1-(Data.work3$RISOMA)

Data.work3$APastos<-(Data.work3$Count*Data.work3$INVRISOMA)

Data.work3$ARISOMA<-(Data.work3$Count*Data.work3$RISOMA)

Data.work3<-Data.work3 %>% 
#  mutate(CLCROP = replace(CLCROP, CLCROP == "Pastos/Cultivos", "Pastos"))

Data.work2$INVRISOMA<-1-(Data.work2$RISOMA)



library(sf)
library(dplyr)
setwd("/Users/alexhernandez/Desktop/Gines")
library(here)

### Read in the ETA Crops file
Data.raw<-read.csv(here("Crops/Eta_Crops.csv"))
### Read in the Deptos Names
Deptos<-read.csv(here("Crops/deptonames.csv"))
Data.work<-full_join(Data.raw, Deptos, by = c("DEPTOS"="CodDepto"))

# Selecting everything that is not No cultivos and Pastos/cultivos as they are processed later in the script

target<-c("No Cultivos","Pastos/Cultivos")
Data.work1<- Data.work%>%
  filter(!CLCROP %in% target & ETA !=10 )


# Replacing "Agricultura Tecnificada" with "Banana" in the following provinces
target2<-c("COLON", "CORTES", "ATLANTIDA","YORO")
target3<-c("Agricultura Tecnificada","Musácea")
Data.work2<-Data.work1 %>% 
  mutate(CLCROP = replace(CLCROP, CLCROP %in% target3 & Name %in% target2, "Banana"))

# Convert arrozales in Intibuca to Agricultura Tecnificada
Data.work2<-Data.work2 %>% 
  mutate(CLCROP = replace(CLCROP, CLCROP == "Arrozales" & Name == "INTIBUCA", "Agricultura Tecnificada"))

# Getting summaries per province by crop
Sumario<- Data.work2 %>%
  group_by(Name, CLCROP) %>%
  summarise( areatot = sum(Count))
########################################
########################################

target4<-c("Pastos/Cultivos")
Data.work3<- Data.work%>%
  filter(CLCROP %in% target4 & ETA !=10 )

Data.work3$INVRISOMA<-1-(Data.work3$RISOMA)

Data.work3$APastos<-(Data.work3$Count*Data.work3$INVRISOMA)

Data.work3$ARISOMA<-(Data.work3$Count*Data.work3$RISOMA)

# Getting summaries per province by pastos / cereales IFPRI
Sumario.pastocereal<- Data.work3 %>%
  group_by(Name) %>%
  summarise( Pastizales = sum(APastos), Cereales = sum(ARISOMA))












Data.work3<-Data.work3 %>% 
#  mutate(CLCROP = replace(CLCROP, CLCROP == "Pastos/Cultivos", "Pastos"))

Data.work2$INVRISOMA<-1-(Data.work2$RISOMA)



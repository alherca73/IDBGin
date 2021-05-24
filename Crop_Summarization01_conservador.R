library(sf)
library(dplyr)
setwd("/Users/alexhernandez/Desktop/Gines")
library(here)

### Read in the ETA Crops file
Data.raw<-read.csv(here("Crops/Iotaf_Crops.csv"))
### Read in the Deptos Names
Deptos<-read.csv(here("Crops/deptonames.csv"))
Data.work<-full_join(Data.raw, Deptos, by = c("DEPTOS"="CodDepto"))
Data.work$RISOMA<-(Data.work$RICE+Data.work$SORG+Data.work$MAIZ)/10000
Data.work$BEVE<-(Data.work$BEAN+Data.work$VEGE)/10000

# Selecting everything that is not No cultivos and Pastos/cultivos as they are processed later in the script

target<-c("No Cultivos","Pastos/Cultivos")
target2<-c(10,101)
Data.work1<- Data.work%>%
  filter(!CLCROPS %in% target & !IOTAF %in% target2 )


# Replacing "Agricultura Tecnificada" with "Banana" in the following provinces
target3<-c("COLON", "CORTES", "ATLANTIDA","YORO")
target4<-c("Agricultura Tecnificada","Musácea")
Data.work2<-Data.work1 %>% 
  mutate(CLCROPS = replace(CLCROPS, CLCROPS %in% target4 & Name %in% target3, "Banana"))

# Convert arrozales in Intibuca to Agricultura Tecnificada
Data.work2<-Data.work2 %>% 
  mutate(CLCROPS = replace(CLCROPS, CLCROPS == "Arrozales" & Name == "INTIBUCA", "Agricultura Tecnificada"))

# Getting summaries per province by crop
Sumario<- Data.work2 %>%
  group_by(Name, CLCROPS) %>%
  summarise( areatot = sum(Count))
########################################
########################################

target5<-c("Pastos/Cultivos")
Data.work3<- Data.work%>%
  filter(CLCROPS %in% target5 & !IOTAF %in% target2 )

Data.work3$INVRISOMA<-1-(Data.work3$RISOMA+Data.work3$BEVE)

Data.work3$APastos<-(Data.work3$Count*Data.work3$INVRISOMA)

Data.work3$ARISOMA<-(Data.work3$Count*Data.work3$RISOMA)

Data.work3$ABEVE<-(Data.work3$Count*Data.work3$BEVE)

# Getting summaries per province by pastos / cereales IFPRI
Sumario.pastocereal.FrijolVeg<- Data.work3 %>%
  group_by(Name) %>%
  summarise( Pastizales = sum(APastos), Cereales = sum(ARISOMA), FrijolVeg = sum(ABEVE))












Data.work3<-Data.work3 %>% 
#  mutate(CLCROP = replace(CLCROP, CLCROP == "Pastos/Cultivos", "Pastos"))

Data.work2$INVRISOMA<-1-(Data.work2$RISOMA)



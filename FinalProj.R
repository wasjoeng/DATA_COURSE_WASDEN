library(plyr)
library(tidyverse)
library(ggmap)
library(stringi)
library(ggplot2)
library(dplyr)

#SETUP

col1 = "#011f4b"

col2 = "#6497b1"

col3 = "#b3cde0"

col4 = "#CC0000"

colme = c(col1,col2,col3,col4)

df1 = read.csv("Wasden.csv", na.strings = c("","NA"))
df = df1[c(2:4,7,11:15)]
df = na.omit(df)

unique(df1$StreamName)
unique(df1$WRIA_Num)

dfINDX = df %>%
  filter(Species == "INDX")

dfFOOT = df %>%
  filter(Species == "FOOT")

dfSPOT = df %>% 
  filter(Species == "SPOT")

dfSUPP = df %>% 
  filter(Species == "SUPP")

dfREMOVE.1 = join(dfINDX, dfFOOT)
dfREMOVE.2 = join(dfREMOVE.1, dfSPOT)
dfREMOVE = join(dfREMOVE.2, dfSUPP)

dfFINAL.1 = anti_join(df, dfINDX)

dfFINAL.2 = anti_join(dfFINAL.1, dfFOOT)

dfFINAL.3 = anti_join(dfFINAL.2, dfSPOT) 

dfFINAL = anti_join(dfFINAL.3, dfSUPP)

dfCOORD = dfFINAL[-grep('^Unnamed',df$StreamName),]

dfCOORD.1 = dfCOORD %>%
  filter(PercentSeen > 100)

dfCOORD = anti_join(dfCOORD, dfCOORD.1)

dfCOORD$WRIA_Num =  str_replace_all(dfCOORD$WRIA_Num, "1", "Nooksack")
dfCOORD$WRIA_Num =  str_replace_all(dfCOORD$WRIA_Num, "2", "San Juan")
dfCOORD$WRIA_Num =  str_replace_all(dfCOORD$WRIA_Num, "3", "Lower Skagit-Samish")
dfCOORD$WRIA_Num =  str_replace_all(dfCOORD$WRIA_Num, "4", "Upper Skagit")
dfCOORD$WRIA_Num =  str_replace_all(dfCOORD$WRIA_Num, "5", "Stillaguamish")
dfCOORD$WRIA_Num =  str_replace_all(dfCOORD$WRIA_Num, "6", "Island")
dfCOORD$WRIA_Num =  str_replace_all(dfCOORD$WRIA_Num, "7", "Snohomish")
dfCOORD$WRIA_Num =  str_replace_all(dfCOORD$WRIA_Num, "8", "Cedar/Samish")
dfCOORD$WRIA_Num =  str_replace_all(dfCOORD$WRIA_Num, "9", "Duwamish/Green")
dfCOORD$WRIA_Num =  str_replace_all(dfCOORD$WRIA_Num, "10", "Puyallup/White")
dfCOORD$WRIA_Num =  str_replace_all(dfCOORD$WRIA_Num, "11", "Nisqually")
dfCOORD$WRIA_Num =  str_replace_all(dfCOORD$WRIA_Num, "12", "Chambers-Clover")
dfCOORD$WRIA_Num =  str_replace_all(dfCOORD$WRIA_Num, "13", "Deschutes")
dfCOORD$WRIA_Num =  str_replace_all(dfCOORD$WRIA_Num, "14", "Kennedy-Goldsborough")
dfCOORD$WRIA_Num =  str_replace_all(dfCOORD$WRIA_Num, "15", "Kitsap")
dfCOORD$WRIA_Num =  str_replace_all(dfCOORD$WRIA_Num, "16", "Skokomish-Dosewallips")
dfCOORD$WRIA_Num = str_replace_all(dfCOORD$WRIA_Num, "NooksackStillaguamish", "Stillaguamish")



dfCOORD$RunYear = as.numeric(as.character(dfCOORD$RunYear))


#Filter the years
dfFIRST = dfCOORD %>%
  filter(RunYear <= 2005)

dfSECOND = dfCOORD %>%
  filter(RunYear >=2006)

complete.cases(dfFIRST)

Streams<-as.data.frame(unique(dfFIRST$StreamName))

STREAMS <- as.data.frame(Streams[-c(3,5,8,12,16,17,20,26,66,78,79,81,83,91,94,98,101,106,109,
                      110,114,118,119,120,122,126,129,131,132,135,137,139,141,
                      142,149,152,155,156,159:165,167,168,170,171,173,176,178,
                      183,188,190,191,196,199,206:208,213,214,217,219,224,228,
                      230,232,235,238,244,246,249,253,256,258,261:263,304,305,
                      310,318,319,324,327,336,338,341,343,347,351,371,373:376,
                      379,380,382,383,385,393,399,400,402,406,409),])
STREAMS <- as.data.frame(STREAMS[-c(50),])
STREAMS <- as.data.frame(STREAMS[-c(24),])
colnames(STREAMS) <- c("StreamName")

Final.Coords = read.csv("Coords.csv")
Final.Coords = as.data.frame(Final.Coords[,c(1:3)])
Final.Coords = na.omit(Final.Coords)

Final.Template = right_join(dfCOORD,Final.Coords)

Final.Template$Live_Count = as.numeric(as.character(Final.Template$Live_Count))
Final.Template$Dead_Count = as.numeric(as.character(Final.Template$Dead_Count))

First.Final = Final.Template %>%
  filter(RunYear <= 2000)

Second.Final = Final.Template %>%
  filter(RunYear >2000 & RunYear <=2005)

Third.Final = Final.Template %>%
  filter(RunYear > 2005 & RunYear <= 2010)

Fourth.Final = Final.Template %>%
  filter(RunYear > 2010 & RunYear <= 2015)



# Models
library(modelr)

mod1.1 = lm(Live_Count ~ RunYear*Species, Final.Template)
mod1.2 = lm(Live_Count ~ RunYear*Species*Flow, Final.Template)
mod1.3 = lm(Live_Count ~ RunYear*Species*Dead_Count, Final.Template)
mod1.4 = lm(Live_Count ~ RunYear*Species*Flow*Dead_Count, Final.Template)


summary(mod1.1)


pred1 = add_predictions(Final.Template,mod1.2)

ggplot(pred1, aes(x=RunYear,color=Species)) +
  geom_smooth(aes(y=Live_Count),color="Black") +
  geom_smooth(aes(y=pred),color="Red")

mod2 = lm(Live_Count ~ RunYear*Species*Flow, First.Final)
pred2 = add_predictions(First.Final, mod2)

mod3 = lm(Live_Count ~ RunYear*Species*Flow, Second.Final)
pred3 = add_predictions(Second.Final, mod3)

mod4 = lm(Live_Count ~ RunYear*Species*Flow, Third.Final)
pred4 = add_predictions(Third.Final, mod4)

mod5 = lm(Live_Count ~ RunYear*Species*Flow, Fourth.Final)
pred5 = add_predictions(Fourth.Final, mod5)




pred1 = na.omit(pred1)
#Test with ggmap


ggmap::register_google(key = "AIzaSyDjTK7ZjYKGr1nE5PmaSncAg4g9L913C_o")

ggmap(get_googlemap(center = c(lon = -121.335167, lat = 47.608013),
                    zoom = 7, scale = 2,
                    maptype ='terrain')) + 
  geom_point(aes(x = Longitude, y = Latitude,  colour = pred), data = pred1, size = 0.5) +
  theme(legend.position="right") +
  facet_wrap(facets = "Species")

ggmap(get_googlemap(center = c(lon = -121.335167, lat = 47.608013),
                    zoom = 7, scale = 2,
                    maptype ='terrain')) + 
  geom_point(aes(x = Longitude, y = Latitude,  colour = pred), data = pred, size = 0.5) +
  theme(legend.position="right") +
  facet_wrap(facets = "Species")

ggmap(get_googlemap(center = c(lon = -121.335167, lat = 47.608013),
                         zoom = 8, scale = 2,
                         maptype ='terrain')) + 
  geom_point(aes(x = Longitude, y = Latitude,  colour = Live_Count), data = First.Final, size = 0.5) +
  theme(legend.position="right") +
  facet_wrap(facets = "Species")


ggmap(get_googlemap(center = c(lon = -121.335167, lat = 47.608013),
                    zoom = 7, scale = 2,
                    maptype ='terrain',
                    color = 'color')) + 
  geom_point(aes(x = Longitude, y = Latitude,  colour = Live_Count < Dead_Count), data = Final.Template, size = 0.5) +
  theme(legend.position="right") +
  scale_color_manual(values = colme)

ggmap(get_googlemap(center = c(lon = -122.335167, lat = 47.608013),
                    zoom = 9, scale = 2,
                    maptype ='terrain',
                    color = 'color')) + 
  geom_point(aes(x = Longitude, y = Latitude,  colour = Species), data = Final.Template, size = 0.5) +
  theme(legend.position="right") +
  scale_color_manual(values = colme)






#Test with ggplot


ggplot(dfFINAL.2, aes(x=StreamName, y=Live_Count)) +
  geom_point() +
  facet_wrap("Species")

ggplot(dfFINAL.3, aes(x=StreamName, y=Live_Count)) +
  geom_point() +
  facet_wrap("Species")

ggplot(dfFINAL, aes(x=StreamName, y=Live_Count)) +
  geom_point() +
  facet_wrap("Species")

ggplot(dfCOORD, aes(x=StreamName, y=Live_Count)) +
  geom_point() +
  facet_wrap("Species")

ggplot(dfCOORD, aes(x=StreamName, y=PercentSeen)) +
  geom_point() +
  facet_wrap("Species")

ggplot(dfCOORD, aes(x=StreamName, y=PercentSeen)) +
  geom_point() +
  facet_wrap("Species")

ggplot(dfCOORD, aes(x=StreamName, y=PercentSeen)) +
  geom_point() +
  facet_wrap("RunYear")

ggplot(dfCOORD, aes(x=StreamName, y=Live_Count)) +
  geom_point() +
  facet_wrap("Species")

ggplot(dfCOORD, aes(x=StreamName, y=Live_Count)) +
  geom_point() +
  facet_wrap("RunYear")

ggplot(dfFINAL.1, aes(x=StreamName, y=Live_Count)) +
  geom_point() +
  facet_wrap("Species")

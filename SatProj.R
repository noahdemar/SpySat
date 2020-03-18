#Satellite Project
library(ggplot2)
library(dplyr)
library(tidyverse)
library('e1071')
library('caret')
dataurl = "https://www.ucsusa.org/media/11492"

df = read.csv('UCS_Satellite_Database_4-1-2019.csv',stringsAsFactors = FALSE)

#Type of Orbit
p1 = ggplot(df, aes(x=launch_mass,y=Apogee, col=type_of_orbit))+
  ggtitle("Apogee vs Launch Mass") +
  xlab("Launch Mass (Kg)")+
  ylab("Apogee (Km)")+
  guides(color=guide_legend(title="Type of Orbit"))+
  geom_point()+
  theme_bw()
p1 

#Type of Use
p2 = ggplot(df, aes(x=launch_mass,y=Apogee, col=Users))+
  ggtitle("Apogee vs Orbit Type (log plot)") +
  scale_y_continuous(trans='log10')+
  scale_x_continuous(trans='log10')+
  xlab("Launch Mass (Kg)")+
  ylab("Apogee (Km)")+
  guides(color=guide_legend(title="Satellite Users"))+
  geom_point()+
  theme_bw()
p2 

p3 = ggplot(df, aes(x=Period,y=Inclination, col=Purpose))+
  ggtitle("Orbital Geometry Characterized by Purpose") +
  scale_y_continuous(trans='log10')+
  scale_x_continuous(trans='log10')+
  ylab("Inclination (deg)")+
  xlab("Period (hrs)")+
  guides(color=guide_legend(title="Satellite Purpose"))+
  geom_point()+
  theme_bw()
p3 



#known spy sats
spy = subset(df,df$Purpose=="Surveillance")

p4 = ggplot(spy, aes(x=Perigee,y=Apogee, col=Purpose))+
  ggtitle("Surveillance Satellite Orbits") +
  ylab("Perigee (km)")+
  xlab("Apogee (Km)")+
  guides(color=guide_legend(title="Satellite Purpose"))+
  geom_point()+
  theme_bw()
p4 


mil = subset(df,df$Users %in% c("Military","Commercial/Military","Government/Commercial/Military","Government/Military","Military/Civil","Military/Commercial","Military/Government"))

mil$Users = 'Military'

`%notin%` <- Negate(`%in%`)
com = subset(df,df$Users %notin% c("Military","Commercial/Military","Government/Commercial/Military","Government/Military","Military/Civil","Military/Commercial","Military/Government"))

com$Users = 'Commercial'

combined = rbind(com,mil)


# Orbital characteristics 
orbchar = dplyr::select(df, class_of_orbit, Apogee, Perigee)
orbchar=orbchar[-c(818,1243,1874),]
m = svm(class_of_orbit~., data = orbchar,type = "C")
plot(m,orbchar,title="Orbital Characteristic Classifications")


#Mil sats by country 
milnat = dplyr::select(combined,Purpose,Apogee,Perigee)
m1 = svm(Purpose~., data = milnat,type = "C")
plot(m1,milnat, color.palette = rainbow,)


#Military sats
simporbchar = dplyr::select(combined,Users,Apogee,Perigee)
m2 = svm(Users~., data =simporbchar, type = "C", kernel='radial',cost=10,gamma=0.2)
plot(m2,simporbchar,fill = T)


points(mil$Perigee,mil$Apogee)


pred = predict(m2,simporbchar)
predresults = confusionMatrix(pred,as.factor(combined$Users),positive = "Military")
predresults


anomal1 = subset(mil,mil$Perigee>35000)
anomal = subset(mil,mil$Perigee<35000)

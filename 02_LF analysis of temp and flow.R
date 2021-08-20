library(tidyverse)
library(lubridate)
library(corrplot)

# Palette -----------------------------------------------------------------
#just for fun palette
library(wesanderson)
pal <- wes_palette(name = "Zissou1", type = "continuous")

pal2 <- wes_palette(5, name = "Zissou1", type = "discrete")

color1<-pal[1] #blue
color2<-pal[2] #light blue
color3<-pal[3] #yellow
color4<-pal[4] #orange
color5<-pal[5] #red
new_pal <- c(color1, color3, color5)

#scale_color_manual(values=wes_palette(n=3, name="Zissou1"))
#scale_fill_manual(values=wes_palette(n=3, name="Zissou1"))

##Load data 
data <- read.csv("LocalFreeze.csv", header=T)

# correlations in temp and flow--------------------------------------------------------

##redo with new data file
coredata <- cor(data[], use="pairwise.complete.obs")
corrplot.mixed(coredata, lower.col="black", upper="square")


# visualize relationships with year and ordinal date -------------------------------
ggplot(data, aes(x=IceOrd, y=meanFlow, col=month))+ geom_smooth() +
  scale_color_manual(values=new_pal) +
  labs(x = "est Ice in Date", y = "avg water flow") +
  ggtitle("River flow vs local freeze up")

ggplot(data, aes(x=year, y=meanFlow, col=month))+ geom_smooth()+
  scale_color_manual(values=new_pal)+
  labs(x = "year", y = "avg water flow") +
  ggtitle("River flow over time")

ggplot(data, aes(x=IceOrd, y=avg_temp, col=month))+ geom_smooth() +
  scale_color_manual(values=new_pal) +
  labs(x = "est Ice in Date", y = "avg monthly temp") +
  ggtitle("Air temp vs local freeze up")

ggplot(data, aes(x=year, y=avg_temp, col=month))+ geom_smooth()+
  scale_color_manual(values=new_pal)+
  labs(x = "year", y = "avg monthly temp") +
  ggtitle("Pre-freeze air temp over time")

# AIC and regression ------------------------------------------------------
##help!

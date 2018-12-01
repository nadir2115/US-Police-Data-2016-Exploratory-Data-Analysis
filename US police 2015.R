#Unsupervised Machine Learning project to study US Police killings- Nadir Nibras

# clear workspace variables
rm(list = ls()); 
# clear window (same as ctrl+L. )
cat("\014")   
# close all plots
graphics.off() 


library(dplyr)
library(cluster)
library(factoextra)
library(ggplot2)
theme_set(theme_bw())
library(sf)
library(rworldmap)
library(readr)
library(datasets)
library(corrplot)
library(stats)
library(ggrepel)
library(textir) 
library(BBmisc)
library(rstudioapi)
library(USAboundaries)
library(ggrepel)

# main code ---------------------------------------------------------------

# set directory to R script folder
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))


# load data ---------------------------------------------------------------
pkdata<-read.csv("US police 2015.csv")

pkdata$name=NULL
pkdata$year=NULL
pkdata$state_fp= NULL
pkdata$tract_ce= NULL
pkdata$geo_id= NULL
pkdata$county_id= NULL
pkdata$streetaddress= NULL
pkdata$namelsad= NULL
pkdata$lawenforcementagency= NULL



pkdata$share_black=as.numeric(levels(pkdata$share_black))[pkdata$share_black]
pkdata$share_white=as.numeric(levels(pkdata$share_white))[pkdata$share_white]
pkdata$share_hispanic=as.numeric(levels(pkdata$share_hispanic))[pkdata$share_hispanic]
pkdata$pov=as.numeric(levels(pkdata$pov))[pkdata$pov]
pkdata$age=as.numeric(levels(pkdata$age))[pkdata$age]
pkdata$p_income=as.numeric(levels(pkdata$p_income))[pkdata$p_income]


pkdata$county_fp=factor(pkdata$county_fp)

# Summarize data
summary(pkdata)


ggplot(pkdata, aes(day)) + geom_histogram(color="black",fill="orange",binwidth=1)

ggplot(pkdata, aes(share_black)) + 
  geom_histogram(aes(y=..density..),color="black",fill="orange",binwidth=2.5)+
  geom_density(alpha=.2, fill="grey")

ggplot(pkdata, aes(share_white)) + 
  geom_histogram(aes(y=..density..),color="black",fill="pink")+
  geom_density(alpha=.2, fill="grey")

ggplot(pkdata, aes(share_hispanic)) + geom_histogram(color="black",fill="pink")

ggplot(pkdata, aes(age))+ 
  geom_histogram(color="black",fill="pink",binwidth=1)


# statePop<-read.csv("state data.csv")
# 
# stateCount=table(pkdata$state)
# stateCount= data.frame(stateCount)
# colnames(stateCount) <- c("state", "count")
# statedata= merge(stateCount, statePop)
# statedata$killingPercent=100*statedata$count/sum(statedata$count)
# 
# ggplot(statedata, aes(killingPercent, percent))+
#   geom_jitter()+
#   geom_text_repel(aes(label=state), size=4)+
#   geom_smooth(method= 'lm')


world <- getMap(resolution = "low")
class(world)
world <- st_as_sf(world)
# class(world)



ggplot(data = world)+
   geom_sf(fill="grey")+
  geom_jitter(data=pkdata, aes(longitude,latitude,col= age), size = 2.2)+
  coord_sf(xlim = c(min(pkdata$longitude)+37, max(pkdata$longitude)), 
           ylim = c(min(pkdata$latitude)+6, max(pkdata$latitude)-10))+
  xlab("Longitude")+ 
  ylab("Latitude")+
  ggtitle("Police killing locations in 2015")
  
ggplot(data = world)+
  geom_sf(fill="grey")+
  geom_jitter(data=pkdata, aes(longitude,latitude,col= raceethnicity), size = 2,alpha=0.5)+
  coord_sf(xlim = c(min(pkdata$longitude)+37, max(pkdata$longitude)), 
           ylim = c(min(pkdata$latitude)+6, max(pkdata$latitude)-10))+
  xlab("Longitude")+ 
  ylab("Latitude")+
  ggtitle("Police killing locations in 2015")


blacks= pkdata[pkdata$raceethnicity=="Black",]
whites= pkdata[pkdata$raceethnicity=="White",]
hisLats= pkdata[pkdata$raceethnicity=="Hispanic/Latino",]

ggplot(hisLats, aes(age))+ 
  geom_histogram(color="black",fill="pink",binwidth=1)
ggplot(blacks, aes(age))+ 
  geom_histogram(color="black",fill="pink",binwidth=1)
ggplot(whites, aes(age))+ 
  geom_histogram(color="black",fill="pink",binwidth=1)

summary(blacks)
summary(whites)
summary(hisLats)

# Boxplots
ggplot(data=pkdata,aes(x=raceethnicity,y=age,  fill=raceethnicity ))+
  geom_boxplot(outlier.colour="Black",  outlier.size=1, notch=FALSE)+
  labs(x='Race/Ethnicity', y= 'Age')

ggplot(data=pkdata,aes(x=raceethnicity,y=h_income,  fill=raceethnicity ))+
  geom_boxplot(outlier.colour="Black",  outlier.size=1, notch=FALSE)+
  labs(x='Race/Ethnicity', y= 'Tract-level median household income')


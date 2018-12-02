#Unsupervised Machine Learning project to study US Police killings- Nadir Nibras

# clear workspace variables
rm(list = ls()); 
# clear window (same as ctrl+L. )
cat("\014")   
# close all plots
graphics.off() 

library(dplyr)
library(ggplot2)
library(plotly)
library(sf)
library(rworldmap)
library(readr)
library(datasets)
library(stats)
library(ggrepel)
library(textir) 
library(BBmisc)
library(rstudioapi)
library(gtable)
library(grid)
library(gridExtra)


# set directory to R script folder
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))


# load data ---------------------------------------------------------------
pkdata15<-read.csv("2015.csv")
pkdata16<-read.csv("2016.csv")

pkdata<- rbind(pkdata15,pkdata16)

pkdata$name=NULL
pkdata$year=NULL
pkdata$streetaddress= NULL
pkdata$uid= NULL
pkdata$age=as.numeric(levels(pkdata$age))[pkdata$age]

# Summarize data
summary(pkdata)

ggplot(pkdata, aes(day)) + geom_histogram(color="black",fill="orange",binwidth=1)

# Getting a map
world <- getMap(resolution = "low")
class(world)
world <- st_as_sf(world)
class(world)

locdata= subset(pkdata,longitude!="NA" &latitude!='NA')

ggplot(data = world)+
  geom_sf(fill="grey")+
  geom_jitter(data= locdata, aes(longitude,latitude,col= age), alpha=0.5)+
  coord_sf(xlim = c(min(locdata$longitude)+37, max(locdata$longitude)), 
           ylim = c(min(locdata$latitude)+6, max(locdata$latitude)-10))+
  xlab("Longitude")+ 
  ylab("Latitude")+
  ggtitle("Police killing locations 2015-16")

ggplot(data = world)+
  geom_sf(fill="grey")+
  geom_jitter(data= subset(locdata,armed=="No"), aes(longitude,latitude,color= classification),alpha=0.7)+
  coord_sf(xlim = c(min(locdata$longitude)+37, max(locdata$longitude)), 
           ylim = c(min(locdata$latitude)+6, max(locdata$latitude)-10))+
  xlab("Longitude")+ 
  ylab("Latitude")+
  ggtitle("Police killings of unarmed individuals in 2015-16")


#Police killing by race
ggplot(data = world)+
  geom_sf(fill="grey")+
  geom_jitter(data=locdata, aes(longitude,latitude,col= raceethnicity), size = 2.5,alpha=0.4)+
  coord_sf(xlim = c(min(locdata$longitude)+37, max(locdata$longitude)), 
           ylim = c(min(locdata$latitude)+6, max(locdata$latitude)-10))+
  xlab("Longitude")+ 
  ylab("Latitude")+
  ggtitle("Police killings by race/ethnicity in 2015-16")


# Just looking at black, white and latino/hispanic individuals
bwhdata= subset(locdata, raceethnicity=="Black"|raceethnicity=="White"|raceethnicity=="Hispanic/Latino")
ggplot(data = world)+
  geom_sf(fill="grey")+
  geom_jitter(data=bwhdata, aes(longitude,latitude,col= raceethnicity), size = 2.5,alpha=0.4)+
  coord_sf(xlim = c(min(locdata$longitude)+37, max(locdata$longitude)), 
           ylim = c(min(locdata$latitude)+6, max(locdata$latitude)-10))+
  xlab("Longitude")+ 
  ylab("Latitude")+
  ggtitle("Police killings by race/ethnicity in 2015-16")



# Separating into regions
west= pkdata[pkdata$state=="CA"|pkdata$state=="AZ"|pkdata$state=="NM"|pkdata$state=="CO"|
               pkdata$state=="UT"|pkdata$state=="NV"|pkdata$state=="WY"|pkdata$state=="MT"|
               pkdata$state=="ID"|pkdata$state=="OR"|pkdata$state=="WA"|pkdata$state=="AK"|pkdata$state=="HI",]
south= pkdata[pkdata$state=="TX"|pkdata$state=="OK"|pkdata$state=="AR"|pkdata$state=="LA"|pkdata$state=="MS"|
                pkdata$state=="AL"|pkdata$state=="TN"|pkdata$state=="FL"|pkdata$state=="GA"|pkdata$state=="NC"|
                pkdata$state=="SC"|pkdata$state=="VA"|pkdata$state=="KY"|pkdata$state=="WV",]
midwest= pkdata[pkdata$state=="KS"|pkdata$state=="MO"|pkdata$state=="IL"|pkdata$state=="IN"|pkdata$state=="OH"|
                  pkdata$state=="IA"|pkdata$state=="NE"|pkdata$state=="SD"|pkdata$state=="ND"|pkdata$state=="MN"|
                  pkdata$state=="WI"|pkdata$state=="MI",]
northeast= pkdata[pkdata$state=="NJ"|pkdata$state=="PA"|pkdata$state=="DE"|pkdata$state=="MD"|pkdata$state=="RI"|
                    pkdata$state=="CT"|pkdata$state=="NY"|pkdata$state=="MA"|pkdata$state=="NH"|pkdata$state=="ME"|
                    pkdata$state=="VT"|pkdata$state=="DC",]

northeast$region= "Northeast"
south$region= "South"
midwest$region= "Midwest"
west$region= "West"
pkdata= rbind(northeast,midwest,south,west)


blacks= pkdata[pkdata$raceethnicity=="Black",]
whites= pkdata[pkdata$raceethnicity=="White",]
hisLats= pkdata[pkdata$raceethnicity=="Hispanic/Latino",]


summary(midwest)
summary(west)
summary(south)
summmary(northeast)

# Overlapping histograms for age and race
ggplot(pkdata,aes(x=age)) + 
  geom_histogram(data=subset(pkdata,raceethnicity == 'Black'),fill = "red", alpha = 0.2,binwidth = 1) +
  geom_histogram(data=subset(pkdata,raceethnicity == 'White'),fill = "blue", alpha = 0.2,binwidth = 1) +
  geom_histogram(data=subset(pkdata,raceethnicity == 'Hispanic/Latino'),fill = "yellow", alpha = 0.3,binwidth = 1)

# Histograms breaking down race and age
p1=ggplot(hisLats, aes(age))+ 
  geom_histogram(color="black",fill="pink",binwidth=1, alpha=0.8)+
  ggtitle("Race/ethnicity: Hispanic/latino") + xlim(6, 87)
p2=ggplot(blacks, aes(age))+ 
  geom_histogram(color="black",fill="green",binwidth=1, alpha=0.3)+
  ggtitle("Race/ethnicity: Black") + xlim(6, 87)
p3=ggplot(whites, aes(age))+ 
  geom_histogram(color="black",fill="pink",binwidth=1, alpha=0.3)+
  ggtitle("Race/ethnicity: White") + xlim(6, 87)
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
g <- rbind(g1,g2, g3, size = "first")
g$widths <- unit.pmax(g1$widths,g2$widths, g3$widths)
grid.newpage()
grid.draw(g)

summary(blacks)
summary(whites)
summary(hisLats)

# Boxplots
ggplot(data=pkdata,aes(x=raceethnicity,y=age,  fill=raceethnicity ))+
  geom_boxplot(outlier.colour="Black",  outlier.size=1, notch=FALSE)+
  labs(x='Race/Ethnicity', y= 'Age')

# Age break down with armed %
ggplot(data=pkdata,aes(x=region,y=age,  fill=armed ))+
  geom_boxplot(outlier.colour="Black",  outlier.size=1, notch=FALSE)+
  labs(x='Race/Ethnicity', y= 'Age')

# Age break down with classification of death
ggplot(data=pkdata,aes(x=region,y=age,  fill=classification ))+
  geom_boxplot(outlier.colour="Black",  outlier.size=1, notch=FALSE)+
  labs(x='Race/Ethnicity', y= 'Age')

ggplot(data=pkdata,aes(x=region,y=h_income,  fill=raceethnicity ))+
  geom_boxplot(outlier.colour="Black",  outlier.size=1, notch=FALSE)+
  labs(x='Race/Ethnicity', y= 'Tract-level median household income')

ggplot(data=pkdata,aes(x=region,y=age,  fill=raceethnicity))+
  geom_boxplot(outlier.colour="Black",  outlier.size=1, notch=FALSE)+
  labs(x='Region', y= 'Age')

unarmedS =sum(south$armed=="No")/nrow(south)
unarmedNE =sum(northeast$armed=="No")/nrow(northeast)
unarmedMW =sum(midwest$armed=="No")/nrow(midwest)
unarmedW =sum(west$armed=="No")/nrow(west)

unarmedTX =sum(pkdata$armed=="No" & pkdata$state=="TX")/sum(pkdata$state=="TX")
unarmedCA =sum(pkdata$armed=="No" & pkdata$state=="CA")/sum(pkdata$state=="CA")
unarmedNY =sum(pkdata$armed=="No" & pkdata$state=="NY")/sum(pkdata$state=="NY")
unarmedFL =sum(pkdata$armed=="No" & pkdata$state=="FL")/sum(pkdata$state=="FL")

unarmedblackp =sum(pkdata$armed=="No" & pkdata$raceethnicity=="Black")/sum(pkdata$armed=="No" )
unarmedwhitep =sum(pkdata$armed=="No" & pkdata$raceethnicity=="White")/sum(pkdata$armed=="No" )
unarmedhispp =sum(pkdata$armed=="No" & pkdata$raceethnicity=="Hispanic/Latino")/sum(pkdata$armed=="No" )


shhotingS =sum(south$classification=="Gunshot")/nrow(south)
shootingNE =sum(northeast$classification=="Gunshot")/nrow(northeast)
shoowtingMW =sum(midwest$classification=="Gunshot")/nrow(midwest)
shootingW =sum(west$classification=="Gunshot")/nrow(west)

# analyzing shootings by crime
vcrime<- read.csv("Crime 2015.csv")
vcrime$Population <- as.numeric(gsub(",","",vcrime$Population))
vcrime$Violent.crime <- as.numeric(gsub(",","",vcrime$Violent.crime))

stateCount=data.frame(table(pkdata15$state))
colnames(stateCount) <- c("State", "Killings")

statedata= merge(stateCount, vcrime)

ggplot(statedata, aes(Population, Violent.crime, size= Violent.crime/Population))+
  geom_point(col= "orange3",alpha=0.5)+
  geom_text_repel(aes(label=State), size=4)+
  geom_abline(intercept=0, slope=sum(statedata$Violent.crime)/sum(statedata$Population))+
  ggtitle("Police killings by state- 2015-16")

ggplot(statedata, aes(Killings,Violent.crime ,size= Violent.crime/Population))+
  geom_point(alpha=0.3,color="red")+
  geom_text_repel(aes(label=State), size=4)+
  geom_abline(intercept=0, slope=sum(statedata$Violent.crime)/sum(statedata$Killings), size=1)+
  ggtitle("Police killings by state- 2015-16")


set.seed(417)
plot_ly(x=log10(statedata$Population), y=log10(statedata$Violent.crime), 
        z=log10(statedata$Killings), type="scatter3d",label=statedata$State,
        color= statedata$Population)%>%
  layout(scene = list(xaxis = list(title = 'log population'),
                    yaxis = list(title = 'log violent crime'),
                    zaxis = list(title = 'log killings')))


#Unsupervised Machine Learning project to study US Police killings- Nadir Nibras

# clear workspace variables
rm(list = ls()); 
# clear window (same as ctrl+L. )
cat("\014")   
# close all plots
graphics.off() 

library(tidyverse)
library(plotly)
library(sf)
library(maps)
library(rworldmap)
library(datasets)
library(stats)
library(ggrepel)
library(textir) 
library(BBmisc)
library(rstudioapi)
library(gtable)
library(grid)
library(gridExtra)
library(tools)
library(scales)

# set directory to R script folder
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))

# load data
pkdata15<-read.csv("2015.csv")
pkdata16<-read.csv("2016.csv")
pkdata15extra<-read.csv("US police 2015.csv")
statedata<- read.csv("Crime 2015.csv")

pkdata<- rbind(pkdata15,pkdata16)

pkdata[c("name", "year", "streetaddress", "uid","day","month")] <- list(NULL)
pkdata$age=as.numeric(levels(pkdata$age))[pkdata$age]

pkdata15extra[c("name", "year", "streetaddress", "uid","day","month",
                "state_fp","tract_ce","geo_id","county_id","namelsad",
                "county_fp")] <- list(NULL)


pkdata15extra$share_black=as.numeric(levels(pkdata15extra$share_black))[pkdata15extra$share_black]
pkdata15extra$share_white=as.numeric(levels(pkdata15extra$share_white))[pkdata15extra$share_white]
pkdata15extra$share_hispanic=as.numeric(levels(pkdata15extra$share_hispanic))[pkdata15extra$share_hispanic]
pkdata15extra$pov=as.numeric(levels(pkdata15extra$pov))[pkdata15extra$pov]
pkdata15extra$age=as.numeric(levels(pkdata15extra$age))[pkdata15extra$age]
pkdata15extra$p_income=as.numeric(levels(pkdata15extra$p_income))[pkdata15extra$p_income]



# Summarize data
summary(pkdata)
summary(pkdata15extra)

locdata= subset(pkdata,longitude!="NA" &latitude!='NA')

usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))
usa <- cbind(usa, st_coordinates(st_centroid(usa)))
usa$ID <- toTitleCase(usa$ID)

# Changing state names to abbreviations
usa$ID= statedata$State[c(1,3:11,13:51)]

# Police killings by age
ggplot(usa)+
  geom_sf(fill="grey")+
  geom_jitter(data= locdata, aes(longitude,latitude,col= age), alpha=0.5)+
  coord_sf(xlim = c(min(locdata$longitude)+37, max(locdata$longitude)), 
           ylim = c(min(locdata$latitude)+6, max(locdata$latitude)-20))+
  xlab("Longitude")+ 
  ylab("Latitude")+
  ggtitle("Police killing locations 2015-16")+
  geom_text_repel(data = usa, aes(X, Y, label = ID), size =3.5, col= "brown")

# Police killings of unarmed individuals
ggplot(usa)+
  geom_sf(fill="grey")+
  geom_jitter(data= subset(locdata,armed=="No"), aes(longitude,latitude,color= classification),size=3,alpha=0.7)+
  coord_sf(xlim = c(min(locdata$longitude)+37, max(locdata$longitude)), 
           ylim = c(min(locdata$latitude)+6, max(locdata$latitude)-20))+
  xlab("Longitude")+ 
  ylab("Latitude")+
  ggtitle("Police killings of unarmed individuals in 2015-16")+
  geom_text_repel(data = usa, aes(X, Y, label = ID), size =3.5, col= "brown")


#Police killing by race/ethnicity
ggplot(data = usa)+
  geom_sf(fill="grey")+
  geom_jitter(data=locdata, aes(longitude,latitude,col= raceethnicity), size = 2.5,alpha=0.4)+
  coord_sf(xlim = c(min(locdata$longitude)+37, max(locdata$longitude)), 
           ylim = c(min(locdata$latitude)+6, max(locdata$latitude)-20))+
  xlab("Longitude")+ 
  ylab("Latitude")+
  ggtitle("Police killings by race/ethnicity in 2015-16")+
  geom_text_repel(data = usa, aes(X, Y, label = ID), size =3.5, col= "brown")


#looking at black, white and latino/hispanic individuals
bwhdata= subset(locdata, raceethnicity=="Black"|raceethnicity=="White"|
                  raceethnicity=="Hispanic/Latino"|raceethnicity== "Native American")
ggplot(data = usa)+
  geom_sf(fill="grey")+
  geom_jitter(data=bwhdata, aes(longitude,latitude,col= raceethnicity), size = 2.5,alpha=0.5)+
  coord_sf(xlim = c(min(locdata$longitude)+37, max(locdata$longitude)), 
           ylim = c(min(locdata$latitude)+6, max(locdata$latitude)-20))+
  xlab("Longitude")+ 
  ylab("Latitude")+
  ggtitle("Police killings by race/ethnicity in 2015-16")+
  geom_text_repel(data = usa, aes(X, Y, label = ID), size =3.5, col= "brown")


# Adding region feature
pkdata=
  mutate(pkdata, region = 
           ifelse(state=="CA"|state=="AZ"|state=="NM"|state=="CO"|state=="ID"|state=="OR"|
                    state=="WA"|state=="AK"|state=="HI", "West",
                  ifelse(state=="TX"|state=="OK"|state=="AR"|state=="LA"|state=="MS"|state=="AL"|
                           state=="TN"|state=="FL"|state=="GA"|state=="NC"|state=="SC"|state=="VA"|
                           state=="KY"|state=="MD"|state=="WV"|state=="DC","South",
                         ifelse(state=="KS"|state=="MO"|state=="IL"|state=="IN"|state=="OH"
                                |state=="IA"|state=="NE"|state=="SD"|state=="ND"|state=="MN"|
                                  state=="WI"|state=="MI","Midwest", "Northeast"))))

pkdata= mutate(pkdata, agegroup = 
           ifelse(age<18, "under-18",
                  ifelse(age>17 & age<36,"18-35","over 35")))
  

summary(subset(pkdata,region=="Midwest"))
summary(subset(pkdata,region=="West"))
summary(subset(pkdata,region=="South"))
summary(subset(pkdata,region=="Northeast"))


blacks= pkdata[pkdata$raceethnicity=="Black",]
whites= pkdata[pkdata$raceethnicity=="White",]
hisLats= pkdata[pkdata$raceethnicity=="Hispanic/Latino",]
summary(blacks)
summary(whites)
summary(hisLats)


  # Overlapping histograms for age and race
ggplot(pkdata,aes(x=age)) + 
  geom_histogram(data=subset(pkdata,raceethnicity == 'Black'),fill = "red", alpha = 0.2,binwidth = 1) +
  geom_histogram(data=subset(pkdata,raceethnicity == 'White'),fill = "blue", alpha = 0.2,binwidth = 1) +
  geom_histogram(data=subset(pkdata,raceethnicity == 'Hispanic/Latino'),fill = "yellow", alpha = 0.3,binwidth = 1)

# individual Histograms breaking down race and age
p1=ggplot(hisLats, aes(age))+ 
  geom_histogram(color="black",fill="pink",binwidth=1, alpha=0.8)+
  ggtitle("Individuals killed by age- Race/ethnicity: Hispanic/latino") + xlim(6, 87)
p2=ggplot(blacks, aes(age))+ 
  geom_histogram(color="black",fill="green",binwidth=1, alpha=0.3)+
  ggtitle("Individuals killed by age- Race/ethnicity: Black") + xlim(6, 87)
p3=ggplot(whites, aes(age))+ 
  geom_histogram(color="black",fill="pink",binwidth=1, alpha=0.3)+
  ggtitle("Individuals killed by age- Race/ethnicity: White") + xlim(6, 87)
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
g <- rbind(g1,g2, g3, size = "first")
g$widths <- unit.pmax(g1$widths,g2$widths, g3$widths)
grid.newpage()
grid.draw(g)


# Boxplots
ggplot(data=pkdata,aes(x=raceethnicity,y=age,  fill=raceethnicity ))+
  geom_boxplot(outlier.colour="Black",  outlier.size=1, notch=FALSE)+
  labs(x='Race/Ethnicity', y= 'Age')+
  ggtitle("Age and 'armed' status of the deceased")


# Age break down with armed by region
ggplot(data=pkdata,aes(x=region,y=age,  fill=armed ))+
  geom_boxplot(outlier.colour="Black",  outlier.size=1, notch=FALSE)+
  labs(x='Race/Ethnicity', y= 'Age')+
  ggtitle("Age and 'armed' status of deceased by region")

# Age break down with classification/cause of death by region
ggplot(data=pkdata,aes(x=region,y=age,  fill=classification ))+
  geom_boxplot(outlier.colour="Black",  outlier.size=1, notch=FALSE)+
  labs(x='Race/Ethnicity', y= 'Age')+
  ggtitle("Age and cause of death")

# Age breakdown with race by region
ggplot(data=pkdata,aes(x=region,y=age,  fill=raceethnicity))+
  geom_boxplot(outlier.colour="Black",  outlier.size=1, notch=FALSE)+
  labs(x='Region', y= 'Age')

# household income by race/ethnicity
ggplot(data=pkdata15extra,aes(x=raceethnicity,y=h_income, fill= raceethnicity))+
  geom_boxplot(outlier.colour="Black",  outlier.size=1, notch=FALSE)+
  labs(x='Race/Ethnicity', y= 'Tract-level median household income')+
  ggtitle("Household median income at tract-level vs race/ethnicty")

# personal income by race/ethnicity
ggplot(data=pkdata15extra,aes(x=raceethnicity,y=p_income, fill= raceethnicity))+
  geom_boxplot(outlier.colour="Black",  outlier.size=1, notch=FALSE)+
  labs(x='Race/Ethnicity', y= 'Tract-level median personal income')+
  ggtitle("Personal median income at tract-level vs race/ethnicty")

# county income by race/ethnicity
ggplot(data=pkdata15extra,aes(x=raceethnicity,y=county_income, fill= raceethnicity))+
  geom_boxplot(outlier.colour="Black",  outlier.size=1, notch=FALSE)+
  labs(x='Race/Ethnicity', y= 'Tract-level median county income')+
  ggtitle("Country median income at tract-level vs race/ethnicty")


# Share of pop that is non-Hispanic white
ggplot(data=pkdata15extra,aes(x=raceethnicity,y=share_white, fill= raceethnicity))+
  geom_boxplot(outlier.colour="Black",  outlier.size=1, notch=FALSE)+
  labs(x='Race/Ethnicity', y= 'Share of pop that is non-Hispanic white')+
  ggtitle("Share of pop in tract that is non-Hispanic white vs Race/Ethnicity of deceased")

# Share of pop that is black
ggplot(data=pkdata15extra,aes(x=raceethnicity,y=share_black, fill= raceethnicity))+
  geom_boxplot(outlier.colour="Black",  outlier.size=1, notch=FALSE)+
  labs(x='Race/Ethnicity', y= 'Share of pop that is black only')+
  ggtitle("Share of pop in tract that is black vs Race/Ethnicity of deceased")

# Share of pop that is hispanic
ggplot(data=pkdata15extra,aes(x=raceethnicity,y=share_hispanic, fill= raceethnicity))+
  geom_boxplot(outlier.colour="Black",  outlier.size=1, notch=FALSE)+
  labs(x='Race/Ethnicity', y= 'Share of pop that is hispanic')+
  ggtitle("Share of pop in tract that is hispanic vs Race/Ethnicity of deceased")

# Poverty rate in tract
ggplot(data=pkdata15extra,aes(x=raceethnicity,y=pov, fill= raceethnicity))+
  geom_boxplot(outlier.colour="Black",  outlier.size=1, notch=FALSE)+
  labs(x='Race/Ethnicity', y= 'Poverty rate in tract')+
  ggtitle("Poverty at tract-level vs race/ethnicty")


# unemployment rate in tract
ggplot(data=pkdata15extra,aes(x=urate,y=pov, fill= raceethnicity))+
  geom_boxplot(outlier.colour="Black",  outlier.size=1, notch=FALSE)+
  labs(x='Race/Ethnicity', y= 'Unemployment rate in tract')

# Share of 25+ pop with BA or higher
ggplot(data=pkdata15extra,aes(x=college,y=pov, fill= raceethnicity))+
  geom_boxplot(outlier.colour="Black",  outlier.size=1, notch=FALSE)+
  labs(x='Race/Ethnicity', y= 'Share of 25+ pop with BA or higher')+
  ggtitle("Education of tract vs race/ethnicity")

# stacked bar plots and histograms

knownraced=subset(pkdata, raceethnicity!="Other"&raceethnicity!="Unknown")

# overall deaths by race
ggplot(knownraced,aes(raceethnicity)) + 
  geom_bar(fill="pink") +
  ggtitle("Killings vs race/ethnicity of deceased")

# Armed/how by race/ethnicity
ggplot(knownraced,aes(raceethnicity)) + 
  geom_bar(aes(fill = armed), position = "fill")+
  ggtitle("'Armed' status breakdown by race")


# Cause of death by race/ethnicity
ggplot(knownraced,aes(raceethnicity)) + 
  geom_bar(aes(fill = classification), position = "fill")+
  ggtitle("Cause of death by race/ethnicity")


# Cause of death by region
ggplot(knownraced,aes(region)) + 
  geom_bar(aes(fill = classification), position = "fill")+
  ggtitle("Cause of death by region")

# Armed status by region
ggplot(knownraced,aes(region)) + 
  geom_bar(aes(fill = armed), position = "fill")+
  ggtitle("'armed' status of deceased by region")


unarmedS =sum(pkdata$armed=="No" & pkdata$region=="South")/sum(pkdata$region=="South")
unarmedNE =sum(pkdata$armed=="No" & pkdata$region=="Northeast")/sum(pkdata$region=="Northeast")
unarmedMW =sum(pkdata$armed=="No" & pkdata$region=="Midwest")/sum(pkdata$region=="Midwest")
unarmedW =sum(pkdata$armed=="No" & pkdata$region=="West")/sum(pkdata$region=="West")

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

# states
statedata$Population <- as.numeric(gsub(",","",statedata$Population))
statedata$Violent.crime <- as.numeric(gsub(",","",statedata$Violent.crime))
stateCount=data.frame(table(pkdata15$state))
colnames(stateCount) <- c("State", "Killings")

statedata= merge(stateCount, statedata)

ggplot(statedata, aes(Population, Violent.crime, size= Violent.crime/Population))+
  geom_point(col= "orange3",alpha=0.5)+
  geom_text_repel(aes(label=State), size=4)+
  geom_abline(intercept=0, slope=sum(statedata$Violent.crime)/sum(statedata$Population))+
  ggtitle("Police killings by state- 2015")

ggplot(statedata, aes(Killings,Violent.crime ,size= Violent.crime/Population))+
  geom_point(alpha=0.3,color="red")+
  geom_text_repel(aes(label=State), size=4)+
  geom_abline(intercept=0, slope=sum(statedata$Violent.crime)/sum(statedata$Killings), size=1)+
  ylab("Incidences of violent crime")+
  ggtitle("Police killings by state- 2015")


set.seed(417)
plot_ly(x=log10(statedata$Population), y=log10(statedata$Violent.crime), 
        z=log10(statedata$Killings), type="scatter3d",label=statedata$State,
        color= statedata$Population)%>%
  layout(scene = list(xaxis = list(title = 'log population'),
                      yaxis = list(title = 'log violent crime'),
                      zaxis = list(title = 'log killings')))


# Cities
cities= data.frame(table(pkdata$city))
colnames(cities) <- c("city", "Killings")
cities= subset(cities, Killings>6)
cityData= read.csv("city data.csv")
cityData= subset(cityData, population>100000)
cities= merge(cities,cityData)
cities=cities[-c(11,18),c(1:9)]


ggplot(cities, aes(Killings,population_proper ,size= density))+
  geom_point(alpha=0.3, col="red3")+
  geom_text_repel(aes(label=city), size=3.8)+
  geom_abline(intercept=0, slope=sum(cities$population_proper)/sum(cities$Killings), size=1)+
  ggtitle("Police killings by city- 2015")


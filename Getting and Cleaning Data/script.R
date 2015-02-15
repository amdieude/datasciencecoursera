rm(list=ls())
setwd("C:\\DATA SCIENCE SPECIALIZATION\\Getting and Cleaning Data")
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl, destfile = "ss06hid.csv")
ss06hid<-read.csv("ss06hid.csv", header=TRUE, sep=",")


ss06hid$agricultureLogical[ss06hid$ACR==3 & ss06hid$AGS==6]<-1
ss06hid$agricultureLogical<-as.logical(agricultureLogical)
which(ss06hid$agricultureLogical)






list.files()
table(ss06hid$VAL)
View(head(ss06hid$FES,20))
table(ss06hid$FES)
View(ss06hid$FES)
levels(ss06hid$FES)

DATA.gov<-read.csv("getdata-data-DATA.gov_NGAP.csv", sep=";")

View(DATA.gov[18:23,7:15])
dat<-DATA.gov[18:23,7:15]
names(dat)<-c("Zip","CuCurrent","PaCurrent","PoCurrent","Contact","Ext","Fax","email","Status")
dat$Zip<-as.numeric(dat$Zip)
dat$Ext<-as.numeric(dat$Ext)
str(dat)
sum(dat$Zip*dat$Ext,na.rm=T) 

library(XML)
fileUrl <-"http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
dataxml<-xmlTreeParse(fileUrl, useInternal=TRUE)
rootNode<-xmlRoot(dataxml)
xmlName(rootNode)


library(XML)
fileUrl <- "http://www.w3schools.com/xml/simple.xml"
doc <- xmlTreeParse(fileUrl,useInternal=TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)
names(rootNode)

install.packages("data.table")
library(data.table)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(fileUrl, destfile = "ss06pid.csv")
DT<-fread("ss06pid.csv", header=TRUE, sep=",")
DT<-as.data.frame(DAT)


install.packages("data.table")
library(data.table)
plot(xlab=12, main)




library(lattice)
state<-data.frame(state.x77, region=state.region)
class(xyplot(Life.Exp ~ Income | region, data = state, layout = c(4, 1)))

library(nlme)
library(lattice)
xyplot(weight ~ Time | Diet, BodyWeight)


library(lattice)
library(datasets)
data(airquality)
p <- xyplot(Ozone ~ Wind | factor(Month), data = airquality)

p

class(p)


library(datasets)
data(airquality)

install.packages("ggplot2")
library(ggplot2)

airquality = transform(airquality, Month = factor(Month))
qplot(Wind, Ozone, data = airquality, facets = . ~ Month)


library(ggplot2)
g <- ggplot(movies, aes(votes, rating))
print(g)

qplot(votes, rating, data = movies)
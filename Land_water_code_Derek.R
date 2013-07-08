rm(list = ls())

#examine regional level residuals, plot them on a map and make sure they look like noise

setwd("C:/Users/Derek/Dropbox/Postdoc/Land water project")

options(stringsAsFactors=F)

#data<-read.delim(file="MRB3_Loads_Annual.txt", header = TRUE, sep = "\t", quote="\"", dec=".",
#          fill = TRUE)


#Connect to remote Postgres database and download nutrient data
library(RPostgreSQL)
dbname <-"db_landwater"
user <- "dgray"
password <- "mk3m3asandwich"
host <- "isis.nceas.ucsb.edu"
# make the connection
con <- dbConnect(dbDriver("PostgreSQL"), user=user,
                 password=password, dbname=dbname, host=host)
#read the mrb3_loads_annual data into R
data<-dbReadTable(con, "mrb3_loads_annual")

library(doBy)

#Unique stations
stations<-unique(data$station_id)

#Read in metadata file
metadata<-read.delim(file="mrb3_stations.txt", header = TRUE, sep = "\t", quote="\"", dec=".",
                     fill = TRUE)

#summarize stations by number time span of observations
date.range<-summaryBy(wyear~station_id,data=data,FUN=c(length))
#date.range$no.years<-date.range$wyear.max-date.range$wyear.min

date.range<-summaryBy(wyear~station_id,data=data,FUN=c(min, max))


#specify plot for means here, nitrogen or phosophorus
analysis.type<-"nitrogen"


#summarize by nutrients
#should we correct by watershed size?
#Decided to convert to concentration mg/L yr
#The conversion factor is 3.1746*10^-5
if(analysis.type=="phosphorus"){data$Pconcentration<-(data$load_tp_kg/data$meanq_m3s)*3.174603e-05}
if(analysis.type=="nitrogen"){data$Pconcentration<-(data$load_tn_kg/data$meanq_m3s)*3.174603e-05}
Phosphorus0<-summaryBy(Pconcentration~station_id, data=data,FUN=mean)
Latandlong<-data.frame("station_id"=metadata$station_id, "lat"=metadata$lat,"lon"=metadata$lon)
Pwithcoords<-merge(Latandlong,Phosphorus0,by="station_id")
Pwithcoords.filtered<-Pwithcoords[which(Pwithcoords$Pconcentration.mean<9999),]

#length(Pwithcoords[which(Pwithcoords$Pconcentration.mean>=9999),1])

#filter out high P ("wastewater") sites 
#Phosphorus<-Phosphorus[which(Phosphorus0[,2]<5)]
Phosphorus<-Pwithcoords.filtered

#histogram of the mean over all years
hist(Phosphorus$Pconcentration.mean, xlim=c(0,4), breaks=20)

#plot a histogram of the number of years covered by nutrient data
#hist(date.range$no.years, xlab="Number of Years", main="Number of years of nutrient data")


#plot a map of nutrient stations
library(mapproj)
library(plotrix)

#map colour scale
mapcols<-color.scale(log10(Phosphorus[,4]),c(0,1,1),c(1,1,0),0)

#plotchar<-rep(1, length(sdgroup))
#par(mar=c(2,2,2,5))
#map <- openmap(c(50,-75), c(-30,-50),type='bing')

datapts<-mapproject(Phosphorus$lon,Phosphorus$lat,projection="rectangular", parameters=100, orientation=NULL)
map("state", region=c("new york","north dakota","south dakota","north carolina","minnesota","iowa","missouri","wisconsin","kentucky","tennessee","pennsylvania","indiana","illinois","michigan","ohio","west virginia"),proj="rectangular",parameters=100) 
#map_longlat <- openproj(map, projection = "+proj=longlat")

#pdf(paste("hotspots_insitu",nneigh,"neighbours",".pdf",sep="_"), width=10,height=5)
#plot(map_longlat,raster=TRUE)
points(datapts, col=mapcols, pch=16, cex=0.75)
n<-sort(log10(as.numeric(Phosphorus[,4])))
lgnd<-round(seq(min(n),max(n),length.out=7),2)
color.legend(-70,35,-65,50,legend=lgnd,rect.col=color.scale(sort(na.exclude(as.numeric(log10(Phosphorus[,4])))),c(0,1,1),c(1,1,0),c(0,0,0)),gradient="y")
if (analysis.type=="phosphorus"){title("Phosphorus Loads")}
if (analysis.type=="nitrogen"){title("Nitrogen Loads")}

map.axes()


#Trend analyses: Mid 1980s
#breakpoints (longer time period) vs linear regression
#spending data goes back to early 1990s, 1990-2010
#summary stats from each site, slope, P-value, R squared, intercept, number of years

#Figure out number of unique stations
stat<-unique(data$station_id)

#convert loads to concentrations
data$Pconc<-(data$load_tp_kg/data$meanq_m3s)*3.174603e-05
data$Nconc<-(data$load_tn_kg/data$meanq_m3s)*3.174603e-05

#################################################
####1 for phosphorus concentrations##############
#################################################


#Create lists that will be filled when loop below is running
slope<-list(); int<-list(); p<-list(); rsquare<-list()

#start a text progress bar that will display progress of loop
pb<-txtProgressBar(min = 0, max = 100, initial = 0, char = "=",style = 2)

#start loop, loop over all stations (length of stat)
for (i in 1:length(stat)){
  #try function allows for failures without stopping loop (e.g. if all values for a station are NAs)
  #Do the linear regression (lm function)
  try(res<-lm(data$Pconc[which(data$station_id==stat[i])]~data$wyear[which(data$station_id==stat[i])],na.action=na.exclude))
  
  #Fill our list with NA values in case the code below doesn't work
  slope[[i]]<-NA; int[[i]]<-NA; p[[i]]<-NA; rsquare[[i]]<-NA
  
  #check if res object exists (did the linear regression work). If so, save the results to our lists
  if(exists("res")){
    try(slope[[i]]<-coefficients(res)[2])
    try(int[[i]]<-coefficients(res)[1])
    try(p[[i]]<-summary(res)$coefficients[2,4])
    try(rsquare[[i]]<-summary(res)$adj.r.squared)
    rm(res)} 
  
  #update our progress bar during each loop
  Sys.sleep(0.5); setTxtProgressBar(pb, (i/length(stat)*100))
}
#close the progress bar
Sys.sleep(1)
close(pb)

#Create a dataframe with station id, lat and lon, and regression results
stat.locs<-merge(data.frame("station_id"=stat), Latandlong, by="station_id")
reg.results<-data.frame("station_id"=stat, "slope"=unlist(slope), "p"=unlist(p), "rsquare"=unlist(rsquare),"intercept"=unlist(int))
reg.results.withlocs<-merge(stat.locs,reg.results,by="station_id")
hist(reg.results.withlocs$slope, breaks=20)

#Take results between -0.1 and 0.1
filtered.reg.results<-reg.results.withlocs[which(reg.results.withlocs$slope<0.04 & reg.results.withlocs$slope>-0.04),]
hist(filtered.reg.results$slope)

library(doBy)
filtered.reg.results.ordered <- orderBy(~slope, data=filtered.reg.results)


#assign filtered data to new object that will be used for plotting
plotdata<-filtered.reg.results.ordered$slope
negdata<-plotdata[plotdata<=0]
posdata<-plotdata[plotdata>0]

#load required packages
library(mapproj)
library(plotrix)

#####map colour scale
#for positive slopes
mapcols1<-color.scale(posdata,c(0,1,1),c(1,1,0),0)
#for negative slopes
mapcols2<-color.scale(negdata,extremes=c("darkblue","lightblue"))
#put the list of colours together so that all points on the plot will be in one list
mapcols<-append(mapcols1,mapcols2)

#Choose plot characters for map
plotchar<-rep(1, length(plotdata)) #first make a list of 1s (closed circles) for all points
plotchar[which(filtered.reg.results$p>0.051)]<-16 #change plot character to open symbol (16) if p value is greater than 0.051

#create a pdf starting here
pdf("trends in phosphorus concentrations.pdf")

par(mar=c(2,2,2,5))
#map <- openmap(c(50,-75), c(-30,-50),type='bing')

datapts<-mapproject(filtered.reg.results$lon,filtered.reg.results$lat,projection="rectangular", parameters=100, orientation=NULL)
map("state", region=c("new york","north dakota","south dakota","north carolina","minnesota","iowa","missouri","wisconsin","kentucky","tennessee","pennsylvania","indiana","illinois","michigan","ohio","west virginia"),proj="rectangular",parameters=100) 
#map_longlat <- openproj(map, projection = "+proj=longlat")

#pdf(paste("hotspots_insitu",nneigh,"neighbours",".pdf",sep="_"), width=10,height=5)
#plot(map_longlat,raster=TRUE)
points(datapts, col=mapcols, pch=plotchar, cex=0.75)

#Create the two halves of the legend, positive slopes and negative slopes
lgnd1<-round(seq(min(posdata),max(posdata),length.out=4),2)
lgnd2<-round(seq(min(negdata),max(negdata),length.out=4),2)

#create a colour legend corresponding to the points
color.legend(-67,40,-65,50,legend=lgnd1,rect.col=color.scale(sort(na.exclude(as.numeric(posdata))),c(0,1,1),c(1,1,0),0),gradient="y")
color.legend(-67,30,-65,40,legend=lgnd2,rect.col=color.scale(negdata,extremes=c("darkblue","lightblue")),gradient="y")
#color.legend(200, -90, 220, 0,rect.col=sort(b),gradient="y")


title("Trends in phosphorus concentrations")
#title("Nitrogen Loads")

map.axes()
dev.off()  #output the figure to a pdf


#################################################
####2 for nitrgen concentrations##############
#################################################

#Create lists that will be filled when loop below is running
slope<-list(); int<-list(); p<-list(); rsquare<-list()

#start a text progress bar that will display progress of loop
pb<-txtProgressBar(min = 0, max = 100, initial = 0, char = "=",style = 2)

#start loop, loop over all stations (length of stat)
for (i in 1:length(stat)){
  #try function allows for failures without stopping loop (e.g. if all values for a station are NAs)
  #Do the linear regression (lm function)
  try(res<-lm(data$Nconc[which(data$station_id==stat[i])]~data$wyear[which(data$station_id==stat[i])],na.action=na.exclude))
  
  #Fill our list with NA values in case the code below doesn't work
  slope[[i]]<-NA; int[[i]]<-NA; p[[i]]<-NA; rsquare[[i]]<-NA
  
  #check if res object exists (did the linear regression work). If so, save the results to our lists
  if(exists("res")){
    try(slope[[i]]<-coefficients(res)[2])
    try(int[[i]]<-coefficients(res)[1])
    try(p[[i]]<-summary(res)$coefficients[2,4])
    try(rsquare[[i]]<-summary(res)$adj.r.squared)
    rm(res)} 
  
  #update our progress bar during each loop
  Sys.sleep(0.5); setTxtProgressBar(pb, (i/length(stat)*100))
}
#close the progress bar
Sys.sleep(1)
close(pb)

#Create a dataframe with station id, lat and lon, and regression results
stat.locs<-merge(data.frame("station_id"=stat), Latandlong, by="station_id")
reg.results<-data.frame("station_id"=stat, "slope"=unlist(slope), "p"=unlist(p), "rsquare"=unlist(rsquare),"intercept"=unlist(int))
reg.results.withlocs<-merge(stat.locs,reg.results,by="station_id")
hist(reg.results.withlocs$slope, breaks=20)

#Take results between -0.06 and 0.06
filtered.reg.results<-reg.results.withlocs[which(reg.results.withlocs$slope<0.06 & reg.results.withlocs$slope>-0.06),]
hist(filtered.reg.results$slope)

library(doBy)
filtered.reg.results.ordered <- orderBy(~slope, data=filtered.reg.results)


#assign filtered data to new object that will be used for plotting
plotdata<-filtered.reg.results.ordered$slope
negdata<-plotdata[plotdata<=0]
posdata<-plotdata[plotdata>0]

#load required packages
library(mapproj)
library(plotrix)

#map colour scale
mapcols1<-color.scale(posdata,c(0,1,1),c(1,1,0),0)
mapcols2<-color.scale(negdata,extremes=c("darkblue","lightblue"))
mapcols<-append(mapcols1,mapcols2)
plotchar<-rep(1, length(plotdata))
plotchar[which(filtered.reg.results$p>0.051)]<-16

pdf("trends in nitrogen concentrations.pdf")

par(mar=c(2,2,2,5))
#map <- openmap(c(50,-75), c(-30,-50),type='bing')

datapts<-mapproject(filtered.reg.results$lon,filtered.reg.results$lat,projection="rectangular", parameters=100, orientation=NULL)
map("state", region=c("new york","north dakota","south dakota","north carolina","minnesota","iowa","missouri","wisconsin","kentucky","tennessee","pennsylvania","indiana","illinois","michigan","ohio","west virginia"),proj="rectangular",parameters=100) 
#map_longlat <- openproj(map, projection = "+proj=longlat")

#pdf(paste("hotspots_insitu",nneigh,"neighbours",".pdf",sep="_"), width=10,height=5)
#plot(map_longlat,raster=TRUE)
points(datapts, col=mapcols, pch=plotchar, cex=0.75)

lgnd1<-round(seq(min(posdata),max(posdata),length.out=4),2)
lgnd2<-round(seq(min(negdata),max(negdata),length.out=4),2)

color.legend(-67,40,-65,50,legend=lgnd1,rect.col=color.scale(sort(na.exclude(as.numeric(posdata))),c(0,1,1),c(1,1,0),0),gradient="y")
color.legend(-67,30,-65,40,legend=lgnd2,rect.col=color.scale(negdata,extremes=c("darkblue","lightblue")),gradient="y")
#color.legend(200, -90, 220, 0,rect.col=sort(b),gradient="y")


title("Trends in nitrogen concentrations")
#title("Nitrogen Loads")

map.axes()
dev.off()
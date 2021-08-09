rm(list=ls())
library(maptools)
library(maps)
library(rgdal)
library(sp)
library(rgeos)
library(ggmap)
library(maptools)
library(maps)
library(ggplot2)
library(raster)


## LOAD PACK DATA ----
setwd("C:/Personal_Cloud/OneDrive/Work/Book/SocialCarnivores/SocialCarnivores/")
pack <- read.csv("packSizeFernandez.csv")
packAimee <- read.csv("Pack_Size.csv")

## LOAD IUCN WOLF DISTRIBUTION ----
setwd("C:/Personal_Cloud/OneDrive/Work/Book/SocialCarnivores/SocialCarnivores/WolfDistributionIUCN")
ExtantTokeep <- c("Extant (non-breeding)","Extant (resident)",
                  "Possibly Extant (resident)", "Extant & Origin Uncertain (resident)",
                  "Extant & Reintroduced (resident)")

Maps <- readOGR(paste("data_0" ,".shp", sep=""))## Map of Scandinavia (including Finland & parts of Russia)

Maps <- gSimplify(Maps[Maps$LEGEND %in% ExtantTokeep ,], tol = 0.001, topologyPreserve = T)
Maps$ID <- c(1:length(Maps))
Maps <- aggregate(Maps, by=list("ID"=c(1:length(Maps))), FUN=mean)
plot(Maps)

## LOAD Human Footprint, 2018 Release (2009) ----
setwd("C:/Personal_Cloud/OneDrive/Work/Book/SocialCarnivores/SocialCarnivores/Human_Footprint")

footprint <- raster("wildareas-v3-2009-human-footprint.tif")## Map of Scandinavia (including Finland & parts of Russia)
footprint[footprint[]==128] <- NA

CRS(proj4string(footprint))

# reproject maps
footprint_projected <- projectRaster(footprint, crs = CRS(proj4string(Maps)))


## GET SHAPEFILE OF THE STUDY AREAS  ----
setwd("C:/Personal_Cloud/OneDrive/Work/Book/SocialCarnivores/SocialCarnivores/")

StudyAreas <- readOGR(paste("wolf_study_areas" ,".shp", sep=""))## Map of Scandinavia (including Finland & parts of Russia)
StudyAreasProj <- spTransform(StudyAreas,CRSobj = CRS(proj4string(footprint)))

plot(footprint)
plot(StudyAreasProj, add=T)

#
StudyAreasProjProj <- spTransform(StudyAreas,CRSobj = CRS(proj4string(footprint_projected)))

plot(footprint_projected)
centroids <- getSpPPolygonsLabptSlots(StudyAreasProjProj)
points(centroids,col="red",pch=3)



## MAKE A MAP WITH HUMAN FOOTPRING, WOLF DISTRIBUTION AND DISTRIBUTION OF STUDIES  ----
MAX <- max(footprint_projected[], na.rm = T)
cuts <- seq(0, MAX,length.out = 100)   #set breaks
colfunc <- colorRampPalette(c("white", "slateblue", "yellow", "orange", "red", "red"))
col <- colfunc(100)


##########################
pdf(file="Figure2.pdf", width= 8, height=4)
par(mar=c(0,0,0,0))
plot(Maps,  col="NA",
     border=NA)#,ylim=c(1170000, 8750338), xlim=c(-10000000,10000000))

plot(footprint_projected,legend=F,axes=F,boxes=F,add=T,breaks=c(cuts, max(cuts)+1000), col = col)
plot(Maps,  col=adjustcolor("darkgreen", alpha.f = 0.2),add=T, 
     border = "darkgreen", lwd=0.5)  

#points( -110.49762369597458, 44.55149918381928, bg=adjustcolor("red",alpha.f = 0.4), col="red", lwd=1, pch=24, cex=2)
#points(13.601729203074163, 60.079157132222726, bg=adjustcolor("black",alpha.f = 0.5),col="black", lwd=1, pch=22, cex=2)
points(centroids, add=T,col="black",pch="+")


plot(footprint_projected, legend.only=TRUE,breaks=cuts, col=col,
     legend.width = 1,
     axis.args=list(at=c(0,50),#round(seq(0, MAX, length.out = 5),digits = 1),
                    labels=c("Low","High"),#,round(seq(0, MAX, length.out = 5),digits = 1),
                    cex.axis=0.6),
     smallplot=c(0.81, 0.84, 0.73, 0.82),
     
     legend.args=list(text='Human \n footprint', side=4, font=2, line=2.5, cex=0.7))

polygon(x=c(133,122,122,133) 
          ,
        y=c(95,95,100,100),  col=adjustcolor("darkgreen", alpha.f = 0.2),
        border = "darkgreen", lwd=1)

text(158,97,"Wolf distribution", cex=0.7,font=2)

dev.off()






## EXTRACT HUMAN FOOT PRINT FOR EACH STUDY  ----
ext <- extract(footprint, StudyAreasProj)
StudyAreasProj$HumanFootPrint <- unlist(lapply(ext,function(x) mean(x,na.rm=T)))

packAimee$HumanFootPrint <- 0 
ids <- unique(packAimee$ArcMap_ID)

for(i in 1:nrow(packAimee)){
  if(!is.na(packAimee$ArcMap_ID[i])){
  packAimee$HumanFootPrint[i] <-   StudyAreasProj$HumanFootPrint[which(StudyAreasProj$Id%in% packAimee$ArcMap_ID[i])]
  }
}


## MAKE A FIGURE SHOWING PACK SIZE ~ HUMAN FOOTPRINT|CONTINENT ----
packAimee$Mean.Pack.Size <- as.numeric(packAimee$Mean.Pack.Size)


modNull <- lm(packAimee$Mean.Pack.Size~1)
summary(modNull)
AIC(modNull)

modAdd <- lm(packAimee$Mean.Pack.Size~packAimee$HumanFootPrint+packAimee$Continentfact)
summary(modAdd)
AIC(modAdd)


modCont <- lm(packAimee$Mean.Pack.Size~packAimee$Continentfact)
summary(modCont)
AIC(modCont)

modHum <- lm(packAimee$Mean.Pack.Size~packAimee$HumanFootPrint)
summary(modHum)
AIC(modHum)

modInt <- lm(packAimee$Mean.Pack.Size~packAimee$HumanFootPrint*packAimee$Continentfact)
summary(modInt)
AIC(modInt)



library(AICcmodavg)


AICTab <- aictab(list(modInt,modAdd,modNull,modHum,modCont),modnames =c ("Cont*Hum","Cont+Hum","Null","Hum","Cont") )
write.csv(AICTab,file="AICTab.csv")
write.csv(summary(modHum)$coefficients,file="modHum.csv")



pdf(file="PackSizeHumanFootprint.pdf", width= 8, height=7)

plot(packAimee$Mean.Pack.Size ~ packAimee$HumanFootPrint, xlab="Human footprint ",
     ylab="Average pack size", pch=21,col="white",
     bg=adjustcolor(ifelse(as.numeric(packAimee$Continentfact)==1,"white","white"),alpha.f = 0.3))
abline(modHum)
points(packAimee$Mean.Pack.Size ~ packAimee$HumanFootPrint, pch=21,col=packAimee$Continentfact,
       bg=adjustcolor(ifelse(as.numeric(packAimee$Continentfact)==1,"black","red"),alpha.f = 0.3))
legend("topright", pch=c(16,16), col=c("red","black"), border=adjustcolor(c("black","red"),alpha.f = 0.3),
legend = c("North America","Eurasia"))

dev.off()




####
wolfdist <- raster("C:/Users/cymi/Dropbox (Old)/Dropbox (AQEG)/AQEG Team Folder/RovQuant/wolf/WolfRuns20122020/Figures/9.F1218CachedPPfunct2012_2020UpdateTracks/RasterForRovbase/UD5kmRaster100km22020_2021.tif")## Map of Scandinavia (including Finland & parts of Russia)
plot(wolfdist)
library(adehabitatHR)

contour <- rasterToContour(wolfdist,maxpixels = 10000000)
plot(contour)
wolfdistSpx <- as(wolfdist, "SpatialPixelsDataFrame")
estUDm2spixdf(wolfdistSpx)
getverticeshr(wolfdistSpx)
library(ggmap)
library(rgdal)


map <- get_googlemap("Montpellier, France", zoom = 8, maptype = "terrain")

# Plot it
ggmap(map) + 
  theme_void() + 
  ggtitle("terrain") + 
  theme(
    plot.title = element_text(colour = "orange"), 
    panel.border = element_rect(colour = "grey", fill=NA, size=2)
  )

library(OpenStreetMap)
# get world map
map <- openmap(c(70,-179), c(-70,179))
plot(map)

bingmap <- openmap(c(70,-179), c(-70,179), type = "bing")
plot(bingmap)






head(packAimee)
nrow(packAimee)

mean(packAimee$Mean.Pack.Size)
sd(packAimee$Mean.Pack.Size)

packAimee$Continentfact <- as.factor(packAimee$Continent)
plot(packAimee$Mean.Pack.Size ~ packAimee$Continentfact, xlab="Continent", ylab="Average pack size", xaxt="n")


axis(1, at=c(1,2), labels = c(paste("Eurasian (n=", table(packAimee$Continent)[1],")", sep=""),
                              paste("North America (n=", table(packAimee$Continent)[2],")", sep="")))





plot(packAimee$Mean.Pack.Size ~ packAimee$HumanFootPrint, xlab="Human Impact Index", ylab="Average pack size")





packAimee[packAimee$Continent%in% "North America",]
packAimee["19",]




## simply season 
packAimee$Season1 <- substr(packAimee$Season, 1, 6)
## exclude year round
packAimeeSeason <- packAimee[!packAimee$Season %in% "year round",]
packAimeeSeason <- packAimeeSeason[!packAimeeSeason$Season %in% "fall",]

##MAKE LATE WINTER: WINTER
packAimeeSeason$Season1[packAimeeSeason$Season1 %in% "late w" ] <- "winter"
packAimeeSeason$Season1[packAimeeSeason$Season1 %in% "late s" ] <- "summer"

packAimeeSeason$Seasonfact <- as.factor(packAimeeSeason$Season1)

plot(packAimeeSeason$Mean.Pack.Size ~ packAimeeSeason$Seasonfact, xlab="Continent", ylab="Average packAimee size", xaxt="n")


axis(1, at=c(1,2), labels = c(paste("Summer (n=", table(packAimee$Continent)[1],")", sep=""),
                              paste("Winter", table(packAimee$Continent)[2],")", sep="")))


##INTERACTIONS
packAimeeSeason$SeasonContinentfact <- as.factor(paste(packAimeeSeason$Season1,packAimeeSeason$Continent))

par(mar=c(7,5,5,5))
plot(packAimeeSeason$packAimee.size ~ packAimeeSeason$SeasonContinentfact,xlab="",
     ylab="Average pack size", xaxt="n")

axis(1, at=c(1:4), labels = c(paste("Eurasia (n=", table(packSeason$SeasonContinentfact)[1],")", sep=""),
                              paste("North America (n=", table(packSeason$SeasonContinentfact)[2],")", sep=""),
                              paste("Eurasia (n=", table(packSeason$SeasonContinentfact)[3],")", sep=""),
                              paste("North America (n=", table(packSeason$SeasonContinentfact)[4],")", sep="")),cex.axis=0.7)


axis(1, at=c(1.5,3.5), labels = c(paste("Summer (n=", table(packSeason$Seasonfact)[1],")", sep=""),
                                  paste("Winter (n=", table(packSeason$Seasonfact)[2],")", sep="")),
     cex.axis=1, col="white", line = +2)


##
mod1 <- lm(packSeason$Pack.size~1)
summary(mod1)
AIC(mod1)


mod <- lm(packSeason$Pack.size~packSeason$Season1+packSeason$Continentfact )
summary(mod)
AIC(mod)
##
mod1 <- lm(packSeason$Pack.size~packSeason$Season1*packSeason$Continentfact )
summary(mod1)
AIC(mod1)












### add data 
addData <- pack[1,]
addData[] <- NA
#YNP page 46 Stahler et al 2020 
addData[] <- c("North America", "YNP", NA, "winter", 10, "2-37", NA, "Stahler et al 2020",NA  )
pack <- rbind(pack, addData)

#Nakamura, M., Rio-Maior, H., Godinho, R., Petrucci-Fonseca, F., & Álvares, F. (2021). 
#Source-sink dynamics promote wolf persistence in human-modified landscapes: Insights from long-term monitoring. Biological Conservation, 256, 109075.
# assign to summer as it is a minimum estimate with a combination of all methods used (inluding summer counts)
addData[] <- c("Eurasia", "Portugal", NA, "summer", 6.2, "0.3", NA, "Nakamura et al 2021",NA  )
pack <- rbind(pack, addData)

#Nordli, K. T. On the way to independence: Ebbing cohesion in Scandinavian wolf family groups. (Inland Norway University of Applied Sciences, 2018).
addData[] <- c("Eurasia", "Scandinavia", NA, "winter", 5.7, "0.3", NA, "Nordli et al 2018",NA  )
pack <- rbind(pack, addData)



pack$Pack.size <- as.numeric(pack$Pack.size)
head(pack)
nrow(pack)

mean(pack$Pack.size)
sd(pack$Pack.size)

pack$Continentfact <- as.factor(pack$Continent)
plot(pack$Pack.size ~ pack$Continentfact, xlab="Continent", ylab="Average pack size", xaxt="n")


axis(1, at=c(1,2), labels = c(paste("Eurasian (n=", table(pack$Continent)[1],")", sep=""),
                              paste("North America (n=", table(pack$Continent)[2],")", sep="")))


## simply season 
pack$Season1 <- substr(pack$Season, 1, 6)
## exclude year round
packSeason <- pack[!pack$Season %in% "year round",]
packSeason <- packSeason[!packSeason$Season %in% "fall",]

##MAKE LATE WINTER: WINTER
packSeason$Season1[packSeason$Season1 %in% "late w" ] <- "winter"
packSeason$Season1[packSeason$Season1 %in% "late s" ] <- "summer"

packSeason$Seasonfact <- as.factor(packSeason$Season1)

plot(packSeason$Pack.size ~ packSeason$Seasonfact, xlab="Continent", ylab="Average pack size", xaxt="n")


axis(1, at=c(1,2), labels = c(paste("Summer (n=", table(pack$Continent)[1],")", sep=""),
                              paste("Winter", table(pack$Continent)[2],")", sep="")))


##INTERACTIONS
packSeason$SeasonContinentfact <- as.factor(paste(packSeason$Season1,packSeason$Continent))

par(mar=c(7,5,5,5))
plot(packSeason$Pack.size ~ packSeason$SeasonContinentfact,xlab="",
     ylab="Average pack size", xaxt="n")

axis(1, at=c(1:4), labels = c(paste("Eurasia (n=", table(packSeason$SeasonContinentfact)[1],")", sep=""),
                              paste("North America (n=", table(packSeason$SeasonContinentfact)[2],")", sep=""),
                              paste("Eurasia (n=", table(packSeason$SeasonContinentfact)[3],")", sep=""),
                              paste("North America (n=", table(packSeason$SeasonContinentfact)[4],")", sep="")),cex.axis=0.7)


axis(1, at=c(1.5,3.5), labels = c(paste("Summer (n=", table(packSeason$Seasonfact)[1],")", sep=""),
                              paste("Winter (n=", table(packSeason$Seasonfact)[2],")", sep="")),
                       cex.axis=1, col="white", line = +2)


##
mod1 <- lm(packSeason$Pack.size~1)
summary(mod1)
AIC(mod1)


mod <- lm(packSeason$Pack.size~packSeason$Season1+packSeason$Continentfact )
summary(mod)
AIC(mod)
##
mod1 <- lm(packSeason$Pack.size~packSeason$Season1*packSeason$Continentfact )
summary(mod1)
AIC(mod1)




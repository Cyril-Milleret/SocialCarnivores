rm(list=ls())
library(maptools)
library(maps)
library(rgdal)
library(sp)
library(rgeos)
library("ggmap")
library(maptools)
library(maps)
library(ggplot2)
library(raster)
# 
# data(countryExData)
# sPDF <- joinCountryData2Map( countryExData,joinCode = "ISO3",nameJoinColumn = "ISO3V10")
# sPDF$id <- 1
# sPDF <- unionSpatialPolygons(sPDF,sPDF$id)
# 
# plot(sPDF)
#LOAD THE SPECIES DISTRIBUTION MAPS
setwd("C:/Personal_Cloud/OneDrive/Work/Book/SocialCarnivores/SocialCarnivores/WolfDistributionIUCN")


ExtantTokeep <- c("Extant (non-breeding)","Extant (resident)",
                  "Possibly Extant (resident)", "Extant & Origin Uncertain (resident)",
                  "Extant & Reintroduced (resident)")

Maps <- readOGR(paste("data_0" ,".shp", sep=""))## Map of Scandinavia (including Finland & parts of Russia)

Maps <- gSimplify(Maps[Maps$LEGEND %in% ExtantTokeep ,], tol = 0.001, topologyPreserve = T)
Maps$ID <- c(1:length(Maps))
Maps <- aggregate(Maps, by=list("ID"=c(1:length(Maps))), FUN=mean)

##Human Footprint, 2018 Release (2009)
setwd("C:/Personal_Cloud/OneDrive/Work/Book/SocialCarnivores/SocialCarnivores/Human_Footprint")

footprint <- raster("wildareas-v3-2009-human-footprint.tif")## Map of Scandinavia (including Finland & parts of Russia)
footprint[footprint[]==128] <- NA
plot(footprint)


# reproject maps
footprint_projected <- projectRaster(footprint, crs = CRS(proj4string(Maps)))


###YNP 44.55149918381928, -110.49762369597458
### SWEDEN 60.079157132222726, 13.601729203074163

## WOLVES 
MAX <- max(footprint_projected[], na.rm = T)
cuts <- seq(0, MAX,length.out = 100)   #set breaks
#col <- rev(terrain.colors(100))
colfunc <- colorRampPalette(c("white", "slateblue", "yellow", "orange", "red", "red"))
col <- colfunc(100)

#Maps <- spTransform(Maps,CRS(proj4string(footprint)))
pdf(file="Figure1.pdf", width= 8, height=4)
par(mar=c(0,0,0,0))
plot(Maps,  col="NA",
     border=NA)#,ylim=c(1170000, 8750338), xlim=c(-10000000,10000000))

plot(footprint_projected,legend=F,axes=F,boxes=F,add=T,breaks=c(cuts, max(cuts)+1000), col = col)
plot(Maps,  col=adjustcolor("darkgreen", alpha.f = 0.2),add=T, 
     border = "darkgreen", lwd=0.5)  

points( -110.49762369597458, 44.55149918381928, bg=adjustcolor("red",alpha.f = 0.4), col="red", lwd=1, pch=24, cex=2)
points(13.601729203074163, 60.079157132222726, bg=adjustcolor("black",alpha.f = 0.5),col="black", lwd=1, pch=22, cex=2)



points( 110, 95, bg=adjustcolor("red",alpha.f = 0.4), col="red", lwd=1, pch=24, cex=2)
points( 110, 85, bg=adjustcolor("black",alpha.f = 0.5),col="black", lwd=1, pch=22, cex=2)
text(143, 85,"Scandinavia")
text(145, 95,"Yellowstone NP")

plot(footprint_projected, legend.only=TRUE,breaks=cuts, col=col,
     legend.width = 1,
     axis.args=list(at=c(0,50),#round(seq(0, MAX, length.out = 5),digits = 1),
                    labels=c("Low","High"),#,round(seq(0, MAX, length.out = 5),digits = 1),
                    cex.axis=0.6),
     smallplot=c(0.82, 0.85, 0.25, 0.4),
     
     legend.args=list(text='Human footprint', side=4, font=2, line=2.5, cex=0.7))



dev.off()

# 
# 
# legend("bottomleft", fill=c(adjustcolor("red", alpha.f = 0.6), adjustcolor("blue", alpha.f = 0.7)),
#        legend = c(as.expression(bquote( italic("Ursus arctos"))), 
#                   as.expression(bquote(paste(italic("Ursus arctos"), " with ", italic("Canis lupus"))))),
#        bg = adjustcolor("white", alpha.f = 0.5),
#        cex=0.8)





plot(footprint_projected,legend=F,axes=F,boxes=F,add=T)

plot(footprint_projected, legend.only=TRUE,breaks=cuts, col=col,
     legend.width = 2,
     axis.args=list(at=round(seq(0, MAX, length.out = 5),digits = 1),
                    labels=round(seq(0, MAX, length.out = 5),digits = 1),
                    cex.axis=0.6),
     legend.args=list(text='Density', side=4, font=2, line=2.5, cex=0.8))




pdf(file="MapBearandWolves.pdf", width= 10, height=4)
  # BEAR AND WOLF 
 map("world",fill=TRUE, col="white",
      bg="lightblue", ylim=c(5, 90), mar=c(0,0,0,0), border="white")
  #plot("world", fill=TRUE, col="white", bg="lightblue", ylim=c(5, 90), mar=c(0,0,0,0), border="white")
  plot(InterBearWolf, add=T, col=adjustcolor("blue", alpha.f = 0.7),
       border=NA)  
  plot(NotInterBearWolf, add=T, col=adjustcolor("red", alpha.f = 0.6),
       border=NA)  
  
  legend("bottomleft", fill=c(adjustcolor("red", alpha.f = 0.6), adjustcolor("blue", alpha.f = 0.7)),
         legend = c(as.expression(bquote( italic("Ursus arctos"))), 
                  as.expression(bquote(paste(italic("Ursus arctos"), " with ", italic("Canis lupus"))))),
                  bg = adjustcolor("white", alpha.f = 0.5),
                    cex=0.8)
  
dev.off()
  
## BEAR AND LYNX  
  InterBearLynx <- gIntersection(Maps[["Ursusarctos"]],Maps[["Lynxlynx"]])
  NotInterBearLynx <- gDifference(Maps[["Ursusarctos"]],Maps[["Lynxlynx"]],drop_lower_td = T)
  
  InterBearLynxCan <- gIntersection(Maps[["Ursusarctos"]],Maps[["Lynxcanadensis"]])
  NotInterBearLynxCan <- gDifference(Maps[["Ursusarctos"]],Maps[["Lynxcanadensis"]],drop_lower_td = T)
  
  
  pdf(file="MapBearandlLynx.pdf", width= 10, height=4)
  # BEAR AND WOLF 
  map("world",fill=TRUE, col="white",
      bg="lightblue", ylim=c(5, 90), mar=c(0,0,0,0), border="white")
  #plot("world", fill=TRUE, col="white", bg="lightblue", ylim=c(5, 90), mar=c(0,0,0,0), border="white")
  plot(InterBearLynx, add=T, col=adjustcolor("orange", alpha.f = 0.9),
       border=NA)  
  plot(NotInterBearLynx, add=T, col=adjustcolor("red", alpha.f = 0.6),
       border=NA)  
  plot(InterBearLynxCan, add=T, col=adjustcolor("purple", alpha.f = 0.6),
       border=NA)  
  
  legend("bottomleft", fill=c(adjustcolor("red", alpha.f = 0.6), "orange", adjustcolor("purple", alpha.f = 0.5)),
         legend = c(as.expression(bquote( italic("Ursus arctos"))), 
                    as.expression(bquote(paste(italic("Ursus arctos"), " with ", italic("Lynx lynx")))),
                    as.expression(bquote(paste(italic("Ursus arctos"), " with ", italic("Lynx canadensis"))))),
         bg = adjustcolor("white", alpha.f = 0.5),
         cex=0.8)
  
  dev.off()
  
  ## BEAR AND Pantheratigris  
  InterBeartigris <- gIntersection(Maps[["Ursusarctos"]],Maps[["Pantheratigris"]])
  NotInterBeartigris <- gDifference(Maps[["Ursusarctos"]],Maps[["Pantheratigris"]],drop_lower_td = T)
  InterBeartpardus <- gIntersection(Maps[["Ursusarctos"]],Maps[["Pantherapardus"]])
  InterBearttigrispardus <- gDifference(InterBeartpardus,NotInterBeartigris,drop_lower_td = T)
  InterBeartpuma <- gIntersection(Maps[["Ursusarctos"]],Maps[["Pumaconcolor"]])
  InterPantherauncia <- gIntersection(Maps[["Ursusarctos"]],Maps[["Pantherauncia"]])
  
  pdf(file="MapBearandlPantheraPuma.pdf", width= 10, height=4)
  # BEAR AND WOLF 
  map("world",fill=TRUE, col="white",
      bg="lightblue", ylim=c(5, 90), mar=c(0,0,0,0), border="white")
  #plot("world", fill=TRUE, col="white", bg="lightblue", ylim=c(5, 90), mar=c(0,0,0,0), border="white")
  plot(InterBeartigris, add=T, col=adjustcolor("green", alpha.f = 0.8),
       border=NA)  
  plot(NotInterBeartigris, add=T, col=adjustcolor("red", alpha.f = 0.6),
       border=NA)  
  plot(InterBeartpardus, add=T, col=adjustcolor(grey(0.5), alpha.f = 1),
       border=NA)  
  plot(InterBeartpuma, add=T, col=adjustcolor("yellow", alpha.f = 0.8),
       border=NA)  
  plot(InterPantherauncia, add=T, col=adjustcolor("purple", alpha.f = 1),
       border=NA)
  plot(InterBearttigrispardus, add=T, col=adjustcolor("black", alpha.f = 1),
       border=NA)

  legend("bottomleft", fill=c(adjustcolor("red", alpha.f = 0.6),
                              adjustcolor("green", alpha.f = 0.8),
                              adjustcolor(grey(0.5), alpha.f = 1),
                              adjustcolor("yellow", alpha.f = 0.8),
                              adjustcolor("black", alpha.f = 1),
                              adjustcolor("purple", alpha.f = 1)),
         legend = c(as.expression(bquote( italic("Ursus arctos"))), 
                    as.expression(bquote(paste(italic("Ursus arctos"), " with ", italic("Panthera tigris")))),
                    as.expression(bquote(paste(italic("Ursus arctos"), " with ", italic("Panthera pardus")))),
                    as.expression(bquote(paste(italic("Ursus arctos"), " with ", italic("Puma concolor")))),
                    as.expression(bquote(paste(italic("Ursus arctos"), " with ",
                                               italic("Panthera pardus"), " and ", 
                                               italic("Panthera tigris")))),
                    as.expression(bquote(paste(italic("Ursus arctos"), " with ", italic("Panthera uncia"))))
                    
                    ), 
         bg = adjustcolor("white", alpha.f = 0.5),
         cex=0.8)
  
  dev.off()
  
  
  
SpeciesNamesNotbears <- SpeciesNames[SpeciesNames!="Ursusarctos"]

pdf(file="MapBearandOthers.pdf", width= 10, height=6)
nf <- layout(rbind(c(1,2,3), c(4,5,6)), widths = c(1,1,1),
        heights = c(1,1))
layout.show(nf)
for(i in SpeciesNamesNotbears){
# BEAR AND WOLF 
InterBearOther <- gIntersection(Maps[["Ursusarctos"]],Maps[[i]])
NotInterBearOther <- gDifference(Maps[["Ursusarctos"]],Maps[[i]],drop_lower_td = T)
map("world",fill=TRUE, col="white",
    bg="lightblue", ylim=c(5, 90), mar=c(0,0,0,0), border="white")
#plot("world", fill=TRUE, col="white", bg="lightblue", ylim=c(5, 90), mar=c(0,0,0,0), border="white")
plot(InterBearOther, add=T, col=adjustcolor("red", alpha.f = 0.9),
       border=NA)  
plot(NotInterBearOther, add=T, col=adjustcolor("blue", alpha.f = 0.8),
     border=NA)  

legend("bottomleft", fill=c("blue", "red"),
       legend = c("Ursus arctos alone", paste("Ursus arctos with", i )), cex=0.8)

}


dev.off()



# BEAR AND TIGERS
InterBearTigers <- gIntersection(Maps[["Ursusarctos"]],Maps[["Pantheratigris"]])
NotInterBearTigers<- gDifference(Maps[["Ursusarctos"]],Maps[["Pantheratigris"]],drop_lower_td = T)
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(5, 90), mar=c(0,0,0,0), border="white")
plot(InterBearTigers, add=T, col=adjustcolor("red", alpha.f = 0.9),
     border=NA)  
plot(NotInterBearTigers, add=T, col=adjustcolor("blue", alpha.f = 0.8),
     border=NA)  


# BEAR AND LYNX
InterBearLynx <- gIntersection(Maps[["Ursusarctos"]],Maps[["Lynxlynx"]])
NotInterBearLynx <- gDifference(Maps[["Ursusarctos"]],Maps[["Lynxlynx"]],drop_lower_td = T)
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(5, 90), mar=c(0,0,0,0), border="white")
plot(InterBearLynx, add=T, col=adjustcolor("red", alpha.f = 0.9),
     border=NA)  
plot(NotInterBearLynx, add=T, col=adjustcolor("blue", alpha.f = 0.8),
     border=NA) 



plot(Maps[["Canislupus"]], add=T, col=adjustcolor("orange", alpha.f = 0.2),
     border=NA)
#}



##
Canislupus <- readOGR("Canislupus.shp") ## Map of Scandinavia (including Finland & parts of Russia)
unique(Canislupus$LEGEND)

Tokeep <- c("Extant (non-breeding)","Extant (resident)",
            "Possibly Extant (resident)", "Extant & Origin Uncertain (resident)",
            "Extant & Reintroduced (resident)")
Canislupus <- gSimplify(Canislupus[Canislupus$LEGEND %in% Tokeep ,],tol = 0.01,topologyPreserve = T)
Canislupus$ID <- c(1:length(Canislupus))
Canislupus <- aggregate(Canislupus, by=list("ID"=c(1:length(Canislupus))), FUN=mean)
plot(Canislupus, add=T, col=adjustcolor("orange", alpha.f = 0.2), border=NA)

inter <- gIntersection(Canislupus,Ursusarctos)
plot(Canislupus, add=T, col=adjustcolor("red", alpha.f = 0.8), border=NA)




setwd("C:/Personal_Cloud/OneDrive/Work/Book/SocialCarnivores")
pack <- read.csv("packSizeFernandez.csv")


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




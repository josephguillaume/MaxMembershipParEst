
source("CatchCrop.R")

Years <- 10
StartMonth <- NA

Seasons <- t(as.matrix(read.csv("data/Seasons.csv",header=FALSE)))
Rainfall <- t(as.matrix(read.csv("data/_Rainfall.csv")))
RegionNumber <- 1
PET <- t(as.matrix(read.csv("data/PET.csv")))
Crops2 <- t(as.matrix(read.csv("data/Crops2.csv",header=FALSE)))
rownames(Crops2)[1:15] <- c("Crop","Lini","Ldev","Lmid","Lend","KCini","KCmid","KCend","RDini","RDend","P","CC","KY","YM","Cfact")
Soils2 <- t(as.matrix(read.csv("data/Soils2.csv",header=FALSE)))
rownames(Soils2)[1:7] <-c("lu","TAW","TEW","REW","IS","SD","CS")
Irrig <- t(as.matrix(read.csv("data/Irrig.csv",header=FALSE)))
LF <- 2
PIR <- matrix(0,ncol=1)


## Defaults
CatchCrop(Crops2,Soils2,Irrig,
          Years,StartMonth,
          Rainfall,PET,
          RegionNumber,LF,PIR,
          Seasons
          )

CatchCropSelect.yield <- function(pars){
  stopifnot(is.numeric(l))
  tempCrops <- Crops2[,l,drop=FALSE]
  tempSeasons <- Seasons[,l,drop=FALSE]
  tempCrops["YM",] <- pars[1]
  tempCrops["KY",] <- pars[2]
  res0 <- CatchCrop(tempCrops,Soils2,
                    matrix(0),
                    Years,StartMonth,
                    Rainfall,PET,
                    RegionNumber,LF,PIR,
                    tempSeasons
                    )
  res1 <- CatchCrop(tempCrops,Soils2,
                    matrix(1),
                    Years,StartMonth,
                    Rainfall,PET,
                    RegionNumber,LF,PIR,
                    tempSeasons
                    )
  c(
    Dryland.max=max(res0$Yield[-c(1:2),2]),Dryland.min=min(res0$Yield[-c(1:2),2]),
    Irrigated.max=max(res1$Yield[-c(1:2),2]),Irrigated.min=min(res1$Yield[-c(1:2),2])
    )
}


l <- 7 ##Crop number
f=CatchCropSelect.yield
## Run with current settings
f(Crops2[c("YM","KY"),l])

ranges <- data.frame(Variable=c("YM","KY",
                       "Dryland.max","Dryland.min",
                       "Irrigated.max","Irrigated.min"),
                     Modeled=c(10,1,2.55,2.5,6,6),
                     Min=c(0,0.01,0,0,0,0),
                     Max=c(15,5,20,20,20,20))
which.response <- c("Dryland.max","Dryland.min",
                    "Irrigated.max","Irrigated.min")

library(shiny)
runApp(".",port=8500+l)


## ################################################################################
## ## Manually
## limit.val <- data.frame(Min=c(0,0.01,0,0,0,0),
##                         Max=c(15,5,20,20,20,20))
## which.active <- c("YM","KY")
## max.membership(f,ranges,limit.val,
##                which.active,which.response)


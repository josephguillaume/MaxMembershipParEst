
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


CatchCropSelect.ys<- function(pars){
  stopifnot(is.numeric(l))
  tempCrops <- Crops2[,l,drop=FALSE]
  tempSeasons <- Seasons[,l,drop=FALSE]
  tempCrops["KCini",] <- pars[1]
  tempCrops["KCmid",] <- pars[2]
  tempCrops["KCend",] <- pars[3]
  tempCrops["P",] <- pars[4]
  tempSoils <- Soils2
  tempSoils["TAW",] <- pars[5]
  tempSoils["TEW",] <- pars[6]
  tempSoils["REW",] <- pars[7]
  tempSoils["IS",] <- pars[8]
  res <- CatchCrop(tempCrops,tempSoils,Irrig,
                   Years,StartMonth,
                   Rainfall,PET,
                   RegionNumber,LF,PIR,
                   tempSeasons
                   )
  c(DIVTOT.max=max(res$CropWU[,2]),
    DIVTOT.min=min(res$CropWU[,2])
    )
}


l <- 6 ## Crop number
f=CatchCropSelect.ys
## Run with current settings
f(c(Crops2[c("KCini", "KCmid", "KCend", "P"),l],Soils2[c("TAW", "TEW", "REW", "IS"),1]))

which.response=c("DIVTOT.max","DIVTOT.min")
ranges <- data.frame(Variable=c("KCini", "KCmid", "KCend", "P", "TAW", "TEW", "REW", "IS",
                       which.response
                       ),
                     Modeled=c(Crops2[c("KCini", "KCmid", "KCend", "P"),l],
                       Soils2[c("TAW", "TEW", "REW", "IS"),1],
                       ##150,30,10,5, ## Clayey soil, Perez 2002
                       ##275,260, ## Wheat IWR
                       ##270,260 ##faba IWR
                       ##790,560 ##cotton
                       170,170 ##chickpea
                       ),
                     Min=c(0, 0.5, 0.2, 0.1, 100, 20, 1, 1,
                       50,50
                       ),
                     Max=c(1.5, 1.5, 1.5, 1, 300, 50, 20, 20,
                       800,800
                       )
                     )

library(shiny)
runApp(".",port=8100+l)

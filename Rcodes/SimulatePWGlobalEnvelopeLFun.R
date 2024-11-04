

# Spatial interaction: attraction, repulsion, and avoidance


# Inhomogeneous cross-type L-function function


# Simulation of Global Envelope L-function


rm(list=ls(all=TRUE))


SimulationEnvelope <- local({

  
SimulationEnvelope <- function(bearperm)
  
{
  
  
set.seed(4786439)


nrank <- 5


nsim <- 2500


source("./Rcodes/loadRpackagesR.R")


# Read data


BearData0 <- source("./Rcodes/readdata.R")$value


# create marked point pattern


# It requires an input bear data: BearData


xym00 <- source("./Rcodes/markedpointpattern.R")


xym00 <- xym00$value


permbear <- source("./Rcodes/blackbearpairs.R")


bearperm00 <- permbear$value 


# Select pairs of black bears that share their core 


# home ranges for at least five months


coreHR5months <- unname(rbind.data.frame(c("487", "488"), c("487", "490"), 
                                         c("487", "491"), c("491", "501"),
                                         c("488", "487"), c("490", "487"), 
                                         c("491", "487"), c("501", "491")))


bearperm0 <- unname(bearperm00)


for(fg in 1:length(bearperm0))
{
  
PermBear <- unname(as.data.frame(bearperm0[[fg]]))
  

BearPerM <- PermBear[do.call(paste0, PermBear) %in% do.call(paste0, coreHR5months),]


if(nrow(BearPerM) == 0)
{
  
  
bearperm0[[fg]]<- BearPerM
  

}
else{
  
  
rownames(BearPerM) <- 1:dim(BearPerM)[1]
  
  
bearperm0[[fg]]<- BearPerM
  
}


}


RbindBeaer <- as.factor(unique(unlist(do.call(rbind, bearperm0))))



xym <- xym00[xym00$marks%in%RbindBeaer,]



xym$marks <- droplevels(xym$marks)



Recode <- BearData0$bearid$recode[BearData0$bearid$id%in%RbindBeaer]


bearid00 <- BearData0$bearid[BearData0$bearid$id%in%RbindBeaer, ]



bearid00$recode <- 1:length(bearid00$recode)


rownames(bearid00) <- 1:length(bearid00$recode)


BearData <- list()


for(dg in 1:length(Recode))
  
{

ds <- Recode[dg]


BearData$blackbears[[dg]] <- BearData0$blackbears[[ds]]
  
  
BearData$windows[[dg]] <- BearData0$windows[[ds]]

}


BearData$bearid <- bearid00


Bears <- BearData$blackbears


windows <- BearData$windows



bearid <- BearData$bearid






ngroup <- nmb <- length(bearperm0)


bearperm <- vector(mode = "list", length = nmb)



for(id in 1:length(bearperm0))
{
  

if(length(unlist(bearperm0[[id]])) == 0)  
{
  
bearperm[[id]]$permbear <- list()

  
bearperm[[id]]$Bears <- list()


bearperm[[id]]$windows <- list()


bearperm[[id]]$bearid <- list()


bearperm[[id]]$xym <- list()


bearperm[[id]]$ngroup <- list()


bearperm[[id]]$nrank <- list()

  
bearperm[[id]]$nsim <- list()

}

else{
  
  
bearperm[[id]]$permbear <- bearperm0[[id]]


bearperm[[id]]$Bears <-  Bears


bearperm[[id]]$windows <- windows


bearperm[[id]]$bearid <- bearid 


bearperm[[id]]$xym <- xym


bearperm[[id]]$ngroup <- ngroup


bearperm[[id]]$nrank <- nrank


bearperm[[id]]$nsim <- nsim 
  
}

}



# Estimate spatial intensity functions


source("./Rcodes/spatial_intensity_estimation.R")



# call simulation function


source("./Rcodes/LFunEnvelope.R")


# Point-wise envelopes (global = FALSE) or simultaneous (global = TRUE)



# The envelope command performs simulations and computes envelopes of a 



# summary statistic based on the simulated data



# Tests of independence: Cross-type  L - function


LcrossInhom <- local({
  
  
LcrossInhom <- function(yy, i, j, lambdaI = lambdaI, lambdaJ = lambdaJ,
                        
                        correction = "best", update = FALSE, ...) 
{
  
CrossLInhom0 <- Lcross.inhom(yy, i, j, lambdaI = lambdaI, lambdaJ = lambdaJ,
                               
                               correction = "best", update = FALSE)
  
CrossLInhom <- eval.fv(CrossLInhom0)
  
  
return(CrossLInhom)
  
  
}

})


  
if(nrank > nsim/2)
{

stop("The number of rank should be less than nsim/2")

}

  
do.call("envelope_simulateLfun", list(bearperm, LcrossInhom))
  

}


})

do.call("SimulationEnvelope", list())




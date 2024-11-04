

# Spatial interaction: attraction, repulsion, and avoidance


# Inhomogeneous cross-type J-function function


# Simulation of Global Envelope J-function


rm(list=ls(all=TRUE))


SimulationEnvelope <- local({
  
  
SimulationEnvelope <- function(bearperm)
  
{
  
  
set.seed(4786439)


nrank <- 5


nsim <- 2500


source("./Rcodes/loadRpackagesR.R")


# Read data


BearData <- source("./Rcodes/readdata.R")$value


BearData0 <- BearData


# create marked point pattern


# It requires an input bera data: BearData


xym <- source("./Rcodes/markedpointpattern.R")$value


xym00 <- xym



permbear <- source("./Rcodes/blackbearpairs.R")$value


bearperm00 <- permbear



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


source("./Rcodes/JFunEnvelope.R")


# Point-wise envelopes (global = FALSE) or simultaneous (global = TRUE)



# The envelope command performs simulations and computes envelopes of a 



# summary statistic based on the simulated data



# Tests of independence: Cross-type  J - function



JcrossInhom <- local({

  
JcrossInhom <- function(yy, I, J, i = i, j = j, lambdaI = lambdaI ,  
                        
                     lambdaJ = lambdaJ, lam = lam, model= model,  
                     
                     correction = "best", ...) 
                        
                       
{
  
  
lmin <- min(terra::predict(model)[[2]], lam)*0.9

  
ginhom <- GmultiInhom(yy, I, J, lambdaI = lam[I], lambdaJ = lam[J], 
                      
                      lambdamin = lmin, correction = "best")

  
finhom <- FmultiInhom(yy, J,  lambdaJ = lam[J], lambdamin = lmin, 
                      
                      correction = "best")
  
Jinhom <- ginhom

  
Jinhom$theo <- (1-ginhom$theo)/(1-finhom$theo)

  
Jinhom$bord <- (1-ginhom$bord)/(1-finhom$bord)


ibear <- as.numeric(make.parseable(paste(i)))


jbear <- as.numeric(make.parseable(paste(j)))


ylab <- substitute(J[inhom, i, j](r), list(i = ibear, j = jbear))


attributes(Jinhom)$ylab <- ylab


yexp <- substitute(J[list(inhom, i, j)](r), list(i = ibear, j = jbear))


attributes(Jinhom)$yexp <- yexp


fname0 <- substitute(J[list(inhom, i, j)], list(i = ibear, j = jbear))


fname <- paste (fname0, collapse = "")


attributes(Jinhom)$fname <- fname


return(Jinhom)

}


})



  
if(nrank > nsim/2)
{

stop("The number of rank should be less than nsim/2")

}

  
do.call("envelope_simulateJfun", list(bearperm, JcrossInhom))
  

}


})


do.call("SimulationEnvelope", list())




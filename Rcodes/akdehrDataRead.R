

rm(list=ls(all=TRUE))


akdehr_Data_Read  <- local({
  
  
akdehr_Data_Read <- function()

{
  

probb <- c(0.5, 0.95)


source("./Rcodes/loadRpackagesR.R")


# Read data


BearData <- source("./Rcodes/readdata.R")$value


Bears <- BearData$blackbears


windows <- BearData$windows


bearid <- BearData$bearid


BBID <- source("./Rcodes/groupsofblackbears.R")$value


# Set time zone


# tz <- Sys.timezone()


tz <- "UTC"


projection_type <- "+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs"


# Datum


datum <- "NAD83"


id0 <- bearid         



# 1st group


id1 <- BBID$bbid1


# 2nd group


id2 <- BBID$bbid2



coded <- list()


coded[[1]] <- id1


coded[[2]] <- id2


txy <- list()


xyt0 <- list()


for(i in 1:length(Bears))

{

Bear00 <- Bears[[i]] 


Bear0 <- Bear00[order(Bear00$Date_Time),]


t00 <- as.data.frame(Bear0$Date_Time)


t0 <- as_datetime(t00[,1])


t <-  as_datetime(t0, tz = tz)


id <- Bear0$bear


timestamp  <- t


x <- Bear0$Longitude_  


y <- Bear0$Latitude_1


Latitude  <- Bear0$Latitude 


Longitude <- Bear0$Longitude 


yx <- cbind.data.frame(id, Longitude, Latitude, timestamp, x, y)


xty00 <- yx[!duplicated(yx$timestamp),]


xyt0[[i]] <- xty00[order(xty00$timestamp),]


}


for(i in 1:length(Bears))

{


txy[[i]] <- xyt0[[i]][order(xyt0[[i]]$timestamp),]


}


xty0 <- as.data.frame(do.call(rbind, txy))


xyt0 <- ctmm::as.telemetry(xty0, timezone = tz,  
         
         projection = projection_type, 
         
         datum = datum, keep = TRUE)

xyt1  <- xyt0 



# Do the plots suggest range resident with no obvious migrations?


# A scatter plots of each positional observation


for(i in 1:length(xyt1))

{

ctmm::plot(xyt1[[i]], col = "blue")


Sys.sleep(1) 

}

# A scatter plot of position observations


ctmm::plot(xyt1, col = rainbow(length(xyt1)))


# For individual black bear


# Empirical variogram from movement data


# Plot variogram with confidence intervals 


xyt <- vector("list", length(coded))


for(k in 1:length(coded))

{

idd <- coded[[k]]

for (i in 1:dim(idd)[1])
{


I <- idd[i, 2]


xyt[[k]][[i]] <- xyt1[[I]]


}

}

return(list(xyt = xyt, coded = coded, probb = probb))

}

})



do.call("akdehr_Data_Read", list())



      
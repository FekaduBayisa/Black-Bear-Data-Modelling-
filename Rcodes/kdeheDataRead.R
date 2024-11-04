

kdehe_Data_Read <- local({
 
   
kdehe_Data_Read <- function(BearData)
{
  
  
Bears <- BearData$blackbears


windows <- BearData$windows


bearid <- BearData$bearid


mn <- length(Bears)


probb <- c(0.5, 0.95)


nm <- length(probb)


beargroup <- source("./Rcodes/groupsofblackbears.R")$value


codebears <- source("./Rcodes/groupsofblackbears.R")$value


# black bears


# group 1


# The first group of the black bears


xkd1 <- list()


# group 1


for(hk in 1:dim(beargroup$bbid1)[1])
{
  
  
xkd1[[hk]]  <- as.data.frame(Bears[[beargroup$bbid1$recode[hk]]])
  
  
}



xyb1 <- list() 


# Obtain the spatial relocation and id of the data


for(i in 1:length(xkd1))

{

id <- xkd1[[i]]$bear


x <- xkd1[[i]]$Longitude_


y <- xkd1[[i]]$Latitude_1


xyb1[[i]] <- cbind.data.frame(id = id, x = x, y = y)

}


# First group: bind the data sets of the black bears together


xyb10 <- as.data.frame(do.call(rbind, xyb1))


# Group 2


xkd2 <- list()


for(hk in 1:dim(beargroup$bbid2)[1])
{


xkd2[[hk]]  <- Bears[[beargroup$bbid2$recode[hk]]]


}


xyb2 <- list() 


# Obtain the spatial relocation and id of the data


for(i in 1:length(xkd2))

{

id <- xkd2[[i]]$bear


x <- xkd2[[i]]$Longitude_


y <- xkd2[[i]]$Latitude_1


xyb2[[i]] <- cbind.data.frame(id = id, x = x, y = y)

}

# Second group: bind the data sets of the black bears together


xyb20 <- as.data.frame(do.call(rbind, xyb2))


xyz <- vector("list", length = nm)


xyz[[1]] <- xyb10


xyz[[2]] <- xyb20


return(list(xyz = xyz, windows = windows, bearid = bearid))

}

})







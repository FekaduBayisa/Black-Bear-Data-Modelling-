


study_area_fun <- local({
  
  
study_area_fun <- function()
    
{
    
source("./Rcodes/loadRpackagesR.R")

  

counties <- st_read("./Studyregion/county.shp")


# Read data


BearData <- source("./Rcodes/readdata.R")$value


source("./Rcodes/kdeheDataRead.R")


xyzt <-  do.call("kdehe_Data_Read", list(BearData))



load("./Plots/colors.RData")



set.seed(4786439)


col <- unique(colors)


xyz <- xyzt$xyz


windows <- xyzt$windows


bearid <- xyzt$bearid


Bears <- do.call(rbind, xyz)


Bearsxy0 <- Bears[order(as.numeric(as.character(Bears$id))),]


rownames(Bearsxy0) <-1:dim(Bears)[1]


unbearcode <- unique(Bearsxy0$id)


Bearsxy <- list()


for(pq in 1:length(unbearcode))
{

xyc <- Bearsxy0[Bearsxy0$id == unbearcode[pq], ]  
  
x <- xyc$x

y <- xyc$y

xy <- SpatialPoints(cbind(x, y))


proJ <- st_crs(windows[[pq]])$proj4string


crs(xy) <- proJ


Bearsxy[[pq]] <- xy   

} 
  


mn <- length(Bearsxy)


CRS <-distinctColorPalette(k = mn)


# merge windows

winh <- windows[[1]]

winh <- as.owin(winh)

winh <- dilation(winh, r = 3)

nmw <- length(windows)

for (g in 2:nmw) 
  
{

wind <- windows[[g]]

wind <- as.owin(wind)

wind <- dilation(wind, r = 3)

winh <- union.owin(winh, wind)

}


# plot the data on the same window

for(i in 1:mn)
  
{


if(i==1)

{

sp::plot(winh, main = NULL)

  
sp::plot(Bearsxy[[i]],  pch = 46, col =  CRS[i], cex = 2,  add = TRUE)

}

sp::plot(Bearsxy[[i]],  pch = 46, col =  CRS[i], cex = 2,  add = TRUE)


Sys.sleep(1)

}


xy00 <- Bearsxy0


id0 <- cbind.data.frame(id = xy00$id)


xy <- SpatialPointsDataFrame(xy00[, c(2,3)], id0)


xy000 <- SpatialPoints(xy00[, c(2,3)])


sp::plot(winh, main = NULL, border = "blue")


sp::plot(xy000, pch = 46, col = "blue", cex = 2, add = TRUE)


id <- factor(as.matrix(id0))


xy <-  as.data.frame(id) # id of the bears


coordinates(xy) <- as.data.frame(xy000)


proJ <- st_crs(windows[[1]])$proj4string


crs(xy) <- proJ


sp::plot(winh, main = NULL, border = "blue")


sp::plot(xy, pch = 46, col = "red", cex = 2, add = TRUE)


prj <- st_crs(counties)$proj4string


cnties <- counties[counties$NAME=="Mobile"|counties$NAME=="Chatom"|
                     
                     
                     counties$NAME=="Mount Vernon"|counties$NAME=="Fruitdale"|
                     
                     
                     counties$NAME=="Wagarville",] 


ncnties <- length(cnties$NAME)


border <-  rainbow(ncnties)


namecounties <-  cnties$NAME



# plot the data on the same window

xyg <- list()


for(dg in 1:length(Bearsxy))
{
  
xyc <- xy[xy$id==unbearcode[dg], ]
  

xyg[[dg]] <- spTransform(xyc, prj)

}


for(i in 1:mn)
  
{


if(i==1)

{

nbn <- length(border)
  
  
sp::plot(cnties$geometry, main = NULL, border = border, lwd = 1.75)#,  lty = c(1:nbn))


sp::plot(xyg[[i]],  pch = 46, col =  col[i], cex = 2,  add = TRUE)

}

sp::plot(xyg[[i]],  pch = 46, col =  col[i], cex = 2,  add = TRUE)



}


legend("bottom", inset =  c(-0.75, -0.075), legend = namecounties,  #lty = c(1:nbn), 
       
       
       lwd = 1.5, ncol = ceiling(ncnties/2), col = border, cex = 0.35, xpd = NA) 


dev.copy2pdf(file = './Results/StudyArea.pdf')



return(xy)

}


})


do.call("study_area_fun", list())




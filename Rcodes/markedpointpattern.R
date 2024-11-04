

# create marked point pattern


exploratory_data_analysis <- local({

  
exploratory_data_analysis <- function(BearData)
{


current_directory <- getwd()


current_working_directory3 <- paste(current_directory, "/Plots/col.RData", sep = "")


load(current_working_directory3) 
  
  
Bearsxy <- list() 


blackbears <- BearData$blackbears 


windows <- BearData$windows


bearid <- BearData$bearid


for(dv in 1:length(blackbears))

{

x <- blackbears[[dv]]$Longitude_


y <- blackbears[[dv]]$Latitude_1


xyz0 <- SpatialPoints(cbind(x, y))


subw <- windows[[dv]]


own0 <- as.owin(subw)


# Expand the window to accommodate points lying on the boundary


own <- expand.owin(own0, distance = 5)


xyz <- suppressWarnings(ppp(x, y, window = own))


# Remove points lying outside the study area

xy <- xyz[own,] 


Bearsxy[[dv]] <- xy


sp::plot(xy, cols = "red", pch = 46, main = NULL)


Sys.sleep(1)

}


bearcodes <- as.numeric(as.character(bearid$id)) 


Marks <- list()


BEARS <- list()


for(i in 1:length(Bearsxy))

{

beari <- Bearsxy[[i]]


BEARS[[i]] <- as.data.frame(Bearsxy[[i]])


Marks[[i]] <- cbind(rep(bearcodes[i], npoints(beari)))

}


xyz <- c();


marks <- c();


for(k in 1:length(BEARS))

{

xyz <- rbind(xyz, BEARS[[k]]);


marks <- rbind(marks, Marks[[k]]);

}


marks <- as.factor(marks)


xyzm <- cbind.data.frame(xyz, marks)


# Remove duplicates


xbym <- xyzm[!duplicated(xyzm),]


d2d <- as.owin(windows[[1]])


for(i in 2:length(windows))
{

sub <- as.owin(windows[[i]])


d2d <- union.owin(d2d, sub)


sp::plot(d2d, main = NULL)


Sys.sleep(1)

}



# Marked spatial point pattern/multitype point pattern 


win <- expand.owin(d2d, distance = 5)


xby <- xbym


xym0 <- ppp(xby$x, xby$y, window = win, marks = xbym$marks)


xym <-  as.ppp(xym0)


sp::plot(win, main = NULL)


sp::plot(xym,  pch = 46, cex = 2, add = TRUE)


is.multitype(xym) 


for(h in 1:length(bearcodes))
{

if (h == 1)
  
{
  
  sp::plot(win, main = NULL)
  
}

sp::plot(xym[xym$marks == bearcodes[h]], cols = col[h, 1],  pch = 46,  cex = 2, add = TRUE)


print(h)


Sys.sleep(1)

}

return(xym)

}

})



xym <- do.call("exploratory_data_analysis", list(BearData))




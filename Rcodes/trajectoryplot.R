

rm(list=ls(all=TRUE))


trajectory_plot <- local({
  

trajectory_plot <- function()
{


source("./Rcodes/loadRpackagesR.R")



BearData <- source("./Rcodes/readdata.R")$value




Bears <- BearData$blackbears



windows <- BearData$windows



bearid <- BearData$bearid


group <- unname(source("./Rcodes/groupsofblackbears.R")$value)



counties <- st_read("./Studyregion/county.shp")



set.seed(4786439)


current_directory <- getwd()



prj <- st_crs(counties)$proj4string



cnties <- counties[counties$NAME=="Mobile"|counties$NAME=="Chatom"|
                     
                     
                     counties$NAME=="Mount Vernon"|counties$NAME=="Fruitdale"|
                     
                     
                     counties$NAME=="Wagarville",] 


# The two regions


regions <- list()


ncTies <- list()


R <- list()


regions[[1]] <- cnties[cnties$NAME=="Chatom"| cnties$NAME=="Fruitdale"|
                      
                      
                          cnties$NAME=="Wagarville",] 



R[[1]] <- c("Chatom", "Fruitdale","Wagarville")



regions[[2]] <- cnties[cnties$NAME=="Mobile"|
                           
                           
                           cnties$NAME=="Mount Vernon", ] 



R[[2]] <- c("Mobile", "Mount Vernon")


  
ncty <- c("Mobile",  "Chatom",  "Fruitdale",   "Wagarville", "Mount Vernon")



TFV <-  ncty %in% R[[1]] 



border <-  rainbow(length(ncty))



ncTies[[1]] <- border[TFV]



ncTies[[2]] <- border[!TFV]




projects <- "+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs"



cnties <- st_transform(cnties, projects)



regions <- lapply(regions, function(reg){st_transform(reg, projects)})



ncnties <- length(cnties$NAME)


for(xd in 1:length(group))
{
 
rgns <- regions[[xd]]
  

border <- ncTies[[xd]]


for(gd in group[[xd]]$recode)
{
  
x <- Bears[[gd]]$Longitude_


y <- Bears[[gd]]$Latitude_1


bear <- Bears[[gd]]$bear


date <- Bears[[gd]]$Date_Time 


xdy <- data.frame(x, y, date, bear)


xy <- xdy[!duplicated(xdy$date),]


xyd <- xy[order(xy$date),]


rownames(xyd)<- 1:dim(xyd)[1]


x <- xyd$x


y <- xyd$y


date <- xyd$date


xys <- data.frame(x,y)


tz <- Sys.timezone()


t0 <- as_datetime(xyd$date)


t00 <-  as_datetime(t0, tz = tz)


date <- t00


id <- bearid$id[gd]


FName <- paste(current_directory, "/", "Results", "/", "Bear", gd, "Traj",  ".pdf", sep = "")


Bearsbm <- as.ltraj(xys, date, id, burst = id, slsp = "remove")


cntiessf <- as(st_geometry(rgns$geometry), "Spatial")


Ext <- extent(cntiessf)



xlim <- c(Ext[1],  Ext[2])



ylim <- c(Ext[3],  Ext[4])



# xlim = xlim, ylim = ylim 



# The initial and final relocations of each burst are indicated in blue and red


nbv <- length(R[[xd]])

         
sp::plot(Bearsbm,   axes = FALSE, spoldf = cntiessf, colspoldf = "cornsilk", 
         

                 xlab = NA,  ylab = NA)         


sp::plot(rgns$geometry, border  =  border, lwd = 1.75, add = TRUE) 


legend("bottomleft", inset = c(0.05, 0.05), legend = R[[xd]],
       
       
       
       lwd = 1.75,  ncol = nbv, col = border, cex = 0.35, xpd = NA)



dev.copy2pdf(file = FName)


Sys.sleep(2)


}

}


}  


})


do.call("trajectory_plot", list())









# Parametric modelling of spatial intensity


# Spatial intensity estimation


# Group: There are two groups in our bear dataset


# group <- 1 or 2


# Load R-Packages


load_R_packages <- function()
{

Packages <- list("readxl", "conflicted", "tidyverse","lubridate",
                 
                 "spatstat", "sp",
                 
               
                 "sf", "proj4","terra","stars","parallel", 
               
               
                "randomcoloR", "adehabitatHR",
               
               
                "matrixStats","amt", "ggplot2", "tidygraph", 
               
               
                "ggraph", "ctmm","plot.matrix",
               
               
                "psych", "combinat", "gtools", "GET","nlme", 
               
               
               "spatstat.utils", "spatstat.explore",
               
               
               "spatstat.data", "spatstat.geom", "spatstat.random")


# Loading R-Packages


lapply(Packages, require, character.only = TRUE)

}


do.call("load_R_packages", list())


# Read data

read_data <- local({ read_data <- function()
{

current_directory <- getwd()

# To read shaplefile data

current_working_directory1 <- paste(current_directory, 
                                    
                              "/Data", sep = "")


allShpFiles1 <- list.files(path = current_working_directory1, 
                           
                           
                      pattern = "\\.shp$", full.names = TRUE)


blackbears <- lapply(allShpFiles1, st_read)


# To read boundary data

current_working_directory2 <- paste(current_directory, 
                                    
                              
                              "/Boundary", sep = "")


allBoundFiles2 <- list.files(path = current_working_directory2, 
                             
                             
                             
                      pattern = "\\.shp$", full.names = TRUE)


Boundaries <- lapply(allBoundFiles2, st_read)


projects <- "+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs"


windows <- list()


for(sd in 1:length(Boundaries))
{

st_crs(Boundaries[[sd]]) <- projects


windows[[sd]]  <- st_union(Boundaries[[sd]])

}


nrow <- length(blackbears)


bearid <- data.frame(matrix(nrow = nrow, ncol = 2))


colnames(bearid)<-c("id", "recode")


for(ds in 1:length(Boundaries))
{

bearid[ds, 1] <- unique(as.integer(substr(blackbears[[ds]]$bear, 
                                      
                                      
                            nchar(blackbears[[ds]]$bear)-2, 
                                      
                                      
                            nchar(blackbears[[ds]]$bear))))


bearid[ds, 2] <- ds

}


bearid$id <- as.factor(bearid$id)


return(list(blackbears = blackbears, 
            
            
            windows = windows, bearid = bearid))
}

})




BearData <- do.call("read_data", list())



# To read color data for plotting: col


current_directory <- getwd()


current_working_directory3 <- paste(current_directory, 
                                    
                        
                      "/Plots/col.RData", sep = "")


load(current_working_directory3) 


# Exploratory data analysis


# create planar point pattern and marked point pattern


exploratory_data_analysis <- local({
  
exploratory_data_analysis <- function(BearData)
{


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

sp::plot(xym[xym$marks == bearcodes[h]], 
         
         
         
cols = col[h, 1],  pch = 46,  cex = 2, add = TRUE)


print(h)


Sys.sleep(1)

}

return(xym)

}

})



xym <- do.call("exploratory_data_analysis", list(BearData))



# Exploratory data analysis reveals that 


#  the bears reside in two distinct regions


# That is, two groups of bears are observed


# We need to treat the regions separately


G1 <- c(493, 495, 503, 505)


G2 <- c(487, 488, 490, 491, 498, 501, 504, 506)


separate_two_group_bears <-local({ 
  
  
separate_two_group_bears <-function(xym, BearData, G1, G2)

{

  
windows <- BearData$windows
  

bearcodes <- BearData$bearid[,1]
  
bearid <- BearData$bearid

xyr1 <- xym[xym$marks%in%G1]


xyr1 <- subset(xyr1, marks == xyr1$marks, drop = TRUE)


xyr2 <- xym[xym$marks%in%G2]


xyr2 <- subset(xyr2, marks == xyr2$marks, drop = TRUE)


# Windows

II <- 1:length(bearcodes)


I1 <- II[bearcodes%in%G1]


I2 <- II[bearcodes%in%G2]


# Window 1


d2d1 <- as.owin(windows[[I1[1]]])


if(length(I1) > 1)
{

  
for(i in 2:length(I1))

{

  
sub1 <- as.owin(windows[[I1[i]]])


d2d1 <- union.owin(d2d1, sub1)


sp::plot(d2d1, main = NULL)


Sys.sleep(1)


}

  
}


# Window 2

d2d2 <- as.owin(windows[[I2[1]]])


if(length(I2) > 1)
{

for(i in 2:length(I2))

{
  

sub2 <- as.owin(windows[[I2[i]]])


d2d2 <- union.owin(d2d2, sub2)


sp::plot(d2d2, main = NULL)


Sys.sleep(1)


}  

  
}

# Region G1


win1 <- expand.owin(d2d1, distance = 5)


current_directory <- getwd()


current_working_directory4 <- paste(current_directory, 
                                    
                                    
                          "/Plots/col1.RData", sep = "")


load(current_working_directory4) 


for(h in 1:length(G1))
{

if (h == 1)

{

  
sp::plot(win1, main = NULL)

  
}

sp::plot(xyr1[xyr1$marks == G1[h]], cols = col[h], 
   
         pch = 46, cex = 2, add = TRUE)


print(h)


Sys.sleep(1)

}



# Region G2


current_directory <- getwd()


current_working_directory5 <- paste(current_directory, 
                                    
                                    
                      "/Plots/col2.RData", sep = "")


load(current_working_directory5) 


win2 <- expand.owin(d2d2, distance = 5)


for(h in 1:length(G2))
{

if (h == 1)

{

  
sp::plot(win2, main = NULL)

  
}

sp::plot(xyr2[xyr2$marks == G2[h]], cols = col[h],  
       
           
   pch = 46, cex = 2, add = TRUE)


print(h)


Sys.sleep(1)


}



# Display the marked spatial point pattern


# G1


load(current_working_directory4) 


for(h in 1:length(G1))
{

  
if (h == 1)

  
{

  
sp::plot(win1, main = NULL)


}

sp::plot(xyr1[xyr1$marks == G1[h]], cols = col[h],  
         
   pch = 46, cex = 2, add = TRUE)


print(h)


Sys.sleep(1)

}


# G2


load(current_working_directory5) 


for(h in 1:length(G2))
{

  
if (h == 1)

{

  
sp::plot(win2, main = NULL)

  
}

sp::plot(xyr2[xyr2$marks == G2[h]], cols = col[h],  
         
   pch = 46, cex = 2, add = TRUE)


print(h)


Sys.sleep(1)

}

return(list(xyr1 = xyr1, xyr2 = xyr2, bearcodes = bearcodes, 
            
            
            bearid = bearid, 
            
            
            windows = windows, G1 = G1, G2 = G2)) 


}

})

  
xym12codes <- do.call("separate_two_group_bears", list(xym = xym, 
                                                
                                                BearData = BearData, 
                                                
                                                G1 = G1, 
                                                
                                                G2 = G2))



# For spatial interaction modelling, we consider pairs of 


# black bears that share their core home ranges for at least five months

# Two groups


pairs_black_bears <-local({ 

  
pairs_black_bears <- function()
{

  
# Group I

mak <- c(495, 503, 505)


bearperm1 <- permutations(n = length(mak), r = 2, v = mak)


for(i in 1:length(mak))

{

bearperm1[bearperm1[,1] == as.character(i), 1] <- as.character(mak[i])

}


# Group II


R22 <- matrix(cbind(487, 487, 487, 487, 488, 
            
            491, 488, 490, 491,
            
            504, 504, 501), 
      
            nrow = 6, ncol = 2)



R23 <- R22



R23[ , c(1,2)]  <- R23[ , c(2,1)]



R24 <- rbind(R22, R23)


bearperm2 <- cbind(as.character(R24[, 1]), as.character(R24[, 2]))


return(list(bearperm1 = bearperm1, bearperm2 = bearperm2))


}

})


bearpairs0 <- do.call("pairs_black_bears", list())


# The variogram analysis indicates that the following 


# pairs of black bears share their core 


# home ranges for at least five months and feasible for 


#  spatial interaction modelling



coreHR5months <- unname(rbind.data.frame(c("487", "488"), c("487", "490"), 
                                         c("487", "491"), c("491", "501"),
                                         c("488", "487"), c("490", "487"), 
                                         c("491", "487"), c("501", "491")))

# That is, we need to remove the black bears that don't belong to coreHR5months



bearperm0 <- bearpairs0


for(fg in 1:length(bearperm0))
{

PermBear <- unname(as.data.frame(bearperm0[[fg]]))


BearPerM <- PermBear[do.call(paste0, PermBear) %in% do.call(paste0, coreHR5months),]


if(nrow(BearPerM) == 0)
{


bearperm0[[fg]] <- BearPerM


}
else{


rownames(BearPerM) <- 1:dim(BearPerM)[1]


bearperm0[[fg]] <- BearPerM

}


}


bearpairs <- bearperm0


fnames <- names(bearpairs)


pairSbear <- list()


for(as in 1:length(bearpairs))
{
  
dfg <- unlist(bearpairs[[as]])
  
  
if(length(dfg) == 0)
{
 
pairSbear[[as]] <- list()
  
}

else{
  
ghr <- bearpairs[[as]]
  
  
for(dz in 1:dim(ghr)[2])
{
  
ghr[,dz] <- as.factor(ghr[,dz])
  
} 
  
pairSbear[[as]] <- ghr

}

}


bearpairs <- pairSbear




names(bearpairs) <- fnames


RbindBeaer <- unique(unlist(do.call(rbind, bearpairs)))


xym1 <- xym12codes$xyr1[xym12codes$xyr1$marks%in%RbindBeaer,]


xym1$marks <- droplevels(xym1$marks)


xym12codes$xyr1 <- xym1


RbindBeaer <- as.factor(unique(unlist(do.call(rbind, bearpairs))))


xym2 <- xym12codes$xyr2[xym12codes$xyr2$marks%in%RbindBeaer,]


xym2$marks <- droplevels(xym2$marks)


xym12codes$xyr2 <- xym2


Recode <- xym12codes$bearid$recode[xym12codes$bearid$id%in%RbindBeaer]


bearid00 <- xym12codes$bearid[xym12codes$bearid$id%in%RbindBeaer, ]


bearid00$recode <- 1:length(bearid00$recode)


rownames(bearid00) <- 1:length(bearid00$recode)


BearData <- list()


for(dg in 1:length(Recode))

{

ds <- Recode[dg]


BearData$windows[[dg]] <- xym12codes$windows[[ds]]

}


BearData$bearid <- bearid00


BearData$G1 <- list()


BearData$G2 <- as.numeric(as.character(RbindBeaer))


BearData$bearcodes <- RbindBeaer


BearData$xyr1 <- xym12codes$xyr1


BearData$xyr2 <- xym12codes$xyr2


xym12codes <- BearData



# Fitting model

model_fit <- local({
  
  
model_fit <- function(xybearperm)
  
{
 
Resultsmodel20800 <- list()
  
  
X <- xybearperm$xyr
  

bearperm <- xybearperm$bearperm


bearid <- xybearperm$bearid


windows <- xybearperm$windows


nm <- dim(bearperm)[1]


current_directory <- getwd()


current_working_directory6 <- paste(current_directory, 
                                    
                                    
                      "/Plots/col3.RData", sep = "")


load(current_working_directory6) 

     
for (g in 1:nm) 
  
{
  

ij <- bearperm[g,]


i <- ij[,1]


j <-  ij[,2]


YY <- X[X$marks == i | X$marks == j]


ki <- bearid[bearid$id %in% i, 2]


kj <- bearid[bearid$id %in% j, 2]


windi <- as.owin(windows[[ki]])


sp::plot(windi, main = NULL)


windj <- as.owin(windows[[kj]])


sp::plot(windj, main = NULL)


winn <- expand.owin(boundingbox(union.owin(windi, windj)), distance = 5)


sp::plot(winn, main = NULL)


yymarks <- droplevels(YY$marks)


yy <- ppp(YY$x, YY$y, window = winn, marks = yymarks)


sp::plot(yy, pch = 46, use.marks = TRUE, 
          cols = c(col[ki], "orange"), main = NULL)



I <- (as.character(yy$marks) == as.character(i))


J <- (as.character(yy$marks) == as.character(j))


xy <- yy


yy <- spatstat.geom::rescale(xy, 1000)


# ppm.ppp


model <- ppm(yy ~ marks * x, interaction = NULL, method = "logi")


# compute the fitted conditional intensity or fitted trend of the


# model at the points of the pattern, or at the points of


# the quadrature scheme used to fit the model


# fitted.ppm


lam <- fitted(model, type = "lambda", dataonly = TRUE)


# predict.ppm:  given a fitted point process model obtained by ppm,	evaluate


# the spatial trend or the conditional intensity of the model at new locations


lambda <- lam


lambdaI <- lambda[I]


lambdaJ <- lambda[J]


Resultsmodel20800$lambda[[g]] <- lambda


Resultsmodel20800$lambdaI[[g]] <- lambdaI


Resultsmodel20800$lambdaJ[[g]] <- lambdaJ


Resultsmodel20800$model[[g]]<- model


Resultsmodel20800$from[[g]] <- i


Resultsmodel20800$to[[g]] <- j


Resultsmodel20800$xy[[g]] <- xy


Resultsmodel20800$window[[g]] <- winn


print(model)


Sys.sleep(1)

}

return(Resultsmodel20800)

}


})



estimate_spatial_intensity <-local({ 

estimate_spatial_intensity <- function(xym12codes)

{
  

# The spatial analysis will be made for G1 and G2


xybears <- list(list(xyr = xym12codes$xyr1, bearid = xym12codes$bearid, 
                     
                     
           bearperm = bearpairs$bearperm1, windows = xym12codes$windows), 
                
           
           list(xyr = xym12codes$xyr2, bearid = xym12codes$bearid, 
                
                          
          bearperm = bearpairs$bearperm2, windows = xym12codes$windows))



Resultsmodel2080  <- lapply(xybears, function(x)
{
       if(spatstat.geom::is.empty(x$xyr))
          {
    
               return(list())
    
           }
   
        else
         {
    
               model_fit(x)
    
         }
 
  })


current_directory <- getwd()  

  
FName <- paste(current_directory, "/", "Results", "/", 
               
               
            "IntEstResults2080R", ".RData", sep = "")


save(Resultsmodel2080, file = FName)



}

})


do.call("estimate_spatial_intensity", list(xym12codes))







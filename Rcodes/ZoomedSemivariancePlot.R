
# plot theoretical semivariance and empirical semivariance


# Looking for fraction of the variogram to be displayed


# The last half of a variogram is pretty erroneous


# The plots suggest that we need to exclude 


# bear 503, 505, and 504 from the home range analysis


rm(list=ls(all=TRUE))


source("./Rcodes/loadRpackagesR.R")


beargroup <-unname(source("./Rcodes/groupsofblackbears.R")$value)


current_directory <- getwd()


current_working_directory1 <- paste(current_directory, 
                                    
                                    
    "/Results",  "/",  "EstimatedSVF.RData", sep = "")


loadSemv <- paste(current_working_directory1)


load(loadSemv)


current_working_directory1 <- paste(current_directory, 
                                    
                                    
            "/Results", "/",  "BestModels.RData", sep = "")


loadModel <- paste(current_working_directory1)


load(loadModel)


# Empirical variogram from movement data


# Plot variogram with confidence intervals 


# Estimate of theoretical semivariance function


# group: 1st ... qa; bear: 2nd .... gh; 


# BestModels[[qa]][[gh]] 


theosve <- BestModels[[2]][[8]] 


# Empirical semivariance function


Esve <- EstimatedSVF[[2]][[8]]


# Bear: 493 (1, 1), 495 (1, 2),  p <- 0.57;  


# Bear: 503 (1, 3),  p <- 0.85;  


# Bear: 505 (1, 4), p <- 0.81; Bear: 487 (2, 1), 488 (2, 2),  


# 490 (2, 3), p <- 0.65; 


# Bear: 491 (2, 4), p <- 0.6; Bear: 498 (2, 5), p <- 0.83


# Bear: 501 (2, 6), p <- 0.55; 


# Bear: 504 (2, 7), p <- 0.63; Bear: 506 (2, 8), p <- 0.63


p <- 0.63


m <- length(theosve)


BearCode <- substr(unlist(unname(slot(theosve, "info")[1])), 3, 1000000L)



Identity <- paste("Bear:", BearCode)



Legend <- c("Theoretical", "Empirical")



current_working_dir <- paste(current_directory, "/Results", "/", 
                             
                             
                             
                      "Semivariance", BearCode, ".png", sep = "")



png(filename = current_working_dir, width = 119, height = 75, units = "mm", 
     
     
     pointsize = 12, res = 1200)


par(family = "Arial", mai= c(0.85,  0.85,  0.5,  0.5))


# Bears: 493, 495, probs = 0.75; 503, probs = 1; 505, probs = 0.987; 


# 487, probs = 1; Bears: 488, probs = 0.95; 490, probs = 1; 491, probs = 0.7


# 498, probs = 0.98; 501, probs = 0.7; 504, probs = 0.93; 506, 0.97


ymax <- quantile(Esve$SVF, probs = 0.97)


ymin <-  min(Esve$SVF)


ylim <- c(ymin, ymax)


ctmm::plot(Esve, CTMM = theosve, col.CTMM =  "red",  col = "blue",
           
           
                lty = 6, lwd = 1.5, pch = 5, mgp = c(1.25, 0.25, 0), tcl = -0.2,
     
     
                level = 0,  fraction = p, ylim = ylim)


legend("bottom", title = Identity, legend = Legend, inset = c(0.03, 0.025), 
       
       
       lty = 6, lwd = 1.5,  ncol = 2, col = c("red",  "blue"), 
       
       
       cex = 0.56,   xpd = NA)


dev.off()





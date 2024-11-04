
# plot theoretical semivariance and empirical semivariance


rm(list=ls(all=TRUE))


semivariance_plot <- local({
  

semivariance_plot <- function()
{
  
  
source("./Rcodes/loadRpackagesR.R")


beargroup <-unname(source("./Rcodes/groupsofblackbears.R")$value)


current_directory <- getwd()


current_working_directory1 <- paste(current_directory, "/Results", "/", 
                                    
                                    
                                    
                                    "EstimatedSVF.RData", sep = "")


loadSemv <- paste(current_working_directory1)


load(loadSemv)



current_working_directory1 <- paste(current_directory, "/Results", "/",
                                    
                                    
                                    
                                    "BestModels.RData", sep = "")


loadModel <- paste(current_working_directory1)


load(loadModel)


# Empirical variogram from movement data


# Plot variogram with confidence intervals 


# Estimate of theoretical semivariance function


# group and bear


for(qa in 1:length(beargroup))
{

 
for(gh in 1:dim(beargroup[[qa]])[1]) 

{  
  
theosve <- BestModels[[qa]][[gh]] 


# Empirical semivariance function


Esve <- EstimatedSVF[[qa]][[gh]]




m <- length(theosve)



BearCode <- substr(unlist(unname(slot(theosve, "info")[1])), 3, 1000000L)



Identity <- paste("Bear:", BearCode)


Legend <- c("Theoretical", "Empirical")


par(family = "serif", pin = c(width = 6.86, height = 6),  mai= c(0.7,  0.5, 0.5, 0.5), pch = 46, cex = 0.5)


ctmm::plot(Esve, CTMM = theosve, col.CTMM =  "red",  col = "blue",  lty = 2, lwd = 1.75,
           
           
           mgp = c(2.5, 0.8, 0), level = 0,  fraction = 1,
           
           
           ylim = c(min(Esve$SVF),  max(Esve$SVF)))


legend("bottom", title = Identity,  legend = Legend, inset = c(0.05, 0.05), lty = 2,
       
       
       lwd = 1.75,  ncol = 2, col = c("red", "blue"), cex = 0.35, xpd = NA)


Sys.sleep(1)


current_working_dir <- paste(current_directory, "/Results", "/", 
                                    
                                    
                                "Semivariance", BearCode, ".pdf", sep = "")



dev.copy2pdf(file = current_working_dir)


}

}

}

})



do.call("semivariance_plot", list())





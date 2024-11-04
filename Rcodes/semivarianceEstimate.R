

# Semivariance (or variogram) estimation


rm(list=ls(all=TRUE))


set.seed(234567) 


DataForakdehr <- source("./Rcodes/akdehrDataRead.R")$value


xyt <- DataForakdehr$xyt


coded <- DataForakdehr$coded


semivariance_Fun <- local({ 


semivariance_Fun <- function(xyt, coded)

{


# Parameter estimates 


EstimatedSVF <- vector("list", length(coded))


EstimatedCTMMParam <- vector("list", length(coded))


# i varies from  1, 2, 3, 4 for group 1 in xyt


# i varies from  1, 2, 3, . . ., 8 for group 2 in xyt



sd_dt <- vector("list", length(coded))



for(k in 1:length(xyt))

{


for(i in 1:length(xyt[[k]]))

{ 


sd_dt[[k]][[i]] <- diff(xyt[[k]][[i]]$t)


}

}



# Fit continuous time stochastic process models and 


# select the best model that fit the data



for(k in 1:length(xyt))
  
{
  
for(i in 1:length(xyt[[k]]))

{ 


dt <- quantile(sd_dt[[k]][[i]], probs = 0.85)


# Empirical variogram from movement data


vg.bear <- variogram(xyt[[k]][[i]], fast  =  FALSE, CI = "Gauss", dt = dt, error = TRUE)


EstimatedSVF[[k]][[i]] <- vg.bear


# Fit theoretical variogram to the Empirical variogram


EstimatedCTMMParam[[k]][[i]] <- variogram.fit(vg.bear,  name = "GUESS", 
                                            
                                            
                                            fraction = 1, interactive = FALSE)




}

}  



# Save parameter estimates

  
current_directory <- getwd()


FName1 <- paste(current_directory, "/", "Results", "/", "EstimatedSVF", ".RData", sep = "")



save(EstimatedSVF, file = FName1)



FName2 <- paste(current_directory, "/", "Results", "/", "EstimatedCTMMParam", ".RData", sep = "")



save(EstimatedCTMMParam, file = FName2)



# Fit continuous time stochastic process models and 


# select the best model that fit the data

FittedCTMMs <- lapply(1:length(xyt), 
                      
                      function(k){
                        
                        lapply(1:length(xyt[[k]]), 
                               
                               function(i){
                                 
                                 ctmm.select(xyt[[k]][[i]], EstimatedCTMMParam[[k]][[i]], 
                                             
                                             method = "HREML",  IC = "AIC", 
                                             
                                             trace = TRUE, verbose = TRUE)
                                 
                               })})



BestModels <- vector("list", length(coded))


# Best model 

# Best model 


for (k in 1:length(xyt))

{

for (i in 1:length(xyt[[k]]))

{

FittedModels <- FittedCTMMs[[k]][[i]]


BestModels[[k]][[i]] <- FittedModels[[1]]

} 

}


FName3 <- paste(current_directory, "/", "Results", "/", "BestModels", ".RData", sep = "")


save(BestModels, file = FName3)


FName4 <- paste(current_directory, "/", "Results", "/", "FittedCTMMs", ".RData", sep = "")



save(FittedCTMMs, file = FName4)



return(list(xyt = xyt, BestModels = BestModels, FittedCTMMs = FittedCTMMs, 
            
            
            EstimatedCTMMParam = EstimatedCTMMParam, 
            
            
            EstimatedSVF = EstimatedSVF, coded = coded))

}


})



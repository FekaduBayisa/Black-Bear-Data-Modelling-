
# Autocorrelated Kernel Density estimation


# Information criterion used for model selection


rm(list=ls(all=TRUE))


set.seed(234567) 


SemiVarianceEst <- source("./Rcodes/semivarianceEstimate.R")$value


semivariance <- do.call(SemiVarianceEst, list(xyt, coded))


current_directory <- getwd()


FName4 <- paste(current_directory, "/", "Results", "/", "Semivariance", ".RData", sep = "")



save(semivariance, file = FName4)



xytz <- semivariance$xyt



coded <- semivariance$coded


BestMODELS <- semivariance$BestModels




# The empirical semivariance plots suggest that we need to exclude 



# bear 503, 505, and 504 from the home range analysis 



exclude <- c(503, 505, 504)



xyt <- vector("list", length(xytz))



BestModels <- vector("list", length(BestMODELS))




for(gh in 1:length(xytz))
{
  
  
code <- coded[[gh]]$code 


k <- 1 
  

for(dh in 1:length(code))
{
  
 
if(!(code[dh]%in% exclude)) 
{
  
  
xyt[[gh]][[k]] <- xytz[[gh]][[dh]]
 


BestModels[[gh]][[k]] <- BestMODELS[[gh]][[dh]]



k <- k + 1


}
  
  
}

  
}





akde_Fun <- local({ 
  
  
akde_Fun <- function(xyt, BestModels)
  
{

  
# Autocorrelated Kernel density estimate
  
  
akdeEstimat <- lapply(1:length(xyt), 
                      
                      function(k){
                        
                       lapply(1:length(xyt[[k]]), 
                               
                               
                               function(i){
                                 
                                 akde(xyt[[k]][[i]], CTMM = BestModels[[k]][[i]])
                                 
                               })})


FName5 <- paste(current_directory, "/", "Results", "/", "akdeEstimat",  ".RData", sep = "")
                
                
              

save(akdeEstimat, file = FName5)



return(list(akdeEstimates = akdeEstimat))


}


})



Estakde <- do.call("akde_Fun", list(xyt, BestModels))






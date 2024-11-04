

# Simulate from J-function

envelope_simulateJfun <- local({

  
envelope_simulateJfun <- function(bear23pem0, JcrossInhom)
{
 
  
current_directory <- getwd()


current_working_directory3 <- paste(current_directory, "/Plots/col.RData", sep = "")


load(current_working_directory3) 


current_working_directory7 <- paste(current_directory, "/Results/IntEstResults2080R.RData", sep = "")


load(current_working_directory7)


# Type of Envelope: global (TRUE) and Pointwise (FALSE) envelope


typeOfEnvelope <- list(TRUE, FALSE)



# Do we have data for the two groups of black bears?

# Variogram data analysis may exclude the black 

# bears from spatial interaction analysis


Resultsmodel20800 <- Resultsmodel2080


bear23pem <- list()


Resultsmodel2080 <- list()


k <- 1


for(ng in 1:length(bear23pem0))
{

if(!(is_empty(bear23pem0[[ng]]$xym))) 
{

bear23pem[[k]] <- bear23pem0[[ng]]


Resultsmodel2080[[k]] <- Resultsmodel20800[[ng]]


k <- k + 1

} 

}


ngroup <- length(bear23pem)

    
for (group in 1:ngroup)
{
  
bearid <- bear23pem[[group]]$bearid


permbear <- bear23pem[[group]]$permbear


windows <- bear23pem[[group]]$windows


X <-  bear23pem[[group]]$xym


ngroup <-  bear23pem[[group]]$ngroup


nrank <- bear23pem[[group]]$nrank


nsim <-   bear23pem[[group]]$nsim

  
Resultsmodel <- Resultsmodel2080[[group]]
 

# Simulate for global envelope and  pointwise envelope


for (t in 1:length(typeOfEnvelope))
{
  
  
global <- typeOfEnvelope[[t]]

  
for (g in 1:dim(permbear)[1])
{
  
  
ij <- permbear[g,]


i <- ij[,1]


j <- ij[,2]


YY <- X[X$marks == i | X$marks == j]


ki <- bearid[bearid$id %in% i, 2]


kj <- bearid[bearid$id %in% j, 2]


windi <- as.owin(windows[[ki]])


windj <- as.owin(windows[[kj]])


# We approximated the irregular boundary by the smallest 


# rectangle containing point patterns


# edge = 'torus' is only meaningful for rectangular windows


# rshift works for edge = 'torus'


winn <- expand.owin(boundingbox(union.owin(windi, windj)), distance = 5)


yymarks <- droplevels(YY$marks)


yy <- ppp(YY$x, YY$y, window = winn, marks = yymarks)


sp::plot(yy, pch = 46, use.marks = TRUE, 
     
     
     cols = c(col[ki, 1], "orange"), main = NULL)



I <- (yy$marks == i)


J <- (yy$marks == j)


lam <- Resultsmodel$lambda[[g]]


lambdaI <- Resultsmodel$lambdaI[[g]]


lambdaJ <- Resultsmodel$lambdaJ[[g]]


model <- Resultsmodel$model[[g]]



# global = TRUE, Global envelope


# global = FALSE, Pointwise envelope


# use.theory = FALSE, Pointwise envelope


# use.theory = FALSE, Global envelope

if(global) 
{
  
Envelope <- spatstat.explore::envelope(yy,  fun = JcrossInhom,  nsim = nsim, 
                                   
                                   
                                   I = I, J = J,  i = i, j = j, nrank = nrank,
                                   
                                   
                                   simulate = expression(rshift(yy, which = j, radius = 150)),
                                   
                                   
                                   lambdaI = lambdaI , lambdaJ = lambdaJ, lam = lam, 
                                   
                                   
                                   model = model, alternative = "two.sided", global = global,
                                   
                                   
                                   use.theory = FALSE, clipdata = TRUE, savefuns = TRUE, 
                                   
                                   
                                   savepatterns = TRUE,  nsim2 = nsim, verbose = TRUE)


current_directory <- getwd() 


FName <- paste(current_directory, "/", "Results", "/", "GlobalEnvelopeNonHom2500J", 
               
               as.character(group), as.character(g),  ".RData", sep = "")


save(Envelope, file = FName)

  
}else
{
    
Envelope <- spatstat.explore::envelope(yy,  fun = JcrossInhom,  nsim = nsim, 
                                       
                                       
                                       I = I, J = J,  i = i, j = j, nrank = nrank,
                                       
                                       
                                       simulate = expression(rshift(yy, which = j, radius = 150)),
                                       
                                       
                                       lambdaI = lambdaI , lambdaJ = lambdaJ, lam = lam, 
                                       
                                       
                                       model = model, alternative = "two.sided", global = global,
                                       
                                       
                                       use.theory = FALSE, clipdata = TRUE, savefuns = TRUE, 
                                       
                                       
                                       savepatterns = TRUE,  nsim2 = nsim, verbose = TRUE)


current_directory <- getwd() 


FName <- paste(current_directory, "/", "Results", "/", "PointWiseEnvelopeNonHom2500J", 
               
               as.character(group), as.character(g),  ".RData", sep = "")


save(Envelope, file = FName)

}
  
  
}


}



}


}


})




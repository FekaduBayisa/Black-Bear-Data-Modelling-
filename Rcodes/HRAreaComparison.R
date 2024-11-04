


rm(list = ls(all=TRUE))



HRareacpm <- local({
  
  
HRareacpm <- function()

{
  
  
source("./Rcodes/loadRpackagesR.R")



beargroup <- source("./Rcodes/groupsofblackbears.R")$value



exclude <- c(503, 505, 504)



beargroup <- unname(beargroup)



bearg <- do.call(rbind, beargroup)



bearg <- bearg[order(bearg$recode),]



bearg <- bearg[!(bearg$code%in%exclude),] 



bearg$recode <- 1:length(bearg$recode)



rownames(bearg) <-1:length(bearg$recode)



bearcode <- list()


for(sd in 1:length(beargroup))
{
  
 
groupbear <-  beargroup[[sd]]
  
  

bearcode[[sd]] <- bearg[bearg$code%in%beargroup[[sd]]$code,] 
  
}



Rnames0 <- do.call(rbind, bearcode)


Rnames <- Rnames0[, 1]



cdr <- getwd()



# akde: 50 % and 95% home range


probb <- c(0.95, 0.5)


n <- 0


beargroup <- bearcode

  
for(hb in 1:length(beargroup))
{
  
  
n <-   n + dim(beargroup[[hb]])[1]
  
  
}  
  


Aak  <- data.frame(matrix(nrow = n, ncol = length(probb)))



names(Aak) <-c("95%", "50%")


DIR <- paste(cdr, "/Results", "/", "akdeEstimat", ".RData", sep = "")



load(DIR)



for(vb in 1:length(probb))
  
{

  
k <- 0
  


for(dv in 1:length(beargroup))
  
{

  
for (v in 1:length(akdeEstimat[[dv]])) 
{
  
  
k <- k + 1


if(vb == 1)
{

Aak[k, vb] <-  summary(akdeEstimat[[dv]][[v]], convex = FALSE ,level = 0, level.UD = 0.95, units = TRUE)$CI[2]
  
}

else{
 
summ <- summary(akdeEstimat[[dv]][[v]], convex = FALSE ,level = 0, level.UD = 0.5, units = TRUE)$CI[2]
 

if(summ > Aak[k, (vb-1)]) 
{
  
# Convert hectare to km^2 ( 100 hectare = 1km^2)
  
  
Aak[k, vb] <- summary(akdeEstimat[[dv]][[v]], convex = FALSE ,level = 0, level.UD = 0.5, units = TRUE)$CI[2]/100
  
}

else{ 
  
  
Aak[k, vb] <-  summary(akdeEstimat[[dv]][[v]], convex = FALSE ,level = 0, level.UD = 0.5, units = TRUE)$CI[2]


}
  
}


}

}

}


pdf(file = paste(cdr, "/Results", "/", "HRareaAKDE.pdf", sep = ""))


Aak <- round(Aak, digits = 3)


rownames(Aak) <- Rnames


grid.table(Aak)


dev.off()






# kde


# In hectare, divide it by 100 to get in km^2


probb <- c(0.95, 0.5)


Arkern  <- data.frame(matrix(nrow = n, ncol = length(probb)))


names(Arkern) <-c("95%", "50%")



for(vb in 1:length(probb))
{

  
hr <- 100*probb[vb]
  

k <- 0
  

for(dv in 1:length(beargroup))
{
  

   
DIR <- paste(cdr, "/Results", "/", "kdeEstimate", dv, hr, ".RData", sep = "")


load(DIR)


for (v in 1:length(homerange$area)) 
{
  
  
k <- k + 1 
  

Arkern[k, vb] <- homerange$area[v]/100
  

}

}

}


pdf(file = paste(cdr, "/Results", "/", "HRareaKDE.pdf", sep = ""))


Arkern <- round(Arkern, digits = 3)


rownames(Arkern) <- Rnames



grid.table(Arkern)


dev.off()





# mcp

# In hectare, divide it by 100 to get in km^2


probb <- c(0.95, 0.5)


HRmcp  <- data.frame(matrix(nrow = n, ncol = length(probb)))


names(HRmcp ) <-c("95%", "50%")



for(vb in 1:length(probb))
{
  

hr <- 100*probb[vb]


k <- 0


for(dv in 1:length(beargroup))
{



DIR <- paste(cdr, "/Results", "/", "mcpEstimat", dv, hr, ".RData", sep = "")


load(DIR)


for (v in 1:length(mcp$area)) 
{


k <- k + 1 


HRmcp[k, vb] <- mcp$area[v]/100


}

}

}


pdf(file = paste(cdr, "/Results", "/", "HRareaMCP.pdf", sep = ""))


HRmcp <- round(HRmcp, digits = 3)


rownames(HRmcp) <- Rnames



grid.table(HRmcp)


dev.off()



HRAREA <- cbind.data.frame(HRmcp, Arkern, Aak)


rownames(HRAREA) <- Rnames


pdf(file = paste(cdr, "/Results", "/", "HRAREA.pdf", sep = ""))


grid.table(HRAREA)


dev.off()



}


})


do.call("HRareacpm", list())




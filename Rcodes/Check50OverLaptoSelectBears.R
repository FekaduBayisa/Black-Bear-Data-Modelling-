

# Are 50% home ranges (core areas) overlap? 

# If the core homerange of two black bears overlap, then we will continue 

# to model the spatial interaction between the black bears


# plot pairwise homeranges


rm(list=ls(all=TRUE))


check50_overLap_select_bears <-local({
  
  
check50_overLap_select_bears <- function()

{
  
  
source("./Rcodes/loadRpackagesR.R")


beargroup <- unname(source("./Rcodes/groupsofblackbears.R")$value)


# Group 1

current_directory <- getwd()


current_working_directory1 <- paste(current_directory, "/Results", sep = "")


allRData <- list.files(path = current_working_directory1, pattern = "\\.RData$", full.names = TRUE)


akdeEstimatFiles <- allRData[grepl("akdeEstimat50", allRData)]


par(bty="n",  xaxt = "n", yaxt = "n", col.axis = "white", col.lab = "white", 
    
    
    tck = 0, mai= c(0.7,  0.5, 0.5, 0.5))



R <- list()


for(gr in 1:length(beargroup))
{
  
  
R[[gr]] <- beargroup[[gr]]$code
 

}


load(akdeEstimatFiles)


for(sd in 1:length(R))
{

RR <- R[[sd]]  


akdeBear <- akdeEstimat[[sd]]


for(dg in 1:(length(RR) -1))
{
  
for(hg in (dg + 1):length(RR)) 
{  
   
contour(akdeBear[[dg]]$r, z = akdeBear[[dg]]$CDF, col = "red", 
        
        
        level = 0.5, lwd = 2, drawlabels = FALSE)


contour(akdeBear[[hg]]$r, z = akdeBear[[hg]]$CDF, col = "blue", 
        
        
        level = 0.5, lwd = 2, drawlabels = FALSE, add= TRUE)


A <- as.character(RR[dg])


B <- as.character(RR[hg])


legend("bottom",  inset = c(-0.5, -0.05),  c(A,  B), col = c("red", "blue"), 
       
       lty = 1, lwd = 2, ncol = 2, cex = 0.35, xpd = NA, title = "AKDE")

Sys.sleep(1)

}
  
  
}


}


# We also reselect based on 50% home range obtained by KDE


allRData <- list.files(path = current_working_directory1, pattern = "\\.RData$", full.names = TRUE)


cdir <- getwd()


for(sd in 1:length(R))
{

RR <- R[[sd]]  


FP <- paste(cdir, "/Results", "/", "kdeEstimate", sd, 50, ".RData", sep = "")


load(FP) 


HR0 <- substring(homerange$id, 3)


for(dg in 1:(length(RR) -1))
{

hkde1 <- homerange[HR0 == HR0[dg],] 
  
  
for(hg in (dg + 1):length(RR)) 
{  


hkde2 <- homerange[HR0 == HR0[hg],] 


sp::plot(hkde1, border = "red", lwd = 2)


sp::plot(hkde2, border = "blue", lwd = 2, add = TRUE)


A <- as.character(RR[dg])


B <- as.character(RR[hg])


legend("bottom",  inset = c(-0.75, -0.15),  c(A,  B), col = c("red", "blue"), 
       
     
     lty = 1, lwd = 2, ncol = 2, cex = 0.35, xpd = NA, title = "KDE")


Sys.sleep(1)

}


}


}





# We also reselect based on 50% home range obtained by MCP



allRData <- list.files(path = current_working_directory1, pattern = "\\.RData$", full.names = TRUE)


cdir <- getwd()


for(sd in 1:length(R))
{

RR <- R[[sd]]  


FP <- paste(cdir, "/Results", "/", "mcpEstimat", sd, 50, ".RData", sep = "")


load(FP) 


HR0 <- substring(mcp$id, 3)


for(dg in 1:(length(RR) -1))
{

hkde1 <- mcp[HR0 == HR0[dg],] 


for(hg in (dg + 1):length(RR)) 
{  


hkde2 <- mcp[HR0 == HR0[hg],] 


sp::plot(hkde1, border = "red", lwd = 2)


sp::plot(hkde2, border = "blue", lwd = 2, add = TRUE)


A <- as.character(RR[dg])


B <- as.character(RR[hg])


legend("bottom",  inset = c(-0.75, -0.15),  c(A,  B), col = c("red", "blue"), 


lty = 1, lwd = 2, ncol = 2, cex = 0.35, xpd = NA, title = "MCP")


Sys.sleep(1)

}


}


}

}

})


do.call("check50_overLap_select_bears", list())










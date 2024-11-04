
rm(list=ls(all=TRUE))


akde_homerange_plot <- local({
  
  
akde_homerange_plot <- function()
{
  

source("./Rcodes/loadRpackagesR.R")


DataAkdehr <- source("./Rcodes/akdehrDataRead.R")$value


set.seed(234567) 


xytz <- DataAkdehr$xyt
  
  
coded <- DataAkdehr$coded


probb <- DataAkdehr$probb


exclude <- c(503, 505, 504)


xyt <- vector("list", length(xytz))


for(gh in 1:length(xytz))
{


code <- coded[[gh]]$code 


k <- 1 


for(dh in 1:length(code))
{


if(!(code[dh]%in% exclude)) 
{


xyt[[gh]][[k]] <- xytz[[gh]][[dh]]


k <- k + 1


}


}


}
  
  
codd <- do.call(rbind, coded)



codd <- codd[!codd$code%in%exclude,]


  
codd <- codd[order(codd$recode),]



codd$recode <- 1:length(codd$recode)


reCode <- list()


for(dds in 1:length(coded))
{
  
  
reCode[[dds]] <- codd[codd$code%in%coded[[dds]]$code,]


}



load("./Results/akdeEstimat.RData") 


# Empirical variogram from movement data


# Plot variogram with confidence intervals 


len <- length(xyt)


par(mfrow = c(2, 2))


# AKDE


nn <-list()


for(kd in 1:len)
  
{

xty <- xyt[[kd]]


nn[[kd]] <-length(xty)


}



for(dk in 1:len)

{

colors <- c()
  
  
xyz <-  xyt[[dk]] 


if(dk == 1)
{

  
load("./Plots/col1.RData")

  
col <- sample(col, size = nn[[dk]])


for(dg in 1:length(xyz))
{
  
  
mnb <- length(xyz[[dg]]$id)
  

coll <- rep(col[dg], mnb) 
  
  
colors <- c(colors, coll) 
  
}


}
else{

load("./Plots/col2.RData")

  
col <- sample(col, size = nn[[dk]])


for(dg in 1:length(xyz))
{


mnb <- length(xyz[[dg]]$id)


coll <- rep(col[dg], mnb) 


colors <- c(colors, coll) 

}


}



for(ds in length(probb):1)
{


if(probb[ds] == 0.5)
{

  
n <- length(col)


akdeB <- akdeEstimat[[dk]]


akdeBear <- akdeB


p <- probb[ds]


par(bty="n",  xaxt = "n", yaxt = "n", col.axis = "white", col.lab = "white", 

    
tck = 0, mai= c(0.7,  0.5, 0.5, 0.5), pch = 46, cex = 1.25)


sp::plot(xyz, col = col)


for(i in 1:length(xyz))

{

  
akdeB[[i]]$r$x <-akdeBear[[i]]$r$x/1000


akdeB[[i]]$r$y <-akdeBear[[i]]$r$y/1000


contour(akdeB[[i]]$r, z = akdeB[[i]]$CDF, col = col[i], level = p, 
        
        
        lwd = 2, drawlabels = FALSE, add = TRUE)

} 


if(dk == 1)
  
{
 
   
Legend <- as.character(reCode[[dk]]$code)
  
  
legend("bottom", inset = c(-0.75, -0.15), title = "Utilisation Distributions of Black Bears",
         
       Legend, box.lwd = 0.25, border="gray",
         
         lty = 1, lwd = 1.5,  col = col,   ncol = n, cex = 0.35, xpd = NA) 
  
}

else{
  
Legend <- as.character(reCode[[dk]]$code)
  
  
legend("bottom",inset = c(-0.75, -0.22), title="Utilisation Distributions of Black Bears",
         
       Legend,  box.lwd = 0.25, border="gray",
         
         lty = 1, lwd = 1.5,  col = col,   ncol = ceiling(n/2), cex = 0.35, xpd = NA)
  
}



} 

else{
  
  
n <- length(col)


akdeB <- akdeEstimat[[dk]]


akdeBear <- akdeB


p <- probb[ds]


par(bty="n",  xaxt = "n", yaxt = "n", col.axis = "white", col.lab = "white", 

    
tck = 0, mai= c(0.7,  0.5, 0.5, 0.5), pch = 46, cex = 1.25)



sp::plot(xyz, col = col)


for(i in 1:length(xyz))

{

akdeB[[i]]$r$x <-akdeBear[[i]]$r$x/1000


akdeB[[i]]$r$y <-akdeBear[[i]]$r$y/1000


contour(akdeB[[i]]$r, z = akdeB[[i]]$CDF, col = col[i], level = p, 
        
        
        lwd = 2, drawlabels = FALSE, add = TRUE)

} 


if(dk == 1)
  
{

  
Legend <- as.character(reCode[[dk]]$code)
  
  
legend("bottom", inset = c(-0.75, -0.15), title = "Utilisation Distributions of Black Bears",
         
        Legend, box.lwd = 0.25, border="gray",
         
         lty = 1, lwd = 1.5,  col = col,   ncol = n, cex = 0.35, xpd = NA)  
  
}

else{
  
  
Legend <- as.character(reCode[[dk]]$code)
  
  
legend("bottom",inset = c(-0.75, -0.22), title="Utilisation Distributions of Black Bears",
       
       Legend,  box.lwd = 0.25, border="gray",
       
       lty = 1, lwd = 1.5,  col = col,   ncol = ceiling(n/2), cex = 0.35, xpd = NA)

}


}


}


}



dev.copy2pdf(file = "./Results/akde95and50.pdf")


graphics.off()

}

})


do.call("akde_homerange_plot", list())





rm(list=ls(all=TRUE))


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


# AKDE


nn <-list()


for(kd in 1:len)

{

xty <- xyt[[kd]]


nn[[kd]] <-length(xty)


}


dev.off()


# Bear group: dk <- 1, or dk <- 2 


# Home range (50% or 95%): ds <- 1, or ds <- 2


dk <- 1; ds <- 1 # 50%


# dk <- 2; ds <- 1 # 50%


# dk <- 1; ds <- 2 # 95%


# dk <- 2; ds <- 2 # 95%


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

if(dk == 2)
{

load("./Plots/col2.RData")


col <- sample(col, size = nn[[dk]])


for(dg in 1:length(xyz))
{


mnb <- length(xyz[[dg]]$id)


coll <- rep(col[dg], mnb) 


colors <- c(colors, coll) 

}


}




if(probb[ds] == 0.5)
{



cwdd <- getwd()



FName <- paste(cwdd, "/Results/", "akdeJournal", dk,
     
     
     100*probb[ds], ".png", sep = "")



png(filename = FName, width = 119, height = 75, units = "mm", 


pointsize = 12, res = 1200)


par(family = "Arial",  mai= c(0.85,  0.85,  0.5,  0.5), 


bty="n",  xaxt = "n", yaxt = "n", 


col.axis = "white", col.lab = "white", 


tck = 0, cex = 0.56)



n <- length(col)


akdeB <- akdeEstimat[[dk]]


akdeBear <- akdeB


p <- probb[ds]



ctmm::plot(xyz,  pch = 46, col = col)


for(i in 1:length(xyz))

{


akdeB[[i]]$r$x <-akdeBear[[i]]$r$x/1000


akdeB[[i]]$r$y <-akdeBear[[i]]$r$y/1000


contour(akdeB[[i]]$r, z = akdeB[[i]]$CDF, col = col[i], 


level = p, lwd = 1.2,  drawlabels = FALSE, add = TRUE)

} 


if(dk == 1)

{


Legend <- as.character(reCode[[dk]]$code)


legend("bottom", inset = c(-0.75, 0.2), 


title = "Home Ranges of Black Bears",


Legend, box.lwd = 0.25, border="gray",


lty = 1, lwd = 1,  col = col,   



ncol = n, cex = 0.56, xpd = NA) 

}

else{

Legend <- as.character(reCode[[dk]]$code)


legend("bottom",inset = c(-0.75, 0.19), 


title="Home Ranges of Black Bears",


Legend,  box.lwd = 0.25, border="gray",


lty = 1, lwd = 1,  col = col,


ncol = ceiling(n/2), cex = 0.4, xpd = NA)

}



} 


if(probb[ds] == 0.95)

{


cwdd <- getwd()



FName <- paste(cwdd, "/Results/", "akdeJournal", dk,
     
     
     
     100*probb[ds], ".png", sep = "")



png(filename = FName, width = 119, height = 75, units = "mm", 


pointsize = 12, res = 1200)


par(family = "Arial",  mai= c(0.85,  0.85,  0.5,  0.5), 


bty="n",  xaxt = "n", yaxt = "n",


col.axis = "white", col.lab = "white", 


tck = 0, cex = 0.56)


n <- length(col)


akdeB <- akdeEstimat[[dk]]


akdeBear <- akdeB


p <- probb[ds]


ctmm::plot(xyz,  pch = 46, col = col)


for(i in 1:length(xyz))

{

akdeB[[i]]$r$x <-akdeBear[[i]]$r$x/1000


akdeB[[i]]$r$y <-akdeBear[[i]]$r$y/1000


contour(akdeB[[i]]$r, z = akdeB[[i]]$CDF, 


col = col[i], level = p,


lwd = 1.2, drawlabels = FALSE, add = TRUE)

} 


if(dk == 1)

{


Legend <- as.character(reCode[[dk]]$code)



legend("bottom", inset = c(-0.75, 0.17), 



title = "Home Ranges of Black Bears",


Legend, box.lwd = 0.25, border="gray",


lty = 1, lwd = 1,  col = col,   


ncol = n, cex = 0.56, xpd = NA)  

}

else{


Legend <- as.character(reCode[[dk]]$code)


legend("bottom",inset = c(-0.75, 0.19), 


title="Home Ranges of Black Bears",


Legend,  box.lwd = 0.25, border="gray",


lty = 1, lwd = 1,  col = col,   


ncol = ceiling(n/2), cex = 0.4, xpd = NA)

}


}


dev.off()



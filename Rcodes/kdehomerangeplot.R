

rm(list=ls(all=TRUE))


kde_HRPlot_DataRead <- local({


kde_HRPlot_DataRead <- function()
{


xy <- source("./Rcodes/studyAreaMcpKdeDataFun.R")$value 


beargroup <- source("./Rcodes/groupsofblackbears.R")$value


codebears <- source("./Rcodes/groupsofblackbears.R")$value


beargroup0 <-  unname(beargroup)



codebears0 <-  unname(codebears)



xbdy <- vector("list", length = length(beargroup0))


for(hg in 1:length(beargroup0))
{

  
for(bc in 1:dim(beargroup0[[hg]])[1])

{


xbdy[[hg]][[bc]] <- xy[grepl(as.character(codebears0[[hg]]$code[bc]), 
                             
                             
                             as.character(xy$id)), ]


}

}


ybdx <- list()


for(gh in 1:length(beargroup0))
{

xbmdy <- do.call(rbind, xbdy[[gh]])


xbndy <- as.data.frame(xbmdy)


id <- factor(xbndy$id)


xbhdy <-  as.data.frame(id) # id of the bears


coordinates(xbhdy) <- as.data.frame(xbndy[,c(2, 3)])


ybdx[[gh]] <- xbhdy

}

return(list(ybdx = ybdx, codebears0 = codebears0))

}


})




xbdycode <- do.call("kde_HRPlot_DataRead", list())



ybdx <- xbdycode$ybdx



codebears0 <- xbdycode$codebears0


xytz <- xbdycode$ybdx


coded <- xbdycode$codebears0


set.seed(234567) 


exclude <- c(503, 505, 504)


xyt <- list()


for(gh in 1:length(xytz))
{


xty <- xytz[[gh]]


TF <- !as.numeric(substr(xty$id, 3, 5))%in%exclude 


xyt[[gh]]<- xty[TF, ]


}


for(cv in 1:length(xyt))
{

xyt[[cv]]$id <- droplevels(xyt[[cv]]$id)

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


ybdx <- xyt


codebears0 <- reCode


# Plot

kde_homerange_plot <- local({


kde_homerange_plot <- function(ybdx, codebears0)
{

  
probb <- c(50, 95)
  

BearCode <- source("./Rcodes/groupsofblackbears.R")$value



par(mfrow = c(2, 2))



len <- length(BearCode)


# KDE

nn <-list()


for(kd in 1:len)
  
{
  
  
nn[[kd]] <-length(unique(ybdx[[kd]]$id))
  
  
}

for(dk in 1:len)

{
  
  
colors <- c()
  
  
if(dk == 1)
{

yxy <- ybdx[[dk]]


load("./Plots/col1.RData")


col <- sample(col, size = nn[[dk]])


codd2d <- codebears0[[dk]] 


for(cd in 1:length(codd2d$code))  
{


mnb <- sum(as.numeric(substr(yxy$id, 3, 5)) == codd2d$code[cd])


coll <- rep(col[cd], mnb) 


colors <- c(colors, coll) 

}


}
else{

# Group 2


yxy <- ybdx[[dk]]


load("./Plots/col2.RData")


col <- sample(col, size = nn[[dk]])


codd2d <- codebears0[[dk]] 


for(cd in 1:length(codd2d$code))  
{


mnb <- sum(as.numeric(substr(yxy$id, 3, 5)) == codd2d$code[cd])


coll <- rep(col[cd], mnb) 


colors <- c(colors, coll) 

}
  
}
  
  

for(ds in 1:length(probb))
{


if(probb[ds] == 50)
{



if(dk == 1)

{

load("./Results/kdeEstimate150.RData")


n <- length(col)


p <- probb[ds]


par(bty="n",  xaxt = "n", yaxt = "n", col.axis = "white", col.lab = "white", 
    
    
    tck = 0, mai= c(0.7,  0.5, 0.5, 0.5), pch = 46, cex = 1.25)


sp::plot(yxy[,1],  pch = 46,  cex = 1.25, col = colors)



sp::plot(homerange, border = col, lwd = 2.25, add = TRUE)

  
  
codebb <- as.character(codebears0[[dk]]$code)
  
  
legend("bottom", inset = c(-0.75, -0.15), title = "Home Ranges of Black Bears",
       
       codebb, box.lwd = 0.25, border="gray",
       
       lty = 1, lwd = 1.5,  col = col,   ncol = n, cex = 0.35, xpd = NA) 

}

else{

load("./Results/kdeEstimate250.RData")


n <- length(col)


p <- probb[ds]


par(bty="n",  xaxt = "n", yaxt = "n", col.axis = "white", col.lab = "white", 
    
    
    tck = 0, mai= c(0.7,  0.5, 0.5, 0.5), pch = 46, cex = 1.25)


sp::plot(yxy[,1],  pch = 46,  cex = 1.25, col = colors)



sp::plot(homerange, border = col, lwd = 2.25, add = TRUE)

codebb <- as.character(codebears0[[dk]]$code)
  

legend("bottom",inset = c(-0.75, -0.15), title="Home Ranges of Black Bears",
       
       codebb,  box.lwd = 0.25, border="gray",
       
       lty = 1, lwd = 1.5,  col = col,   ncol = ceiling(n/2), cex = 0.35, xpd = NA)

}



} 

else{


if(dk == 1)

{
  
load("./Results/kdeEstimate195.RData") 


n <- length(col)


p <- probb[ds]


par(bty="n",  xaxt = "n", yaxt = "n", col.axis = "white", col.lab = "white", 
    
    
    tck = 0, mai= c(0.7,  0.5, 0.5, 0.5), pch = 46, cex = 1.25)



sp::plot(yxy[,1],  pch = 46,  cex = 1.25, col = colors)



sp::plot(homerange, border = col, lwd = 2.25, add = TRUE)

  
  
codebb <- as.character(codebears0[[dk]]$code)


legend("bottom", inset = c(-0.75, -0.15), title = "Home Ranges of Black Bears",
       
       codebb, box.lwd = 0.25, border="gray",
       
       lty = 1, lwd = 1.5,  col = col,   ncol = n, cex = 0.35, xpd = NA)  

}

else{

load("./Results/kdeEstimate295.RData") 


n <- length(col)


p <- probb[ds]


par(bty="n",  xaxt = "n", yaxt = "n", col.axis = "white", col.lab = "white", 
    
    
    tck = 0, mai= c(0.7,  0.5, 0.5, 0.5), pch = 46, cex = 1.25)



sp::plot(yxy[,1],  pch = 46,  cex = 1.25, col = colors)



sp::plot(homerange, border = col, lwd = 2.25, add = TRUE)


  
codebb <- as.character(codebears0[[dk]]$code)
  
  
legend("bottom",inset = c(-0.75, -0.15), title="Home Ranges of Black Bears",
       
       codebb,  box.lwd = 0.25, border="gray",
       
       lty = 1, lwd = 1.5,  col = col,   ncol = ceiling(n/2), cex = 0.35, xpd = NA)

}


}


}


}


dev.copy2pdf(file = "./Results/kde95and50.pdf")


graphics.off()

}

})


do.call("kde_homerange_plot", list(ybdx, codebears0))




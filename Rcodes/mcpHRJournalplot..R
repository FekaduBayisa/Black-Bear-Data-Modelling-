

rm(list=ls(all=TRUE))


mcp_HRPlot_DataRead <- local({


mcp_HRPlot_DataRead <- function()
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




xbdycode <- do.call("mcp_HRPlot_DataRead", list())



xytz <- xbdycode$ybdx


codebears0 <- xbdycode$codebears0


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


probb <- c(50, 95)


BearCode <- source("./Rcodes/groupsofblackbears.R")$value



len <- length(BearCode)


# MCP


nn <-list()


for(kd in 1:len)

{


nn[[kd]] <-length(unique(ybdx[[kd]]$id))


}


# Bear group: dk <- 1, or dk <- 2 


# Home range (50% or 95%): ds <- 1, or ds <- 2


# dk <- 1; ds <- 1 # 50%


 dk <- 2; ds <- 1 # 50%


# dk <- 1; ds <- 2 # 95%


# dk <- 2; ds <- 2 # 95%


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


if(dk == 2)
{

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

dev.off()


if(probb[ds] == 50)
{

cwdd <- getwd()


FName <- paste(cwdd, "/Results/", "mcpJournal", dk,
       
       
       probb[ds], ".png", sep = "")



png(filename = FName, width = 119, height = 75, units = "mm", 


pointsize = 12, res = 1200)


par(family = "Arial",  mai= c(0.85,  0.85,  0.5,  0.5), 


bty="n",  xaxt = "n", yaxt = "n", 


col.axis = "white", col.lab = "white", 


tck = 0, cex = 0.56)



if(dk == 1)
{

load("./Results/mcpEstimat150.RData")


n <- length(col)


p <- probb[ds]


sp::plot(yxy[,1],  pch = 1,  cex = 0.05, col = colors)



sp::plot(mcp, border = col, lwd = 1, add = TRUE)


codebb <- as.character(codebears0[[dk]]$code)


legend("bottom", inset = c(-0.75, 0.2), title = "Home Ranges of Black Bears",
       
       
       codebb, box.lwd = 0.25, border="gray",
       
       
       lty = 1, lwd = 1,  col = col,   ncol = n, cex = 0.56, xpd = NA) 

}

  
if(dk == 2)
{
  

load("./Results/mcpEstimat250.RData")


n <- length(col)


p <- probb[ds]



sp::plot(yxy[,1],  pch = 1,  cex = 0.05, col = colors)


sp::plot(mcp, border = col, lwd = 1, add = TRUE)


codebb <- as.character(codebears0[[dk]]$code)


legend("bottom", inset = c(-0.75, 0.2), title = "Home Ranges of Black Bears",
       
       
       codebb, box.lwd = 0.25, border="gray",
       
       
       lty = 1, lwd = 1,  col = col,   ncol = ceiling(n/2), cex = 0.4, xpd = NA) 

}


} 


if(probb[ds] == 95)
{


cwdd <- getwd()


FName <- paste(cwdd, "/Results/", "mcpJournal", dk,
         
         
         probb[ds], ".png", sep = "")



png(filename = FName, width = 119, height = 75, units = "mm", 


pointsize = 12, res = 1200)


par(family = "Arial",  mai= c(0.85,  0.85,  0.5,  0.5), 


bty="n",  xaxt = "n", yaxt = "n", 


col.axis = "white", col.lab = "white", 


tck = 0, cex = 0.56)

  
if(dk == 1)

{


load("./Results/mcpEstimat195.RData") 


n <- length(col)


p <- probb[ds]


sp::plot(yxy[,1],  pch = 1,  cex = 0.05, col = colors)



sp::plot(mcp, border = col, lwd = 1, add = TRUE)



codebb <- as.character(codebears0[[dk]]$code)


legend("bottom", inset = c(-0.75, 0.2), title = "Home Ranges of Black Bears",
       
       
      codebb, box.lwd = 0.25, border="gray",
       
       
      lty = 1, lwd = 1,  col = col,   ncol = n, cex = 0.56, xpd = NA) 

}

  
if(dk == 2)
  
{
  
load("./Results/mcpEstimat295.RData") 


n <- length(col)


p <- probb[ds]


sp::plot(yxy[,1],  pch = 1,  cex = 0.05, col = colors)



sp::plot(mcp, border = col, lwd = 1, add = TRUE)



codebb <- as.character(codebears0[[dk]]$code)


legend("bottom",inset = c(-0.75, 0.2), title="Home Ranges of Black Bears",
       
       
       codebb,  box.lwd = 0.25, border="gray",
       
       
       lty = 1, lwd = 1,  col = col,   ncol = ceiling(n/2), cex = 0.4, xpd = NA)

}


}



dev.off()








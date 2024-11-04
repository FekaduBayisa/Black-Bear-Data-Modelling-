
# kernel estimation and the utilization distribution

# Estimation of Kernel Home-Range


rm(list=ls(all=TRUE))


kde_Fun <- local({
  
  
kde_Fun <- function()
{  
  
  
xy <- source("./Rcodes/studyAreaMcpKdeDataFun.R")$value


codebears <- source("./Rcodes/groupsofblackbears.R")$value


xytz <- xy


coded <- codebears


set.seed(234567) 


exclude <- c(503, 505, 504)

  
TF <- !as.numeric(substr(xytz$id, 3, 5))%in%exclude 
  
  
xyt <- xytz[TF, ]
  
  
xyt$id <- droplevels(xyt$id)
  

codd <- do.call(rbind, coded)


codd <- codd[!codd$code%in%exclude,]


codd <- codd[order(codd$recode),]


codd$recode <- 1:length(codd$recode)


rownames(codd) <- 1:length(codd$recode)


reCode <- list()


for(dds in 1:length(coded))
{
  
  
reCode[[dds]] <- codd[codd$code%in%coded[[dds]]$code,]
  
  
}


xy <- xyt


codebears <- reCode


probb <- c(50, 95)


len <- length(codebears)


nn <-list()


for(kd in 1:len)
  
{
  
  
nn[[kd]] <-length(unique(codebears[[kd]]$code))
  
  
}


load("./Plots/col1.RData")


col1 <- sample(col, size = nn[[1]])


load("./Plots/col2.RData")


col2 <- sample(col, size = nn[[2]])


colors1 <- c()


colors2 <- c()


for(dg in 1:length(codebears))
{

codeb <- codebears[[dg]]


for(da in 1:length(codeb$code))

{

mnb <- sum(as.numeric(substr(xy$id, 3, 5))==codeb$code[da])

if (dg == 1)
{
  
coll <- rep(col1[da], mnb) 


colors1 <- c(colors1, coll) 

}

if(dg == 2)

{
  
col22 <- rep(col2[da], mnb) 
  
  
colors2 <- c(colors2, col22) 
  
}

}  
  
}


current_directory <- getwd()


# Group 1


xy1 <- list()

xy2 <- list()


for(vd in 1:length(codebears))
{
  
cdbb <-  codebears[[vd]] 

  
if(vd == 1)
{
  
for(bc in 1:length(cdbb$code))
  
{
  
x1y <- xy[grepl(as.character(cdbb$code[bc]), as.character(xy$id)), ]


x1y$id <- droplevels(x1y$id)


xy1[[bc]] <- x1y
  
  
} 
  
}

if(vd == 2)
{

for(bc in 1:length(cdbb$code))

{
  
x2y  <- xy[grepl(as.character(cdbb$code[bc]), as.character(xy$id)), ]
  

x2y$id <- droplevels(x2y$id)

xy2[[bc]] <- x2y

} 

}

}


xyb1 <- do.call(rbind, xy1)


xy11 <- as.data.frame(xyb1)


id <- factor(xy11$id)


yx1 <-  as.data.frame(id) # id of the bears


coordinates(yx1) <- as.data.frame(xy11[,c(2, 3)])



# Group 2


xyb2 <- do.call(rbind, xy2)


xy22 <- as.data.frame(xyb2)


id <- factor(xy22$id)


yx2 <-  as.data.frame(id) # id of the bears


coordinates(yx2) <- as.data.frame(xy22[,c(2, 3)])




# Group 1

codebb <- substring(unique(yx1[,1]$id), 3)


kde1 <- list()


for (vb in 1:length(probb)) 
  
{

p <- probb[vb]


kernud <- kernelUD(yx1, h = "href", same4all = TRUE, 
                   
                   
                   kern = "bivnorm", grid = 1000)


kernud[[1]]@h


kernud[[2]]@h


homerange <- getverticeshr(kernud,  percent = p)


kde1[[vb]] <- homerange 


FName <- paste(current_directory, "/", "Results", "/", 
               
               
               
              "kdeEstimate", 1,  p, ".RData", sep = "")


save(homerange, file = FName)  

}



for(ds in 1:length(kde1))
{

n <- length(col1)


p <- probb[ds]


sp::plot(yx1[,1],  pch = 46,  cex = 1.25, col = colors1)


sp::plot(kde1[[ds]], border = col1, lwd = 2.25, add = TRUE)


legend("bottom", inset =  c(-0.75, -0.15), 


title = "Home Ranges of Black Bears",


codebb, 


lty = 1, lwd = 2,  col = col1,  


ncol = n, cex = 0.35, xpd = NA)


FName <- paste(current_directory, "/", "Results", "/", 
               
               
               "KDE", 1,  p, ".pdf", sep = "")


dev.copy2pdf(file = FName)


}



# Group 2

kde2 <- list()


codebb <- substring(unique(yx2[,1]$id), 3)


for (vb in 1: length(probb)) 

{


p <- probb[vb]


kernud <- kernelUD(yx2, h = "href", same4all = TRUE, 
                   
                   
                   kern = "bivnorm", grid = 1000)


kernud[[1]]@h


kernud[[2]]@h


homerange <- getverticeshr(kernud,  percent = p)


kde2[[vb]] <- homerange 


FName <- paste(current_directory, "/", "Results", "/", 
               
               
               "kdeEstimate", 2,  p, ".RData", sep = "")


save(homerange, file = FName)  

}



for(ds in 1:length(kde2))
{

n <- length(col2)


p <- probb[ds]


sp::plot(yx2[,1],  pch = 46,  cex = 1.25, col = colors2)


sp::plot(kde2[[ds]], border = col2, lwd = 2.25, add = TRUE)


legend("bottom", inset =  c(-0.75, -0.15), 


title = "Home Ranges of Black Bears",


codebb, 


lty = 1, lwd = 2,  col = col2,  


ncol = ceiling(n/2), cex = 0.35, xpd = NA)


FName <- paste(current_directory, "/", "Results", "/", "KDE", 2,  p, ".pdf", sep = "")


dev.copy2pdf(file = FName)


}

}

})


do.call("kde_Fun", list())




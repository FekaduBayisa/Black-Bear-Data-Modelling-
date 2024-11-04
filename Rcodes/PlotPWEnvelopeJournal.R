

# Statistical test of independent of types


# Spatial interaction: attraction, repulsion, and avoidance


rm(list=ls(all=TRUE))



source("./Rcodes/loadRpackagesR.R")


# Select pairs of black bears that share their core 


# home ranges for at least five months


groupcode00 <- unname(rbind.data.frame(c("487", "488"), c("487", "490"), 
                                   
                                   
                                   c("487", "491"), c("491", "501"),
                                   
                                   
                                   c("488", "487"), c("490", "487"), 
                                   
                                   
                                   c("491", "487"), c("501", "491")))


vectorizeddf0 <- lapply(groupcode00, as.vector)


codegroup <- c()


vectorizeddf <- for(gr in 1:length(vectorizeddf0))
{


codegroup <- c(codegroup, vectorizeddf0[[gr]])


} 

groupcode <- unique(codegroup)            



current_directory <- getwd()


# To read shaplefile data


current_working_directory1 <- paste(current_directory, "/Results", sep = "")



allRData <- list.files(path = current_working_directory1, 
                   
                   
                   pattern = "\\.RData$", full.names = TRUE)



PointWiseEnvelopeNonHom2500 <- allRData[grepl("PointWiseEnvelopeNonHom2500", allRData)]


n <- length(PointWiseEnvelopeNonHom2500)



# vs <- 1, 2, 3, ..., n


vs <- 16



# Pointwise envelope: 


FName <- "PWE"


groupcodeFun <- c("L", "J")


load(PointWiseEnvelopeNonHom2500[vs])


E <- terra::plot(Envelope, cbind(obs, mmean)~ r, legend = FALSE,
               
               
               col = c("blue", "red"), main = NULL, lwd = 2.5, 
               
               
               mgp = c(2.1, 0.75, 0), cex.lab = 1.75)  


dev.off()


InhomcodeB <- unlist(strsplit(E$meaning[1], " "))


InhomB  <- unlist(strsplit(InhomcodeB[4], ","))


codeFun <- InhomB[1]


IsLabelBelongFun <- str_extract(codeFun, groupcodeFun)


TFF <- !is.na(IsLabelBelongFun)


Fun <- groupcodeFun[TFF]


if(Fun == "L")
{

II <- InhomB[2] 


IsLabelB <- str_extract(II, groupcode)


TBF <- !is.na(IsLabelB)


I <- IsLabelB[TBF]


}

if(Fun == "J")
{


I <- InhomB[2]

}

JJ <- InhomB[3]


IsLabelBelongB <- str_extract(JJ, groupcode)


TFB <- !is.na(IsLabelBelongB)


J <- IsLabelBelongB[TFB]


Bcode <- paste(c(I, J), collapse = "")


cwdd <- getwd()


FName <- paste(cwdd, "/Results/", FName, "Journal", Fun, Bcode, ".png", sep = "")



png(filename = FName, width = 119, height = 75, units = "mm", 
    
    
    pointsize = 12, res = 1200)


par(family = "Arial",  mai= c(0.85,  0.85,  0.5,  0.5), 
  
    
    
     tck = 0, cex = 0.56)


E <- terra::plot(Envelope, cbind(obs, mmean)~ r, legend = FALSE,
                 
                 
     col = c("blue", "red"), main = NULL, lwd = 2.5,  ylab = Fun,
                 
                 
      mgp = c(2.1, 0.75, 0), cex.lab = 1.75)  


dev.off()








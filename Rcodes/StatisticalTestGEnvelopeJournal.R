

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


# Global envelope


GlobalEnvelopeNonHom2500 <- allRData[grepl("GlobalEnvelopeNonHom2500", allRData)]



n <- length(GlobalEnvelopeNonHom2500)



# vs <- 1, 2, 3, ..., n



vs <- 1



groupcodeFun <- c("L", "J")



load(GlobalEnvelopeNonHom2500[vs])



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



print(paste("The test is based on", Fun, "-", "function", sep = " "))



if(Fun == "L")
{
  


# MAD test


print(summary(mad.test(Envelope, alternative = "two.sided")))


# DCLF test 


print(summary(dclf.test(Envelope, alternative = "two.sided")))

  
}
  

if(Fun == "J")
{
  
  

# MAD test


print(mad.test(Envelope, alternative = "two.sided"))


# DCLF test 


print(dclf.test(Envelope, alternative = "two.sided"))


}









# Statistical test of independent of types


# Spatial interaction: attraction, repulsion, and avoidance


rm(list=ls(all=TRUE))


statistical_test_envelope <- local({


StatisticalTestEnvelope <- function()
{


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



# Pointwise envelope: 


FName <- "PWE"


groupcodeFun <- c("L", "J")


for (vs in 1:length(PointWiseEnvelopeNonHom2500))

{

load(PointWiseEnvelopeNonHom2500[vs])


E <- terra::plot(Envelope, cbind(obs, mmean)~ r, legend = FALSE,
           
           
           col = c("blue", "red"), main = NULL, lwd = 2.5, 
           
           
           mgp = c(2.1, 0.75, 0), cex.lab = 1.75)  


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
else{


I <- InhomB[2]

}

JJ <- InhomB[3]


IsLabelBelongB <- str_extract(JJ, groupcode)


TFB <- !is.na(IsLabelBelongB)


J <- IsLabelBelongB[TFB]


Bcode <- paste(c(I, J), collapse = "")


Fpath <- paste("./Results", "/", FName, Fun, Bcode, ".pdf", sep = "")


dev.copy2pdf(file = Fpath)

} 



# Global envelope


GlobalEnvelopeNonHom2500 <- allRData[grepl("GlobalEnvelopeNonHom2500", allRData)]


madtest <- list()


dclftest <- list()


for (vs in 1:length(GlobalEnvelopeNonHom2500))

{

load(GlobalEnvelopeNonHom2500[vs])


# MAD test


madtest[[vs]] <- mad.test(Envelope, alternative = "two.sided")


# DCLF test 


dclftest[[vs]] <- dclf.test(Envelope, alternative = "two.sided")


}



FName5 <- paste("./Results", "/", "MADTest", ".RData", sep = "")


save(madtest, file = FName5)



FName6 <- paste("./Results", "/", "DCLF", ".RData", sep = "")


save(dclftest, file = FName6)

}

})


do.call("statistical_test_envelope", list())









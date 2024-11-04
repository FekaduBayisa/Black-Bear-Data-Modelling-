

# Pairs of black bears that share their core home ranges for at least five months


pairs_black_bears <-local({ 


pairs_black_bears <- function()
{


# Group I

mak <- c(495, 503, 505)


bearperm1 <- permutations(n = length(mak), r = 2, v = mak)


for(i in 1:length(mak))

{

bearperm1[bearperm1[,1] == as.character(i), 1] <- as.character(mak[i])

}


# Group II


R22 <- matrix(cbind(487, 487, 487, 487, 488, 
                  
                  491, 488, 490, 491,
                  
                  504, 504, 501), 
            
                  nrow = 6, ncol = 2)



R23 <- R22



R23[ , c(1,2)]  <- R23[ , c(2,1)]



R24 <- rbind(R22, R23)


bearperm2 <- cbind(as.character(R24[, 1]), as.character(R24[, 2]))


return(list(bearperm1 = bearperm1, bearperm2 = bearperm2))


}

})


bearpairs <- do.call("pairs_black_bears", list())





# Bear data and coding


# 12 black bears divided into two groups


groups_of_black_bears <- function()
{
  

# Two groups of black bears 


# 1st group


bbid1 <- rbind.data.frame(c(493, 5), c(495, 6), c(503, 9), c(505, 11))


names(bbid1) <- c("code", "recode")




# 2nd group


bbid2 <- rbind.data.frame(c(487, 1), c(488, 2), c(490, 3), c(491, 4), 
                        
                        c(498, 7), c(501, 8), c(504, 10), c(506, 12))



names(bbid2) <- c("code", "recode")


return(list(bbid1 = bbid1, bbid2 = bbid2))

}


do.call("groups_of_black_bears", list())




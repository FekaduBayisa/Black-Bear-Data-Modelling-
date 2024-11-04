

# Read data

read_data <- local({ read_data <- function()
{

current_directory <- getwd()

# To read shaplefile data

current_working_directory1 <- paste(current_directory, "/Data", sep = "")


allShpFiles1 <- list.files(path = current_working_directory1, pattern = "\\.shp$", full.names = TRUE)


blackbears <- lapply(allShpFiles1, st_read)


# To read boundary data

current_working_directory2 <- paste(current_directory, "/Boundary", sep = "")


allBoundFiles2 <- list.files(path = current_working_directory2, pattern = "\\.shp$", full.names = TRUE)


Boundaries <- lapply(allBoundFiles2, st_read)


projects <- "+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs"


windows <- list()


for(sd in 1:length(Boundaries))
{

st_crs(Boundaries[[sd]]) <- projects


windows[[sd]]  <- st_union(Boundaries[[sd]])

}


nrow <- length(blackbears)


bearid <- data.frame(matrix(nrow = nrow, ncol = 2))


colnames(bearid)<-c("id", "recode")


for(ds in 1:length(Boundaries))
{

bearid[ds, 1] <- unique(as.integer(substr(blackbears[[ds]]$bear, 
                                        
                                        
                                        nchar(blackbears[[ds]]$bear)-2, 
                                        
                                        
                                        nchar(blackbears[[ds]]$bear))))


bearid[ds, 2] <- ds

}


bearid$id <- as.factor(bearid$id)


return(list(blackbears = blackbears, windows = windows, bearid = bearid))
}

})


BearData <- do.call("read_data", list())




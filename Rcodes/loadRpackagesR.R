

# Load R-packages


load_R_packages <- function()
{

Packages <- list("readxl", "conflicted", "tidyverse","lubridate","spatstat", "sp",
               
               "sf", "proj4","terra","stars","parallel", "randomcoloR", "adehabitatHR",
               
               "matrixStats","amt", "ggplot2", "tidygraph","ggraph", "ctmm","plot.matrix",
               
               "psych", "combinat", "gtools", "GET","nlme", "spatstat.utils", "spatstat.explore",
               
               "spatstat.data", "spatstat.geom", "spatstat.random", "gridExtra", "extrafont")


# Loading R-Packages


lapply(Packages, require, character.only = TRUE)


conflicted::conflicts_prefer(terra::head)

}


do.call("load_R_packages", list())





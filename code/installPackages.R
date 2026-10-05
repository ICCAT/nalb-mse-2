#### HOW TO INSTALL####


 library(devtools)
 install.packages(c("plyr", "ggplot2", "nloptr", "mvtnorm", "triangle"))
 install.packages("flr/FLCore")
 install.packages("flr/FLFleet")
 install.packages(c("FLAssess","FLash","FLXSA"), repos="http://flr-project.org/R")
 install_github("flr/FLBEIA")
 install.packages("TMB", type="source")
 remotes::install_github("DTUAqua/spict/spict")


# Section 1: Libraries----------------------- ####

install.packages(c("ggplot2", "Matrix", "nloptr", "mvtnorm", "triangle", "XLConnect"))
install.packages("FLBEIA", repos="http://flr-project.org/R")
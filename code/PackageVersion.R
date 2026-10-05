
#
#       PACKAGE VERSIONS

#............................................................

library(mvtnorm)
library(triangle)
library(XLConnect)
library(FLXSA)
library(FLAssess)
library(FLash)
library(nloptr)
library(FLCore)     
library(FLFleet)
library(FLBEIA)
library(devtools)
library(r4ss)
library(ss3om)
library(here)

proj_dir = here::here()
setwd(proj_dir)

# Version of the libraries used in the file packageVersionMSE.csv

ip <- as.data.frame(installed.packages()[,c(1,3:4)])
rownames(ip) <- NULL
ip <- ip[is.na(ip$Priority),1:2,drop=FALSE]
print(ip, row.names=FALSE)

write.csv(ip, file="PackageVersions.csv",row.names=FALSE)

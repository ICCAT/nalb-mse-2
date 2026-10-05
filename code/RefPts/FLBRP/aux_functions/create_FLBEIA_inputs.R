# Create objects:
first.yr          <- first_yr
proj.yr           <- proj_yr
hist.yrs  <- as.character(first.yr:(proj.yr-1))
yrs <- c(first.yr=first.yr,proj.yr=proj.yr,last.yr=last.yr)

# Section 5: Names, age, dimensions-----------------------####
#ss3$fleetNames
fls <-   c("BB","BBisl","TRGN","MWT",       
           "JPLLN",     "JPLLS",     "TAILLN",    "TAILLS",   
           "USLLN",     "USLLS",    "VENLL",     "MIXKRPA",
           "OthLL",     "OthSurf",   "BBisls2") 
stks <- c('ALB')

BB.mets <- c('BB') 
BB.BB.stks <- c('ALB') 

BBisl.mets <- c('BBisl') 
BBisl.BBisl.stks <- c('ALB')

TRGN.mets <- c('TRGN') 
TRGN.TRGN.stks <- c('ALB')

MWT.mets <- c('MWT') 
MWT.MWT.stks <- c('ALB')

JPLLN.mets <- c('JPLLN') 
JPLLN.JPLLN.stks <- c('ALB')

JPLLS.mets <- c('JPLLS') 
JPLLS.JPLLS.stks <- c('ALB')

TAILLN.mets <- c('TAILLN') 
TAILLN.TAILLN.stks <- c('ALB')

TAILLS.mets <- c('TAILLS') 
TAILLS.TAILLS.stks <- c('ALB')

USLLN.mets <- c('USLLN') 
USLLN.USLLN.stks <- c('ALB')

USLLS.mets <- c('USLLS') 
USLLS.USLLS.stks <- c('ALB')

VENLL.mets <- c('VENLL') 
VENLL.VENLL.stks <- c('ALB')

MIXKRPA.mets <- c('MIXKRPA') 
MIXKRPA.MIXKRPA.stks <- c('ALB')

OthLL.mets <- c('OthLL') 
OthLL.OthLL.stks <- c('ALB')

OthSurf.mets <- c('OthSurf') 
OthSurf.OthSurf.stks <- c('ALB')

BBisls2.mets <- c('BBisls2') 
BBisls2.BBisls2.stks <- c('ALB')

# all stocks the same
ni             <- n_iter
ns             <- n_seasons

# stock stk1
ALB.age.min    <- as.vector(stock@range["min"])
ALB.age.max    <- as.vector(stock@range["max"]) #in this case the same as plusgroup
ALB.unit       <- n_seasons     

# Section 6: Biols-------------------------------####
#
#  Historical data
#  stk1_n.flq, m, spwn, fec, wt
#

ALB_n.flq     <- propagate(stock@stock.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1)),]
ALB_m.flq     <- propagate(stock@m,ni,fill.iter=TRUE) [,as.character(first.yr:(proj.yr-1)),]
ALB_spwn.flq  <- propagate(stock@m.spwn,ni,fill.iter=TRUE) [,as.character(first.yr:(proj.yr-1)),]

#The modifications in mat due to an error in maturation from the function readFLSss3

ALB_mat.flq   <- propagate(stock@mat,ni,fill.iter=TRUE) [,as.character(first.yr:(proj.yr-1)),]
ALB_fec.flq   <- propagate(stock@mat,ni,fill.iter=TRUE) [,as.character(first.yr:(proj.yr-1)),]
ALB_fec.flq[] <- 1  #eggs/kg #In ss3 Is assumed egg=wt*(a+b*wt); a=1 and b=0
ALB_wt.flq    <- propagate(stock@stock.wt,ni,fill.iter=TRUE) [,as.character(first.yr:(proj.yr-1)),]

ALB_range.min       <- ALB.age.min
ALB_range.max       <- ALB.age.max
ALB_range.plusgroup <- ALB.age.max
ALB_range.minyear   <-  as.vector(stock@range["minyear"])
ALB_range.minfbar   <- as.vector(stock@range["minfbar"])
ALB_range.maxfbar   <- as.vector(stock@range["maxfbar"])

#The modifications in mat due to an error in maturation from the function readFLSss3
# ...................  Projection: ...................................
#  we assume that the projection values of some variables are equal to 
#  the average of some historical years:weight,fecundity,mortality and spawning    

ALB_biol.proj.avg.yrs<- c((proj.yr-3):(proj.yr-1)) 

#..................................................................
#              FLBEIA input object: biols
#..................................................................
stks.data <- list(ALB=ls(pattern="^ALB")) 
biols   <- create.biols.data(yrs,ns,ni,stks.data)
M_0 <- as.numeric(biols$ALB@m[1,1])
biolsMOD <- biols
biolsMOD[[1]]@n[1,]<- biols[[1]]@n[1,]*exp(-M_0*5/12)  
#biolsMOD[[1]]@m[1,] <- biols[[1]]@m[1,]/2 #check ASPG equation
biolsMOD[[1]]@spwn[] <-0
biolsMOD$ALB@m[1,] <- biols$ALB@m[1,]*7/12 # estimated from ss3

# Section 7: Fleets -----------------------####
#........................................................
#
#     we need some extra functions
#     to sum the catch.n and catch.wt across different fleets.
#........................................................

sum.catch.fleet <- function(bs,idx,var){
  aux <-0
  for(i in 1:length(idx)){
    aux <- aux + lapply(bs$fisheries, var)[[idx[i]]] #$A???
  }
  return(aux)}

#........................................................

flq   <- FLQuant(dimnames=list(age = 'all', year = hist.yrs,unit=1,season=1:ns, iter = 1:ni))
flq1  <- FLQuant(1,dimnames=list(age = 'all', year = hist.yrs, unit=1,season=1:ns, iter = 1:ni))

# BB 
BB_effort.flq        <- flq1
BB_capacity.flq      <- flq1*1e7
BB.BB.ALB_landings.n.flq <- propagate(bs$fisheries[[1]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
BB.BB.ALB_landings.wt.flq <- propagate(bs$fisheries[[1]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
landings_BB.ALB <- quantSums(BB.BB.ALB_landings.n.flq*BB.BB.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]

#check total landings the same
landings_BB.ALB/quantSums(bs$fisheries[[1]]$A@landings.n*bs$fisheries[[1]]$A@landings.wt)

# BBisl 
BBisl_effort.flq        <- flq1
BBisl_capacity.flq      <- flq1*1e7
BBisl.BBisl.ALB_landings.n.flq <- propagate(bs$fisheries[[2]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
BBisl.BBisl.ALB_landings.wt.flq <- propagate(bs$fisheries[[2]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
landings_BBisl.ALB <- quantSums(BBisl.BBisl.ALB_landings.n.flq*BBisl.BBisl.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]

# TRGN 
TRGN_effort.flq        <- flq1
TRGN_capacity.flq      <- flq1*1e7
TRGN.TRGN.ALB_landings.n.flq <- propagate(bs$fisheries[[3]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
TRGN.TRGN.ALB_landings.wt.flq <- propagate(bs$fisheries[[3]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
landings_TRGN.ALB <- quantSums(TRGN.TRGN.ALB_landings.n.flq*TRGN.TRGN.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]

# MWT 
MWT_effort.flq        <- flq1
MWT_capacity.flq      <- flq1*1e7
MWT.MWT.ALB_landings.n.flq <- propagate(bs$fisheries[[4]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
MWT.MWT.ALB_landings.wt.flq <- propagate(bs$fisheries[[4]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
landings_MWT.ALB <- quantSums(MWT.MWT.ALB_landings.n.flq*MWT.MWT.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]

# JPLLN 
JPLLN_effort.flq        <- flq1
JPLLN_capacity.flq      <- flq1*1e7
JPLLN.JPLLN.ALB_landings.n.flq <- propagate(bs$fisheries[[5]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
JPLLN.JPLLN.ALB_landings.wt.flq <- propagate(bs$fisheries[[5]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
landings_JPLLN.ALB <- quantSums(JPLLN.JPLLN.ALB_landings.n.flq*JPLLN.JPLLN.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]

# JPLLs
JPLLS_effort.flq        <- flq1
JPLLS_capacity.flq      <- flq1*1e7
JPLLS.JPLLS.ALB_landings.n.flq <- propagate(bs$fisheries[[6]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
JPLLS.JPLLS.ALB_landings.wt.flq <- propagate(bs$fisheries[[6]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
landings_JPLLS.ALB <- quantSums(JPLLS.JPLLS.ALB_landings.n.flq*JPLLS.JPLLS.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]

# TAILLN
TAILLN_effort.flq        <- flq1
TAILLN_capacity.flq      <- flq1*1e7
TAILLN.TAILLN.ALB_landings.n.flq <- propagate(bs$fisheries[[7]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
TAILLN.TAILLN.ALB_landings.wt.flq <- propagate(bs$fisheries[[7]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
landings_TAILLN.ALB <- quantSums(TAILLN.TAILLN.ALB_landings.n.flq*TAILLN.TAILLN.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]

# TAILLS
TAILLS_effort.flq        <- flq1
TAILLS_capacity.flq      <- flq1*1e7
TAILLS.TAILLS.ALB_landings.n.flq <- propagate(bs$fisheries[[8]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
TAILLS.TAILLS.ALB_landings.wt.flq <- propagate(bs$fisheries[[8]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
landings_TAILLS.ALB <- quantSums(TAILLS.TAILLS.ALB_landings.n.flq*TAILLS.TAILLS.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]

# USLLN
USLLN_effort.flq        <- flq1
USLLN_capacity.flq      <- flq1*1e7
USLLN.USLLN.ALB_landings.n.flq <- propagate(bs$fisheries[[9]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
USLLN.USLLN.ALB_landings.wt.flq <- propagate(bs$fisheries[[9]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
landings_USLLN.ALB <- quantSums(USLLN.USLLN.ALB_landings.n.flq*USLLN.USLLN.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]

# USLLS
USLLS_effort.flq        <- flq1
USLLS_capacity.flq      <- flq1*1e7
USLLS.USLLS.ALB_landings.n.flq <- propagate(bs$fisheries[[10]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
USLLS.USLLS.ALB_landings.wt.flq <- propagate(bs$fisheries[[10]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
landings_USLLS.ALB <- quantSums(USLLS.USLLS.ALB_landings.n.flq*USLLS.USLLS.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]

# VENLL
VENLL_effort.flq        <- flq1
VENLL_capacity.flq      <- flq1*1e7
VENLL.VENLL.ALB_landings.n.flq <- propagate(bs$fisheries[[11]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
VENLL.VENLL.ALB_landings.wt.flq <- propagate(bs$fisheries[[11]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
landings_VENLL.ALB <- quantSums(VENLL.VENLL.ALB_landings.n.flq*VENLL.VENLL.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]

# MIXKRPA
MIXKRPA_effort.flq        <- flq1
MIXKRPA_capacity.flq      <- flq1*1e7
MIXKRPA.MIXKRPA.ALB_landings.n.flq <- propagate(bs$fisheries[[12]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
MIXKRPA.MIXKRPA.ALB_landings.wt.flq <- propagate(bs$fisheries[[12]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
landings_MIXKRPA.ALB <- quantSums(MIXKRPA.MIXKRPA.ALB_landings.n.flq*MIXKRPA.MIXKRPA.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]

# OthLL
OthLL_effort.flq        <- flq1
OthLL_capacity.flq      <- flq1*1e7
OthLL.OthLL.ALB_landings.n.flq <- propagate(bs$fisheries[[13]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
OthLL.OthLL.ALB_landings.wt.flq <- propagate(bs$fisheries[[13]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
landings_OthLL.ALB <- quantSums(OthLL.OthLL.ALB_landings.n.flq*OthLL.OthLL.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]

# OthSurf
OthSurf_effort.flq        <- flq1
OthSurf_capacity.flq      <- flq1*1e7
OthSurf.OthSurf.ALB_landings.n.flq <- propagate(bs$fisheries[[14]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
OthSurf.OthSurf.ALB_landings.wt.flq <- propagate(bs$fisheries[[14]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
landings_OthSurf.ALB <- quantSums(OthSurf.OthSurf.ALB_landings.n.flq*OthSurf.OthSurf.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]

# BBisls2
BBisls2_effort.flq        <- flq1
BBisls2_capacity.flq      <- flq1*1e7
BBisls2.BBisls2.ALB_landings.n.flq <- propagate(bs$fisheries[[15]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
BBisls2.BBisls2.ALB_landings.wt.flq <- propagate(bs$fisheries[[15]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
landings_BBisls2.ALB <- quantSums(BBisls2.BBisls2.ALB_landings.n.flq*BBisls2.BBisls2.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]

# Projection 
#==============================================================================
BB_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
BB.BB_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
BB.BB.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))  

BBisl_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
BBisl.BBisl_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
BBisl.BBisl.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))  

TRGN_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
TRGN.TRGN_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
TRGN.TRGN.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))  

MWT_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
MWT.MWT_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
MWT.MWT.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))  

JPLLN_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
JPLLN.JPLLN_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
JPLLN.JPLLN.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))  

JPLLS_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
JPLLS.JPLLS_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
JPLLS.JPLLS.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))  

TAILLN_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
TAILLN.TAILLN_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
TAILLN.TAILLN.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))  

TAILLS_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
TAILLS.TAILLS_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
TAILLS.TAILLS.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))  

USLLN_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
USLLN.USLLN_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
USLLN.USLLN.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))  

USLLS_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
USLLS.USLLS_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
USLLS.USLLS.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))  

VENLL_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
VENLL.VENLL_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
VENLL.VENLL.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))  

MIXKRPA_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
MIXKRPA.MIXKRPA_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
MIXKRPA.MIXKRPA.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))

OthLL_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
OthLL.OthLL_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
OthLL.OthLL.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))

OthSurf_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
OthSurf.OthSurf_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
OthSurf.OthSurf.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))

BBisls2_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
BBisls2.BBisls2_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
BBisls2.BBisls2.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))

# ..................................................................
# FLBEIA input object: fleets ####
#..................................................................

BB.BB_effshare.flq <- flq1
BBisl.BBisl_effshare.flq <- flq1
TRGN.TRGN_effshare.flq <- flq1
MWT.MWT_effshare.flq <- flq1
JPLLN.JPLLN_effshare.flq <- flq1
JPLLS.JPLLS_effshare.flq <- flq1
TAILLN.TAILLN_effshare.flq <- flq1
TAILLS.TAILLS_effshare.flq <- flq1
USLLN.USLLN_effshare.flq <- flq1
USLLS.USLLS_effshare.flq <- flq1
VENLL.VENLL_effshare.flq <- flq1
MIXKRPA.MIXKRPA_effshare.flq <- flq1
OthLL.OthLL_effshare.flq <- flq1
OthSurf.OthSurf_effshare.flq <- flq1
BBisls2.BBisls2_effshare.flq <- flq1

fls.data <- list(BB=ls(pattern="^BB"), BBisl=ls(pattern="^BBisl"), TRGN=ls(pattern="^TRGN"), 
                 MWT=ls(pattern="^MWT"),JPLLN=ls(pattern="^JPLLN"),JPLLS=ls(pattern="^JPLLS"),
                 TAILLN=ls(pattern="^TAILLN"),TAILLS=ls(pattern="^TAILLS"),USLLN=ls(pattern="^USLLN"),
                 USLLS=ls(pattern="^USLLS"),VENLL=ls(pattern="^VENLL"),MIXKRPA=ls(pattern = "^MIXKRPA"),
                 OthLL=ls(pattern="^OthLL"),OthSurf=ls(pattern="^OthSurf"),BBisls2=ls(pattern = "^BBisls2")) 

stks.data <- list(ALB=ls(pattern="^ALB")) 
fleets<- create.fleets.data(yrs,ns,ni,fls.data,stks.data)

#  Section 8: ALB SRs -----------------####

ALB_sr.model        <- 'bevholt'
ALB_params.n        <- 2
ALB_params.name     <- c('a','b') 

R0 <- exp(replist$parameters$Value[replist$parameters$Label=="SR_LN(R0)"])
sigma <- (replist$parameters$Value[replist$parameters$Label=="SR_sigmaR"])

h <- replist$parameters$Value[replist$parameters$Label=="SR_BH_steep"] # 09/23 AGUR
B0 <- c(replist$derived_quants$Value[replist$derived_quants$Label=="SSB_Virgin"])

alfa <- (B0/R0)*(1-h)/(4*h)   #Mangel et al 2010 "Reproductive ecology..."
beta <- (5*h-1)/(4*h*R0)
M_0 <- as.numeric(biolsMOD$ALB@m[1,1])
#a1=(1/exp(-M_0/(1/(5/12))))*1/beta
a1 <- 1/beta
b1 = alfa/beta
ALB_params.array    <- array(as.vector(c(a1,b1)),
                             dim = c(ALB_params.n,length(first.yr:last.yr),ns,ni), 
                             dimnames = list(param=ALB_params.name,
                                             year = ac(first.yr:last.yr),
                                             season=ac(1:ns), iter = 1:ni))   
rec1<- stock@stock.n[1,,] # numbers at age 1 in each unit and season   #UNIT RECRUITMENT 1000s

ssb1 <- unitSums(ssb(stock)) # sum over all the units,all contribute to rec

ALB_rec.flq         <- rec1[,as.character(first.yr:(proj.yr-1)),]           
ALB_ssb.flq         <- ssb1[,as.character(first.yr:(proj.yr-1)),]

sd1 <-replist$parameters$Value[replist$parameters$Label=="SR_sigmaR"]

ALB_uncertainty.flq <- FLQuant(exp(rnorm(length(first.yr:last.yr)*ni*ns,mean=0,sd1)),
                               dimnames=list(age='all',year=first.yr:last.yr,season=1:ns,iter=1:ni)) 

prop1 <- replist$recruitment_dist$recruit_dist$recr_dist_F  #proportion of recruitment per season 

ALB_proportion.flq <-FLQuant(rep(0,length(first.yr:last.yr)*ni),
                             dimnames=list(age='all',year=first.yr:last.yr,season=1:ns,iter=1:ni)) 
ALB_proportion.flq[,,,1][] <- prop1[1]
age.rec1 <- 0 #the age at recruitment
ss.rec1<- 1 
ALB_timelag.matrix <- matrix(c(age.rec1,ss.rec1),2,ns,byrow=TRUE, dimnames = list(c('year', 'season'),season=1:ns)) 


#..............................................................................
#              FLBEIA input object: SRs
#..............................................................................
stks.data <- list(ALB=ls(pattern="^ALB")) 
SRs      <- create.SRs.data(yrs,ns,ni,stks.data)
SRsUnc <- SRs
SRs[["ALB"]]@uncertainty[] <- 1

#  Section 10: Advice:TAC/TAE/quota.share ------------ ####
ALB_advice.TAC.flq<-FLQuant(NA,dimnames=list(year=first.yr:last.yr), iter = ni)
ALB_advice.avg.yrs<- c((proj.yr-3):(proj.yr-1))
ALB_advice.TAC.flq[,ac(first.yr:(proj.yr-1))] <- seasonSums(unitSums(quantSums(catchWStock(fleets,"ALB"))[,ac(first.yr:(proj.yr-1))]))


#CORRECT AGUR 09/23
ALB_advice.TAC.1.flq <- propagate(ALB_advice.TAC.flq, iter=1, fill.iter= TRUE)

#..............................................................................
#              FLBEIA input object: advice
#..............................................................................
rm(ALB_advice.TAC.flq)
stks.data <- list(ALB=ls(pattern="^ALB")) 
#CORRECT AGUR 09/23
advice1 <- create.advice.data(yrs,ns,ni,stks.data,fleets)
advice1$TAC['ALB',ac(proj.yr:last.yr)] <- 31153

advice <- advice1
#  Section 11: main.ctrl  ------------------------- ####


main.ctrl           <- list()
main.ctrl$sim.years <- c(initial = proj.yr, final = last.yr)


#  Section 12: biols.ctrl ------------------------####

growth.model     <- rep('ASPG',1)
biols.ctrl       <- create.biols.ctrl (stksnames=stks,growth.model= growth.model)


#  Section 13: fleets.ctrl ----------------------- ####

n.fls.stks      <- rep(1,15)
fls.stksnames   <- rep('ALB',15)
effort.models    <- rep('SMFB',15)

effort.restr.BB<- 'ALB'
restriction.BB  <- 'catch'
effort.restr.BBisl <-'ALB'
restriction.BBisl <-'catch'
effort.restr.TRGN <-'ALB'
restriction.TRGN<-'catch'
effort.restr.MWT<-'ALB'
restriction.MWT <-'catch'
effort.restr.JPLLN <-'ALB'
restriction.JPLLN <-'catch'
effort.restr.JPLLS <-'ALB'
restriction.JPLLS <- 'catch'
effort.restr.TAILLN <-'ALB'
restriction.TAILLN <-'catch'
effort.restr.TAILLS <-  'ALB'
restriction.TAILLS <-'catch'
effort.restr.USLLN <- 'ALB'
restriction.USLLN <-'catch'
effort.restr.USLLS<-'ALB'
restriction.USLLS <-'catch'
effort.restr.VENLL <- 'ALB'
restriction.VENLL <-'catch'
effort.restr.MIXKRPA<-'ALB'
restriction.MIXKRPA<-'catch'
effort.restr.MIXKRPA<-'ALB'
restriction.MIXKRPA<-'catch'
effort.restr.OthLL<-'ALB'
restriction.OthLL<-'catch'
effort.restr.OthSurf<-'ALB'
restriction.OthSurf<-'catch'
effort.restr.BBisls2<-'ALB'
restriction.BBisls2<-'catch'

catch.models     <- rep("CobbDouglasAge",15) #later change to Baranov
capital.models   <- rep('fixedCapital',15)
price.models     <- NULL    

flq     <- FLQuant(dimnames=list(age = 'all', year = first.yr:last.yr,season=1:4, iter = 1:ni))


fleets.ctrl.SMFB     <- create.fleets.ctrl(fls=fls,n.fls.stks=n.fls.stks,fls.stksnames=fls.stksnames,
                                           effort.models= effort.models, catch.models=catch.models,
                                           capital.models=capital.models, price.models=price.models,flq=flq)

#............................ different options of fleets.ctrl.SMFB.................

fleets.ctrl.SMFB.ALB    <- fleets.ctrl.SMFB


fleets.ctrl.SMFB.ALB$BB$effort.restr <- 'ALB'
fleets.ctrl.SMFB.ALB$BBisl$effort.restr <- 'ALB'
fleets.ctrl.SMFB.ALB$TRGN$effort.restr <- 'ALB'
fleets.ctrl.SMFB.ALB$MWT$effort.restr <- 'ALB'
fleets.ctrl.SMFB.ALB$JPLLN$effort.restr <- 'ALB'
fleets.ctrl.SMFB.ALB$JPLLS$effort.restr <- 'ALB'
fleets.ctrl.SMFB.ALB$TAILLN$effort.restr <- 'ALB'
fleets.ctrl.SMFB.ALB$TAILLS$effort.restr <- 'ALB'
fleets.ctrl.SMFB.ALB$USLLN$effort.restr <- 'ALB'
fleets.ctrl.SMFB.ALB$USLLS$effort.restr <- 'ALB'
fleets.ctrl.SMFB.ALB$VENLL$effort.restr <- 'ALB'
fleets.ctrl.SMFB.ALB$MIXKRPA$effort.restr <- 'ALB'

fleets.ctrl.SMFB.ALB$OthLL$effort.restr <- 'ALB'
fleets.ctrl.SMFB.ALB$OthSurf$effort.restr <- 'ALB'
fleets.ctrl.SMFB.ALB$BBisls2$effort.restr <- 'ALB'


for(fl in names(fleets)) fleets.ctrl.SMFB[[fl]]$effort.restr[] <- 'ALB'
fleets.ctrl.SMFB$seasonal.share$ALB <- fleets.ctrl.SMFB$seasonal.share$ALB[,,1,1,1,1]
fleets.ctrl.SMFB$seasonal.share$ALB[] <- 1



#  Section 14: advice.ctrl ------------------- ####
advice.ctrl   <- create.advice.ctrl(stksnames = stks, HCR.models = rep('fixedAdvice', length(stks)))

#  Section 15: assess.ctrl ------------------ ####
assess.models    <- rep('NoAssessment',length(stks))
assess.ctrl      <- create.assess.ctrl(stksnames = stks, assess.models = assess.models)
assess.ctrl[["ALB"]]$work_w_Iter<- TRUE

#  Section 16: obs.ctrl --------------------------####
stkObs.models <- c(rep('perfectObs',length(stks)))
obs.ctrl        <- create.obs.ctrl(stksnames = stks,  stkObs.models = stkObs.models)

#  Section 17: covars ---------------------------####
BDs <- NULL

covars <- NULL
#  Section 18: covars.ctrl -------------------------------####
covars.ctrl <- NULL

#........................................
#OEM - INDICES
# No indices
indices <- NULL

# -------------------------------------------------------------------------
# OBSERVATION

stkObs.models <- "perfectObs"
flq.stk1 <- FLQuant(dimnames = list(age = 'all', 
                                    year = first.yr:last.yr, 
                                    unit = 1, season = 1:ns, iter = 1:ni)) 

obs.ctrl <- create.obs.ctrl(stksnames = "ALB",  
                            stkObs.models = stkObs.models,
                            flq.stk1 = flq.stk1)

#........................................................
#....ADVICE
#....................................................

advice$TAC[1,as.character(first.yr:(proj.yr-1))] <- tlandStock(fleets, "ALB")[1,as.character(first.yr:(proj.yr-1))]
advice$TAC[,as.character(c(proj.yr:last.yr))] <- advice$TAC[1,as.character(proj.yr-1)]

for (fl in names(fleets)) {
  fleets.ctrl.SMFB[[fl]]$LandObl <- FALSE
  
  for(st in names(fleets[[fl]]@metiers[[1]]@catches)){
    fleets.ctrl.SMFB[[fl]][[st]]$discard.TAC.OS <- FALSE
  }
}

for (fl in names(fleets)) {
  fleets.ctrl.SMFB[[fl]]$LandObl <- FALSE
  
  for(st in names(fleets[[fl]]@metiers[[1]]@catches)){
    fleets.ctrl.SMFB[[fl]][[st]]$discard.TAC.OS <- FALSE
  }
}

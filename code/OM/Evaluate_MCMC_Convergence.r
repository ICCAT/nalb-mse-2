# ============================================================
# Script: Evaluate_MCMC_Convergence.R
#
# Purpose:
#   Evaluate MCMC convergence diagnostics, identify poor
#   model fits, and select representative runs for the
#   Atlantic albacore Operating Model.
#
# Inputs:
#   - SS3 MCMC output files
#   - MCMC summary objects
#
# Outputs:
#   - Convergence diagnostics plots
#   - Parameter distribution plots
#   - Recruitment and B/BMSY uncertainty plots
#   - SelectedRuns4OM.csv
#   - convBratio.csv
#
# Author: AZTI
# ============================================================

# ---------------------------------------------------------------------------
# MCMC Diagnostics and OM Selection
#
# Objectives:
#
#   1. Evaluate SS3 MCMC convergence using:
#
#        - Likelihood
#        - Maximum gradient
#        - B/BMSY trajectories
#
#   2. Identify problematic runs:
#
#        - High convergence gradients
#        - Extreme likelihood values
#        - Unusual B/BMSY behaviour
#
#   3. Explore biological parameter uncertainty:
#
#        - Natural mortality (M)
#        - Recruitment variability (SigmaR)
#        - Steepness (h)
#
#   4. Compare:
#
#        - All MCMC runs
#        - Converged runs only
#        - Subset of selected runs
#
#   5. Generate uncertainty envelopes for:
#
#        - B/BMSY
#        - Recruitment
#
#   6. Select a representative subset of runs
#      for Operating Model conditioning.
#
# Notes:
#
#   - Convergence is evaluated using the maximum
#     gradient component.
#
#   - Runs with poor convergence or extreme
#     likelihood values can be excluded.
#
#   - A subset of converged runs is randomly
#     selected for OM conditioning.
#
# ---------------------------------------------------------------------------

# Load Libraries
library(r4ss)
library(snow)
library(doParallel)
library(ggplot2)
library(ggpubr)
library(here)

proj_dir = here::here()
setwd(proj_dir)

# Sharepoint path:
source('sharepoint_path.R')
setwd(shrpoint_path)

# Set Working Directory
mywd="OM/BaseCase"

setwd(mywd)
plotdir <- paste0(mywd,"/Plots")
dir.create(paste0(mywd,"/Plots"))
outdir <- paste0(mywd,"/Results")
dir.create(paste0(mywd,"/Results"))

# Define MC Terms
ntrials=400	# Number of Monte Carlo Draws/SS Model Iterations
sigR_CV=0.2		# Coefficient of Variation for Normal Distribution MC of SigmaR
natM_CV=0.2	  # Coefficient of Variation for Normal Distribution MC of Natural Mortality
start_yr=1930	 # SS Model Start Year
end_yr=2051	  # SS Model End Year Including Projection Years

runMCMC <- FALSE
ReadRep <- FALSE


if(ReadRep==TRUE){
#Thtis can not be done in a PC for lack of memory better a supercomputer or servidor 
  # I run ReadOm_Cluster in the servidor 172.21.22.40 using MobaxTerm to conect
closeAllConnections()
SumReport=SSgetoutput(keyvec=paste0("_",1:ntrials),getcovar=FALSE,getcomp=FALSE,forecast=TRUE)
save(SumReport,  file="Results/Output.RData")
}else{
  load("Results/Output_summary.RData")

}



#.................................
#
# Plot summary Results
#
#.............................................
F_yr <- Summary$quants[371:(371+91),]

jpeg(filename='Plots/Summary.jpg',width=800,height=800)
par(mfcol=c(3,2),mai=c(0.4,0.8,0.2,0.2))
hist(as.numeric(Summary$pars[1,1:ntrials]),breaks=seq(0,1,0.02),main="Natural M",xlim=c(0,1))
hist(as.numeric(Summary$pars[17,1:ntrials]),breaks=seq(0,1,0.02),main="SigmaR",xlim=c(0,1))
hist(as.numeric(Summary$pars[16,1:ntrials]),breaks=seq(0,1,0.02),main="Steepness",xlim=c(0,1))

plot(start_yr:end_yr,rep("",length(start_yr:end_yr)),ylim=c(0,6),ylab="SSB/SSBmsy")
sapply(1:ntrials,function(i) lines(start_yr:end_yr,Summary$Bratio[,i],col=rgb(0,0,0,0.1))) #rainbow(ntrials)[i]))

dev.off()

plot(start_yr:end_yr,rep("",length(start_yr:end_yr)),ylim=c(0,0.5),ylab="F")
sapply(1:ntrials,function(i) lines((start_yr):end_yr,Summary$Fvalue[,i],col=rgb(0,0,0,0.1))) #rainbow(ntrials)[i]))

#....................................................
#
#         PLOT CONVERGENCY
#................................................
out <- NULL

p1 <-ggplot()+geom_point(aes(as.numeric(Summary$likelihoods[1,-ntrials-1]),as.numeric(Summary$maxgrad)))+
  labs(x="Likelihood", y="Convergency")+geom_point()+theme_bw()+ theme(axis.text = element_text(size = 10))
p1 


p2 <-ggplot()+geom_point(aes(as.numeric(Summary$Bratio[1,-ntrials-1:2]),as.numeric(Summary$maxgrad)))+
  labs(x="Bratio", y="Convergency")+geom_point()+theme_bw()+ theme(axis.text = element_text(size = 10))
p2 

pAll <- ggarrange(p1, p2, 
        #  labels = c("A", "B", "C","D"),
          ncol = 1, nrow = 2)
pAll
ggsave('Plots/ConvAndBratio.jpg',pAll, width = 3000, height = 3000, units = "px", dpi = 400)


#.......................................................
#   TESTING ALL THE VARIABLES
#   
#.........................................................

a <- unlist(lapply(SumReport,function(x) x$maximum_gradient_component)) #an error might indicate some of the runs didnt complete
indNConv <- which(as.vector(a)>0.001)
length(indNConv)
indConvHigh<- which(a>1)

b <- Summary$Bratio[1,-ntrials-1:2] #an error might indicate some of the runs didnt complete
indNBr <- which(b>6) 
indNBrNconv <- which(b>6 & a>0.001) 
#size 9 12 15 19 21 23 31 33 44 47 51 56 59 62 86 92
length(b)
length(indNBr)# summarize output
length(indNConv)
length(indNBrNconv)
length(indConvHigh)
colBratio <- rep(1,ntrials)
colBratio[indNConv] <- 2
colBratio[indNBr] <- 3
colBratio[indNBrNconv]<-4
colBratio[indConvHigh] <- 5
LklMean <- mean(as.numeric(Summary$likelihoods[1,c(-indConvHigh,-ntrials-1)]))
LklOut <- which((as.numeric(Summary$likelihoods[1,c(-ntrials-1)])>
 LklMean+100)) 
LklOut <- c(LklOut,which(as.numeric(Summary$likelihoods[1,c(-ntrials-1)])<
   LklMean-100))
aux <- which(LklOut %in% as.numeric(indConvHigh))
if (length(aux)==0){colBratio[LklOut[-aux] ] <- 7}
colBratio[LklOut ]<- 7 

dfconv <- NULL
dfconv$run <- 1:100
dfconv$Bratio <- as.numeric(Summary$Bratio[1,-ntrials-1:2])
dfconv$conv <- Summary$maxgrad
dfconv$type <- colBratio
dfconv <- as.data.frame(dfconv)
dfconv
write.csv(dfconv,file="Results/convBratio.csv")
p1 <-ggplot()+geom_point(aes(as.numeric(Summary$pars[16,1:ntrials]),as.numeric(Summary$Bratio[1,-ntrials-1:2])),color=colBratio)+
  labs(x="Steepness", y="Bratio")+geom_point()+theme_bw()+ theme(axis.text = element_text(size = 10))
p1

p2 <-ggplot()+geom_point(aes(as.numeric(Summary$pars[16, c(1:ntrials)[-indConvHigh]]),as.numeric(Summary$maxgrad)[-indConvHigh]),color=colBratio[-indConvHigh])+
  labs(x="Steepness", y="Convergency")+geom_point()+theme_bw()+ theme(axis.text = element_text(size = 10))
p2 
if(length(c(1:ntrials)[-indConvHigh])==0){
  p2 <-ggplot()+geom_point(aes(as.numeric(Summary$pars[16, c(1:ntrials)]),as.numeric(Summary$maxgrad)),color=colBratio)+
    labs(x="Steepness", y="Convergency")+geom_point()+theme_bw()+ theme(axis.text = element_text(size = 10))
  p2 
  
}

p3 <-ggplot()+geom_point(aes(as.numeric(Summary$pars[1,1:ntrials]),as.numeric(Summary$Bratio[1,-ntrials-1:2])),color=colBratio)+
  labs(x="M", y="Bratio")+geom_point()+theme_bw()+ theme(axis.text = element_text(size = 10))
p3


 p5 <-ggplot()+geom_point(aes(as.numeric(Summary$pars[17,1:ntrials]),as.numeric(Summary$Bratio[1,-ntrials-1:2])),color=colBratio)+
   labs(x="sigmaR", y="Bratio")+geom_point()+theme_bw()+ theme(axis.text = element_text(size = 10))
 p5



p4 <-ggplot()+
  geom_point(aes(as.numeric(Summary$pars[17,1:ntrials]),as.numeric(Summary$pars[1,1:ntrials])),color=colBratio)+
  labs(x="sigmaR", y="M")+geom_point()+theme_bw()+ theme(axis.text = element_text(size = 10))
p4

p6 <-ggplot()+geom_point(aes(as.numeric(Summary$pars[1,1:ntrials]),as.numeric(Summary$pars[16,1:ntrials])),color=colBratio)+
  labs(x="M", y="Steepness")+geom_point()+theme_bw()+ theme(axis.text = element_text(size = 10))
p6
p7 <-ggplot()+geom_point(aes(as.numeric(Summary$pars[17,1:ntrials]),as.numeric(Summary$pars[16,1:ntrials])),color=colBratio)+
  labs(x="sigmaR",y="Steepness")+geom_point()+theme_bw()+ theme(axis.text = element_text(size = 10))
p7

p8 <-ggplot()+geom_point(aes(as.numeric(Summary$maxgrad),as.numeric(Summary$likelihoods[1,c(-ntrials-1)])),color=colBratio)+
  labs(x="Convergency", y="Likelihood")+geom_point()+theme_bw()+ theme(axis.text = element_text(size = 10))
p8 

 
if(length(c(1:ntrials)[-indConvHigh])!=0){
  p8 <-ggplot()+geom_point(aes(as.numeric(Summary$maxgrad)[-indConvHigh],as.numeric(Summary$likelihoods[1,c(-indConvHigh,-ntrials-1)])),color=colBratio[-indConvHigh])+
    labs(x="Convergency", y="Likelihood")+geom_point()+theme_bw()+ theme(axis.text = element_text(size = 10))
  p8
  
}


pAll <- ggarrange(p1, p3,p5,p2,p4,p6,p7,p8,
                #  labels = c("A", "B", "C","D"),
                  ncol = 4, nrow = 2)
pAll
ggsave('Plots/VarBio_conv.jpg',pAll, width = 5000, height = 2000, units = "px", dpi = 400)
#After removing i <0.001 and the 2 runs 59, 95.

#....................................................................

#Some outliers
#Removing the runs with convergency value high solved?


colConv <- colBratio 
indConv <- which(colConv %in% c(1,2)) #indConv only runs with good convergency
niter <- length(indConv)
SubSummary <- SSsummarize(SumReport[indConv])


jpeg(filename='Plots/Summary_AfterConv.jpg',width=800,height=800)
par(mfcol=c(3,2),mai=c(0.4,0.8,0.2,0.2))
hist(as.numeric(SubSummary$pars[1,1:(length(indConv))]),breaks=seq(0,1,0.02),main="Natural M",xlim=c(0,1))
hist(as.numeric(SubSummary$pars[17,1:(length(indConv))]),breaks=seq(0,1,0.02),main="SigmaR",xlim=c(0,1))
hist(as.numeric(SubSummary$pars[16,1:(length(indConv))]),breaks=seq(0,1,0.02),main="Steepness",xlim=c(0,1))

plot(start_yr:end_yr,rep("",length(start_yr:end_yr)),ylim=c(0,6),ylab="SSB/SSBmsy")
sapply(1:length(indConv),function(i) lines(start_yr:end_yr,SubSummary$Bratio[,i],col=rgb(0,0,0,0.1))) #rainbow(ntrials)[i]))

plot(start_yr:end_yr,rep("",length(start_yr:end_yr)),ylim=c(0,1e6),ylab="Recruits")
sapply(1:length(indConv),function(i) lines((start_yr-2):end_yr,SubSummary$recruits[,i],col=rgb(0,0,0,0.1))) #rainbow(ntrials)[i]))
dev.off()


#afterConvergncy


p1 <-ggplot()+geom_point(aes(as.numeric(SubSummary$likelihoods[1,-niter-1]),as.numeric(SubSummary$maxgrad)))+
  labs(x="Likelihood", y="Convergency")+geom_point()+theme_bw()+ theme(axis.text = element_text(size = 10))
p1 

SubSummary$npars
p2 <-ggplot()+geom_point(aes(as.numeric(SubSummary$Bratio[1,-niter-1:2]),as.numeric(SubSummary$maxgrad)))+
  labs(x="Bratio", y="Convergency")+geom_point()+theme_bw()+ theme(axis.text = element_text(size = 10))
p2 

pAll <- ggarrange(p1, p2, 
                  #  labels = c("A", "B", "C","D"),
                  ncol = 1, nrow = 2)
pAll
ggsave('Plots/ConvAndBratio_AfterFilterConv.jpg',pAll, width = 3000, height = 3000, units = "px", dpi = 400)


#............................................
library(reshape2)

colConv=ifelse(colConv<=2,1,2)
length(which(colConv==1)) #check 1 value onlly good runs

df <- NULL
df <- data.frame(M=as.numeric(Summary$pars[1,1:ntrials]),
                 sigmaR=as.numeric(Summary$pars[17,1:ntrials]),
                 H=as.numeric(Summary$pars[16,1:ntrials]),
                 conv=colConv)
df$conv <- as.factor(df$conv)

df_rec <- melt(Summary$recruits[,-c(401)][,c(1:400,401)], id.vars = "Yr")
df_conv_rec <- melt(Summary$recruits[,-c(401)][,c(indConv,401)], id.vars = "Yr")
df_conv_rec

df_B <- melt(Summary$Bratio[,-c(401)][,c(1:400,401)], id.vars = "Yr")
df_conv_B <- melt(Summary$Bratio[,-c(401)][,c(indConv,401)], id.vars = "Yr")
df_conv_B



p1<-ggplot()+
  geom_histogram(data=df,aes(M,y=..density..)) +
  geom_density(aes(M, fill = "All"), alpha = .2, data = df) +
  geom_density(aes(M, fill = "GoodConv"), alpha = .3, data = df[df$conv==1,]) +
  scale_fill_manual(name = "dataset", 
                    values = c(All = "red", GoodConv = "green"))+
  geom_vline(xintercept=mean(df$M),color="red",linewidth=1.5,linetype = "dashed")+
  geom_vline(xintercept=mean(df$M[df$conv==1]),color="green",linewidth=1.5,linetype = "dashed")+
  annotate(geom="text", x=0.25, y=9, label=paste("n=",length(indConv), "GoodConv"),
                 color="black")+
    guides(color="none")
  
p1  

p2<-ggplot()+geom_histogram(data=df,aes(sigmaR,y=..density..)) +
  geom_density(aes(sigmaR, fill = "All"), alpha = .2, data = df) +
  geom_density(aes(sigmaR, fill = "GoodConv"), alpha = .3, data = df[df$conv==1,]) +
  scale_fill_manual(name = "dataset", 
                    values = c(All = "red", GoodConv = "green"))+
  geom_vline(xintercept=mean(df$sigmaR),color="red",linewidth=1.5,linetype = "dashed")+
  geom_vline(xintercept=mean(df$sigmaR[df$conv==1]),color="green",linewidth=1.5,linetype = "dashed")+
  guides(color="none")
p2
p3<-ggplot()+geom_histogram(data=df,aes(H,y=..density..)) +
  geom_density(aes(H, fill = "All"), alpha = .2, data = df) +
  geom_density(aes(H, fill = "GoodConv"), alpha = .3, data = df[df$conv==1,]) +
  scale_fill_manual(name = "dataset", 
                    values = c(All = "red", GoodConv = "green"))+
  geom_vline(xintercept=mean(df$H),color="red",linewidth=1.5,linetype = "dashed")+
  geom_vline(xintercept=mean(df$H[df$conv==1]),color="green",linewidth=1.5,linetype = "dashed")+
  guides(color="none")
p3

pAll <- ggarrange(p1,  p2,p3,
                  #  labels = c("A", "B", "C","D"),
                  ncol = 1, nrow = 3)

pAll
ggsave('Plots/DensityPlot_conv.jpg',pAll, width = 3000, height = 3000, units = "px", dpi = 400)


#..................................................................
#
#  first 100 iteration with good convergency
#
#..................................................................

colConv=ifelse(colBratio<=2,1,2)
indConv <- which(colConv==1)
seednum <- ifelse(mywd=="OM/BaseCase",100, 
            ifelse(mywd=="OM/Age",200,
                   ifelse(mywd=="OM/CPUE",300,400)))

set.seed(seednum)
s100 <- sample(indConv,100,replace=F)

indConv <- sort(s100)
colConv <- colConv[s100]
niter <- length(indConv)
SubSummary <- SSsummarize(SumReport[indConv])
df <- NULL
df$Runs<- sort(s100)
write.csv(df,"Results/SelectedRuns4OM.csv", row.names=FALSE)

jpeg(filename='Plots/Summary_AfterConv_first100.jpg',width=800,height=800)
par(mfcol=c(3,2),mai=c(0.4,0.8,0.2,0.2))
hist(as.numeric(SubSummary$pars[1,1:(length(indConv))]),breaks=seq(0,1,0.02),main="Natural M",xlim=c(0,1))
hist(as.numeric(SubSummary$pars[17,1:(length(indConv))]),breaks=seq(0,1,0.02),main="SigmaR",xlim=c(0,1))
hist(as.numeric(SubSummary$pars[16,1:(length(indConv))]),breaks=seq(0,1,0.02),main="Steepness",xlim=c(0,1))

plot(start_yr:end_yr,rep("",length(start_yr:end_yr)),ylim=c(0,6),ylab="SSB/SSBmsy")
sapply(1:length(indConv),function(i) lines(start_yr:end_yr,SubSummary$Bratio[,i],col=rgb(0,0,0,0.1))) #rainbow(ntrials)[i]))

plot(start_yr:end_yr,rep("",length(start_yr:end_yr)),ylim=c(0,1e6),ylab="Recruits")
sapply(1:length(indConv),function(i) lines((start_yr-2):end_yr,SubSummary$recruits[,i],col=rgb(0,0,0,0.1))) #rainbow(ntrials)[i]))
dev.off()


#afterConvergncy


p1 <-ggplot()+geom_point(aes(as.numeric(SubSummary$likelihoods[1,-niter-1]),as.numeric(SubSummary$maxgrad)))+
  labs(x="Likelihood", y="Convergency")+geom_point()+theme_bw()+ theme(axis.text = element_text(size = 10))
p1 

SubSummary$npars
p2 <-ggplot()+geom_point(aes(as.numeric(SubSummary$Bratio[1,-niter-1:2]),as.numeric(SubSummary$maxgrad)))+
  labs(x="Bratio", y="Convergency")+geom_point()+theme_bw()+ theme(axis.text = element_text(size = 10))
p2 

pAll <- ggarrange(p1, p2, 
                  #  labels = c("A", "B", "C","D"),
                  ncol = 1, nrow = 2)
pAll
ggsave('Plots/ConvAndBratio_AfterFilterConv100.jpg',pAll, width = 3000, height = 3000, units = "px", dpi = 400)


#............................................



library(reshape2)

df <- NULL
df <- data.frame(M=as.numeric(Summary$pars[1,1:ntrials]),
                 sigmaR=as.numeric(Summary$pars[17,1:ntrials]),
                 H=as.numeric(Summary$pars[16,1:ntrials]),
                 conv=rep(2,ntrials))
df$conv[s100] <- 1  #only the chosen 100 iterations.

df_rec <- melt(Summary$recruits[,-c(401)][,c(1:400,401)], id.vars = "Yr")
df_conv_rec <- melt(Summary$recruits[,-c(401)][,c(indConv,401)], id.vars = "Yr")
df_conv_rec

df_B <- melt(Summary$Bratio[,-c(401)][,c(1:400,401)], id.vars = "Yr")
df_conv_B <- melt(Summary$Bratio[,-c(401)][,c(indConv,401)], id.vars = "Yr")
df_conv_B

p1<-ggplot()+geom_histogram(data=df,aes(M,y=..density..)) +
  geom_density(aes(M, fill = "All"), alpha = .2, data = df) +
  geom_density(aes(M, fill = "GoodConv100"), alpha = .3, data = df[df$conv==1,]) +
  scale_fill_manual(name = "dataset", 
                    values = c(All = "red", GoodConv100 = "green"))+
  geom_vline(xintercept=mean(df$M),color="red",linewidth=1.5,linetype = "dashed")+
  geom_vline(xintercept=mean(df$M[df$conv==1]),color="green",linewidth=1.5,linetype = "dashed")+
  annotate(geom="text", x=0.25, y=9, label=paste("n=",length(indConv), "runs GoodConv"),
           color="black")+
  guides(color="none")

p1  

p2<-ggplot()+geom_histogram(data=df,aes(sigmaR,y=..density..,color=conv)) +
  geom_density(aes(sigmaR, fill = "All"), alpha = .2, data = df) +
  geom_density(aes(sigmaR, fill = "GoodConv100"), alpha = .3, data = df[df$conv==1,]) +
  scale_fill_manual(name = "dataset", 
                    values = c(All = "red", GoodConv100 = "green"))+
  geom_vline(xintercept=mean(df$sigmaR),color="red",linewidth=1.5,linetype = "dashed")+
  geom_vline(xintercept=mean(df$sigmaR[df$conv==1]),color="green",linewidth=1.5,linetype = "dashed")+
  guides(color="none")
p2
p3<-ggplot()+geom_histogram(data=df,aes(H,y=..density..,color=conv)) +
  geom_density(aes(H, fill = "All"), alpha = .2, data = df) +
  geom_density(aes(H, fill = "GoodConv100"), alpha = .3, data = df[df$conv==1,]) +
  scale_fill_manual(name = "dataset", 
                    values = c(All = "red", GoodConv100 = "green"))+
  geom_vline(xintercept=mean(df$H),color="red",linewidth=1.5,linetype = "dashed")+
  geom_vline(xintercept=mean(df$H[df$conv==1]),color="green",linewidth=1.5,linetype = "dashed")+
  guides(color="none")
p3


pAll <- ggarrange(p1,  p2,p3,
                  #  labels = c("A", "B", "C","D"),
                  ncol = 1, nrow = 3)

pAll
ggsave('Plots/DensityPlot_conv100.jpg',pAll, width = 3000, height = 3000, units = "px", dpi = 400)

#...................................................
#
#   Adding quantiles - uncertainty plots
#
#............................................


library(tidyr)                                   
      
prob = c(0.95,0.5,0.05)
p_names <- paste("q",ifelse(nchar(substr(prob,3, nchar(prob)))==1, 
                            paste(substr(prob,3, nchar(prob)), 0, sep = ""), 
                            substr(prob,3, nchar(prob))), sep = "")
res_B <- df_B %>% dplyr::group_by(Yr) %>%
  dplyr::summarise(quantiles = list(p_names), value=list(quantile(value, prob=prob, na.rm = TRUE))) %>% 
  unnest(c(quantiles,value)) %>% tidyr::spread(key='quantiles', value='value')
                                                           
res_B$scenario <- "All"
res_conv_B <- df_conv_B %>% dplyr::group_by(Yr) %>%
  dplyr::summarise(quantiles = list(p_names), value=list(quantile(value, prob=prob, na.rm = TRUE))) %>% 
  unnest(c(quantiles,value)) %>% tidyr::spread(key='quantiles', value='value')
                                                          
res_conv_B$scenario <- "GoodConv"

df_Q_B <- rbind(res_B,res_conv_B)
df_Q_B$indicator <- "B/Bmsy"

res_rec <- df_rec %>% dplyr::group_by(Yr) %>%
  dplyr::summarise(quantiles = list(p_names), value=list(quantile(value, prob=prob, na.rm = TRUE))) %>% 
  unnest(c(quantiles,value)) %>% tidyr::spread(key='quantiles', value='value')
                                                          
res_rec$scenario <- "All"
res_conv_rec <- df_conv_rec %>% dplyr::group_by(Yr) %>%
  dplyr::summarise(quantiles = list(p_names), value=list(quantile(value, prob=prob, na.rm = TRUE))) %>% 
  unnest(c(quantiles,value)) %>% tidyr::spread(key='quantiles', value='value')
                                                      
res_conv_rec$scenario <- "GoodConv"

df_Q_rec <- rbind(res_rec,res_conv_rec)
df_Q_rec$indicator <- "Recruitment"


df_all <- rbind(df_Q_B,df_Q_rec)
df_all$indicator <- as.factor(df_all$indicator)
df_all$scenario <- as.factor(df_all$scenario)
levels(df_all$scenario)

p1 <- ggplot(df_all, aes(x = Yr, y = q50,color = scenario)) +
  facet_wrap(~indicator, scales = "free") + 
  geom_line()+
  labs(x="Year")+theme_bw()+
  geom_ribbon(data=df_all, aes(x = Yr, 
                               ymin = q05, ymax = q95, fill = scenario), inherit.aes = FALSE, 
              alpha = 0.5) +
  theme_bw() + theme(text = element_text(size = 20), title = element_text(size = 20, 
                                                                          face = "bold"), strip.text = element_text(size = 20)) + 
  ylab("") + ggtitle("") + theme(plot.title = element_text(hjust = 0.5))

p1

ggsave('Plots/UncertityConv100.jpg',p1, width = 6000, height = 3000, units = "px", dpi = 400)

#..................................................................
# COMPARISON ASSESSMENT
#
#.................................................................
# 
library(readxl)
# 
# 
#..........................................
#
#  parameters
#..................................



profilesummary <- SSsummarize(SumReport)
plot(1:ntrials,as.numeric(profilesummary$maxgrad), xlab="Runs",ylab="MaxGradient",ylim=c(0,0.001))
profilesummary <- Summary
witch_j_summary <- profilesummary #SSsummarize(witch_j)
profilesummary <- witch_j_summary
#Estimated parameters across runs
pars=witch_j_summary$pars
n <- niter
head(pars)
library(tidyr)
library(ggplot2)
a <- pars %>% pivot_longer(1:n,names_to='run')
g <- ggplot(subset(a,is.na(Yr)),aes(run,value)) + geom_point() + facet_wrap(~Label,scales='free_y') +
  theme(strip.text=element_text(size=8)) + expand_limits(y=0)
g
ggsave('Plots/ParAll.png',g,width=12,height=8,units='in',scale=1.5)


a <- pars[c(1:6,15:18),] %>% pivot_longer(1:n,names_to='run')
g <- ggplot(subset(a,is.na(Yr)),aes(run,value)) + geom_point() + facet_wrap(~Label,scales='free_y') +
  theme(strip.text=element_text(size=8)) + expand_limits(y=0)
g
ggsave('Plots/ParAllBio.png',g,width=12,height=8,units='in',scale=1.5)



a <- pars[c(112:202),] %>% pivot_longer(1:n,names_to='run')
g <- ggplot(subset(a,is.na(Yr)),aes(run,value)) + geom_point() + facet_wrap(~Label,scales='free_y') +
  theme(strip.text=element_text(size=8)) + expand_limits(y=0)
g
ggsave('Plots/ParAllFisher.png',g,width=12,height=8,units='in',scale=1.5)

#.........................................

# ADDING COLOR TO THOSE WITH STEEPNESS HIGHER THAN 0.9

#...............................................

profilesummary <- Summary
witch_j_summary <- profilesummary #SSsummarize(witch_j)
profilesummary <- witch_j_summary
#Estimated parameters across runs
pars=witch_j_summary$pars
n <- ntrials
head(pars)

l <- unlist(lapply(SumReport,function(x) x$maximum_gradient_component)) #an error might indicate some of the runs didnt complete
kk <- which(l>1)
i <- which(l<0.001)
length(l)
length(i)# summarize output


a <- pars[c(1:6,15:18),] %>% pivot_longer(1:n,names_to='run')

col <- 1
df <- a[a$Label=="SR_BH_steep",]
ind <- which(df$value>0.9)
#size 7  9 15 21 23 44 62
df$col <- colBratio

df$col[df$col==1] <- "conv<0.001"
df$col[df$col==2] <- "conv>0.001"
df$col[df$col==3] <- "Bratio_0>6"
df$col[df$col==4] <- "Bratio_0>6 & conv>0.001"
df$col[df$col==5] <- "conv>1"
df$col[df$col==7] <- "LKL outlier"



g <- ggplot(subset(a,is.na(Yr)),aes(run,value,colour=as.factor(rep(df$col,length(unique(a$Label)))))) + 
  geom_point() +
  facet_wrap(~Label,scales='free_y') +
  theme(strip.text=element_text(size=8)) + expand_limits(y=0)+
  guides(color = guide_legend(title = "")) 
g

ggsave('Plots/ParAllBio.png',g,width=12,height=8,units='in',scale=1.5)




a <- pars[c(112:202),] %>% pivot_longer(1:n,names_to='run')
g <- ggplot(subset(a,is.na(Yr)),aes(run,value,colour=as.factor(rep(df$col,length(unique(Label)))))) + 
  geom_point() + facet_wrap(~Label,scales='free_y') +
  theme(strip.text=element_text(size=8)) + expand_limits(y=0)
g
ggsave('Plots/ParAllFisher.png',g,width=12,height=8,units='in',scale=1.5)




a <- pars[c(112:130),] %>% pivot_longer(1:n,names_to='run')
g <- ggplot(subset(a,is.na(Yr)),aes(run,value,colour=as.factor(rep(df$col,length(unique(Label))))))  + 
  geom_point() + facet_wrap(~Label,scales='free_y') +
  theme(strip.text=element_text(size=8)) + expand_limits(y=0)
g
ggsave('Plots/ParAllFisher_1.png',g,width=12,height=8,units='in',scale=1.5)


a <- pars[c(131:150),] %>% pivot_longer(1:n,names_to='run')
g <- ggplot(subset(a,is.na(Yr)),aes(run,value,colour=as.factor(rep(df$col,length(unique(Label))))))  + 
  geom_point() + facet_wrap(~Label,scales='free_y') +
  theme(strip.text=element_text(size=8)) + expand_limits(y=0)
g
ggsave('Plots/ParAllFisher_2.png',g,width=12,height=8,units='in',scale=1.5)

a <- pars[c(151:175),] %>% pivot_longer(1:n,names_to='run')
g <- ggplot(subset(a,is.na(Yr)),aes(run,value,colour=as.factor(rep(df$col,length(unique(Label))))))  + 
  geom_point() + facet_wrap(~Label,scales='free_y') +
  theme(strip.text=element_text(size=8)) + expand_limits(y=0)
g
ggsave('Plots/ParAllFisher_3.png',g,width=12,height=8,units='in',scale=1.5)

#..... REMOVING CONV

profilesummary <- SubSummary
witch_j_summary <- profilesummary 
profilesummary <- witch_j_summary
#Estimated parameters across runs
pars=witch_j_summary$pars
n <- niter
head(pars)

a <- pars[c(1:6,15:18),] %>% pivot_longer(1:n,names_to='run')
g <- ggplot(subset(a,is.na(Yr)),aes(run,value)) + geom_point() + facet_wrap(~Label,scales='free_y') +
  theme(strip.text=element_text(size=15)) + expand_limits(y=0)
g
ggsave('Plots/ParAllBio_Afterconv100.png',g,width=12,height=8,units='in',scale=1.5)

a <- pars[c(112:202),] %>% pivot_longer(1:n,names_to='run')
g <- ggplot(subset(a,is.na(Yr)),aes(run,value)) + geom_point() + facet_wrap(~Label,scales='free_y') +
  theme(strip.text=element_text(size=20)) + expand_limits(y=0)
g
ggsave('Plots/ParAllFisher_AfterConv100.png',g,width=27,height=16,units='in',scale=1.5)

#..................................
#
#       PLOT WITH COORDINATES
#
#............................................


library(plotly)
library(reshape2)
library(tidyverse)
profilesummary <- Summary
witch_j_summary <- profilesummary #SSsummarize(witch_j)
profilesummary <- witch_j_summary
#Estimated parameters across runs
pars=witch_j_summary$pars
n <- 400
head(pars)

a <- pars[c(1:6,15:18),] %>% pivot_longer(1:n,names_to='run')

unique(a$Label)
M <- a$value[a$Label=="NatM_Lorenzen_Fem_GP_1"]
sigmaR <- a$value[a$Label=="SR_sigmaR"]
H <- a$value[a$Label=="SR_BH_steep"]

plot_ly(x=M, y=sigmaR, z=H, type="scatter3d", mode="markers", color=H)
t <- list(
 # family = "Courier New",
  size = 14,
  color = "black")
t1 <- list(
 # family = "Times New Roman",
  color = "black"
)
t2 <- list(
#  family = "Courier New",
  size = 14,
  color = "green")
t3 <- list(family = 'Arial')
g <- plot_ly(x=M, y=sigmaR, z=H, type="scatter", mode="markers", color=H)%>% 
  layout(title= list(text = "Steepness",font = t1), font=t, 
         legend=list(title=list(text='H',font = t2,
                                orientation = "h",   # show entries horizontally
                               # use center of legend as anchor
                                x =- 0.5,y=-1)) ,            # put legend in center of x-axis), 
         xaxis = list(title = list(text ='M', font = t3)),
         yaxis = list(title = list(text ='SigmaR', font = t3)))#,
 #        plot_bgcolor='#e5ecf6')
g
export(g, file = paste0(plotdir,'/M-SigmaR-H.png'))

dev.off()

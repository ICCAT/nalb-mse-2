# Load objects

nm <- c("BaseCase", "AGE", "CPUE", "SIZE")
sc <- file.path(shr_path, paste0("OM/", nm))
sc.i = ifelse(irun %in% 1:100, 1,
              ifelse(irun %in% 101:200, 2,
                     ifelse(irun %in% 201:300, 3, 4)))

# Find nrun (SS3 run):
sel_runs <- read.csv(paste0(sc[sc.i], "/Results/SelectedRuns4OM.csv"))
pos_row = irun %% 100
if(pos_row == 0) pos_row = 100
nrun <- sel_runs[pos_row, 1]

# Read objects:
replist <- SS_output(sc[sc.i],repfile=paste0("Report_",nrun,".sso"),compfile = paste0("CompReport_",nrun,".sso"),verbose=F,printstats=F)
ss3 <- readOutputss3(sc[sc.i],repfile=paste0("Report_",nrun,".sso"),compfile = paste0("CompReport_",nrun,".sso"))
stock <- readFLSss3(dir=sc[sc.i],repfile=paste0("Report_",nrun,".sso"),
                    compfile = paste0("CompReport_",nrun,".sso"),name=stock_name)
bs <- buildFLBFss330(ss3)
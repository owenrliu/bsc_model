# Parameter space outputs?? #
source(file="Indonesia_BSC_model/bsc_src_fxns_2.R")
library(tidyverse)

pp <- c("TRUE","FALSE")
slb <- c("TRUE","FALSE")
sl <- seq(50,150,by=10)
slc <- seq(0,1,by=0.1)
tb <- c("TRUE","FALSE")
tbc <- seq(0,1,by=0.1)
oa <- c("TRUE","FALSE")

params.all <- expand.grid(pp,tbc,slc,sl,slb,tb,oa)
names(params.all) <- c("PricePremium",
                       "TrawlBanCompliance", 
                       "SizeLCompliance",
                       "SIZELIMIT",
                       "SIZELIMITpolicy",
                       "TRAWLBANpolicy",
                       "OPENACCESSpolicy")
ptm <- proc.time()
test<-BSCFUNCTION()
ptm-proc.time()
## this will take ~24hours to run
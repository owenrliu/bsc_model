## COMPLIANCE VS. SIZE LIMIT

source("bsc_src_fxns.R") # Load model functions

# RUN SIM FOR DIFFERENT SIZE LIMITS AND COMPLIANCE LEVELS

comp_levels <- seq(0,1,by=0.1)
size_limits <- seq(50,150,by=10)

test_cases <- expand.grid(comp_levels,size_limits)
names(test_cases) <- c('compliance','sizelimit')
test_cases$bio <- NA
test_cases$harv <- NA

for(i in 1:nrow(test_cases)) { #for each test case...
  # run a simulationw ith given size limit and compliance
  sim <- runSim(sizelimit=T,size=test_cases$sizelimit[i],compliance = test_cases$compliance[i]) 
  # store final biomass and total harvest number
  test_cases[i,"bio"] <- tail(sim$Biomass,1)
  test_cases[i,"harv"] <- tail(sim$TotHarvest,1)
}

# plot heat map raster of output
library(ggplot2)

test_cases$compliance <- as.factor(test_cases$compliance)

# for biomass
ggplot(test_cases,aes(x=sizelimit,y=bio))+
  geom_line()+
  facet_wrap(~compliance)

# for harvest
ggplot(test_cases,aes(x=sizelimit,y=harv))+
  geom_line()+
  facet_wrap(~compliance)

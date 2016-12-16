#BSC Indonesia
#Dec 15 2016
#Ren
#Modified for shiny by Owen Liu, Dec 15, 2016

runSim <- function(sizelimit=F,size=100,compliance=1,
                   trawlban=F,openTrap=T,
                   openNet=T,openTrawl=T) {
  
  Nmat<-180 #number of months in age-structured model
  ModelTimeRange<- -240:120 #stabilizing time and projection time
  
  age<-1:Nmat
  data <- data.frame(age=age)
  data$CW<-187*(1-exp(-1.13*((data$age/12)+0.0038)))
  maturity<- 1/(12*(1+exp(12.5*(1-(data$CW/118.98)))))
  data$m<-0
  data$m[6:length(age)]<-maturity[6:length(age)]
  natmort<-exp(-0.0934)
  eggs<-((19.128*data$CW)-1517.1)*1000
  data$eggs<-eggs*(eggs>=0)
  data$meggs<-data$m*data$eggs
  data$weight<-0.00007*(data$CW^2.997)
  K<-1058543 #can be derived using catch-MSY method
  
  Price<-1997.36 #USD per ton, using data provided by the kkp and conversion of 12,500IDR/USD
  TrapCostPerF<-3450515.822
  NetCostPerF<-1478792.495
  TrawlCostPerF<-985861.6635
  
  #POLICY PARAMETER HERE-------------------------
  #BerriedPolicyCompliance<-0 #max is 1, 0 means no Berried Policy
  epsilon<-0.05 #for the open access scenario
  SizeLimit<- size #unit is mm
  SizeLimitCompliance<-compliance  #max is 1 (full compliance). 0 means no compliance
  
  PopINI<-matrix(1000, ncol = 1, nrow = Nmat)
  
  Leslie <- matrix(0, ncol = Nmat, nrow = Nmat)
  x<-rep(natmort,Nmat)
  y <- diag(x)
  Leslie[c(2:Nmat),]<-y[c(1:Nmat-1),]
  Leslie[1,]<-data$meggs
  
  ##What is Rsurv?
  ###########----------------
  Biomass<-vector()
  Pop<-PopINI
  GK<-7.85e8 ###ADJUST THIS TO GET BIOMASS == K
  count<-0
  RsurvVEC<-vector()
  for (month in ModelTimeRange){
    count<-count+1
    Pop2<-floor(Leslie%*%Pop)
    RsurvVEC[count]<-GK/Pop2[1,1] #output is 2.022027e-06
    Pop2[1,1]<-GK
    Pop<-Pop2
    Biomass[count]<-sum(Pop*data$weight)/(1000*1000) #from grams to mt
  }
  #########------------------
  Rsurv<-tail(RsurvVEC,n=1)
  
  # Rsurv<-2.022027e-06
  
  ######--- MAIN MODEL HERE
  Strap<-1/(1+exp(25*(1-(data$CW/105))))
  Sgnet<-1/(1+exp(10*(1-(data$CW/90))))
  Strawl<-1/(1+exp(10*(1-(data$CW/80))))
  
  alpha<-1 ##ADJUSTABLE
  Biomass<-vector()
  TotHarvest<-vector()
  TrapHarvest<-vector()
  NetHarvest<-vector()
  TrawlHarvest<-vector()
  TotProfit<-vector()
  TrapProfit<-vector()
  NetProfit<-vector()
  TrawlProfit<-vector()
  
  Pop<-PopINI
  PopBerriedPolicy<-PopINI*0
  
  
  Etrap<-0.105
  Egnet<-0.025
  Etrawl<-0.0114
  EtrapVec<-vector()
  EgnetVec<-vector()
  EtrawlVec<-vector()
  
  #catch limit
  data$CatchLimit<- (1-SizeLimitCompliance)
  data$CatchLimit[data$CW>=SizeLimit] = 1
  
  
  #data$CatchLimit[9:length(age)]<-1
  
  BerriedPolicyEfficiency=0
  
  count<-0
  for (month in ModelTimeRange){
    count<-count+1
    
    if (BerriedPolicyCompliance>0){
      if (month>0){
        BerriedPolicyEfficiency=BerriedPolicyCompliance
      }else {}
    }
    
    Ftrap=exp(-Etrap*Strap)
    Fgnet=exp(-Egnet*Sgnet)
    Ftrawl=exp(-Strawl*Etrawl)
    Ftot=Ftrap*Fgnet*Ftrawl #F=exp(-Etrap*Strap-Egnet*Sgnet-Strawl*Etrawl)
    M=rep(natmort,Nmat)
    
    LeslieModel <- matrix(0, ncol = Nmat, nrow = Nmat)
    
    Mort<-M*Ftot
    DiagMort <- diag(Mort)
    LeslieModel[c(2:Nmat),]<-DiagMort[c(1:Nmat-1),]
    LeslieModelBerried<-(LeslieModel>0)*1 
    LeslieModelBerried[1,]<-data$eggs
    LeslieModel[1,]<-data$meggs
    
    #harvest
    Harvest<-Pop*((1-Mort)*(1-Ftot)/ ((1-M)+(1-Ftot)))
    
    #BERRIED POLICY
    PopBerriedPolicy<-floor(Harvest*data$m*BerriedPolicyEfficiency)
    Harvest<-Harvest-PopBerriedPolicy
    
    TotHarvest[count]<-sum(data$weight*Harvest)/(1000*1000)
    
    #population transition
    Pop2<-floor((LeslieModel%*%Pop) + (LeslieModelBerried%*%PopBerriedPolicy))
    
    if ((Pop2[1,1]*Rsurv) < GK){
      Pop2[1,1]<-(GK^alpha+(GK-(Rsurv*Pop2[1,1]))^alpha)^(1/alpha)
    } else {
      Pop2[1,1]<-GK
    }
    
    FracTrap<-(1-Ftrap)/((1-Ftrap)+(1-Fgnet)+(1-Ftrawl))
    FracNet<-(1-Fgnet)/((1-Ftrap)+(1-Fgnet)+(1-Ftrawl))
    FracTrawl<-(1-Ftrawl)/((1-Ftrap)+(1-Fgnet)+(1-Ftrawl))
    
    is.nan.data.frame <- function(x)
      do.call(cbind, lapply(x, is.nan))
    FracTrap[is.nan.data.frame(FracTrap)] <- 0
    FracNet[is.nan.data.frame(FracNet)] <- 0
    FracTrawl[is.nan.data.frame(FracTrawl)] <- 0
    
    TrapHarvest[count]<-sum(data$weight*Harvest*FracTrap)/(1000*1000)
    NetHarvest[count]<-sum(data$weight*Harvest*FracNet)/(1000*1000)
    TrawlHarvest[count]<-sum(data$weight*Harvest*FracTrawl)/(1000*1000)
    
    TrapProfit[count]<-(TrapHarvest[count]*Price) - TrapCostPerF*Etrap
    NetProfit[count]<-(NetHarvest[count]*Price) - NetCostPerF*Egnet
    TrawlProfit[count]<-(TrawlHarvest[count]*Price) - TrawlCostPerF*Etrawl
    
    Pop<-Pop2
    Biomass[count]<-sum(Pop*data$weight)/(1000*1000) #from grams to mt
    
    EtrapVec[count]<-Etrap
    EgnetVec[count]<-Egnet
    EtrawlVec[count]<-Etrawl
    
    
    #POLICIES
    #Size-limit
    #Trawl Ban
    #F forever, Open Access
    
  
    if (month>=0){ # if after ramp-up period...
      ## OPEN ACCESS OPTION FOR EACH GEAR
      if(openTrap) {
        Etrap<-Etrap+(epsilon*(((Price*TrapHarvest[count])/(TrapCostPerF*Etrap))-1)*Etrap)
      }
      if(openNet){
        Egnet<-Egnet+(epsilon*(((Price*NetHarvest[count])/(NetCostPerF*Egnet))-1)*Egnet)
      }
      if(openTrawl){
        Etrawl<-Etrawl+(epsilon*(((Price*TrawlHarvest[count])/(TrawlCostPerF*Etrawl))-1)*Etrawl)
      }
      if(trawlban) {
        Etrawl<-0 #trawl ban
      }
      ## SIZE LIMIT COMPLIANCE
      if(sizelimit) {
        Strap<-Strap*data$CatchLimit
        Sgnet<-Sgnet*data$CatchLimit
        Strawl<-Strawl*data$CatchLimit
      }
    }
  }
  sim.out <- list(data=data,Biomass=Biomass,Pop=Pop,TotHarvest=TotHarvest,
              TrapHarvest=TrapHarvest,NetHarvest=NetHarvest,
              TrawlHarvest=TrawlHarvest,TrapProfit=TrapProfit,
              NetProfit=NetProfit,TrawlProfit=TrawlProfit,
              EtrapVec=EtrapVec,EgnetVec=EgnetVec,EtrawlVec=EtrawlVec)
  return(sim.out)
}

#xrange<- -20:120
#plot(xrange,Biomass[xrange+241], xlab="Time (month)",ylab="Biomass (mt)",main="Total Biomass",type="o")

plotBioTot <- function(sim.out) {
  plot(-10:120,sim.out$Biomass[231:361], xlab="Time (month)",ylab="Biomass (mt)",main="Total Biomass",type="o")
}

plotInd <- function(sim.out) {
  plot(sim.out$Pop,xlab="Age (month)",ylab="Individuals")
}

plotBioAge <- function(sim.out) {
  plot(sim.out$Pop*sim.out$data$weight/(1000*1000), xlab="Age (month)",ylab="Biomass (mt)")
}

plotBioCW <- function(sim.out) {
  plot(sim.out$data$CW,sim.out$Pop*sim.out$data$weight/(1000*1000), xlab="CW (mm)",ylab="Biomass (mt)")
}

plotTotHarvest <- function(sim.out) {
  plot(-10:120,sim.out$TotHarvest[231:361], xlab="Time (month)", ylab="Harvest (mt)",main="Total Harvest",type="o")
}

plotTrapHarvest <- function(sim.out) {
  plot(-10:120,sim.out$TrapHarvest[231:361], xlab="Time (month)", ylab="Harvest (mt)",main="Trap Harvest",type="o")
}

plotNetHarvest <- function(sim.out) {
  plot(-10:120,sim.out$NetHarvest[231:361], xlab="Time (month)", ylab="Harvest (mt)",main="Gillnet Harvest",type="o")
}

plotTrawlHarvest <- function(sim.out) {
  plot(-10:120,sim.out$TrawlHarvest[231:361], xlab="Time (month)", ylab="Harvest (mt)",main="Trawl Harvest",type="o")
}

plotTrapProfit <- function(sim.out) {
  plot(-10:120,sim.out$TrapProfit[231:361], xlab="Time (month)", ylab="Profit (USD)",main="Trap Profit",type="o")
}

plotNetProfit <- function(sim.out) {
  plot(-10:120,sim.out$NetProfit[231:361], xlab="Time (month)", ylab="Profit (USD)",main="Gillnet Profit",type="o")
}

plotTrawlProfit <- function(sim.out) {
  plot(-10:120,sim.out$TrawlProfit[231:361], xlab="Time (month)", ylab="Profit (USD)",main="Trawl Profit",type="o")
}

plotEffort <- function(sim.out) {
  plot(-10:120,sim.out$EtrapVec[231:361], xlab="Time (month)", ylab="Effort",main="Effort",
       ylim=c(0,max(sim.out$EtrapVec,sim.out$EgnetVec,sim.out$EtrawlVec)),type="o")
  points(-10:120,sim.out$EgnetVec[231:361], xlab="Time (month)", col="red",type="o")
  points(-10:120,sim.out$EtrawlVec[231:361], xlab="Time (month)", col="blue",type="o")  
}

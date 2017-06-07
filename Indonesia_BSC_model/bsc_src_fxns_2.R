#### New Model as of 5.16.2017 ####
require(tidyverse)
require(extrafont)
require(ggthemes)

BSCFUNCTION<-function(BerriedPolicyCompliance=0,
                      PricePremium=FALSE,
                      TrawlBanCompliance=1, 
                      SizeLCompliance=1, 
                      Efforts=c(0.0301,0.00793,0.0038),
                      SIZELIMIT=100,
                      SIZELIMITpolicy=FALSE,
                      TRAWLBANpolicy=FALSE,
                      OPENACCESSpolicy=FALSE){
  
  Nmat<-180 #number of months in age-structured model
  AdjustTime<- 12*200 #months to spin up the model (12 months times x years)
  ProjectionTime<- 12*20 #months to project (12 months times x years)
  leadT<-0
  ModelTimeRange<- -AdjustTime:ProjectionTime #stabilizing time and projection time
  
  #### LIFE HISTORY ###
  age<-c(1:Nmat)
  data <- data.frame(age=age)
  data$CW<-187*(1-exp(-1.13*((data$age/12)+0.0038))) #carapace width by age
  maturity<- 1/(12*(1+exp(12.5*(1-(data$CW/118.98))))) #maturity by age
  data$m<-0
  data$m[6:length(age)]<-maturity[6:length(age)]
  natmort<-exp(-0.0934) # natural mortality
  eggs<-((19.128*data$CW)-1517.1)*1000 # fecundity
  data$eggs<-eggs*(eggs>=0)
  data$meggs<-data$m*data$eggs
  data$weight<-0.00007*(data$CW^2.997) # CW-weight relationship
  K<-160960#1058543 #MSY--can be derived using catch-MSY method
  
  ### PRICE ###
  #USD per ton, using data provided by the kkp and conversion of 12,500IDR/USD
  if (PricePremium){ # is there a price premium (scaling with carrying capacity? (Yes/No))
    Price<- ((156.25*data$CW)+6875)*1000/12500
  }else{
    Price<- (50000000/12500)*(data$CW>0)
  }
  
  ### COST ###
  #Cost per unit of effort for three gears. Source: SFP 2014
  TrapCostPerF<-(222000*52488000*.7)/(12*12500*10*Efforts[1]) #10 kg of harvest per day, 52,488MTx1000kg/MT, 12 is harvest per month
  NetCostPerF<- (146000*52488000*.2)/(12*12500*10*Efforts[2])
  TrawlCostPerF<- (343000*52488000*.1)/(12*12500*10*Efforts[3])  
  
  ### POLICIES ###
  # non-take of berried females (compliance with regulation)
  BerriedPolicyCompliance<-BerriedPolicyCompliance #max is 1, 0 means no Berried Policy
  epsilon<-0.1#0.05 #for the open access scenario
  
  ### size limit ###
  SizeLimit<-SIZELIMIT
  SizeLimitCompliance<-SizeLCompliance  #max is 1 (full compliance). 0 means no compliance
  
  # seed population
  PopINI<-matrix(1000000, ncol = 1, nrow = Nmat)
  
  # set up Leslie matrix
  Leslie <- matrix(0, ncol = Nmat, nrow = Nmat)
  x<-rep(natmort,Nmat)
  y <- diag(x)
  Leslie[c(2:Nmat),]<-y[c(1:Nmat-1),]
  Leslie[1,]<-data$meggs
  
  ##What is Rsurv?
  ###########----------------
  Biomass<-vector()
  SSB<-vector()
  Pop<-PopINI
  
  # ----GET GK. Recruitment in the absence of fishing that would lead to biomass K.
  # for (GK in seq(1.197e8,1.2e8,by=0.0001e8)){
  #   count<-0
  #   RsurvVEC<-vector()
  #   for (month in ModelTimeRange){
  #     count<-count+1
  #     Pop2<-floor(Leslie%*%Pop)
  #     RsurvVEC[count]<-GK/Pop2[1,1] #output is 2.022027e-06
  #     
  #     #if (month%%6==0){
  #     Pop2[1,1]<-GK
  #     #} else
  #     #{Pop2[1,1]<-0}
  #     
  #     Pop<-Pop2
  #     Biomass[count]<-sum(Pop*data$weight)/(1000*1000) #from grams to mt
  #     SSB[count]<-sum(floor(data$m*Pop)*data$weight)/(1000*1000)
  #   }
  #   indicator<-tail(Biomass,n=1)
  #   if (indicator >K){break}
  # }
  # indicator
  # GK
  
  GK<-1.1988e8
  count<-0
  RsurvVEC<-vector()
  for (month in ModelTimeRange){
    count<-count+1
    Pop2<-floor(Leslie%*%Pop)
    RsurvVEC[count]<-GK/Pop2[1,1] #output is 2.022027e-06
    
    Pop2[1,1]<-GK
    
    Pop<-Pop2
    Biomass[count]<-sum(Pop*data$weight)/(1000*1000) #from grams to mt
    SSB[count]<-sum(floor(data$m*Pop)*data$weight)/(1000*1000)
  }
  # tail(Biomass,n=1)
  SSBk<-tail(SSB,n=1)
  #########------------------
  Rsurv<-tail(RsurvVEC,n=1)   #Rsurv<-2.022027e-06
  
  ######--- MAIN MODEL HERE
  Strap<-1/(1+exp(25*(1-(data$CW/105))))
  Sgnet<-1/(1+exp(10*(1-(data$CW/90))))
  Strawl<-1/(1+exp(10*(1-(data$CW/80))))
  
  alpha<-1 ##ADJUSTABLE
  Biomass<-vector()
  SSB<-vector()
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
  
  Etrap<-Efforts[1]#0.01#0.105
  Egnet<-Efforts[2]#0.002#0.025
  Etrawl<-Efforts[3]#0.00065#0.0114
  
  EtrapVec<-vector()
  EgnetVec<-vector()
  EtrawlVec<-vector()
  
  #catch limit
  data$CatchLimit<- (1-SizeLimitCompliance)
  data$CatchLimit[data$CW>=SizeLimit] = 1
  
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
    F=Ftrap*Fgnet*Ftrawl #F=exp(-Etrap*Strap-Egnet*Sgnet-Strawl*Etrawl)
    M=rep(natmort,Nmat)
    
    LeslieModel <- matrix(0, ncol = Nmat, nrow = Nmat)
    
    Mort<-M*F
    DiagMort <- diag(Mort)
    LeslieModel[c(2:Nmat),]<-DiagMort[c(1:Nmat-1),]
    LeslieModelBerried<-(LeslieModel>0)*1 
    LeslieModelBerried[1,]<-data$eggs
    LeslieModel[1,]<-data$meggs
    
    #harvest
    Harvest<-Pop*((1-Mort)*(1-F)/ ((1-M)+(1-F)))
    
    #BERRIED POLICY
    PopBerriedPolicy<-floor(Harvest*data$m*BerriedPolicyEfficiency)
    Harvest<-Harvest-PopBerriedPolicy
    
    TotHarvest[count]<-sum(data$weight*Harvest)/(1000*1000)
    
    #population transition
    Pop2<-floor((LeslieModel%*%Pop) + (LeslieModelBerried%*%PopBerriedPolicy))
    
    ##RECRUITMENT
    if ((Pop2[1,1]*Rsurv) < (GK)){
      Pop2[1,1]<-((GK^alpha+(GK-(Rsurv*Pop2[1,1]))^alpha)^(1/alpha))
    } else {
      Pop2[1,1]<-GK
    }
    
    #PLOT the Stock-recruitment function
    if (FALSE){
      
      GK<-100 #max recruitment
      varB<-0.7 #fraction of SSBk where R is 1/2
      SSBk<-100 #SSB at K
      count<-0
      R<-vector()
      SSBLen<-1:150
      for (SSB in SSBLen){
        count<-count+1
        #R[count]<-((GK/(0.66*varB*SSBk))*SSB)/(1+(SSB/(varB*SSBk)))
        R[count]<-((GK/(varB*SSBk))*SSB)/(1+(SSB/(varB*SSBk)))
      }
      #plot(SSBLen,R)
      
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
    
    TrapProfit[count]<-(sum(data$weight*Harvest*FracTrap*Price)/(1000*1000)) - (TrapCostPerF*Etrap)
    NetProfit[count]<-(sum(data$weight*Harvest*FracNet*Price)/(1000*1000)) - (NetCostPerF*Egnet)
    TrawlProfit[count]<-(sum(data$weight*Harvest*FracTrawl*Price)/(1000*1000)) - (TrawlCostPerF*Etrawl)
    
    Pop<-Pop2
    Biomass[count]<-sum(Pop*data$weight)/(1000*1000) #from grams to mt
    
    EtrapVec[count]<-Etrap
    EgnetVec[count]<-Egnet
    EtrawlVec[count]<-Etrawl
    
    #---POLICIES
    if (month>=0){
      #---OPEN ACCESS
      if (OPENACCESSpolicy){
        Etrap<-Etrap+(epsilon*(((sum(data$weight*Harvest*FracTrap*Price)/(1000*1000))/(TrapCostPerF*Etrap))-1)*Etrap)
        Egnet<-Egnet+(epsilon*(((sum(data$weight*Harvest*FracNet*Price)/(1000*1000))/(NetCostPerF*Egnet))-1)*Egnet)
        Etrawl<-Etrawl+(epsilon*(((sum(data$weight*Harvest*FracTrawl*Price)/(1000*1000))/(TrawlCostPerF*Etrawl))-1)*Etrawl)
      }
      
      #---TRAWL BAN
      if (TRAWLBANpolicy){
        
        Etrawl<-Efforts[3]*(1-TrawlBanCompliance)
      } #no need to worry about OA. It should not be OA when there is a trawl ban
      
    }
    
    if (month == 0){
      #---SIZE LIMIT
      if (SIZELIMITpolicy){
        Strap<-Strap*data$CatchLimit
        Sgnet<-Sgnet*data$CatchLimit
        Strawl<-Strawl*data$CatchLimit}
    }
  }
  newList <- list("CW"=data$CW,"Pop"=Pop,"BIOM"=Pop*data$weight/(1000*1000),
                  "Biomass"=Biomass,"TotHarvest"=TotHarvest,
                  "TrapHarvestENDYEAR"=data$weight*Harvest*FracTrap/(1000*1000),
                  "NetHarvestENDYEAR"=data$weight*Harvest*FracNet/(1000*1000),
                  "TrawlHarvestENDYEAR"=data$weight*Harvest*FracTrawl/(1000*1000),
                  "TrapProfit"=TrapProfit,"NetProfit"=NetProfit,"TrawlProfit"=TrawlProfit,
                  "TrapHarvest"=TrapHarvest,"NetHarvest"=NetHarvest,
                  "TrawlHarvest"=TrawlHarvest,"ModelTimeRange"=ModelTimeRange,
                  "EtrapVec"=EtrapVec,"EgnetVec"=EgnetVec,"EtrawlVec"=EtrawlVec,
                  "leadT"=leadT,"ProjectionTime"=ProjectionTime)
  return(newList)
  
}

#### Baseline scenario (default params) ####
baseline <- BSCFUNCTION()
tvec <- -baseline$leadT:baseline$ProjectionTime
simlength <- (length(baseline$Biomass)-baseline$leadT-baseline$ProjectionTime):length(baseline$Biomass)
TotBio <- baseline$Biomass[simlength]
TotHarvest <- baseline$TotHarvest[simlength]
TrapHarvest <- baseline$TrapHarvest[simlength]
NetHarvest <- baseline$NetHarvest[simlength]
TrawlHarvest <- baseline$TrawlHarvest[simlength]

TrapProfit <- baseline$TrapProfit[simlength]
NetProfit <- baseline$NetProfit[simlength]
TrawlProfit <- baseline$TrawlProfit[simlength]

EtrapVec <- baseline$EtrapVec[simlength]
EgnetVec <- baseline$EgnetVec[simlength]
EtrawlVec <- baseline$EtrawlVec[simlength]

base_sim_results <- data_frame(tvec,simlength,TotBio,TotHarvest,TrapHarvest,
                          NetHarvest,TrawlHarvest,TrapProfit,NetProfit,TrawlProfit) %>%
  mutate(sim="base")

base_eff_results <- data_frame(tvec,Trap=EtrapVec,Gillnet=EgnetVec,Trawl=EtrawlVec)%>%
  mutate(sim="base")
base_eff_results <- base_eff_results %>% gather("gear","value",Trap:Trawl)%>%
  mutate(sim="base")

#by age
Pop <- as.numeric(baseline$Pop)
BIOM <- as.numeric(baseline$BIOM)
CW <- as.numeric(baseline$CW)
AGE <- 1:length(Pop)
base_age_results <- data_frame(AGE,Pop,BIOM,CW)%>%
  mutate(sim="base")

#### Plot functions ####
plot_all <- function(sim.out,which_plot,label){

  # The ggplot way
  # by time
  tvec <- -sim.out$leadT:sim.out$ProjectionTime
  simlength <- (length(sim.out$Biomass)-sim.out$leadT-sim.out$ProjectionTime):length(sim.out$Biomass)
  TotBio <- sim.out$Biomass[simlength]
  TotHarvest <- sim.out$TotHarvest[simlength]
  TrapHarvest <- sim.out$TrapHarvest[simlength]
  NetHarvest <- sim.out$NetHarvest[simlength]
  TrawlHarvest <- sim.out$TrawlHarvest[simlength]
  
  TrapProfit <- sim.out$TrapProfit[simlength]
  NetProfit <- sim.out$NetProfit[simlength]
  TrawlProfit <- sim.out$TrawlProfit[simlength]
  
  EtrapVec <- sim.out$EtrapVec[simlength]
  EgnetVec <- sim.out$EgnetVec[simlength]
  EtrawlVec <- sim.out$EtrawlVec[simlength]
  
  sim_results <- data_frame(tvec,simlength,TotBio,TotHarvest,TrapHarvest,
                           NetHarvest,TrawlHarvest,TrapProfit,NetProfit,TrawlProfit)%>%
    mutate(sim="sim") %>%
    bind_rows(base_sim_results)
  
  eff_results <- data_frame(tvec,Trap=EtrapVec,Gillnet=EgnetVec,Trawl=EtrawlVec)
  eff_results <- eff_results %>% gather("gear","value",Trap:Trawl)%>%
    mutate(sim="sim")%>%
    bind_rows(base_eff_results)
  
  #by age
  Pop <- as.numeric(sim.out$Pop)
  BIOM <- as.numeric(sim.out$BIOM)
  CW <- as.numeric(sim.out$CW)
  AGE <- 1:length(Pop)
  age_results <- data_frame(AGE,Pop,BIOM,CW)%>%
    mutate(sim="sim")%>%
    bind_rows(base_age_results)
  # age_results <- age_results %>% gather("result","value",Pop:CW)
  
  #theme
  bsc_theme <- theme_minimal()+
    theme(text = element_text(size = 18,color="black",family="Rockwell"),
          panel.border = element_blank(),
          legend.position = c(0.8,0.8),
          plot.margin = unit(c(6,12,6,6),"points"))
  
  # ggplots
  if(which_plot %in% names(sim_results)) {
    out <- ggplot(sim_results,aes_string("tvec",which_plot,col="sim"))+
            geom_line(size=2)+
            xlab("Time (months)")+
            ylab(label)+
            xlim(0,250)+
            scale_color_manual(name="",values=c("black","forestgreen"),labels=c("base","sim"))+
            bsc_theme
  }
  if(which_plot %in% names(age_results)) {
    out <- ggplot(age_results,aes_string("AGE",which_plot,col="sim"))+
            geom_line(size=2)+
            xlab("Age (months)")+
            ylab(label)+
            scale_color_manual(name="",values=c("black","forestgreen"),labels=c("base","sim"))+
            bsc_theme
  }
  if(which_plot=="BioCW") {
    out <- ggplot(age_results,aes_string("CW","BIOM",col="sim"))+
      geom_line(size=2)+
      xlab("Carapace Width (mm)")+
      ylab(label)+
      scale_color_manual(name="",values=c("black","forestgreen"),labels=c("base","sim"))+
      bsc_theme
  }
  if(which_plot=="Effort") {
    out <- ggplot(eff_results,aes(tvec,value,col=gear,linetype=sim))+
      geom_line(size=2)+
      xlab("Time (months)")+
      ylab(label)+
      scale_linetype_manual(name="",values=c("solid","dotted"),labels=c("base","sim"))+
      bsc_theme+
      theme(legend.title=element_blank(),
            legend.position = c(0.8,0.5),
            legend.key.width = unit(2,"cm"))
  }
  return(out)
}


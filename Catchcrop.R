
num_days <- function(d,m,y){
  ##     Numbers the day in the record (streamflow, ETO, rainfall) from
  ##     the reference point of 1st January 1800.
  Mth=matrix(c(31,28,31,30,31,30,31,31,30,31,30,31),ncol=1)
  day = 0;
  for(i in 1800:y){
    a = i / 4.;
    b = round(i/4);
    Mth[2,1] = 28;
    ndays = 365;
    if (a == b) {
      Mth[2,1] = 29;
      ndays = 366;
    }
    if (i < y) {
      day = day+ndays;
    } else {
      if (m != 1) {
        for(j in 1:(m-1)){
	  day = day+Mth[j,1];
	}
      }
      day = day+d;
    }
  }
  return(day)
}

dates <- function(crop,year){
  ##     Determines the start date in terms of the 'num_days' subroutine */
  ##     for particular crop */
  start_d = Seasons[1,crop];
  start_m = Seasons[2,crop];
  end_d=Seasons[3,crop];
  end_m=Seasons[4,crop];
  nstart <- num_days(start_d, start_m, year);
  if(end_m <= start_m) {
    nend <- num_days(end_d, end_m, year+1);
  } else {
    nend <- num_days(end_d, end_m, year);
  }
  return(c(nstart,nend))
}

CatchCrop <- function(Crops2,Soils2,Irrig,
                      Years,StartMonth,
                      Rainfall,PET,
                      RegionNumber,LF,PIR,
                      Seasons
                      ){
  
  ##    READ IN CROP PARAMETER FILE
  ml=ncol(Crops2); ##number of crops
  Lini <- matrix(0,nrow=ml,ncol=1)
  Ldev <- matrix(0,nrow=ml,ncol=1)
  Lmid <- matrix(0,nrow=ml,ncol=1)
  Lend <- matrix(0,nrow=ml,ncol=1)
  KCini <- matrix(0,nrow=ml,ncol=1)
  KCmid <- matrix(0,nrow=ml,ncol=1)
  KCend <- matrix(0,nrow=ml,ncol=1)
  RDini <- matrix(0,nrow=ml,ncol=1)
  RDend <- matrix(0,nrow=ml,ncol=1)
  P <- matrix(0,nrow=ml,ncol=1)
  CC <- matrix(0,nrow=ml,ncol=1)
  KY <- matrix(0,nrow=ml,ncol=1)
  YM <- matrix(0,nrow=ml,ncol=1)
  for(l in 1:ml){
    Lini[l,1]=Crops2[2,l];
    Ldev[l,1]=Crops2[3,l];
    Lmid[l,1]=Crops2[4,l];
    Lend[l,1]=Crops2[5,l];
    KCini[l,1]=Crops2[6,l];
    KCmid[l,1]=Crops2[7,l];
    KCend[l,1]=Crops2[8,l];
    RDini[l,1]=Crops2[9,l];
    RDend[l,1]=Crops2[10,l];
    P[l,1]=Crops2[11,l];
    CC[l,1]=Crops2[12,l];
    KY[l,1]=Crops2[13,l];
    YM[l,1]=Crops2[14,l];
    ##15 Cfact
  }

  ## C     READ IN SOIL PARAMETER FILE
  mm=ncol(Soils2); ##number of crops
  lu <- matrix(0,nrow=mm,ncol=1)
  TAW <- matrix(0,nrow=mm,ncol=1)
  TEW <- matrix(0,nrow=mm,ncol=1)
  REW <- matrix(0,nrow=mm,ncol=1)
  IS <- matrix(0,nrow=mm,ncol=1)
  SD <- matrix(0,nrow=mm,ncol=1)
  CS <- matrix(0,nrow=mm,ncol=1)
  for(m in 1:mm){
    lu[m,1]=Soils2[1,m];
    TAW[m,1]=Soils2[2,m];
    TEW[m,1]=Soils2[3,m];
    REW[m,1]=Soils2[4,m];
    IS[m,1]=Soils2[5,m];
    SD[m,1]=Soils2[6,m];
    CS[m,1]=Soils2[7,m]; 
  }

  ## ##Use PET instead if available
  ## CreateMatrix(PET,MatrixWidth(MinTemp),1,DOUBLEPRECISION,ERASE);
  ## for (j = 1; j <= MatrixWidth(MinTemp); j++) {
  ## 	PET[1,j]=0.6108*exp((17.27*MinTemp[j,1])/(MinTemp[j,1]+237.3));
  ## }

  nt=365;
  ##nl=13;
  ##nm=19;

  ##OUTPUTS
  NoCrops=ncol(Crops2);
  
  Yield=matrix(0,Years+2,NoCrops+1);
  CropWU=matrix(0,Years,NoCrops+1)
  WB <- matrix(0,Years,5)

  m=1;                                  #number of land unit
  ndays1 <- num_days(1,1,1800) #Using 1800 as first year of record

  for(l in 1:NoCrops){
  
    for(y in 1:Years){
      ## FIXME Using 1800 as dummy input for compatibility with num_dates
      ## WILL AFFECT LEAP YEARS
      year=1800+y-1; #year. 
      aa=dates(l,year);
      start=aa[1]
      end1=aa[2]
      dop=start;
      ##TODO: Fortran code adjusts date of planting to when RR>thres
      neg = round((dop - start) / 10) - 1; ## sets pre-season time-steps
      n = round((end1 - dop) / 10) + 1; ## sets post-planting time-steps
      nsteps=n-neg-1;
    
      RR <- matrix(0,nrow=nsteps,ncol=1)
      ETO <- matrix(0,nrow=nsteps,ncol=1)
      NDR <- matrix(0,nrow=nsteps,ncol=1)
      eto_ave <- matrix(0,nrow=nsteps,ncol=1)

      ##ROi
      RO <- matrix(0,nrow=nsteps,ncol=1)
      Rr_ave <- matrix(0,nrow=nsteps,ncol=1)
      Ro_ave <- matrix(0,nrow=nsteps,ncol=1)
      CC_pre <- matrix(0,nrow=nsteps,ncol=1)
      ##RDi
      RD_adj <- matrix(0,nrow=nsteps,ncol=1)
      ## KEi   
      KE <- matrix(0,nrow=nsteps,ncol=1)
      eso <- matrix(0,nrow=nsteps,ncol=1)
      ##CWS
      CAW <- matrix(0,nrow=nsteps,ncol=1)
      DD <- matrix(0,nrow=nsteps,ncol=1)
      ##demand/CWS
      IR <- matrix(0,nrow=nsteps,ncol=1)
      ##ETMi
      KC_adj <- matrix(0,nrow=nsteps,ncol=1)
      ##ETAi
      KS <- matrix(0,nrow=nsteps,ncol=1)
      ##DEMi
      CR_tot <- matrix(0,nrow=nsteps,ncol=1)

    
      ##       Calculates 10 daily ETO, RR, and NDR (NRD is taken as the */
      ##       number of rainy days within the 10 day timestep that exceeds */
      ##       a threshold of 5.0 mm). */
      for(ni in 1:nsteps){
        RR[ni,1] = 0.;
        ETO[ni,1] = 0.;
        NDR[ni,1] = 0;
        for(j in 1:10){
          jj = dop + (ni - 1) * 10 + j - ndays1;
          RR[ni,1] = RR[ni,1]+Rainfall[RegionNumber,jj]; ## rain over 10-day time-step (in mm)
          ETO[ni,1] = ETO[ni,1]+ PET[1,jj]; ## ETO over 10-day time-step */
          if (Rainfall[RegionNumber,jj] > 5.) {
            NDR[ni,1]=NDR[ni,1]+1;
          }
        }
        eto_ave[ni,1] = ETO[ni,1] / 10.; ## average daily ETO */
      }

    
      ##      SETS SOIL AVAILABLE WATER (SAW) AND GROWING PERIOD (Ltot)
      SAW = TAW[m,1] * (SD[m,1] / 1000.);
      Ltot = Lini[l,1] + Ldev[l,1] + Lmid[l,1] + Lend[l,1];
		
      ##INITIALISES VARIABLES
      SUMETM=0;
      SUMETA=0;
      SUMETC=0;
      SUMDEM=0;
      SUMIR=0;
      SUMRO=0;
      SUMDD=0;
      YA=0;
      ETM=0;
      ETC=0;
      ETA=0;
      DEM=0;

      ##     SETS INITIAL DS SOIL RESERVOIR TO AVE. OF WS LAST TIMESTEP */
      SR <- matrix(0,nrow=nsteps,ncol=1) #actual soil water storage
      CR <- matrix(0,nrow=nsteps,ncol=1) #actual root zone storage spell
      RH <- matrix(0,nrow=nsteps,ncol=1) #root zone relative humidity
			
      ## Fortran code set initial soil moisture read from file depending on season */
                                        #SR[1,1]=SAW; #full water
      SR[1,1]=0;                        #dry soil
      for(ni in 1:nsteps){ ## SR(ni-1) needed in CWS subrout. so ni=-neg-1,n */
        DEM=0;
        CR[ni,1]=0;
        RH[ni,1]=0;
      }
      RTOT=0.;
      PETTOT=0.;
      DDTOT=0.;
			 
      DEMTOT=-9999;
      ETMTOT=-9999;
      ETATOT=-9999;
      ETCTOT=-9999;
      DIVTOT=-9999;


      ## 10 day timesteps for this year
      ##  for (ni = 1; ni <= nsteps; ni++) {
      for(ni in 1:nsteps){
    
        ##ROi(m,l,nl,nm,nt,ni); ##surface runoff
        if (RR[ni,1] > 0. && NDR[ni,1] > 0) {
          Rr_ave[ni,1] = RR[ni,1] / NDR[ni,1]; ## pseudo-daily rainfall */
        }
        if (ni < 1) {
          CC_pre[ni,1] = 1.5; ## bare soil */
        } else {
          CC_pre[ni,1] = CC[l,1]; ## soil has vegetation cover */
        }
        calc = CC_pre[ni,1] * IS[m,1] * CS[m,1];
        if (Rr_ave[ni,1] >= calc) {
          Ro_ave[ni,1] = ((Rr_ave[ni,1] - 0.2 * calc)^2)/(Rr_ave[ni,1] + calc * .8); ## average daily runoff */
          RO[ni,1] = NDR[ni,1] * Ro_ave[ni,1]; ## 10-daily runoff */
        } else {
          RO[ni,1] = 0.;
        }
        SUMRO = SUMRO+RO[ni,1];

       
        ##KEi(eso,eto_ave,m,nm,nt,ni); ##soil evaporation
        if (NDR[ni,1] == 0) {
          KE[ni,1] = 0.;
        } else {
          TD = (10 - NDR[ni,1]) / NDR[ni,1];
          eso[ni,1] = eto_ave[ni,1] * .7; ## 1.15 in old code */
          TL = REW[m,1] / eso[ni,1];
          LEW = TEW[m,1] - REW[m,1];
          if (TL <= TD) {
            x = -(TD - TL) * eso[ni,1] * (REW[m,1] / LEW + 1.) / TEW[m,1];
            KE[ni,1] = (TEW[m,1] - LEW * exp(x)) / (TD * eto_ave[ni,1]);
          } else {
            KE[ni,1] = .7; ## 1.15 in old code */
          }
        }
    
        ##RDi(l,m,nl,nm,nt,ni); ##root depth
        RD_adj[ni,1] = 0.;
        if (ni >= 1 && ni * 10 <= Ltot) {
          if (ni * 10 <= Lini[l,1] + Ldev[l,1]) {
            RD_adj[ni,1] = RDini[l,1] + (RDend[l,1] - RDini[l,1]) * 10. * ni /
              (Lini[l,1] + Ldev[l,1]);
          } else {
            RD_adj[ni,1] = RDend[l,1];
          }
        }
        RD_adj[ni,1] = min(RD_adj[ni,1],SD[m,1]);
    
        ##CWS(m,ni); ##crop water requirement
        IR[ni,1]=DEM*Irrig[l,1];
        SUMIR = SUMIR+ IR[ni,1];
        CAW[ni,1] = TAW[m,1] * RD_adj[ni,1] / 1000.; ## updates crop avail. H20 */
        SR[ni,1] = min(SAW,SR[ni,1] + RR[ni,1] + IR[ni,1] - RO[ni,1]);
        DD[ni,1] = max(0.,SR[ni,1] + RR[ni,1] + IR[ni,1] - RO[ni,1] - SAW);
        if (ni == 1) {
          CR[ni,1] = CAW[ni,1] * (SR[ni,1] / SAW);
        } else {
          CR[ni,1] = min(CAW[ni,1],CR[ni,1] + RR[ni,1] + IR[ni,1] - RO[ni,1]);
        }
        if (CAW[ni,1] == 0.) {
          RH[ni,1] = 0.;
        } else {
          RH[ni,1] = CR[ni,1] / CAW[ni,1];
        }
        SUMDD = SUMDD+ DD[ni,1];
    
        ##ETMi(l,nl,nt,ni); ##Calculates ETM according to the vegetative periods, crop coefficients, crop root depths.
        if (ni < 1) {
          KC_adj[ni,1] = KE[ni,1];
        } else if (ni * 10 <= Lini[l,1]) {
          KC_adj[ni,1] = max(KE[ni,1],KCini[l,1]);
        } else if (ni * 10 <= Ldev[l,1] + Lini[l,1]) {
          KC_adj[ni,1] = KCini[l,1] + (KCmid[l,1] - KCini[l,1]) * (ni * 10. - 
                  Lini[l,1]) / Ldev[l,1];
        } else if (ni * 10 <= Ldev[l,1] + Lini[l,1] + Lmid[l,1]) {
          KC_adj[ni,1] = KCmid[l,1];
        } else if (ni * 10 <= Ltot) {
          KC_adj[ni,1] = KCmid[l,1] + (KCend[l,1] - KCmid[l,1]) * ((ni * 10. - (
                  Ldev[l,1] + Lini[l,1] + Lmid[l,1])) / Lend[l,1]);
        } else {
          KC_adj[ni,1] = KE[ni,1];
        }
        ETM = KC_adj[ni,1] * ETO[ni,1]; ## optimal MET */
        SUMETM = SUMETM+ETM;
    
        ##ETCi(nt,ni); ##correction for fertilisation (LF) ETO->ETC
        if (ni <= 0) {
          ETC = ETM;
        }
        if (ni > 0 && ni * 10 <= Ltot) {
          if (LF == 0) {
            KC_adj[ni,1] = KC_adj[ni,1]*.5; ## no fertilisation */
          } else if (LF == 1) {
            KC_adj[ni,1] = KC_adj[ni,1]* .75; ## half fertilisation */
          } else if (LF == 2) {
            KC_adj[ni,1] = KC_adj[ni,1]; ## full fertilisation */
          }
        }
        ETC = KC_adj[ni,1] * ETO[ni,1]; ## sub-optimal MET */
        SUMETC = SUMETC+ETC;

        ##ETAi(l,nl,nt,ni); ##Actual Evapotranspiration ETC->ETA
        if (ni >= 1.0 && ni * 10.0 <= Ltot) {
          P_adj = P[l,1] + (5.0 - ETC / 10.0) * .04; ## FAO's ETO infl. on P */
          if (RH[ni,1] < 1 - P_adj) {
            KS[ni,1] = RH[ni,1] / (1 - P_adj);
          } else {
            KS[ni,1] = 1.;
          }
          ETA = min(CR[ni,1],KS[ni,1] * ETC);
        } else {
          KS[ni,1] = 1.;
          ETA = min(SR[ni,1],KS[ni,1] * ETC);
        }
        SUMETA = SUMETA+ETA;
    
        ##UWS(nt,ni); ##updates water stores for ETA
        SR[ni,1]=max(0,(SR[ni,1]-ETA));
        CR[ni,1]=max(0,(CR[ni,1]-ETA));
    
        ##DEMi(nt,ni,nl,l,ni+neg); ##Calculates the water demand (DEM) of the crop at each timestep
        ngrow <- ni+neg
        if (ngrow > 0 && ngrow * 10 <= Ltot) {
          ##DEM = max(0.,(1.0 - PIR[l,1]) * CAW[ni,1] - CR[ni,1]);
          ##DEM = max(0.,(1.0 - P[l,1]) * CAW[ni,1] - CR[ni,1]);
          DEM = max(0.,(1.0 - P_adj) * CAW[ni,1] - CR[ni,1]);
          ## TODO: deal with paddy without hardcoding l=1
          ##    } else if (ni == 0 && l == 1) {## l=1 is paddy */
          ##	DEM = 100.;
        } else {
          DEM = 0.;
        }
        SUMDEM = SUMDEM+ DEM; ## cumulative water demand */
        if (DEM > 0.) {
          CR_tot[ni,1] = DEM + ETA;
        }

        ## Calculates total DEM, ETM, ETC, ETA, or IR over growing period - for annual crops
        ## Fortran implementation also included perennial crops
        ## Note: length of growing season in Seasons should be longer than/equal to Ltot

        RTOT=RTOT+RR[ni,1];
        PETTOT=PETTOT+ETO[ni,1];
        DDTOT=DDTOT+DD[ni,1];			
        if (ni == -neg) {
          ETMPRE=SUMETM;
          ETCPRE=SUMETC;
          ETAPRE=SUMETA;
          DIVPRE=SUMIR;
        } else if ((ni+neg) * 10 >= Ltot) {
          DEMTOT=SUMDEM;
          ETMTOT=SUMETM-ETMPRE;
          ETATOT=SUMETA-ETAPRE;
          ETCTOT=SUMETC-ETCPRE;
          DIVTOT=SUMIR-DIVPRE;
        }
    
      } ##for ni
      ##yield(nl, l);
      YA=YM[l,1]*(1-KY[l,1]*(1-ETATOT/ETMTOT));
      YA=max(0.,YA);
  
      Yield[y+2,l+1]=YA;
      CropWU[y,l+1]=DIVTOT;
      WB[y,] <- c(RTOT,ETATOT,SUMRO,DDTOT,ETMTOT)
    } ## for years
  }   ##for crops

  colnames(WB) <- c("RTOT","ETATOT","SUMRO","DDTOT","ETMTOT")
  ##browser()
  list(Yield=Yield,CropWU=CropWU,WB=WB)
} ## function Catchcrop


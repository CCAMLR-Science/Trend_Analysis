require(lm.beta)
options(scipen=999)


#Load Estimates
CPUE_est=read.csv(paste0("Output_CPUE_3yMedian_",Time,".csv"), stringsAsFactors = FALSE)
Chap_est=read.csv(paste0("Output_Chapman_",Time,".csv"), stringsAsFactors = FALSE)
#Load past catch limits
CLs=read.csv("Data/CLs.csv")

#Set Seasons
Seasons=seq(2018,Est_Season)

#Beta slope threshold
b_th=0.1

#Prepare empty dataframe
T_H=data.frame(RB=RBsToDo)
T_H=cbind(T_H,matrix(NA,nrow=nrow(T_H),ncol=length(Seasons)*7-1))
Cnames=paste0(c("Fish","Cp","Ch","Tr","Arec","MaxS","CL"),rep(Seasons,each=7))
Cnames=c("RB",Cnames[-length(Cnames)])
if(length(Cnames)!=ncol(T_H)){stop("Mismatch between column names and T_H dataframe dimensions")}
colnames(T_H)=Cnames
rm(Cnames)
#Now, in the trend history table (T_H), for each season, there will be, if fishing occurred, the CPUE beta slope,
#the Chapman beta slope, the overall trend, Adequate Recaptures, Last season with data,
#and the CL recommended for the next season.
#The last season doesn't get a CL since it hasn't been agreed yet.


#Now loop over seasons and RBs, and fill-in the columns

for(Sx in Seasons){
  
  for (r in unique(RBsToDo)){
    #Get CPUE data for that RB and within 5 years of Sx
    Cpdat=CPUE_est[which(CPUE_est$RB == r & CPUE_est$Season<=Sx & CPUE_est$Season>=(Sx-4)),]
    #Get Chapman data for that RB and within 5 years of Sx
    Chdat=Chap_est[which(Chap_est$RB == r & Chap_est$Season<=Sx & Chap_est$Season>=(Sx-4)),]
    #Fishing occurred?
    if(nrow(Cpdat)==0){
      T_H[T_H$RB==r,paste0("Fish",Sx)]="N"
    }else{
      T_H[T_H$RB==r,paste0("Fish",Sx)]="Y"
    }
    #Get CPUE slope
    if(nrow(Cpdat)<2){
      #not enough data to get slope
      T_H[T_H$RB==r,paste0("Cp",Sx)]="-"
      T_Cp=NA
    }else{
      model.2 <- lm(Est ~ Season, data=Cpdat,weights=1/sd^2)
      mod2_beta<-lm.beta(model.2)
      T_Cp=round(mod2_beta$standardized.coefficients[[2]],3)
      T_H[T_H$RB==r,paste0("Cp",Sx)]=sprintf("%.3f",T_Cp)
      rm(model.2,mod2_beta)
    }
    #Get Chapman slope
    if(nrow(Chdat)<2){
      #not enough data to get slope
      T_H[T_H$RB==r,paste0("Ch",Sx)]="-"
      T_Ch=NA
    }else{
      model.4 <- lm(Est ~ Season, data=Chdat,weights=1/sd^2)
      mod4_beta<-lm.beta(model.4)
      T_Ch=round(mod4_beta$standardized.coefficients[[2]],3)
      T_H[T_H$RB==r,paste0("Ch",Sx)]=sprintf("%.3f",T_Ch)
      rm(model.4,mod4_beta)
    }
    #Determine overall trend
    #No data
    if(is.na(T_Cp)==T & is.na(T_Ch)==T){T_H[T_H$RB==r,paste0("Tr",Sx)]="-"}
    #No Chapman
    if(is.na(T_Cp)==F & is.na(T_Ch)==T){T_H[T_H$RB==r,paste0("Tr",Sx)]="U"}
    #If there is data, assess trends
    if(is.na(T_Cp)==F & is.na(T_Ch)==F){
      #Declining:
      if(
        (T_Cp<(-b_th) | T_Ch<(-b_th))
        &
        (T_Cp<0 & T_Ch<0)
      ){T_H[T_H$RB==r,paste0("Tr",Sx)]="D"}
       
      #Increasing
      if(
        (T_Cp>b_th | T_Ch>b_th)
        &
        (T_Cp>0 & T_Ch>0)
      ){T_H[T_H$RB==r,paste0("Tr",Sx)]="I"}
      #Stable
      if(
        (abs(T_Cp)<b_th & abs(T_Ch)<b_th)
      ){T_H[T_H$RB==r,paste0("Tr",Sx)]="S"}
      #Unclear
      if(
        (T_Cp>b_th & T_Ch<=0)
        |
        (T_Cp<(-b_th) & T_Ch>=0)
        |
        (T_Cp>=0 & T_Ch<(-b_th))
        |
        (T_Cp<=0 & T_Ch>b_th)
       ){T_H[T_H$RB==r,paste0("Tr",Sx)]="U"}  
    }
    #Adequate Recaptures?
    A_Rec=length(which((Chdat$N_recaptures[Chdat$Season>Sx-3]>=3)==T))
    if(A_Rec>=2){A_Rec='Y'}else{A_Rec='N'}
    T_H[T_H$RB==r,paste0("Arec",Sx)]=A_Rec
    #Get last season with data
    if(nrow(Cpdat)==0){MaxS=NA}else{MaxS=max(Cpdat$Season)}
    T_H[T_H$RB==r,paste0("MaxS",Sx)]=MaxS
    #Add catch limit
    if(Sx<Est_Season){
      T_H[T_H$RB==r,paste0("CL",Sx)]=CLs[CLs$RB==r,paste0("X",Sx+1)]
    }
    
  }#End loop over RBs
}#End loop over seasons


#Add ASDs
T_H$ASD=NA
T_H$ASD[grep('481',T_H$RB)]='48.1'
T_H$ASD[grep('482',T_H$RB)]='48.2'
T_H$ASD[grep('486',T_H$RB)]='48.6'
T_H$ASD[grep('5841',T_H$RB)]='58.4.1'
T_H$ASD[grep('5842',T_H$RB)]='58.4.2'
T_H$ASD[grep('5843',T_H$RB)]='58.4.3'
T_H$ASD[grep('5844',T_H$RB)]='58.4.4'
T_H$ASD[grep('882',T_H$RB)]='88.2'
T_H$ASD[grep('883',T_H$RB)]='88.3'


#Now build table of decisions
Dec=T_H[,c("ASD","RB")]
#Add a column for each year
Dec=cbind(Dec,matrix("???",nrow=nrow(Dec),ncol=length(Seasons)))
colnames(Dec)=c("ASD","RB",paste0("S",Seasons))

#Fill-in decisions - loop over RBs and columns
for(Col in seq(3,ncol(Dec))){
  for(Row in seq(1,nrow(Dec))){
    DecS=colnames(Dec)[Col]
    DecS=as.numeric(strsplit(DecS,"S")[[1]][2]) #Season of estimation
    DecR=Dec$RB[Row] #RB
    DecFish=T_H[T_H$RB==DecR,paste0("Fish",DecS)] #fished? 
    DecTr=T_H[T_H$RB==DecR,paste0("Tr",DecS)] #Overall trend 
    DecAR=T_H[T_H$RB==DecR,paste0("Arec",DecS)] #Adequate Recaptures
    DecMaxS=T_H[T_H$RB==DecR,paste0("MaxS",DecS)] #Last season with data
    DecCpsl=T_H[T_H$RB==DecR,paste0("Cp",DecS)] #CPUE slope
    
    #Data in the last 5 seasons?
    if(is.na(DecMaxS)){Dec[Row,Col]="x"} #Can't do trend analysis
    if(is.na(DecMaxS)==F & (DecS-DecMaxS)>4){Dec[Row,Col]="x"} #Can't do trend analysis
    #Data in the last season?
    if(Dec[Row,Col]!="x" & (DecS>DecMaxS)){Dec[Row,Col]="PCL"} #Take Previous Catch Limit
    #Data only in the last season?
    if(Dec[Row,Col]%in%c("x","PCL")==F){
    if(DecFish=="Y" & Dec[Row,Col-1]=="x"){Dec[Row,Col]="CPUEx0.04"}
    }
    #Get trend decision
    if(Dec[Row,Col]%in%c("x","PCL","CPUEx0.04")==F){
      #Insufficent data to get trend (ie only one year)
      if(DecCpsl=="-"){Dec[Row,Col]="-"}
      #Declining
      if(DecTr=="D"){Dec[Row,Col]="PCLx0.8"}
      #Increasing, Stable or Unclear
      if(DecTr%in%c("I","S","U")){
        #Adequate Recaptures
        if(DecAR=="Y"){
          Dec[Row,Col]="Chapman"
        }else{
          #CPUE trend decline
          if(DecCpsl!="-"){
          if(DecCpsl<0){
            Dec[Row,Col]="PCLx0.8"
            }else{
            Dec[Row,Col]="CPUE"
          }}
        }
      }
    }
  }
}

if(any(Dec=="???")){stop("Trend decision history incomplete")}

#Compute percentage of Chapman estimates
Dec=rbind(Dec,NA)
Dec[nrow(Dec),1]="Chapman (%)"

for (Col in seq(3,ncol(Dec))){
  V=Dec[,Col]
  V=V[is.na(V)==F]
  V=V[V!="x"]
  P_Chap=sprintf("%.1f",round(100*(length(which(V=="Chapman"))/length(V)),1))
  Dec[nrow(Dec),Col]=P_Chap
}


#Format tables and export
indx=grep("MaxS",colnames(T_H))
T_H=T_H[,-indx]
T_H=cbind("ASD"=T_H$ASD,T_H[,-which(colnames(T_H)=="ASD")])

write.csv(T_H, paste0("Trends_History_",Time,".csv"),row.names=F)

indx=seq(3,ncol(Dec))
nms=colnames(Dec)[indx]
nms=unlist(strsplit(nms,"S"))
nms=nms[nms!=""]
colnames(Dec)[indx]=nms

write.csv(Dec, paste0("Decisions_History_",Time,".csv"),row.names=F)

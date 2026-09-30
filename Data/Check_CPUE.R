library(dplyr)

Time="26-Aug-2026_V2"
Catch=read.csv(paste0("Output_All_Catch_",Time,".csv"))

Catch$Area=Catch$RESEARCH_BLOCK_CODE_START_SET
Catch$Area[is.na(Catch$Area)]=Catch$REF_AREA_CODE_START_SET[is.na(Catch$Area)]
length(which(is.na(Catch$Area)==T))
Catch=Catch%>%filter(is.na(Area)==F)
Catch$CPUE=Catch$greenweight_caught_kg/(Catch$line_length_m/1000)
length(which(is.na(Catch$CPUE)))
Catch=Catch%>%filter(is.na(CPUE)==F)

Catch=Catch%>%select(
  Area,
  Season=season_ccamlr,
  Species=taxon_code,
  Gear=longline_type,
  Ckg=greenweight_caught_kg,
  CPUE
)

#By gear
Totg=Catch%>%group_by(Area,Season,Species,Gear)%>%summarise(Ckg=sum(Ckg)/1000,CPUE=median(CPUE),.groups = 'drop')%>%as.data.frame()
#All gears combined
Tot=Catch%>%group_by(Area,Season,Species)%>%summarise(Ckg=sum(Ckg)/1000,CPUE=median(CPUE),.groups = 'drop')%>%as.data.frame()

TOPg=Totg[which(Totg$Species=="TOP"),]
TOAg=Totg[which(Totg$Species=="TOA"),]
TOP=Tot[which(Tot$Species=="TOP"),]
TOA=Tot[which(Tot$Species=="TOA"),]

TOPg=TOPg%>%filter(Area=="HIMI")
TOP=TOP%>%filter(Area=="HIMI")

ArnY=TOA%>%group_by(Area)%>%summarise(n=n())
TOAg=TOAg%>%filter(Area%in%c("HIMI",ArnY$Area[ArnY$n<5])==F)
TOA=TOA%>%filter(Area%in%c("HIMI",ArnY$Area[ArnY$n<5])==F)

TOA$ASD=TOA$Area
TOA$ASD[grep("48",TOA$Area)]="48"
TOA$ASD[grep("58",TOA$Area)]="58"
TOA$ASD[grep("882",TOA$Area)]="882"
TOA$ASD[grep("883",TOA$Area)]="883"
TOAg$ASD=TOAg$Area
TOAg$ASD[grep("48",TOAg$Area)]="48"
TOAg$ASD[grep("58",TOAg$Area)]="58"
TOAg$ASD[grep("882",TOAg$Area)]="882"
TOAg$ASD[grep("883",TOAg$Area)]="883"


XL=range(Catch$Season)
Gcols=data.frame(Gear=unique(Catch$Gear))
Gcols$col=rainbow(3)
TOAg=left_join(TOAg,Gcols,by="Gear")


for(a in c("48","58","882","883","RSR_open")){
  
  tmp=TOA%>%filter(ASD==a)
  tmpg=TOAg%>%filter(ASD==a)
  
  Nrbs=length(unique(tmp$Area))
  
  
  png(filename=paste0("Data/Check_CPUE_",a,".png"), width = 1800, height = 800*Nrbs,res=200)
  par(mfrow=c(Nrbs,2))
  if(a=="RSR_open"){
    par(mai=c(0.4,1,0.5,0.2),xaxs="i",yaxs="i",lend=1,xpd=T,cex.axis=1)
  }else{
    par(mai=c(0.3,0.7,0.5,0.2),xaxs="i",yaxs="i",lend=1,xpd=T,cex.axis=1.5)
  }
  
  
  
  
  for(rb in sort(unique(tmp$Area))){
    #Catch
    
    tmprb=tmp%>%filter(Area==rb)
    YL=c(0,max(tmprb$Ckg))
    YL[2]=YL[2]+0.1*YL[2]
    if(rb==sort(unique(tmp$Area))[1]){
      plot(NA,NA,xlim=XL,ylim=YL,xlab="",ylab="",main="Total catch (t.)",axes=F,cex.main=1.5)
    }else{
      plot(NA,NA,xlim=XL,ylim=YL,xlab="",ylab="",main="",axes=F)
    }
    
    tmprbg=tmpg%>%filter(Area==rb)
    for(g in unique(tmprbg$Gear)){
      lines(tmprbg$Season[tmprbg$Gear==g],tmprbg$Ckg[tmprbg$Gear==g],col=tmprbg$col[tmprbg$Gear==g],lwd=2,xpd=T)
      points(tmprbg$Season[tmprbg$Gear==g],tmprbg$Ckg[tmprbg$Gear==g],pch=21,bg=tmprbg$col[tmprbg$Gear==g],cex=1.5,xpd=T)
    }
    lines(tmprb$Season,tmprb$Ckg,col="black",lwd=2,lty=3,xpd=T)
    points(tmprb$Season,tmprb$Ckg,pch=21,bg="black",cex=1.5,xpd=T)
    axis(1)
    axis(2,las=1)
    text(min(XL),mean(YL),rb,srt=90,xpd=T,cex=2,adj=c(0.5,-3))
    
    #Prepare legend
    gcols=tmprbg%>%select(Gear,col)%>%distinct()%>%as.data.frame()
    gcols=rbind(gcols,cbind(Gear="Total",col="black"))
    gcols$lty=1
    gcols$lty[length(gcols$lty)]=3
    
    if(rb%in%c("486_2","5841_2","883_1")){
      legend('topright',legend = gcols$Gear,
             col=gcols$col,lty=gcols$lty,cex=1.2,lwd=1.5,seg.len=1.8,pch=21,pt.bg=gcols$col)
    }
    
    
    #CPUE
    YL=c(0,max(c(tmprb$CPUE,tmprbg$CPUE)))
    YL[2]=YL[2]+0.1*YL[2]
    
    if(rb==sort(unique(tmp$Area))[1]){
      plot(NA,NA,xlim=XL,ylim=YL,xlab="",ylab="",main="Median CPUE (kg/km)",axes=F,cex.main=1.5)
    }else{
      plot(NA,NA,xlim=XL,ylim=YL,xlab="",ylab="",main="",axes=F)
    }
    
    tmprbg=tmpg%>%filter(Area==rb)
    for(g in unique(tmprbg$Gear)){
      lines(tmprbg$Season[tmprbg$Gear==g],tmprbg$CPUE[tmprbg$Gear==g],col=tmprbg$col[tmprbg$Gear==g],lwd=2,xpd=T)
      points(tmprbg$Season[tmprbg$Gear==g],tmprbg$CPUE[tmprbg$Gear==g],pch=21,bg=tmprbg$col[tmprbg$Gear==g],cex=1.5,xpd=T)
    }
    lines(tmprb$Season,tmprb$CPUE,col="black",lwd=2,lty=3,xpd=T)
    points(tmprb$Season,tmprb$CPUE,pch=21,bg="black",cex=1.5,xpd=T)
    axis(1)
    axis(2,las=1)
    
    if(rb%in%c("882_1","RSR_open")){
      legend('topright',legend = gcols$Gear,
             col=gcols$col,lty=gcols$lty,cex=1.2,lwd=1.5,seg.len=1.8,pch=21,pt.bg=gcols$col)
    }
    
  }
  
  
  
  dev.off()
  
}#end of per-Area loop


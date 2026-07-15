options(scipen=999)

#Load plot function
source("Scripts/PlotVar.R")
#Load CPUE Estimates
CPUE_est=read.csv(paste0("Output_CPUE_3yMedian_",Time,".csv"), stringsAsFactors = FALSE)

#Load all past catch limits
CLs=read.csv("Data/AllCLs.csv")

#Load Recommended catch limits
RecCL=read.csv(paste0("Trends_",Est_Season,"_and_CLs_",Time,".csv"), stringsAsFactors = FALSE,check.names=F)
RecCL=RecCL%>%
  select(RB="Research Block",Lim=grep("Recommended CL",colnames(RecCL)))%>%
  filter(RB%in%RBsCAdv & Lim!="x")
RecCL$Lim=as.numeric(RecCL$Lim)


#Merge DFs
CPUE_est=left_join(CPUE_est,CLs,by=c("Season","RB"))

#Keep RBs with at least 3 years of data and with 3 years of CLs
Ny=CPUE_est%>%group_by(RB)%>%summarise(n=n())%>%filter(n>2)
CPUE_est=CPUE_est%>%filter(RB%in%Ny$RB)
Ny=CPUE_est%>%group_by(RB)%>%summarise(n=length(which(is.na(CL)==F)))%>%filter(n>2)
CPUE_est=CPUE_est%>%filter(RB%in%Ny$RB)


#Add area code
CPUE_est$Area=NA
CPUE_est$Area[grep("48",CPUE_est$RB)]="48"
CPUE_est$Area[grep("58",CPUE_est$RB)]="58"
CPUE_est$Area[grep("88",CPUE_est$RB)]="88"

#Plot trends per Area
for(a in sort(unique(CPUE_est$Area))){
  
  rbs=unique(CPUE_est$RB[CPUE_est$Area==a])
  mf=n2mfrow(length(rbs)+2)
  
  png(filename=paste0("CPUE_RBs_Area_",a,"_",Est_Season,"_",Time,".png"), width = mf[2]*800, height = mf[1]*600,res=200)
  par(mfrow=mf)
  par(mai=c(0.3,0.6,0.4,0.35),cex.axis=1.5,lend=1)
  
  
  #Thumbnail
  plot(NA,NA,xlim=c(0,10),ylim=c(0,10),axes=F,xlab='',ylab='')
  inset=png::readPNG(paste0("Data/Area",a,".png"),native = T)
  rasterImage(inset,xleft=0,
              ybottom=-1,
              xright=10,
              ytop=11,
              xpd=T)
  
  #Legend
  plot(NA,NA,xlim=c(0,10),ylim=c(0,10),axes=F,xlab='',ylab='')
  legend('center',legend = c("CPUE-derived biomass",
                             "estimate (t.) with 95%",
                             "Confidence Intervals",
                             "Number of hauls",
                             "Past catch limit (t.)",
                             "Recommended catch limit (t.)"),
         col=c(NA,NA,NA,'blue',"red","orange"),lty=c(NA,NA,NA,NA,1,3),text.col=c(NA,NA,NA,'blue',"red","orange"),
         pch=c(NA,NA,NA,120,NA,NA),cex=1.5,lwd=3,seg.len = 1.8)
  PlotVar(Input=data.frame(x=1.0,ymin=5.1,ymax=7.6),Col=rgb(0,0,1,0.25))
  points(1.0,6.35,pch=21,bg='blue',cex=1.5)
  
  for(r in rbs){
    tmp_cp=CPUE_est[CPUE_est$RB==r,]
    
    
    XL=c(min(tmp_cp$Season),Est_Season+1)
    
    # if(r%in%RecCL$RB){
    #   XL=c(min(tmp_cp$Season),Est_Season+1)
    # } 
    
    YL=c(min(tmp_cp$CI_lower),max(tmp_cp$CI_upper))
    
    di=diff(YL)
    YL[2]=YL[2]+0.05*di
    YL[1]=YL[1]-0.2*di
    
    di=diff(XL)
    XL[2]=XL[2]+0.05*di
    XL[1]=XL[1]-0.05*di
    
    
    
    plot(NA,NA,xlim=XL,ylim=YL,axes=F,xlab='',ylab='')
    
    if(r%in%RBsCAdv){
      rect(xleft=XL[1], ybottom=YL[1], xright=XL[2], ytop=YL[2],col='grey85',border=NA)
    } 
    
    #Catch limits (past and recommended ones)
    if(all(is.na(tmp_cp$CL))==F){
      
      if(r%in%RecCL$RB){
        YLcl=range(c(tmp_cp$CL,RecCL$Lim[RecCL$RB==r]),na.rm=T) #Add RecCL if there
      }else{
        YLcl=range(tmp_cp$CL,na.rm=T)
      }
      
      di=diff(YLcl)
      YLcl[2]=YLcl[2]+0.05*di
      YLcl[1]=YLcl[1]-0.2*di
      par(new=T)
      
      plot(tmp_cp$Season,tmp_cp$CL,xlim=XL,ylim=YLcl,axes=F,xlab='',ylab='',type="l",col="red",lwd=2)
      
      if(r%in%RecCL$RB){
        segments(x0=max(tmp_cp$Season),
                 y0=tmp_cp$CL[tmp_cp$Season==max(tmp_cp$Season)],
                 x1=Est_Season+1,
                 y1=RecCL$Lim[RecCL$RB==r],
                 col="orange",lwd=2,lty=3)
        points(Est_Season+1,RecCL$Lim[RecCL$RB==r],pch=21,bg='orange',cex=1.2)
      }

      points(tmp_cp$Season,tmp_cp$CL,pch=21,bg='red',cex=1.2)
      
      
      
      axis(4,pos=XL[2],las=1,col.axis="red",col="red")
      par(new=T)
      plot(NA,NA,xlim=XL,ylim=YL,axes=F,xlab='',ylab='')
    }
    
    #Estimates
    PlotVar(Input=tmp_cp[,c("Season","CI_lower","CI_upper")],Col=rgb(0,0,1,0.25))
    par(new=T)
    
    plot(tmp_cp$Season,tmp_cp$Est,type='l',col='blue',xlim=XL,ylim=YL,axes=F,lwd=2,xlab='',ylab='',cex.lab=2)
    points(tmp_cp$Season,tmp_cp$Est,pch=21,bg='blue',cex=1.5)
    
    
    text(tmp_cp$Season,YL[1],tmp_cp$RB_N_Hauls,adj=c(-0.1,0.5),cex=1.2,col='blue',xpd=T,srt=90)
    
    text(mean(XL),YL[2],paste0(r,' | ',unique(tmp_cp$Species)),adj=c(0.5,-1.2),cex=2,xpd=T)
    
    
    xtc=seq(min(tmp_cp$Season),Est_Season+1)
    # if(r%in%RecCL$RB){
    #   xtc=seq(min(tmp_cp$Season),(Est_Season+1))
    # }
    
    
    axis(1,pos=YL[1],at=xtc,tcl=-0.25,labels=F)
    
    # if(nrow(tmp_cp)<=3){
    #   axis(1,pos=YL[1],at=xtc,tcl=-0.55)
    # }else{
      xtc=xtc[seq(length(xtc),1,by=-3)]
      axis(1,pos=YL[1],at=xtc,tcl=-0.55)
    # }
    
    axis(2,pos=XL[1],las=1)
  }
  dev.off()
  
}#end of per-Area loop




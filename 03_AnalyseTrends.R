require(lm.beta)
options(scipen=999)

#Load plot function
source("Scripts/PlotVar.R")
#Load Seasonal Estimates
CPUE_est=read.csv(paste0("Output_CPUE_3yMedian_",Time,".csv"), stringsAsFactors = FALSE)
Chap_est=read.csv(paste0("Output_Chapman_",Time,".csv"), stringsAsFactors = FALSE)
#Load most recent estimates
B_Recent=read.csv(paste0("Output_Recent_Bestimates_",Time,".csv"), stringsAsFactors = FALSE)
#Load past catch limits
CLs=read.csv("Data/CLs.csv")
CLs$CL=CLs[,ncol(CLs)] #Take latest CL, then fill with older CLs where needed
CLs$stars=NA #This will mark when the previous CL was agreed and be used in the final table
indx=which(is.na(CLs$CL)==T) #Rows needing a CL from the past
for(i in indx){
  pastCLs=CLs[i,paste0('X',seq(2019,Est_Season-1))]
  ok=max(which(is.na(pastCLs)==F))
  CLs$CL[i]=as.numeric(pastCLs[ok])
  CLy=names(pastCLs[ok])
  CLy=as.numeric(strsplit(CLy,"X")[[1]][2])
  nstars=Est_Season-CLy
  CLs$stars[i]=paste(rep('*',nstars),collapse = "")
  rm(pastCLs,ok,CLy,nstars)
}
rm(i,indx)
#build table footer to indicate when past CLs were agreed
stars=sort(unique(CLs$stars))
foot=paste0(stars," CL agreed in ",Est_Season-nchar(stars),collapse = "; ")
rm(stars)

#Keep all estimates to plot long timeseries
CPUE_all=CPUE_est
Chap_all=Chap_est
#limit to the last 5 years. 
CPUE_est<-CPUE_est[CPUE_est$Season>(Est_Season-5),]
Chap_est<-Chap_est[Chap_est$Season>(Est_Season-5),]
#Split and simplify B_Recent
B_Recent_Chap=B_Recent[B_Recent$Method=='Chapman',c("RB","Est")]
B_Recent_CPUE=B_Recent[B_Recent$Method=="CPUE-by-seabed area",c("RB","Est")]




#CPUE trends
CPUE_trends=NULL 
for (r in unique(RBsToDo)){
  
  if(dim(CPUE_est[CPUE_est$RB == r,])[1] == 0){
    LastS=(CPUE_all[CPUE_all$RB == r,"Season"])
    if(length(LastS)==0){LastS=NA}else{LastS=max(LastS)}
    out<-cbind(r,NA, NA,0,NA,LastS)
  } else {
    model.2 <- lm(Est ~ Season, data=CPUE_est[CPUE_est$RB == r,],weights=1/sd^2)
    mod2_beta<-lm.beta(model.2)
    
    out<-cbind(r,
               round(model.2$coefficients[[2]],2),
               round(mod2_beta$standardized.coefficients[[2]],2),
               dim(CPUE_est[CPUE_est$RB == r,])[1],
               unique(CPUE_est$Species[CPUE_est$RB==r]),
               max(CPUE_est[CPUE_est$RB == r,"Season"])
    )
  }
  CPUE_trends<-(rbind(CPUE_trends, out))
  
}

#Chapman trends
Chap_trends=NULL
for (r in unique(RBsToDo)){
  if(dim(Chap_est[Chap_est$RB == r,])[1] == 0){
    LastS=(Chap_all[Chap_all$RB == r,"Season"])
    if(length(LastS)==0){LastS=NA}else{LastS=max(LastS)}
    out<-cbind(r,NA, NA,0,NA,LastS)
   } else {
    
    model.4 <- lm(Est ~ Season, data=Chap_est[Chap_est$RB == r,],weights=1/sd^2)
    mod4_beta<-lm.beta(model.4)
    
    
    suff_recaps=length(which((Chap_est$N_recaptures[Chap_est$Season>Est_Season-3 & Chap_est$RB==r]>=3)==T))
    if(suff_recaps>=2){suff_recaps='Y'}else{suff_recaps='N'}
    
    out<-cbind(r,
               round(model.4$coefficients[[2]],2),
               round(mod4_beta$standardized.coefficients[[2]],2),
               dim(Chap_est[Chap_est$RB == r,])[1],
               suff_recaps,
               max(Chap_est[Chap_est$RB == r,"Season"]))
  }
  
  Chap_trends<-(rbind(Chap_trends, out))
}

trends<-merge(CPUE_trends, Chap_trends, by="r", all=T,sort=F)

names(trends)<-c("RB",  "CPUElm_weighted","CPUElm_weighted_beta",
                 "CPUE_nyrs","Species","CPUE_lastS",  "Chaplm_weighted", "Chaplm_weighted_beta",
                 "Chap_nyrs", "Sufficent_recaps","Chap_lastS")

trends$Sufficent_recaps[is.na(trends$Sufficent_recaps)]='N'

#Reformat
trends$RB=as.character(trends$RB)
trends$CPUElm_weighted=as.numeric(as.character(trends$CPUElm_weighted))
trends$CPUElm_weighted_beta=as.numeric(as.character(trends$CPUElm_weighted_beta))
trends$CPUE_nyrs=as.numeric(as.character(trends$CPUE_nyrs))
trends$Species=as.character(trends$Species)
trends$Chaplm_weighted=as.numeric(as.character(trends$Chaplm_weighted))
trends$Chaplm_weighted_beta=as.numeric(as.character(trends$Chaplm_weighted_beta))
trends$Chap_nyrs=as.numeric(as.character(trends$Chap_nyrs))
trends$Sufficent_recaps=as.character(trends$Sufficent_recaps)
trends$CPUE_lastS=as.numeric(as.character(trends$CPUE_lastS))
trends$Chap_lastS=as.numeric(as.character(trends$Chap_lastS))

#Fix missing species (may happen if no data in the last 5 years)
indx=which(is.na(trends$Species)==T & trends$RB%in%TOP_target_RBs==F)
if(length(indx)>0){trends$Species[indx]="TOA"}
rm(indx)
if(any(is.na(trends$Species))){stop("Missing species in trends table")}


#Add area code to trends table
trends$Area=NA
trends$Area[grep("48",trends$RB)]="48"
trends$Area[grep("58",trends$RB)]="58"
trends$Area[grep("88",trends$RB)]="88"

#Plot recent trends per Area
for(a in sort(unique(trends$Area))){

rbs=unique(trends$RB[trends$Area==a])
mf=n2mfrow(length(rbs)+2)

png(filename=paste0("Trends_RBs_Area_",a,"_",Est_Season,"_",Time,".png"), width = mf[2]*600, height = mf[1]*600,res=200)
par(mfrow=mf)
par(mai=c(0.3,0.4,0.4,0.35),cex.axis=1.5)
XL=c(Est_Season-4,Est_Season+0.75)

#Thumbnail
plot(NA,NA,xlim=c(0,10),ylim=c(0,10),axes=F,xlab='',ylab='')
inset=png::readPNG(paste0("Data/Area",a,".png"),native = T)
rasterImage(inset,xleft=0,
            ybottom=0,
            xright=10,
            ytop=10,
            xpd=T)

#Legend
plot(NA,NA,xlim=c(0,10),ylim=c(0,10),axes=F,xlab='',ylab='')
legend('center',legend = c("CPUE-by-seabed area","Chapman","CPUE trend","Chapman trend","Number of hauls","Tag recaptures",'(Beta slope)'),
       col=c('blue','red','blue','red','blue','red','black'),lty=c(1,1,2,2,NA,NA,NA),
       pch=c(21,21,NA,NA,120,120,NA),pt.bg=c('blue','red'),cex=1.5,lwd=c(2,2,1.5,1.5),
       title = 'Biomass estimates (t)')

for(r in rbs){
  tmp_cp=CPUE_est[CPUE_est$RB==r,]
  tmp_ch=Chap_est[Chap_est$RB==r,]
  if(dim(tmp_cp)[1]==0){YL=c(0.1,0.2)}else{
    YL=c(min(c(tmp_cp$CI_lower,tmp_ch$CI_lower)),max(c(tmp_cp$CI_upper,tmp_ch$CI_upper)))
  }
  YL[2]=YL[2]+0.1*YL[2]
  YL[1]=YL[1]-0.1*YL[2]
  
  #Get slope coeff
  b_cp=trends$CPUElm_weighted_beta[trends$RB==r]
  b_ch=trends$Chaplm_weighted_beta[trends$RB==r]
  #Get slope coeff location
  BLoc=data.frame(
    x=c(last(tmp_cp$Season),last(tmp_ch$Season)),
    y=c(last(tmp_cp$Est),last(tmp_ch$Est))
  )
  BLoc=BLoc[is.na(BLoc$x)==F,]
  if(nrow(BLoc)==2){
    pc=(max(BLoc$y)-min(BLoc$y))/min(BLoc$y)
    tg=(YL[2]-YL[1])/40000
    if(pc<tg){ #Space locations if needed
      alph=(tg*min(BLoc$y)-max(BLoc$y)+min(BLoc$y))/(tg*min(BLoc$y)+max(BLoc$y)+min(BLoc$y))
      BLoc$y[BLoc$y==max(BLoc$y)]=max(BLoc$y)+alph*max(BLoc$y)
      BLoc$y[BLoc$y==min(BLoc$y)]=min(BLoc$y)-alph*min(BLoc$y)  
    }
  }
  
  #CPUE
  plot(NA,NA,xlim=XL,ylim=YL,axes=F,xlab='',ylab='')
  PlotVar(Input=tmp_cp[,c("Season","CI_lower","CI_upper")],Col=rgb(0,0,1,0.25))
  par(new=T)
  
  plot(tmp_cp$Season,tmp_cp$Est,type='l',col='blue',xlim=XL,ylim=YL,axes=F,lwd=2,xlab='',ylab='',cex.lab=2)
  points(tmp_cp$Season,tmp_cp$Est,pch=21,bg='blue',cex=1.5)
  
  if(dim(tmp_cp)[1]>2){
    lines(tmp_cp$Season,lm(Est ~ Season, data=tmp_cp,weights=1/sd^2)$fitted.values,col='blue',lty=2,lwd=1.5)
  }
  if(dim(tmp_cp)[1]>0){
    text(tmp_cp$Season,YL[1],tmp_cp$RB_N_Hauls,adj=c(0.5,0.1),cex=1.5,col='blue',xpd=T)
  }
  
  
  if(is.na(b_cp)==F){
    text(BLoc$x[1],BLoc$y[1],paste0(c('(',sprintf("%.2f", b_cp),')'),collapse = ''),col='blue',adj=c(-0.2,0.5),cex=2,xpd=T)
  }
  
  #Chapman
  PlotVar(Input=tmp_ch[,c("Season","CI_lower","CI_upper")],Col=rgb(1,0,0,0.25))
  
  par(new=T)
  plot(tmp_ch$Season,tmp_ch$Est,type='l',col='red',xlim=XL,ylim=YL,lwd=2,xlab='',ylab='')
  points(tmp_ch$Season,tmp_ch$Est,pch=21,bg='red',cex=1.5)
  
  if(dim(tmp_ch)[1]>2){
    lines(tmp_ch$Season,lm(Est ~ Season, data=tmp_ch,weights=1/sd^2)$fitted.values,col='red',lty=2,lwd=1.5)
  }
  if(dim(tmp_ch)[1]>0){
    text(tmp_ch$Season,tmp_ch$CI_upper,tmp_ch$N_recaptures,adj=c(0.5,-0.5),cex=2,col='red',xpd=T)
  }
  
  if(is.na(b_ch)==F){
  text(BLoc$x[2],BLoc$y[2],paste0(c('(',sprintf("%.2f", b_ch),')'),collapse = ''),col='red',adj=c(-0.2,0.5),cex=2,xpd=T)
  }
  
  text(mean(XL),YL[2],paste0(r,' | ',trends$Species[trends$RB==r]),adj=c(0.5,-1.2),cex=2,xpd=T)
  
  LastCPUE=trends$CPUE_lastS[trends$RB==r]
  if(is.na(LastCPUE)==T){text(mean(XL),mean(YL),"No data",adj=c(0.5,0.5),cex=2)}else{
  if(LastCPUE<(Est_Season-4)){text(mean(XL),mean(YL),paste0("Last data in ",LastCPUE),adj=c(0.5,0.5),cex=2)}}
}
dev.off()

}#end of per-Area loop




rm(B_Recent)
rm(Chap_all,Chap_est,Chap_trends)
rm(CPUE_all,CPUE_est,CPUE_trends)
rm(mod2_beta,mod4_beta,model.2,model.4,out)
rm(tmp_ch,tmp_cp,b_ch,b_cp,mf,r,rbs,suff_recaps,XL,YL)



#Trend decision
b_th=0.1 #beta threshold
trends$Trend_Decision=NA
#Declining:
trends$Trend_Decision[which(
  
  (trends$CPUElm_weighted_beta<(-b_th) |
     trends$Chaplm_weighted_beta<(-b_th))
  &
    (trends$CPUElm_weighted_beta<0 &
       trends$Chaplm_weighted_beta<0)
  
)]='D'

#Increasing
trends$Trend_Decision[which(
  
  (trends$CPUElm_weighted_beta>b_th |
     trends$Chaplm_weighted_beta>b_th)
  &
    (trends$CPUElm_weighted_beta>0 &
       trends$Chaplm_weighted_beta>0)
  
)]='I' 

#Stable
trends$Trend_Decision[which(
  
  (abs(trends$CPUElm_weighted_beta)<b_th &
     abs(trends$Chaplm_weighted_beta)<b_th)
  
)]='S' 

#Unclear
trends$Trend_Decision[which(    
  
  (trends$CPUElm_weighted_beta>b_th &
     trends$Chaplm_weighted_beta<=0)
  |
    (trends$CPUElm_weighted_beta<(-b_th) &
       trends$Chaplm_weighted_beta>=0)
  |
    (trends$CPUElm_weighted_beta>=0 &
       trends$Chaplm_weighted_beta<(-b_th))
  |
    (trends$CPUElm_weighted_beta<=0 &
       trends$Chaplm_weighted_beta>b_th)
  
)]='U'

#Unclear because no Chapman trend
trends$Trend_Decision[which(    
  
  (is.na(trends$CPUElm_weighted_beta)==F &
     is.na(trends$Chaplm_weighted_beta)==T)
  
)]='U'


trends$Trend_Decision[which(trends$Trend_Decision%in%c('I','S','U'))]='ISU' 


#CPUE Trend decline
trends$CPUE_Trend_D=NA
trends$CPUE_Trend_D[which(trends$Sufficent_recaps=='Y')]='-'
trends$CPUE_Trend_D[which(trends$CPUElm_weighted_beta<0)]='Y'
trends$CPUE_Trend_D[which(trends$CPUElm_weighted_beta>=0)]='N'

#Fix cases where there is no CPUE and no Chapman trend
trends$Trend_Decision[which(is.na(trends$CPUElm_weighted_beta)==T & is.na(trends$Chaplm_weighted_beta)==T)]='-'
trends$CPUE_Trend_D[which(is.na(trends$CPUElm_weighted_beta)==T & is.na(trends$Chaplm_weighted_beta)==T)]='-'


#Estimated Biomass
trends$B_tonnes=NA
#Fill in those with insufficient recaptures and with a trend decline with CPUE estimates (as per Member request in 2019)
indx=which(trends$Sufficent_recaps=='N' & trends$CPUE_Trend_D=='Y')
trends$B_tonnes[indx]=round(B_Recent_CPUE$Est[match(trends$RB[indx],B_Recent_CPUE$RB)])
#Fill in those with data only in the last year
indx=which(trends$CPUE_lastS==Est_Season & trends$CPUE_nyrs==1)
trends$B_tonnes[indx]=round(B_Recent_CPUE$Est[match(trends$RB[indx],B_Recent_CPUE$RB)])



#Sufficient Recaptures
indx=which(trends$Sufficent_recaps=='Y')
trends$B_tonnes[indx]=round(B_Recent_Chap$Est[match(trends$RB[indx],B_Recent_Chap$RB)])
#Insufficient recaptures and declining CPUE
indx=which(trends$Sufficent_recaps=='N' & trends$CPUE_Trend_D=='N')
trends$B_tonnes[indx]=round(B_Recent_CPUE$Est[match(trends$RB[indx],B_Recent_CPUE$RB)])
trends$B_tonnes=as.numeric(trends$B_tonnes)


#Add Past Catch Limits
CLs=CLs[,c("RB","CL","stars")]
trends=dplyr::left_join(trends,CLs,by="RB")


#Potential CLs
trends$B004=round(trends$B_tonnes*0.04)
# trends$B004[which(trends$Sufficent_recaps=='N' & trends$CPUE_Trend_D=='Y')]=NA #Remove these, Biomass was added per Member request
trends$CL08=round(trends$CL*0.8)
trends$CL12=round(trends$CL*1.2)

# Run some checks before recommending CLs
if(paste(sort(unique(trends$Trend_Decision),na.last=T),collapse='-')!='--D-ISU'){stop('Problem with Trend_Decision field')}
if(paste(sort(unique(trends$Sufficent_recaps),na.last=T),collapse='-')!='N-Y'){stop('Problem with Sufficent_recaps field')}
if(paste(sort(unique(trends$CPUE_Trend_D),na.last=T),collapse='-')!='--N-Y'){stop('Problem with CPUE_Trend_D field')}
if(is.numeric(trends$B004)==F){stop('B004 field is not numeric')}
if(is.numeric(trends$CL08)==F){stop('CL08 field is not numeric')}
if(is.numeric(trends$CL12)==F){stop('CL12 field is not numeric')}

#Recommended CLs
trends$Rec_CLs=NA

#Declining biomass trend:
indx=which(trends$Trend_Decision=='D')
trends$Rec_CLs[indx]=trends$CL08[indx]
#ISU biomass trend:
#With adequate recaptures:
indx=which(trends$Trend_Decision=='ISU' & trends$Sufficent_recaps=='Y')
trends$Rec_CLs[indx]=pmin(trends$CL12[indx],pmax(trends$CL08[indx],trends$B004[indx]))
#Without adequate recaptures:
#With CPUE trend decline:
indx=which(trends$Trend_Decision=='ISU' & trends$Sufficent_recaps=='N' & trends$CPUE_Trend_D=='Y')
trends$Rec_CLs[indx]=trends$CL08[indx]
#Without CPUE trend decline:
indx=which(trends$Trend_Decision=='ISU' & trends$Sufficent_recaps=='N' & trends$CPUE_Trend_D=='N')
trends$Rec_CLs[indx]=pmin(trends$CL12[indx],pmax(trends$CL08[indx],trends$B004[indx]))


#Format table
trends$ASD=NA
trends$ASD[grep('481',trends$RB)]='48.1'
trends$ASD[grep('482',trends$RB)]='48.2'
trends$ASD[grep('486',trends$RB)]='48.6'
trends$ASD[grep('5841',trends$RB)]='58.4.1'
trends$ASD[grep('5842',trends$RB)]='58.4.2'
trends$ASD[grep('5843',trends$RB)]='58.4.3'
trends$ASD[grep('5844',trends$RB)]='58.4.4'
trends$ASD[grep('882',trends$RB)]='88.2'
trends$ASD[grep('883',trends$RB)]='88.3'

trends$Species[trends$Species=='TOP']="D. eleginoides"
trends$Species[trends$Species=='TOA']="D. mawsoni"

#Add preliminary step (fishing in last season?) effects
trends$CPUE_lastS[is.na(trends$CPUE_lastS)]=0 #fill-in missing Last Season
indx=which(trends$CPUE_lastS<Est_Season)
trends$Trend_Decision[indx]="-"
trends$Sufficent_recaps[indx]="-"
trends$CPUE_Trend_D[indx]="-"
trends$B_tonnes[indx]="-"
trends$B004[indx]="-"
trends$CL08[indx]="-"
trends$CL12[indx]="-"
trends$Rec_CLs[indx]=trends$CL[indx]
#Now mark those that have no data for 5 years
indx=which(trends$CPUE_lastS<(Est_Season-4))
trends$Trend_Decision[indx]="x"
trends$Sufficent_recaps[indx]="x"
trends$CPUE_Trend_D[indx]="x"
trends$B_tonnes[indx]="x"
trends$B004[indx]="x"
trends$CL08[indx]="x"
trends$CL12[indx]="x"
trends$Rec_CLs[indx]="x"
#Mark CLs that were derived from previous season
trends$stars[is.na(trends$stars)]=""
trends$CL=paste0(trends$CL,trends$stars)
#Fill in those with data only in the last year
indx=which(trends$CPUE_lastS==Est_Season & trends$CPUE_nyrs==1)
trends$Trend_Decision[indx]="[]"
trends$CPUE_Trend_D[indx]="[]"
trends$Rec_CLs[indx]="[]"

#Keep columns of interest
trends=trends[,c("Area","ASD","RB","Species","CL","Trend_Decision","Sufficent_recaps","CPUE_Trend_D","B_tonnes","B004","CL08","CL12","Rec_CLs")]

#Add footnote
trends[nrow(trends)+1,1]=foot

#Rename columns
colnames(trends)=c(
  "Area",
  "Subarea or Division",
  "Research Block",
  "Species",
  "PCL",
  "Trend decision",
  "Adequate recaptures",
  "CPUE Trend Decline",
  "B",
  "Bx0.04",
  "PCLx0.8",
  "PCLx1.2",
  paste0("Recommended CL for ",Est_Season+1)
)
write.csv(trends, paste0("Trends_",Est_Season,"_and_CLs_",Time,".csv"),row.names=F)

#Remove footnote
trends=trends[-nrow(trends),]



#Build diagram
library(dplyr)
library(DiagrammeR)
save_png <- function(plot, path){
  DiagrammeRsvg::export_svg(plot) %>%
    charToRaw() %>%
    rsvg::rsvg() %>%
    png::writePNG(path)
}


RB=trends
if(any(is.na(RB$`Trend decision`))){stop('Trend decision missing')}

#Declining:
tmp=RB[RB$`Trend decision`=='D',]
txt=dplyr::summarise(group_by(tmp,`Subarea or Division`),rbs=paste(`Research Block`,collapse=', '))
D=paste(txt$rbs,collapse='\n')
#Chapman
tmp=RB[RB$`Trend decision`=='ISU' & RB$`Adequate recaptures`=='Y',]
txt=dplyr::summarise(group_by(tmp,`Subarea or Division`),rbs=paste(`Research Block`,collapse=', '))
Chap=paste(txt$rbs,collapse='\n')
#CPUE
tmp=RB[RB$`Trend decision`=='ISU' & RB$`Adequate recaptures`=='N' & RB$`CPUE Trend Decline`=='N',]
txt=dplyr::summarise(group_by(tmp,`Subarea or Division`),rbs=paste(`Research Block`,collapse=', '))
Cpue=paste(txt$rbs,collapse='\n')
#Cpue decline
tmp=RB[RB$`Trend decision`=='ISU' & RB$`Adequate recaptures`=='N' & RB$`CPUE Trend Decline`=='Y',]
txt=dplyr::summarise(group_by(tmp,`Subarea or Division`),rbs=paste(`Research Block`,collapse=', '))
ISUd=paste(txt$rbs,collapse='\n')
#No fishing in last season
tmp=RB[RB$`Trend decision`=='-',]
txt=dplyr::summarise(group_by(tmp,`Subarea or Division`),rbs=paste(`Research Block`,collapse=', '))
NoF=paste(txt$rbs,collapse='\n')

Diag=grViz("
digraph boxes_and_circles{

node [shape = box,color=black,penwidth = 1.5,fontname =arial]
A0[label =
    <
    Fishing in the last Season?<br/>
    <font point-size = '12'>(Option limited to the last 5 years)</font>
    >
]

node [shape = box,color=black,penwidth = 1.5,fontname =arial]
A[label = 
    <
    Biomass estimate trend evaluation
    >
]

node [shape = box, color=black]
A1[label =
    <
    Previous Catch Limit
    >
]

node [shape = box,color=black]
B1[label = 
    <
    Declining
    >
]

node [shape = box, color=black]
B2[label = 
    <
    Previous Catch Limit &#215; 0.8
    >
]

node [shape = box,color=black]
C1[label = 
    <
    Increasing, Stable or Unclear
    >
]

node [shape = box,color=black]
C2[label = 
    <
    Adequate recaptures?<br/>
    <font point-size = '12'>(3 in 2 of last 3 years within time and liberty constraint)</font>
    >
]

node [shape = box,color=black]
D1[label = 
    <
    CPUE Trend Decline 
    >
]

node [shape = box,color=black]
D2[label = 
    <
    Previous Catch Limit &#215; 0.8
    >
]

node [shape = box,color=black]
D3[label = 
    <
    CPUE B estimate &#215; 0.04<br/>
<font point-size = '12'>Change limited to</font><br/>
<font point-size = '12'>Previous Catch Limit &#215; 0.8</font><br/>
<font point-size = '12'>Previous Catch Limit &#215; 1.2</font><br/>>
]

node [shape = box,color=black]
E[label = 
    <
    Chapman B estimate &#215; 0.04<br/>
<font point-size = '12'>Change limited to</font><br/>
<font point-size = '12'>Previous Catch Limit &#215; 0.8</font><br/>
<font point-size = '12'>Previous Catch Limit &#215; 1.2</font><br/>>
]

node [shape = oval,color=grey]
Dec [label='@@1']
node [shape = oval,color=grey]
Chap [label='@@2']
node [shape = oval,color=grey]
Cpue [label='@@3']
node [shape = oval,color=grey]
ISUd [label='@@4']
node [shape = oval,color=grey]
NoF [label='@@5']

A->B1
B1->B2

A->C1
C1->C2

A0->A [label='Y', fontsize=20]
A0->A1 [label='N', fontsize=20]
C2->E [label='Y', fontsize=20]
C2->D1 [label='N', fontsize=20]
D1->D2 [label='Y', fontsize=20]
D1->D3 [label='N', fontsize=20]

B2->Dec [arrowhead=none,color=grey]
E->Chap [arrowhead=none,color=grey]
D3->Cpue [arrowhead=none,color=grey]
D2->ISUd [arrowhead=none,color=grey,length=0]
A1->NoF [arrowhead=none,color=grey]

graph[nodesep=1]
} 
[1]: D
[2]: Chap
[3]: Cpue
[4]: ISUd
[5]: NoF

",width=1000,height=1000)


save_png(Diag,paste0("Diagram_",Est_Season,"_",Time,".png"))



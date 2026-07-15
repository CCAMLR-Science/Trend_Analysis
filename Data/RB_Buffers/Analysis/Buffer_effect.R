#Test effect of different buffers
library(CCAMLRGIS)
library(dplyr)

setwd("I:/Science/Projects/Trend_Analysis/Data/RB_Buffers/Analysis")

ASDs=load_ASDs()

#Build RBs
RBs=read.csv("I:/Science/Projects/Trend_Analysis/Data/RB_Buffers/RBVertices.csv")
RBs=create_Polys(RBs)

#Load buffered RBs
B5k=st_read("I:/Science/Projects/Trend_Analysis/Data/RB_Buffers/BufferedRBs_5km.gpkg",quiet=T)
B1F=st_read("I:/Science/Projects/Trend_Analysis/Data/RB_Buffers/BufferedRBs_1FSR.gpkg",quiet=T)
B2F=st_read("I:/Science/Projects/Trend_Analysis/Data/RB_Buffers/BufferedRBs_2FSR.gpkg",quiet=T)

#Get legend
source("LegSet.R")

#Load labels
RBlabs=read.csv("RBlabs.csv")

#Plot map
png(filename="BufferedRBs_comparison.png", width = 5000, height = 5000,res=600)
par(mai=rep(0,4))
plot(st_geometry(RBs),border=NA,col="white")
plot(st_geometry(ASDs),lwd=0.1,add=T)
plot(st_geometry(Coast[Coast$ID=='All',]),border=NA,col='grey85',add=T)
plot(st_geometry(RBs),border=NA,col="grey50",add=T)
plot(st_geometry(B5k),lwd=0.5,add=T, border="blue")
plot(st_geometry(B1F),lwd=0.5,add=T, border="red")
plot(st_geometry(B2F),lwd=0.5,add=T, border="green")
add_Legend(bb,LegOpt,Items)

for(i in seq(1,nrow(RBlabs))){
  if(RBlabs$Labx2[i]!=RBlabs$Labx[i] | RBlabs$Laby2[i]!=RBlabs$Laby[i]){
    segments(x0=RBlabs$Labx[i],y0=RBlabs$Laby[i],x1=RBlabs$Labx2[i],y1=RBlabs$Laby2[i])
  }
  text(RBlabs$Labx2[i],RBlabs$Laby2[i],RBlabs$ID[i],cex=1,adj=c(RBlabs$adjx[i],RBlabs$adjy[i]))
}

dev.off()


#Plot map
png(filename="BufferedRBs_comparison_zoom.png", width = 4000, height = 5000,res=600)
par(mai=rep(0,4))
plot(st_geometry(B2F[B1F$name%in%c("883_3","883_4","883_11","883_12"),]),border=NA,col="white")
plot(st_geometry(Coast[Coast$ID=='All',]),border=NA,col='grey85',add=T)
plot(st_geometry(RBs[RBs$ID%in%c("883_3","883_4","883_11","883_12"),]),border=NA,col="grey50",add=T)

plot(st_geometry(B2F[B1F$name%in%c("883_3","883_4","883_11","883_12"),]),lwd=3,add=T, border="green")
plot(st_geometry(B1F[B1F$name%in%c("883_3","883_4","883_11","883_12"),]),lwd=2,add=T, border="red")
plot(st_geometry(B5k[B5k$name%in%c("883_3","883_4","883_11","883_12"),]),lwd=1,add=T, border="blue")

text(RBs$Labx[RBs$ID%in%c("883_3","883_4","883_11","883_12")],
     RBs$Laby[RBs$ID%in%c("883_3","883_4","883_11","883_12")],
     RBs$ID[RBs$ID%in%c("883_3","883_4","883_11","883_12")],cex=1)
add_Legend(bbZ,LegOptZ,Items)

dev.off()

# Ps=RBs
# Vp=PolyVor(Ps)
# 
# png(filename="Test.png", width = 5000, height = 5000,res=600)
# par(mai=rep(0,4))
# plot(st_geometry(Ps))
# plot(st_geometry(Vp),add=T,border="green",col=NA,xpd=T)
# dev.off()


#Get data
source("Load_TA_data.R")

#Table No of releases, recaptures and Nhauls

#Releases
Reln=expand.grid(RB=sort(unique(Catch$RB_s)),S=sort(unique(Catch$season_ccamlr)))
tmp=Rels%>%group_by(S=season_ccamlr,RB)%>%summarise(nRB=n(),.groups='drop')
Reln=left_join(Reln,tmp,by=c('S', 'RB'))
tmp=Rels%>%group_by(S=season_ccamlr,RB=B5k)%>%summarise(n5km=n(),.groups='drop')
Reln=left_join(Reln,tmp,by=c('S', 'RB'))
tmp=Rels%>%group_by(S=season_ccamlr,RB=B1F)%>%summarise(n1fsr=n(),.groups='drop')
Reln=left_join(Reln,tmp,by=c('S', 'RB'))
tmp=Rels%>%group_by(S=season_ccamlr,RB=B2F)%>%summarise(n2fsr=n(),.groups='drop')
Reln=left_join(Reln,tmp,by=c('S', 'RB'))
Reln=arrange(Reln,RB)

#Recaptures
Recn=expand.grid(RB=sort(unique(Catch$RB_s)),S=sort(unique(Catch$season_ccamlr)))
tmp=Recs%>%group_by(S=season_ccamlr,RB)%>%summarise(nRB=n(),.groups='drop')
Recn=left_join(Recn,tmp,by=c('S', 'RB'))
tmp=Recs%>%group_by(S=season_ccamlr,RB=B5k)%>%summarise(n5km=n(),.groups='drop')
Recn=left_join(Recn,tmp,by=c('S', 'RB'))
tmp=Recs%>%group_by(S=season_ccamlr,RB=B1F)%>%summarise(n1fsr=n(),.groups='drop')
Recn=left_join(Recn,tmp,by=c('S', 'RB'))
tmp=Recs%>%group_by(S=season_ccamlr,RB=B2F)%>%summarise(n2fsr=n(),.groups='drop')
Recn=left_join(Recn,tmp,by=c('S', 'RB'))
Recn=arrange(Recn,RB)

#Hauls
Hauln=expand.grid(RB=sort(unique(Catch$RB_s)),S=sort(unique(Catch$season_ccamlr)))
tmp=Catch%>%group_by(S=season_ccamlr,RB=RB_s)%>%summarise(nRB=n(),.groups='drop')
Hauln=left_join(Hauln,tmp,by=c('S', 'RB'))
tmp=Catch%>%group_by(S=season_ccamlr,RB=B5k_s)%>%summarise(n5km=n(),.groups='drop')
Hauln=left_join(Hauln,tmp,by=c('S', 'RB'))
tmp=Catch%>%group_by(S=season_ccamlr,RB=B1F_s)%>%summarise(n1fsr=n(),.groups='drop')
Hauln=left_join(Hauln,tmp,by=c('S', 'RB'))
tmp=Catch%>%group_by(S=season_ccamlr,RB=B2F_s)%>%summarise(n2fsr=n(),.groups='drop')
Hauln=left_join(Hauln,tmp,by=c('S', 'RB'))
Hauln=arrange(Hauln,RB)

#Compute differences
Reln$n5km=Reln$n5km-Reln$nRB
Reln$n1fsr=Reln$n1fsr-Reln$nRB
Reln$n2fsr=Reln$n2fsr-Reln$nRB

Recn$n5km=Recn$n5km-Recn$nRB
Recn$n1fsr=Recn$n1fsr-Recn$nRB
Recn$n2fsr=Recn$n2fsr-Recn$nRB

Hauln$n5km=Hauln$n5km-Hauln$nRB
Hauln$n1fsr=Hauln$n1fsr-Hauln$nRB
Hauln$n2fsr=Hauln$n2fsr-Hauln$nRB

#Keep non-zero diffs
Reln=Reln%>%filter(n5km!=0 | n1fsr!=0 | n2fsr!=0)
Recn=Recn%>%filter(n5km!=0 | n1fsr!=0 | n2fsr!=0)
Hauln=Hauln%>%filter(n5km!=0 | n1fsr!=0 | n2fsr!=0)

write.csv(Reln,"Reln.csv",row.names = F)
write.csv(Recn,"Recn.csv",row.names = F)
write.csv(Hauln,"Hauln.csv",row.names = F)
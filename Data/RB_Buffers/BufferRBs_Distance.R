#Script to buffer RBs by distance
library(CCAMLRGIS)
library(dplyr)
setwd('I:/Science/Projects/Trend_Analysis/Data/RB_Buffers')


RBs=read.csv("RBVertices.csv")
RBs=create_Polys(RBs)
RBsEX=load_RBs()
ssrus=load_SSRUs()
ssrus=ssrus[ssrus$GAR_Short_Label=="882H",]
RBsEX=rbind(RBsEX%>%select(id),ssrus%>%select(id))

png(filename="BufferedRBs_Old-vs-New.png", width = 10000, height = 10000,res=600)
par(mai=rep(0,4))
plot(st_geometry(RBs),lwd=3,border="green")
plot(st_geometry(RBsEX),add=T)
text(RBs$Labx,RBs$Laby,RBs$ID,cex=0.5)
dev.off()

#Get Clippers
Cl=read.csv("RBclippers.csv")
Cl=create_Polys(Cl)

plot(st_geometry(RBs))
plot(st_geometry(Cl),add=T,border="red")


#Get centers to label
Cen=st_centroid(st_geometry(RBs))
Cen=st_coordinates(Cen)

par(mai=rep(0,4))
plot(st_geometry(RBs))
text(Cen[,1],Cen[,2],RBs$ID,cex=0.75)

#5km buffer
RBs_B=CCAMLRGIS:::add_buffer(RBs,buf=5/1.852) #5km buffer

RBs_B_done=NULL
for(i in seq(1,nrow(RBs_B))){
  tmp=RBs_B[i,]
  tmp=tmp%>%select(ID)
  if(tmp$ID %in% Cl$ID){ #crop
    tmpcl=Cl[Cl$ID==tmp$ID,]
    tmpcl=tmpcl%>%select(ID)
    tmp=suppressWarnings(st_intersection(tmp,tmpcl))
    tmp=tmp%>%select(ID)
  }
  RBs_B_done=rbind(RBs_B_done,tmp)
}

plot(st_geometry(RBs))
plot(st_geometry(RBs_B_done),add=T,border="red")

RBs_B_done$name=RBs_B_done$ID
RBs_B_done=RBs_B_done%>%select(-ID)
r882=RBs%>%filter(ID=="882H")
r882$name=r882$ID
r882=r882%>%select(name)

RBs_B_done=RBs_B_done%>%filter(name!="882H")
plot(st_geometry(r882))
plot(st_geometry(RBs_B_done),add=T,border='red')
RBs_B_done=suppressWarnings( st_difference(RBs_B_done,r882) )
plot(st_geometry(r882))
plot(st_geometry(RBs_B_done),add=T,border='red')
RBs_B_done=RBs_B_done%>%select(name)
RBs_B_done=rbind(RBs_B_done,r882)

RBs_B_done$col='green'

RBs_B_done$col[RBs_B_done$name=='883_2']='orange'
RBs_B_done$col[RBs_B_done$name=='882_1']='blue'
RBs_B_done$col[RBs_B_done$name=='883_4']='orange'
RBs_B_done$col[RBs_B_done$name=='883_3']='blue'
RBs_B_done$col[RBs_B_done$name=='883_1']='blue'
RBs_B_done$col[RBs_B_done$name=='882_3']='orange'
RBs_B_done$col[RBs_B_done$name=='883_9']='blue'
RBs_B_done$col[RBs_B_done$name=='883_10']='orange'
RBs_B_done$col[RBs_B_done$name=='481_1']='orange'
RBs_B_done$col[RBs_B_done$name=='481_2']='blue'
RBs_B_done$col[RBs_B_done$name=='481_3']='orange'
RBs_B_done$col[RBs_B_done$name=='482_N']='blue'
RBs_B_done$col[RBs_B_done$name=='482_S']='orange'
RBs_B_done$col[RBs_B_done$name=='883_12']='orange'
RBs_B_done$col[RBs_B_done$name=='883_11']='blue'



png(filename="BufferedRBs_5km.png", width = 10000, height = 10000,res=600)
par(mai=rep(0,4))
plot(st_geometry(RBs_B_done),border='black',col=RBs_B_done$col,lwd=0.05)
plot(st_geometry(RBs),lwd=0.05,col=rgb(1,1,1,alpha=0.5),border=rgb(1,1,1,alpha=0.5),add=T)
text(Cen[,1],Cen[,2],RBs_B$ID,cex=0.5)
dev.off()

#Compare to previous buffers
tmp=st_read("I:/Science/Projects/Trend_Analysis/Data/RB_Buffers/Ex/BufferedRBs.shp",quiet=T)

png(filename="BufferedRBs_Old-vs-New_buffers.png", width = 10000, height = 10000,res=600)
par(mai=rep(0,4))
plot(st_geometry(tmp))
plot(st_geometry(RBs_B_done),add=T,border='green')
text(Cen[,1],Cen[,2],RBs_B$ID,cex=0.5)
dev.off()

st_write(RBs_B_done,"BufferedRBs_5km.gpkg",quiet=T,append=F,delete_dsn=T)
# st_write(RBs_B_done, "BufferedRBs.shp",append = F,quiet = T)

#Script to buffer RBs by distance
library(CCAMLRGIS)
library(dplyr)


#Get RBs and 882H
RBs=st_read("https://github.com/ccamlr/geospatial_operations/raw/10c7ee8d3960e22a7dd79c0663c38979c074fb38/Dataset/GeoPackages/CCAMLR_RB.gpkg",quiet=T)
SSRUs=st_read("https://github.com/ccamlr/geospatial_operations/raw/10c7ee8d3960e22a7dd79c0663c38979c074fb38/Dataset/GeoPackages/CCAMLR_SSRU.gpkg",quiet=T)
SSRUs=SSRUs[SSRUs$ID=="882H",]

#Get Clippers
Cl=read.csv("I:/Science/Projects/Trend_Analysis/Data/RB_Buffers/RBclippers.csv")
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
#Clip overlapping buffers
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


SSRUs$name=SSRUs$ID
SSRUs=SSRUs%>%select(name)

plot(st_geometry(SSRUs))
plot(st_geometry(RBs_B_done),add=T,border='red')
RBs_B_done=suppressWarnings( st_difference(RBs_B_done,SSRUs) )
plot(st_geometry(SSRUs))
plot(st_geometry(RBs_B_done),add=T,border='red')
RBs_B_done=RBs_B_done%>%select(name)
RBs_B_done=rbind(RBs_B_done,SSRUs)

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



png(filename="Data/BufferedRBs.png", width = 10000, height = 10000,res=600)
par(mai=rep(0,4))
plot(st_geometry(RBs_B_done),border='black',col=RBs_B_done$col,lwd=0.05)
plot(st_geometry(RBs),lwd=0.05,col=rgb(1,1,1,alpha=0.5),border=rgb(1,1,1,alpha=0.5),add=T)
text(Cen[,1],Cen[,2],RBs_B$ID,cex=0.5)
dev.off()


RBs_B_done=RBs_B_done%>%select(name)
RBs$name=RBs$ID
RBs=RBs%>%select(name)
RBs=rbind(RBs,RBs_B_done[RBs_B_done$name=="882H",])

st_write(RBs_B_done,"Data/BufferedRBs.gpkg",quiet=T,append=F,delete_dsn=T)
st_write(RBs,"Data/RBs.gpkg",quiet=T,append=F,delete_dsn=T)


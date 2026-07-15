#Script to buffer RBs by Fine Scale Rectangles
library(CCAMLRGIS)
library(dplyr)
setwd('I:/Science/Projects/Trend_Analysis/Data/RB_Buffers')

#FSR: 0.5Lat x 1.0Lon

RBs=read.csv("RBVertices.csv")

# #Check clockwiseness
# for(i in seq(1,length(unique(RBs$Name)))){
# rb=unique(RBs$Name)[i]
# plot(RBs$Longitude[RBs$Name==rb],RBs$Latitude[RBs$Name==rb],main=rb)
# lines(RBs$Longitude[RBs$Name==rb],RBs$Latitude[RBs$Name==rb])
# text(RBs$Longitude[RBs$Name==rb],RBs$Latitude[RBs$Name==rb],seq(1,length(which(RBs$Name==rb))),col="red",font=2)
# }


dlon=1
dlat=0.5
Suf="1FSR"
dlon=dlon*2
dlat=dlat*2
Suf="2FSR"


Buf=NULL
#Loop over RBs to build FSRs around each vertex, then keep those outside of the polygon
for(pname in unique(RBs$Name)){
  Lons=RBs$Longitude[RBs$Name==pname]
  Lats=RBs$Latitude[RBs$Name==pname]
  
  Pol=st_polygon(list(cbind(c(Lons,Lons[1]),c(Lats,Lats[1]))))
  
  #segmentize to put FSRs along the boundary of Pol
  Pol=st_segmentize(Pol,dfMaxLength = 0.2)
  tmp=st_coordinates(Pol)
  Lons=tmp[,1]
  Lats=tmp[,2]

  Ps=NULL #Points storage
  pols=list() #FSR storage
  for(i in seq(1,length(Lons))){
    lon=Lons[i]
    lat=Lats[i]

    lon=c(lon-dlon,lon-dlon,lon,lon+dlon,lon+dlon,lon+dlon,lon,lon-dlon)
    lat=c(lat,lat+dlat,lat+dlat,lat+dlat,lat,lat-dlat,lat-dlat,lat-dlat)
    
    Ps=rbind(Ps,data.frame(lon,lat))
    pols[[i]]=st_polygon(list(cbind(c(lon,lon[1]),c(lat,lat[1]))))
  }
  pols=st_sfc(pols, crs = 4326)
  pols=st_union(pols)
  pols=st_make_valid(pols)
  pols=st_cast(pols,"POLYGON")
  pols=st_make_valid(pols)
  pols=st_union(pols,st_make_valid(st_sfc(Pol, crs = 4326)))
  pols=st_union(pols)
  pols=st_simplify(pols,preserveTopology = T,dTolerance = 7000)

  Coo=st_coordinates(pols)
  Coo=distinct(as.data.frame(Coo[,1:2]))
  if(lwgeom::st_is_polygon_cw(pols)==F){Coo=Coo[seq(nrow(Coo),1),]}
  
  #If there are issues, might need to round Lat/Lon after st_simplify
  Buf=rbind(Buf,data.frame(Name=pname,Latitude=Coo$Y,Longitude=Coo$X))
}



RBs=create_Polys(RBs)
Buf=create_Polys(Buf)


png(filename=paste0("BufferedRBs_test_",Suf,".png"), width = 10000, height = 10000,res=600)
par(mai=rep(0,4))
plot(st_geometry(RBs))
plot(st_geometry(Buf),add=T,border="red")
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



RBs_B=Buf

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



png(filename=paste0("BufferedRBs_",Suf,".png"), width = 10000, height = 10000,res=600)
par(mai=rep(0,4))
plot(st_geometry(RBs_B_done),border='black',col=RBs_B_done$col,lwd=0.05)
plot(st_geometry(RBs),lwd=0.05,col=rgb(1,1,1,alpha=0.5),border=rgb(1,1,1,alpha=0.5),add=T)
text(Cen[,1],Cen[,2],RBs_B$ID,cex=0.5)
dev.off()


st_write(RBs_B_done,paste0("BufferedRBs_",Suf,".gpkg"),quiet=T,append=F,delete_dsn=T)
# st_write(RBs_B_done, "BufferedRBs.shp",append = F,quiet = T)

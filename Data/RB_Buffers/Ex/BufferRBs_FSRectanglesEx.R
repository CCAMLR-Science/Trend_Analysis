#Script to buffer RBs properly
library(CCAMLRGIS)
library(dplyr)
# setwd('I:/Science/Projects/Trend_Analysis/Data')

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
Buf=NULL
#Loop over RBs to build FSRs around each vertex, then keep those outside of the polygon
for(pname in unique(RBs$Name)){
  Lons=RBs$Longitude[RBs$Name==pname]
  Lats=RBs$Latitude[RBs$Name==pname]
  
  Pol=st_polygon(list(cbind(c(Lons,Lons[1]),c(Lats,Lats[1]))))
  
  Ps=NULL #Points storage
  for(i in seq(1,length(Lons))){
    lon=Lons[i]
    lat=Lats[i]
    
    if(i == length(Lons)){ #Get next vertex to help order new vertices
      lonNext=Lons[1]
      latNext=Lats[1]
    }else{
      lonNext=Lons[i+1]
      latNext=Lats[i+1]
    }
    
    lon=c(lon-dlon,lon-dlon,lon,lon+dlon,lon+dlon,lon+dlon,lon,lon-dlon)
    lat=c(lat,lat+dlat,lat+dlat,lat+dlat,lat,lat-dlat,lat-dlat,lat-dlat)
    
    FSR=st_as_sf(x=data.frame(lon,lat),coords=c(1,2),crs=4326)
    Vnext=st_as_sf(x=data.frame(lonNext,latNext),coords=c(1,2),crs=4326)
    Ds=st_distance(FSR,Vnext)
    Ds=which(Ds==min(Ds))
    
    if(Ds!=length(lon)){
      if((Ds+1)==length(lon)){
        lon=lon[c(length(lon),1:Ds)]
        lat=lat[c(length(lat),1:Ds)]
      }else{
        lon=lon[c((Ds+1):length(lon),1:Ds)]
        lat=lat[c((Ds+1):length(lat),1:Ds)]
      }
    }
    
    Ps=rbind(Ps,data.frame(lon,lat))
  }
  Pt=st_as_sf(x=Ps,coords=c(1,2))
  
  In=unlist(st_contains(Pol,Pt,sparse = T))
  if(length(In)>0){Pt=Pt[-In,]}
  In=unlist(st_touches(Pol,Pt,sparse = T))
  if(length(In)>0){Pt=Pt[-In,]}
  Coo=st_coordinates(Pt)
  Coo=distinct(as.data.frame(Coo))
  Buf=rbind(Buf,data.frame(Name=pname,Latitude=Coo$Y,Longitude=Coo$X))
}



RBs=create_Polys(RBs)
Buf=create_Polys(Buf)


png(filename="BufferedRBs_FSR_test.png", width = 10000, height = 10000,res=600)
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



# st_write(RBs_B_done, "BufferedRBs.shp",append = F,quiet = T)

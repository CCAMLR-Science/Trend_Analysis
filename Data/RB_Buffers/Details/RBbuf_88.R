#Script to build Research Blocks buffers
library(CCAMLRGIS)
library(dplyr)

#1. 11, 12, 3 and 4

#Load vertices
Vs=read.csv("Verts_88.csv")
Vs=Vs[Vs$Name%in%c("883_3","883_12"),]

#Base polygons
Ps=create_Polys(Vs)
Vsp=create_Points(Vs[,c('Lat','Lon')])

par(mai=rep(0,4),xaxs="i",yaxs="i")
plot(st_geometry(Ps))
text(Vsp$x,Vsp$y,Vsp$ID,col="red",font=2)

#Buffered polygons
Ps_b1=create_Polys(Vs,Buffer = 10,SeparateBuf = F)

par(mai=rep(0,4),xaxs="i",yaxs="i")
plot(st_geometry(Ps))
plot(st_geometry(Ps_b1),add=T,border="blue")


#Isolate common points
CpsIn=create_Points(data.frame(Lat=c(-70,-71),
                               Lon=c(-90,-90)))


plot(st_geometry(Ps))
plot(st_geometry(Ps_b1),add=T,border="blue")
plot(st_geometry(CpsIn),add=T,col="red",lwd=3)

b1p=st_cast(Ps_b1,"POINT")
Ds=st_distance(CpsIn,b1p)
tt=apply(Ds,1,function(x) which(x==min(x)))

plot(st_geometry(Ps))
plot(st_geometry(Ps_b1),add=T,border="blue")
plot(st_geometry(CpsIn),add=T,col="red")
plot(st_geometry(b1p[tt,]),col="green",add=T,lwd=2)

A=st_coordinates(CpsIn)
B=st_coordinates(b1p[tt,])

L1=rbind(A[1,],B[1,])
L2=rbind(A[2,],B[2,])

points(L1[,1],L1[,2],col="orange",lwd=3)
points(L2[,1],L2[,2],col="yellow",lwd=3)

L1=as.data.frame(L1)
L2=as.data.frame(L2)

L1=project_data(L1,NamesIn = c("Y","X"),inv=T)
L2=project_data(L2,NamesIn = c("Y","X"),inv=T)

range(Vs$Lat)
range(Vs$Lon)

#Get intersection with outside line
#c(Longitude_start,Latitude_start,Longitude_end,Latitude_end)
LineOut1=c(-95,-70,-95,-71.5) #west
LineOut2=c(-85,-65,-85,-80) #east

L1=c(L1$Longitude[1],L1$Latitude[1],L1$Longitude[2],L1$Latitude[2])
L2=c(L2$Longitude[1],L2$Latitude[1],L2$Longitude[2],L2$Latitude[2])

P1=get_C_intersection(LineOut1,L1)
P2=get_C_intersection(LineOut2,L2)


#Load vertices
Vs2=read.csv("88clipper.csv")

#Base polygons
Ps2=create_Polys(Vs2)

par(mai=rep(0,4),xaxs="i",yaxs="i")
plot(st_geometry(Ps2))
plot(st_geometry(Ps),add=T)
plot(st_geometry(Ps2),add=T,border="blue",col=rainbow(nrow(Ps2)))






#1. 882_1 and 883_2

#Load vertices
Vs=read.csv("Verts_88.csv")
Vs=Vs[Vs$Name%in%c("882_1","883_2"),]

#Base polygons
Ps=create_Polys(Vs)
Vsp=create_Points(Vs[,c('Lat','Lon')])

par(mai=rep(0,4),xaxs="i",yaxs="i")
plot(st_geometry(Ps))
text(Vsp$x,Vsp$y,Vsp$ID,col="red",font=2)

#Buffered polygons
Ps_b1=create_Polys(Vs,Buffer = 10,SeparateBuf = F)

par(mai=rep(0,4),xaxs="i",yaxs="i")
plot(st_geometry(Ps))
plot(st_geometry(Ps_b1),add=T,border="blue")


#Isolate common points
CpsIn=create_Points(data.frame(Lat=c(-73.8),
                               Lon=c(-105)))


plot(st_geometry(Ps))
plot(st_geometry(Ps_b1),add=T,border="blue")
plot(st_geometry(CpsIn),add=T,col="red",lwd=3)

b1p=st_cast(Ps_b1,"POINT")
Ds=st_distance(CpsIn,b1p)
tt=apply(Ds,1,function(x) which(x==min(x)))

plot(st_geometry(Ps))
plot(st_geometry(Ps_b1),add=T,border="blue")
plot(st_geometry(CpsIn),add=T,col="red",lwd=2)
plot(st_geometry(b1p[tt,]),col="green",add=T,lwd=2)

A=st_coordinates(CpsIn)
B=st_coordinates(b1p[tt,])

L1=rbind(A[1,],B[1,])

points(L1[,1],L1[,2],col="orange",lwd=3)

L1=as.data.frame(L1)

L1=project_data(L1,NamesIn = c("Y","X"),inv=T)

range(Vs$Lat)
range(Vs$Lon)

#Get intersection with outside line
#c(Longitude_start,Latitude_start,Longitude_end,Latitude_end)
LineOut1=c(-111,-70,-111,-78) #west


L1=c(L1$Longitude[1],L1$Latitude[1],L1$Longitude[2],L1$Latitude[2])

P1=get_C_intersection(LineOut1,L1)


#Load vertices
Vs2=read.csv("88clipper.csv")

#Base polygons
Ps2=create_Polys(Vs2)

par(mai=rep(0,4),xaxs="i",yaxs="i")
plot(st_geometry(Ps2))
plot(st_geometry(Ps),add=T)
plot(st_geometry(Ps2),add=T,border="blue",col=rainbow(nrow(Ps2)))






#1. 883_9 and 883_10

#Load vertices
Vs=read.csv("Verts_88.csv")
Vs=Vs[Vs$Name%in%c("883_9","883_10"),]

#Base polygons
Ps=create_Polys(Vs)
Vsp=create_Points(Vs[,c('Lat','Lon')])

par(mai=rep(0,4),xaxs="i",yaxs="i")
plot(st_geometry(Ps))
text(Vsp$x,Vsp$y,Vsp$ID,col="red",font=2)

range(Vs$Lat)
range(Vs$Lon)

#Load vertices
Vs2=read.csv("88clipper.csv")

#Base polygons
Ps2=create_Polys(Vs2)

par(mai=rep(0,4),xaxs="i",yaxs="i")
plot(st_geometry(Ps2))
plot(st_geometry(Ps),add=T)
plot(st_geometry(Ps2),add=T,border="blue",col=rainbow(nrow(Ps2)))



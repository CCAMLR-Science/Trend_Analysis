#Script to build Research Blocks buffers
library(CCAMLRGIS)
library(dplyr)

#Load vertices
Vs=read.csv("Verts_482.csv")

#Base polygons
Ps=create_Polys(Vs)

par(mai=rep(0,4),xaxs="i",yaxs="i")
plot(st_geometry(Ps))

#Buffered polygons
Ps_b1=create_Polys(Vs,Buffer = 10,SeparateBuf = F)

par(mai=rep(0,4),xaxs="i",yaxs="i")
plot(st_geometry(Ps))
plot(st_geometry(Ps_b1),add=T,border="blue")


#Isolate common points
Cps=Vs%>%group_by(Lat,Lon)%>%summarise(n=n(),.groups='drop')
Cps=Cps%>%filter(n>1)
Cps=create_Points(Cps)
CpsIn=Cps[c(1,nrow(Cps)),]



plot(st_geometry(Ps))
plot(st_geometry(Ps_b1),add=T,border="blue")
plot(st_geometry(CpsIn),add=T,col="red")

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
LineOut1=c(-43,-57,-43,-63) #west
LineOut2=c(-31,-57,-31,-63) #east

L1=c(L1$Longitude[1],L1$Latitude[1],L1$Longitude[2],L1$Latitude[2])
L2=c(L2$Longitude[1],L2$Latitude[1],L2$Longitude[2],L2$Latitude[2])

Pwest=get_C_intersection(LineOut1,L1)
Peast=get_C_intersection(LineOut2,L2)


#Load vertices
Vs2=read.csv("482clipper.csv")

#Base polygons
Ps2=create_Polys(Vs2)

par(mai=rep(0,4),xaxs="i",yaxs="i")
plot(st_geometry(Ps2))
plot(st_geometry(Ps),add=T)
plot(st_geometry(Ps2),add=T,border="blue")

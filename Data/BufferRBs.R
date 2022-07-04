#Script to buffer RBs properly
library(CCAMLRGIS)
library(rgeos)
setwd('I:/Science/Projects/Research Block Biomass Estimates/ResearchBlockBiomass/Data')

RBs=load_RBs()

#Get SSRU 882H to add it to RBs
SSRUs=load_SSRUs()
SSRUs=SSRUs[SSRUs$GAR_Short_Label=='882H',]


#Simplify dataframes to merge
RBs@data=data.frame(name=RBs$GAR_Short_Label)
SSRUs@data=data.frame(name=SSRUs$GAR_Short_Label)

#Bind
RBs_B=rbind(RBs,SSRUs)
RBs_B$ID=seq(1,length(RBs_B))
rm(RBs,SSRUs)

#Get centers to label
Cen=gCentroid(RBs_B,byid = T)
Cen=coordinates(Cen)

par(mai=rep(0,4))
plot(RBs_B)
text(Cen[,1],Cen[,2],RBs_B$name,cex=0.75)

#Get the easy ones (no shared boundaries)
easies=c("882_2","882_4","883_5","883_6","883_7","883_8",
         "486_2","486_3","486_4","486_5",
         "5844b_1","5844b_2","5843a_1","5842_1","5842_2","5841_1",
         "5841_2","5841_3","5841_4","5841_5","5841_6")
EasyRBs=RBs_B[RBs_B$name%in%easies,]
if(length(easies)!=length(EasyRBs)){stop("wrong name in easies")}

EasyRBs_B=CCAMLRGIS:::add_buffer(EasyRBs,buf=5/1.852) #5km buffer
NoteasyRBs=RBs_B[!RBs_B$name%in%easies,]

plot(EasyRBs_B,add=T,border='green')

RBs_B_done=EasyRBs_B #Store outputs here
RBs_B_done@data=RBs_B_done@data[,1:2]

plot(NoteasyRBs,border='orange',add=T)

#Now do the not easy ones, case by case

#883_1 and 882_3: buffer then clip with 882H
r1=NoteasyRBs[NoteasyRBs$name%in%c("882_3","883_1"),]
r1=CCAMLRGIS:::add_buffer(r1,buf=5/1.852) #5km buffer
r1=rgeos::gDifference(r1,NoteasyRBs[NoteasyRBs$name%in%c("882H"),],byid=T)
r1=SpatialPolygonsDataFrame(r1,data=data.frame(name=c('883_1','882_3'),ID=c(NA,NA)),match.ID = F)
RBs_B_done=rbind(RBs_B_done,r1)
#Add 882H - this one doesn't get buffered as it's not an actual research block
RBs_B_done=rbind(RBs_B_done,NoteasyRBs[NoteasyRBs$name%in%c("882H"),])

#883_3 | 883_4: line @Lon -85
r883_3=NoteasyRBs[NoteasyRBs$name=="883_3",]
r883_4=NoteasyRBs[NoteasyRBs$name=="883_4",]
r883_3=CCAMLRGIS:::add_buffer(r883_3,buf=5/1.852) #5km buffer
r883_4=CCAMLRGIS:::add_buffer(r883_4,buf=5/1.852) #5km buffer

r883_3c=create_Polys(Input=data.frame(nam=c(1,1,1,1),
                                      Lat=c(-50,-80,-80,-50),
                                      Lon=c(-60,-60,-85,-85)))
r883_3=rgeos::gDifference(r883_3,r883_3c,byid=T)
r883_3=SpatialPolygonsDataFrame(r883_3,data=data.frame(name=c('883_3'),ID=c(NA)),match.ID = F)
RBs_B_done=rbind(RBs_B_done,r883_3)

r883_4c=create_Polys(Input=data.frame(nam=c(1,1,1,1),
                                      Lat=c(-50,-80,-80,-50),
                                      Lon=c(-85,-85,-100,-100)))
r883_4=rgeos::gDifference(r883_4,r883_4c,byid=T)
r883_4=SpatialPolygonsDataFrame(r883_4,data=data.frame(name=c('883_4'),ID=c(NA)),match.ID = F)
RBs_B_done=rbind(RBs_B_done,r883_4)


#883_2 | 882_1: line @Lon -105 and intersection between buffers to get angle
par(mai=rep(0,4))
plot(NoteasyRBs[NoteasyRBs$name%in%c('883_2','882_1'),],col='orange',lwd=0.1)
# add_labels(mode='manual')


r883_2=NoteasyRBs[NoteasyRBs$name=="883_2",]
r882_1=NoteasyRBs[NoteasyRBs$name=="882_1",]
r883_2=CCAMLRGIS:::add_buffer(r883_2,buf=10/1.852) #10km buffer
r882_1=CCAMLRGIS:::add_buffer(r882_1,buf=10/1.852) #10km buffer


plot(r883_2,add=T)
plot(r882_1,add=T)

Int=gIntersection(r883_2,r882_1,drop_lower_td=T)
Int=Int@polygons[[1]]@Polygons[[1]]@coords
Int=project_data(Int,NamesIn = c('y','x'),append = F,inv=T)
Int=Int[Int$Longitude<(-105),]
Int=Int[Int$Latitude>-74,]
Int=Int[2,]

# Latitude Longitude
# -73.38999   -105.31

Int=project_data(Int,NamesIn = c('Latitude','Longitude'),append = F)
points(Int$X,Int$Y,col='red')

r883_2=NoteasyRBs[NoteasyRBs$name=="883_2",]
r882_1=NoteasyRBs[NoteasyRBs$name=="882_1",]
Int=r882_1@polygons[[1]]@Polygons[[1]]@coords
Int=data.frame(x=Int[,1],y=Int[,2])
Int=project_data(Int,NamesIn = c('y','x'),append = F,inv=T)

r883_2=CCAMLRGIS:::add_buffer(r883_2,buf=5/1.852) #5km buffer
r882_1=CCAMLRGIS:::add_buffer(r882_1,buf=5/1.852) #5km buffer

plot(NoteasyRBs[NoteasyRBs$name%in%c("883_2","882_1"),])
plot(r883_2,add=T)
plot(r882_1,add=T)
points(Int$X,Int$Y,col='red')


r883_2c=create_Polys(Input=data.frame(nam=c(1,1,1,1,1),
                                      Lat=c(-73.38999,-73.48,-80,-80,-73.38999),
                                      Lon=c(-105.31,-105,   -105,-120,-120)))
r883_2=rgeos::gDifference(r883_2,r883_2c,byid=T)
r883_2=SpatialPolygonsDataFrame(r883_2,data=data.frame(name=c('883_2'),ID=c(NA)),match.ID = F)
RBs_B_done=rbind(RBs_B_done,r883_2)




r882_1c=create_Polys(Input=data.frame(nam=c(1,1,1,1,1),
                                      Lat=c(-73.48,-80, -80, -73.48,  -73.38999),
                                      Lon=c(-90,   -90, -105, -105, -105.31)))
r882_1=rgeos::gDifference(r882_1,r882_1c,byid=T)
r882_1=SpatialPolygonsDataFrame(r882_1,data=data.frame(name=c('882_1'),ID=c(NA)),match.ID = F)
RBs_B_done=rbind(RBs_B_done,r882_1)

#883_9 | 883_10: line @Lon -75
r883_9=NoteasyRBs[NoteasyRBs$name=="883_9",]
r883_10=NoteasyRBs[NoteasyRBs$name=="883_10",]
r883_9=CCAMLRGIS:::add_buffer(r883_9,buf=5/1.852) #5km buffer
r883_10=CCAMLRGIS:::add_buffer(r883_10,buf=5/1.852) #5km buffer

r883_9c=create_Polys(Input=data.frame(nam=c(1,1,1,1),
                                      Lat=c(-50,-80,-80,-50),
                                      Lon=c(-60,-60,-75,-75)))
r883_9=rgeos::gDifference(r883_9,r883_9c,byid=T)
r883_9=SpatialPolygonsDataFrame(r883_9,data=data.frame(name=c('883_9'),ID=c(NA)),match.ID = F)
RBs_B_done=rbind(RBs_B_done,r883_9)

r883_10c=create_Polys(Input=data.frame(nam=c(1,1,1,1),
                                      Lat=c(-50,-80,-80,-50),
                                      Lon=c(-75,-75,-100,-100)))
r883_10=rgeos::gDifference(r883_10,r883_10c,byid=T)
r883_10=SpatialPolygonsDataFrame(r883_10,data=data.frame(name=c('883_10'),ID=c(NA)),match.ID = F)
RBs_B_done=rbind(RBs_B_done,r883_10)


#481_1, _2 and _3: Lines @Lat -62.6666667 & -64
r481_1=NoteasyRBs[NoteasyRBs$name=="481_1",]
r481_1=CCAMLRGIS:::add_buffer(r481_1,buf=5/1.852) #5km buffer
r481_2=NoteasyRBs[NoteasyRBs$name=="481_2",]
r481_2=CCAMLRGIS:::add_buffer(r481_2,buf=5/1.852) #5km buffer
r481_3=NoteasyRBs[NoteasyRBs$name=="481_3",]
r481_3=CCAMLRGIS:::add_buffer(r481_3,buf=5/1.852) #5km buffer


r_c=create_Polys(Input=data.frame(nam=c(1,1,1,1),
                                      Lat=c(-62.6666667,-64,-64,-62.6666667),
                                      Lon=c(-49,-49,-56,-56)))

plot(r_c,border='red');plot(r481_1,add=T)
r481_1=rgeos::gDifference(r481_1,r_c,byid=T)
plot(r481_1,add=T,border='green')
r481_1=SpatialPolygonsDataFrame(r481_1,data=data.frame(name=c('481_1'),ID=c(NA)),match.ID = F)
RBs_B_done=rbind(RBs_B_done,r481_1)

r_c=create_Polys(Input=data.frame(nam=c(1,1,1,1),
                                  Lat=c(-62.6666667,-60,-60,-62.6666667),
                                  Lon=c(-49,-49,-56,-56)))

plot(r_c,border='red');plot(r481_2,add=T)
r481_2=rgeos::gDifference(r481_2,r_c,byid=T)
plot(r481_2,add=T,border='green')

r_c=create_Polys(Input=data.frame(nam=c(1,1,1,1),
                                  Lat=c(-65,-64,-64,-65),
                                  Lon=c(-49,-49,-56,-56)))

plot(r_c,border='red');plot(r481_2,add=T)
r481_2=rgeos::gDifference(r481_2,r_c,byid=T)
plot(r481_2,add=T,border='green')

r481_2=SpatialPolygonsDataFrame(r481_2,data=data.frame(name=c('481_2'),ID=c(NA)),match.ID = F)
RBs_B_done=rbind(RBs_B_done,r481_2)


r_c=create_Polys(Input=data.frame(nam=c(1,1,1,1),
                                  Lat=c(-63,-64,-64,-63),
                                  Lon=c(-49,-49,-56,-56)))

plot(r_c,border='red');plot(r481_3,add=T)
r481_3=rgeos::gDifference(r481_3,r_c,byid=T)
plot(r481_3,add=T,border='green')
r481_3=SpatialPolygonsDataFrame(r481_3,data=data.frame(name=c('481_3'),ID=c(NA)),match.ID = F)
RBs_B_done=rbind(RBs_B_done,r481_3)




#482 N/S - find intersection of buffers


r482_N=NoteasyRBs[NoteasyRBs$name=="482_N",]
r482_S=NoteasyRBs[NoteasyRBs$name=="482_S",]
r482_N=CCAMLRGIS:::add_buffer(r482_N,buf=10/1.852) #10km buffer to find intersect
r482_S=CCAMLRGIS:::add_buffer(r482_S,buf=10/1.852) #10km buffer to find intersect

Int=gIntersection(r482_N,r482_S,drop_lower_td=T)
Int=Int@polygons[[1]]@Polygons[[1]]@coords

plot(NoteasyRBs[NoteasyRBs$name%in%c("482_N","482_S"),],col='orange',lwd=0.1)
plot(r482_N,add=T,border='blue')
plot(r482_S,add=T,border='red')
points(Int[167:168,])

Int=project_data(Int[167:168,],NamesIn = c('y','x'),append = F,inv=T)
Int=Int[2,]

# Latitude Longitude
# -61.327 -38.31785
r_c=create_Polys(Input=data.frame(nam=1,
                                  Lat=c(-57.5,-57.5,-60.16667,-60.16667,-60.5,-61,-60.83333,-61.66667,-61.327,-60.83333),
                                  Lon=c(-37.5,-33,-33,-34,-35,-35.3333,-35.5,-37.66667,-38.31785,-39.5)))

plot(r_c,border="cyan",add=T,lwd=2)

r_c2=create_Polys(Input=data.frame(nam=1,
                                  Lat=c(-63,-63,-60.16667,-60.16667,-60.5,-61,-60.83333,-61.66667,-61.327,-60.83333),
                                  Lon=c(-42,-33,-33,-34,-35,-35.3333,-35.5,-37.66667,-38.31785,-39.5)))

plot(r_c2,border="darkgreen",add=T,lwd=2)

r482_N=NoteasyRBs[NoteasyRBs$name=="482_N",]
r482_S=NoteasyRBs[NoteasyRBs$name=="482_S",]
r482_N=CCAMLRGIS:::add_buffer(r482_N,buf=5/1.852) #5km 
r482_S=CCAMLRGIS:::add_buffer(r482_S,buf=5/1.852) #5km 

plot(r_c2,border='red');plot(r482_N,add=T)
r482_N=rgeos::gDifference(r482_N,r_c2,byid=T)
plot(r482_N,add=T,border='green')
r482_N=SpatialPolygonsDataFrame(r482_N,data=data.frame(name=c('482_N'),ID=c(NA)),match.ID = F)
RBs_B_done=rbind(RBs_B_done,r482_N)

plot(r_c,border='red');plot(r482_S,add=T)
r482_S=rgeos::gDifference(r482_S,r_c,byid=T)
plot(r482_S,add=T,border='green')
r482_S=SpatialPolygonsDataFrame(r482_S,data=data.frame(name=c('482_S'),ID=c(NA)),match.ID = F)
RBs_B_done=rbind(RBs_B_done,r482_S)


colnames(RBs_B_done@data)[2]='col'
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


#Get centers to label
Cen=gCentroid(RBs_B,byid = T)
Cen=coordinates(Cen)


png(filename="BufferedRBs.png", width = 10000, height = 10000,res=600)
par(mai=rep(0,4))
plot(RBs_B_done,border='black',col=RBs_B_done$col,lwd=0.05)
plot(RBs_B,lwd=0.05,col=rgb(1,1,1,alpha=0.5),border=rgb(1,1,1,alpha=0.5),add=T)
text(Cen[,1],Cen[,2],RBs_B$name,cex=0.5)
dev.off()



rgdal::writeOGR(RBs_B_done,".","BufferedRBs",driver="ESRI Shapefile")

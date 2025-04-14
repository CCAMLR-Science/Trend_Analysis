#Script to buffer RBs properly
library(CCAMLRGIS)
library(dplyr)
setwd('I:/Science/Projects/Trend_Analysis/Data')

RBs=load_RBs()

#Get SSRU 882H to add it to RBs
SSRUs=load_SSRUs()
SSRUs=SSRUs[SSRUs$GAR_Short_Label=='882H',]


#Simplify dataframes to merge
RBs=RBs%>%select(name=GAR_Short_Label)
SSRUs=SSRUs%>%select(name=GAR_Short_Label)
#Bind
RBs_B=rbind(RBs,SSRUs)
RBs_B$ID=seq(1,nrow(RBs_B))
rm(RBs,SSRUs)

#Get centers to label
Cen=st_centroid(st_geometry(RBs_B))
Cen=st_coordinates(Cen)

par(mai=rep(0,4))
plot(st_geometry(RBs_B))
text(Cen[,1],Cen[,2],RBs_B$name,cex=0.75)

#Get the easy ones (no shared boundaries)
easies=c("882_2","882_4","883_5","883_6","883_7","883_8",
         "486_2","486_3","486_4","486_5",
         "5844b_1","5844b_2","5843a_1","5842_1","5842_2","5841_1",
         "5841_2","5841_3","5841_4","5841_5","5841_6")
EasyRBs=RBs_B[RBs_B$name%in%easies,]
if(length(easies)!=nrow(EasyRBs)){stop("wrong name in easies")}

EasyRBs_B=CCAMLRGIS:::add_buffer(EasyRBs,buf=5/1.852) #5km buffer
NoteasyRBs=RBs_B[!RBs_B$name%in%easies,]

plot(st_geometry(EasyRBs_B),add=T,border='green')

RBs_B_done=EasyRBs_B #Store outputs here
RBs_B_done=RBs_B_done%>%select(name)

plot(st_geometry(NoteasyRBs),border='orange',add=T)

#Now do the not easy ones, case by case

#883_1 and 882_3: buffer then clip with 882H
r1=NoteasyRBs[NoteasyRBs$name%in%c("882_3","883_1"),]
r1=CCAMLRGIS:::add_buffer(r1,buf=5/1.852) #5km buffer
r1=suppressWarnings(st_difference(r1,NoteasyRBs[NoteasyRBs$name%in%c("882H"),]))
r1=r1%>%select(name)
RBs_B_done=rbind(RBs_B_done,r1)
#Add 882H - this one doesn't get buffered as it's not an actual research block
r882H=NoteasyRBs[NoteasyRBs$name%in%c("882H"),]
r882H=r882H%>%select(name)
RBs_B_done=rbind(RBs_B_done,r882H)


plot(st_geometry(NoteasyRBs[NoteasyRBs$name%in%c("883_4","883_3","883_11","883_12"),]))
add_RefGrid(bb=st_bbox(NoteasyRBs[NoteasyRBs$name%in%c("883_4","883_3","883_11","883_12"),]),
            ResLat=0.5,ResLon=1)


#883_4: line @Lon -85
r883_4=NoteasyRBs[NoteasyRBs$name=="883_4",]
r883_4=CCAMLRGIS:::add_buffer(r883_4,buf=5/1.852) #5km buffer

r883_4c=create_Polys(Input=data.frame(nam=c(1,1,1,1),
                                      Lat=c(-50,-80,-80,-50),
                                      Lon=c(-85,-85,-100,-100)))
r883_4=suppressWarnings(st_difference(r883_4,r883_4c))
r883_4=r883_4%>%select(name)
RBs_B_done=rbind(RBs_B_done,r883_4)

#883_3: line @Lon -85 and intersection with 883_12
par(mai=rep(0,4))
plot(st_geometry(NoteasyRBs[NoteasyRBs$name%in%c('883_3','883_12'),]),col='orange',lwd=0.1)
plot(st_geometry(RBs_B_done[RBs_B_done$name=="883_4",]),border='green',lwd=0.1,add=T)

r883_3=NoteasyRBs[NoteasyRBs$name=="883_3",]
r883_12=NoteasyRBs[NoteasyRBs$name=="883_12",]
r883_3=CCAMLRGIS:::add_buffer(r883_3,buf=10/1.852) #10km buffer
r883_12=CCAMLRGIS:::add_buffer(r883_12,buf=10/1.852) #10km buffer

plot(st_geometry(r883_3),add=T)
plot(st_geometry(r883_12),add=T)

Int=suppressWarnings( st_intersection(r883_3,r883_12) )
Int=st_coordinates(Int)
Int=project_data(Int,NamesIn = c('Y','X'),inv=T)
#Point1
P1=Int[which(Int$Latitude>-69.91 & Int$Longitude<(-90.25)),]
points(P1$X,P1$Y,col="green")
#Point2
P2=Int[which(Int$Latitude==min(Int$Latitude)),]
points(P2$X,P2$Y,col="green")

#Do the 85W cut first
r883_3=NoteasyRBs[NoteasyRBs$name=="883_3",]
r883_3=CCAMLRGIS:::add_buffer(r883_3,buf=5/1.852) #5km buffer

r883_3c=create_Polys(Input=data.frame(nam=c(1,1,1,1),
                                      Lat=c(-50,-80,-80,-50),
                                      Lon=c(-85,-85,-80,-80)))
r883_3=suppressWarnings(st_difference(r883_3,r883_3c))

plot(st_geometry(r883_3),border='pink',add=T)

#Now do the border with 883_12
r883_3c=create_Polys(Input=data.frame(nam=1,
                                      Lat=c(P1$Latitude,-70,-71,P2$Latitude,-71.5,-69.5),
                                      Lon=c(P1$Longitude,-90,-90,P2$Longitude,-92,-92)))
r883_3=suppressWarnings(st_difference(r883_3,r883_3c))

plot(st_geometry(r883_3),border='cyan',add=T)

r883_3=r883_3%>%select(name)
RBs_B_done=rbind(RBs_B_done,r883_3)


#883_12: Intersection with 883_3 and line at 95W
par(mai=rep(0,4))
plot(st_geometry(NoteasyRBs[NoteasyRBs$name%in%c('883_3','883_12'),]),col='orange',lwd=0.1)
plot(st_geometry(RBs_B_done[RBs_B_done$name=="883_3",]),border='green',lwd=0.1,add=T)

#Do the border with 883_3 first
r883_12=NoteasyRBs[NoteasyRBs$name=="883_12",]
r883_12=CCAMLRGIS:::add_buffer(r883_12,buf=5/1.852) #5km buffer

r883_12c=create_Polys(Input=data.frame(nam=1,
                                      Lat=c(P1$Latitude,-69.5,-71.5,P2$Latitude,-71,-70),
                                      Lon=c(P1$Longitude,-89,-89,P2$Longitude,-90,-90)))
r883_12=suppressWarnings(st_difference(r883_12,r883_12c))

plot(st_geometry(r883_12),border='pink',add=T)

#Now do the 85W cut
r883_12c=create_Polys(Input=data.frame(nam=1,
                                      Lat=c(-50,-80,-80,-50),
                                      Lon=c(-100,-100,-95,-95)))
r883_12=suppressWarnings(st_difference(r883_12,r883_12c))

plot(st_geometry(r883_12),border='cyan',add=T,lwd=2)

r883_12=r883_12%>%select(name)
RBs_B_done=rbind(RBs_B_done,r883_12)


#883_11: line @Lon -95
r883_11=NoteasyRBs[NoteasyRBs$name=="883_11",]
r883_11=CCAMLRGIS:::add_buffer(r883_11,buf=5/1.852) #5km buffer

r883_11c=create_Polys(Input=data.frame(nam=1,
                                      Lat=c(-50,-80,-80,-50),
                                      Lon=c(-90,-90,-95,-95)))
r883_11=suppressWarnings(st_difference(r883_11,r883_11c))
plot(st_geometry(r883_11),border='red',add=T,lwd=2)
r883_11=r883_11%>%select(name)
RBs_B_done=rbind(RBs_B_done,r883_11)


#883_2 | 882_1: line @Lon -105 and intersection between buffers to get angle
par(mai=rep(0,4))
plot(st_geometry(NoteasyRBs[NoteasyRBs$name%in%c('883_2','882_1'),]),col='orange',lwd=0.1)
# add_labels(mode='manual')


r883_2=NoteasyRBs[NoteasyRBs$name=="883_2",]
r882_1=NoteasyRBs[NoteasyRBs$name=="882_1",]
r883_2=CCAMLRGIS:::add_buffer(r883_2,buf=10/1.852) #10km buffer
r882_1=CCAMLRGIS:::add_buffer(r882_1,buf=10/1.852) #10km buffer


plot(st_geometry(r883_2),add=T)
plot(st_geometry(r882_1),add=T)

Int=suppressWarnings( st_intersection(r883_2,r882_1) )
Int=st_coordinates(Int)
Int=project_data(Int,NamesIn = c('Y','X'),append = F,inv=T)
Int=Int[Int$Longitude<(-105),]
Int=Int[Int$Latitude>-74,]
Int=Int[2,]

# Latitude Longitude
# -73.38999   -105.31

Int=project_data(Int,NamesIn = c('Latitude','Longitude'),append = F)
points(Int$X,Int$Y,col='red')

r883_2=NoteasyRBs[NoteasyRBs$name=="883_2",]
r882_1=NoteasyRBs[NoteasyRBs$name=="882_1",]

r883_2=CCAMLRGIS:::add_buffer(r883_2,buf=5/1.852) #5km buffer
r882_1=CCAMLRGIS:::add_buffer(r882_1,buf=5/1.852) #5km buffer

plot(st_geometry(NoteasyRBs[NoteasyRBs$name%in%c("883_2","882_1"),]))
plot(st_geometry(r883_2),add=T)
plot(st_geometry(r882_1),add=T)
points(Int$X,Int$Y,col='red')


r883_2c=create_Polys(Input=data.frame(nam=c(1,1,1,1,1),
                                      Lat=c(-73.38999,-73.48,-80,-80,-73.38999),
                                      Lon=c(-105.31,-105,   -105,-120,-120)))
plot(st_geometry(r883_2c),add=T,border='red')

r883_2=suppressWarnings(st_difference(r883_2,r883_2c))
r883_2=r883_2%>%select(name)
plot(st_geometry(r883_2),add=T,col='green')
RBs_B_done=rbind(RBs_B_done,r883_2)


r882_1c=create_Polys(Input=data.frame(nam=c(1,1,1,1,1),
                                      Lat=c(-73.48,-80, -80, -73.48,  -73.38999),
                                      Lon=c(-90,   -90, -105, -105, -105.31)))

plot(st_geometry(r882_1c),add=T,border='red')

r882_1=suppressWarnings(st_difference(r882_1,r882_1c))
r882_1=r882_1%>%select(name)
plot(st_geometry(r882_1),add=T,col='yellow')
RBs_B_done=rbind(RBs_B_done,r882_1)

#883_9 | 883_10: line @Lon -75
r883_9=NoteasyRBs[NoteasyRBs$name=="883_9",]
r883_10=NoteasyRBs[NoteasyRBs$name=="883_10",]
r883_9=CCAMLRGIS:::add_buffer(r883_9,buf=5/1.852) #5km buffer
r883_10=CCAMLRGIS:::add_buffer(r883_10,buf=5/1.852) #5km buffer

r883_9c=create_Polys(Input=data.frame(nam=c(1,1,1,1),
                                      Lat=c(-50,-80,-80,-50),
                                      Lon=c(-60,-60,-75,-75)))
r883_9=suppressWarnings(st_difference(r883_9,r883_9c))
r883_9=r883_9%>%select(name)
RBs_B_done=rbind(RBs_B_done,r883_9)

r883_10c=create_Polys(Input=data.frame(nam=c(1,1,1,1),
                                      Lat=c(-50,-80,-80,-50),
                                      Lon=c(-75,-75,-100,-100)))
r883_10=suppressWarnings(st_difference(r883_10,r883_10c))
r883_10=r883_10%>%select(name)
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

r481_1=suppressWarnings(st_difference(r481_1,r_c))
r481_1=r481_1%>%select(name)
RBs_B_done=rbind(RBs_B_done,r481_1)

r_c=create_Polys(Input=data.frame(nam=c(1,1,1,1),
                                  Lat=c(-62.6666667,-60,-60,-62.6666667),
                                  Lon=c(-49,-49,-56,-56)))


r481_2=suppressWarnings(st_difference(r481_2,r_c))

r_c=create_Polys(Input=data.frame(nam=c(1,1,1,1),
                                  Lat=c(-65,-64,-64,-65),
                                  Lon=c(-49,-49,-56,-56)))

r481_2=suppressWarnings(st_difference(r481_2,r_c))
r481_2=r481_2%>%select(name)
RBs_B_done=rbind(RBs_B_done,r481_2)


r_c=create_Polys(Input=data.frame(nam=c(1,1,1,1),
                                  Lat=c(-63,-64,-64,-63),
                                  Lon=c(-49,-49,-56,-56)))

r481_3=suppressWarnings(st_difference(r481_3,r_c))
r481_3=r481_3%>%select(name)
RBs_B_done=rbind(RBs_B_done,r481_3)




#482 N/S - find intersection of buffers
r482_N=NoteasyRBs[NoteasyRBs$name=="482_N",]
r482_S=NoteasyRBs[NoteasyRBs$name=="482_S",]
r482_N=CCAMLRGIS:::add_buffer(r482_N,buf=10/1.852) #10km buffer to find intersect
r482_S=CCAMLRGIS:::add_buffer(r482_S,buf=10/1.852) #10km buffer to find intersect

Int=suppressWarnings( st_intersection(r482_N,r482_S) )
Int=st_coordinates(Int)


plot(st_geometry(NoteasyRBs[NoteasyRBs$name%in%c("482_N","482_S"),]),col='orange',lwd=0.1)
plot(st_geometry(r482_N),add=T,border='blue')
plot(st_geometry(r482_S),add=T,border='red')
points(Int[101,1],Int[101,2])

Int=project_data(data.frame(X=Int[101,1],Y=Int[101,2]),NamesIn = c('Y','X'),append = F,inv=T)

# Latitude Longitude
# -61.44067 -38.10247
r_c=create_Polys(Input=data.frame(nam=1,
                                  Lat=c(-57.5,-57.5,-60.16667,-60.16667,-60.5,-61,-60.83333,-61.66667,-61.44067,-60.83333),
                                  Lon=c(-37.5,-33,-33,-34,-35,-35.3333,-35.5,-37.66667,-38.10247,-39.5)),Densify = F)

plot(st_geometry(r_c),border="cyan",add=T,lwd=2)

r_c2=create_Polys(Input=data.frame(nam=1,
                                  Lat=c(-63,-63,-60.16667,-60.16667,-60.5,-61,-60.83333,-61.66667,-61.44067,-60.83333),
                                  Lon=c(-42,-33,-33,-34,-35,-35.3333,-35.5,-37.66667,-38.10247,-39.5)),Densify = F)

plot(st_geometry(r_c2),border="darkgreen",add=T,lwd=2)

r482_N=NoteasyRBs[NoteasyRBs$name=="482_N",]
r482_S=NoteasyRBs[NoteasyRBs$name=="482_S",]
r482_N=CCAMLRGIS:::add_buffer(r482_N,buf=5/1.852) #5km 
r482_S=CCAMLRGIS:::add_buffer(r482_S,buf=5/1.852) #5km 

r482_N=suppressWarnings(st_difference(r482_N,r_c2))
plot(st_geometry(r482_N),add=T,col='green')
r482_N=r482_N%>%select(name)
RBs_B_done=rbind(RBs_B_done,r482_N)

r482_S=suppressWarnings(st_difference(r482_S,r_c))
plot(st_geometry(r482_S),add=T,col='cyan')
r482_S=r482_S%>%select(name)
RBs_B_done=rbind(RBs_B_done,r482_S)

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


png(filename="BufferedRBs.png", width = 10000, height = 10000,res=600)
par(mai=rep(0,4))
plot(st_geometry(RBs_B_done),border='black',col=RBs_B_done$col,lwd=0.05)
plot(st_geometry(RBs_B),lwd=0.05,col=rgb(1,1,1,alpha=0.5),border=rgb(1,1,1,alpha=0.5),add=T)
text(Cen[,1],Cen[,2],RBs_B$name,cex=0.5)
dev.off()

st_write(RBs_B_done, "BufferedRBs.shp",append = F,quiet = T)

#Script to generate RB map and Management Area (48, 58, 88) thumbnails
library(CCAMLRGIS)
library(terra)
library(dplyr)


ASDs=st_read("https://github.com/ccamlr/geospatial_operations/raw/10c7ee8d3960e22a7dd79c0663c38979c074fb38/Dataset/GeoPackages/CCAMLR_ASD.gpkg",quiet=T)
RBs=st_read("Data/RBs.gpkg",quiet=T)
CA=st_read("https://github.com/ccamlr/geospatial_operations/raw/10c7ee8d3960e22a7dd79c0663c38979c074fb38/Dataset/GeoPackages/CCAMLR_CA.gpkg",quiet=T)
#Get coastline
coast=st_read("https://github.com/ccamlr/geospatial_operations/raw/10c7ee8d3960e22a7dd79c0663c38979c074fb38/Dataset/External%20data/Coastline.gpkg",quiet=T)
#Get Bathy (! UP TO DATE)
B=rast("I:/Science/Projects/GEBCO/2026/Processed/GEBCO2026_500.tif")
#Get Ref Areas
RA=st_read("Data/RefAreas.gpkg",quiet=T)


# #TEMP FIX FOR 483A!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MAs=load_MAs()
MAs=MAs%>%select(name=GAR_Short_Label)%>%filter(name=="483A")



#List RBs that require catch advice
RBsCAdv=c("482_N","482_S",
          "486_2","486_3","486_4","486_5",
          "5841_1","5841_2","5841_3","5841_4","5841_5","5841_6",
          "5842_1","5842_2",
          "882_1","882_2","882_3","882_4","882H",
          "883_1","883_2","883_3","883_4","883_6","883_11","883_12"
          )



#The rest should me automatic


RBsY=RBs%>%filter(name%in%RBsCAdv)
RBs=RBs%>%filter(!name%in%RBsCAdv)


#Get Convention Area to extract Isobaths
Iso=get_iso_polys(B,Poly=CA, Cuts=c(-1800,-600))

#Get labels
Labs=read.csv("Data/LabelsRBs.csv")
LabsY=Labs%>%filter(text%in%RBsCAdv | text=="483A")
Labs=Labs%>%filter(!text%in%RBsCAdv)
# Labs=Labs[Labs$text!="483A",]


png(filename = 'Data/Map_TrendAnalysis.png', width = 2000, height = 1900, units = "px", pointsize = 12,
    bg = "white", res = 200)
par(mai=c(1.45,1.45,0.4,0.3),xaxs='i',yaxs='i')
plot(st_geometry(ASDs),lwd=2,col='grey90',border="grey20",xpd=T)

plot(st_geometry(Iso),col="green",border=NA,add=T)
plot(st_geometry(coast[coast$Surface=="Ice",]),col="white",lwd=0.5,add=T)
plot(st_geometry(ASDs),lwd=3,border="grey20",add=T,xpd=T)
plot(st_geometry(coast[coast$Surface=="Land",]),col='grey70',border='grey50',add=T,lwd=0.5)

add_RefGrid(bb=st_bbox(ASDs),ResLat=10,ResLon=20,LabLon=0,offset = 1,lwd=1,fontsize = 0.9)

plot(st_geometry(RBs),add=T,lwd=2,border='red')
plot(st_geometry(RBsY),add=T,lwd=2,border='blue')
plot(st_geometry(MAs),add=T,lwd=2,border='blue') #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
plot(st_geometry(RA),add=T,lwd=2,border=rgb(1,0.5,0,0.5),col=rgb(1,0.5,0,0.4))

text(Labs$x,Labs$y,Labs$text,cex=1,col='darkred',font=2,xpd=T)
text(LabsY$x,LabsY$y,LabsY$text,cex=1,col='darkblue',font=2,xpd=T)
text(-290000,-2950000,"RSR Open",cex=1.3,col='black',font=2,xpd=T)
text(3900000,1050000,"HIMI",cex=1.3,col='black',font=2,xpd=T)


dev.off()






# A48=as_Spatial(A48)
# A58=as_Spatial(A58)
# A88=as_Spatial(A88)

A48=ASDs[ASDs$ID=="48",]
A58=ASDs[ASDs$ID=="58",]
A88=ASDs[ASDs$ID=="88",]

png(filename = 'Data/Area48.png', width = 300, height = 300, units = "px", pointsize = 12,
    bg = "transparent", res = 200)

par(mai=rep(0,4),xaxs='i',yaxs='i')


plot(st_geometry(CA),col='white',lwd=1,xpd=T)
plot(st_geometry(coast[coast$Layer=="Continent",]),col='grey20',border='black',lwd=0.1,add=T)
plot(st_geometry(A48),col=rgb(0.1,0.1,0.1,0.0),lwd=2,add=T,xpd=T)
plot(st_geometry(A58),col=rgb(0.1,0.1,0.1,0.5),lwd=1,add=T,xpd=T)
plot(st_geometry(A88),col=rgb(0.1,0.1,0.1,0.5),lwd=1,add=T,xpd=T)

plot(st_geometry(coast[coast$Surface=="Ice",]),col=NA,lwd=0.1,add=T)


text(-800000,3100000,"48",adj=c(0.5,0.5),cex=1.7,col='darkgreen',font=2)
text(3200000,1500000,"58",adj=c(0.5,0.5),cex=1.7,col='black',font=2)
text(-1240000,-2000000,"88",adj=c(0.5,0.5),cex=1.7,col='black',font=2)

dev.off()

png(filename = 'Data/Area58.png', width = 300, height = 300, units = "px", pointsize = 12,
    bg = "transparent", res = 200)

par(mai=rep(0,4),xaxs='i',yaxs='i')

plot(st_geometry(CA),col='white',lwd=1,xpd=T)
plot(st_geometry(coast[coast$Layer=="Continent",]),col='grey20',border='black',lwd=0.1,add=T)
plot(st_geometry(A48),col=rgb(0.1,0.1,0.1,0.5),lwd=1,add=T,xpd=T)
plot(st_geometry(A58),col=rgb(0.1,0.1,0.1,0.0),lwd=2,add=T,xpd=T)
plot(st_geometry(A88),col=rgb(0.1,0.1,0.1,0.5),lwd=1,add=T,xpd=T)

plot(st_geometry(coast[coast$Surface=="Ice",]),col=NA,lwd=0.1,add=T)


text(-800000,3100000,"48",adj=c(0.5,0.5),cex=1.7,col='black',font=2)
text(3200000,1500000,"58",adj=c(0.5,0.5),cex=1.7,col='darkgreen',font=2)
text(-1240000,-2000000,"88",adj=c(0.5,0.5),cex=1.7,col='black',font=2)

dev.off()

png(filename = 'Data/Area88.png', width = 300, height = 300, units = "px", pointsize = 12,
    bg = "transparent", res = 200)

par(mai=rep(0,4),xaxs='i',yaxs='i')

plot(st_geometry(CA),col='white',lwd=1,xpd=T)
plot(st_geometry(coast[coast$Layer=="Continent",]),col='grey20',border='black',lwd=0.1,add=T)
plot(st_geometry(A48),col=rgb(0.1,0.1,0.1,0.5),lwd=1,add=T,xpd=T)
plot(st_geometry(A58),col=rgb(0.1,0.1,0.1,0.5),lwd=1,add=T,xpd=T)
plot(st_geometry(A88),col=rgb(0.1,0.1,0.1,0.0),lwd=2,add=T,xpd=T)

plot(st_geometry(coast[coast$Surface=="Ice",]),col=NA,lwd=0.1,add=T)


text(-800000,3100000,"48",adj=c(0.5,0.5),cex=1.7,col='black',font=2)
text(3200000,1500000,"58",adj=c(0.5,0.5),cex=1.7,col='black',font=2)
text(-1240000,-2000000,"88",adj=c(0.5,0.5),cex=1.7,col='darkgreen',font=2)

dev.off()
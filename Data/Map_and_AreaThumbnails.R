#Script to generate RB map and Management Area (48, 58, 88) thumbnails
library(CCAMLRGIS)
library(terra)
library(dplyr)
ASDs=load_ASDs()
RBs=load_RBs()
SSRUs=load_SSRUs()
#Get Bathy
B=rast("I:/Science/Projects/GEBCO/2021/Processed/GEBCO2021_500.tif")



#Map
SSRUs=SSRUs[SSRUs$GAR_Short_Label=='882H',]
#Simplify dataframes to merge
RBs=RBs%>%select(name=GAR_Short_Label)
SSRUs=SSRUs%>%select(name=GAR_Short_Label)
#Bind
RBs=rbind(RBs,SSRUs)
RBs$ID=seq(1,nrow(RBs))
#Get Ref Areas
RA=st_read(dsn=path.expand(paste0(getwd(),'/Data')), layer="RefAreas", quiet = TRUE)

#Get contour of ASD to mask bathy outside
Cont=st_buffer(st_union(ASDs),dist=0)
#Extract outer boundary
pol=Cont[[1]][[1]]
pol=st_polygon(list(pol))
#Mask bathy
Bm=terra::mask(B, vect(pol))


#Get labels
Labs=read.csv("Data/LabelsRBs.csv")


#Separate RBs that require catch advice from those that don't
#List RBs that require catch advice
RBsCAdv=c("486_2","486_3","486_4","486_5",
          "5841_1","5841_2","5841_3","5841_4","5841_5","5841_6",
          "5842_1","5842_2",
          "882_1","882_2","882_3","882_4","882H",
          "883_1","883_2","883_3","883_4","883_5","883_6","883_7","883_8","883_9","883_10")

RBsY=RBs%>%filter(name%in%RBsCAdv)
RBs=RBs%>%filter(!name%in%RBsCAdv)

LabsY=Labs%>%filter(text%in%RBsCAdv)
Labs=Labs%>%filter(!text%in%RBsCAdv)

Dcuts=c(-15000,-1800,-600,0,10000)
Dcols=c('grey90','green','grey90','white')

png(filename = 'Data/Map_TrendAnalysis.png', width = 2000, height = 1900, units = "px", pointsize = 12,
    bg = "white", res = 200)
par(mai=c(0,0,0,0),xaxs='i',yaxs='i')

plot(Bm,breaks=Dcuts,col=Dcols,legend=FALSE,axes=FALSE,xlim=c(-4.2e6,4.8e6),ylim=c(-4.2e6,4.4e6),xpd=T)
plot(st_geometry(ASDs),add=T,lwd=2,border="grey20",xpd=T)

plot(st_geometry(Coast[Coast$ID=='All',]),add=T,col='grey70',border='grey50',lwd=0.5)
add_RefGrid(bb=st_bbox(ASDs),ResLat=10,ResLon=20,LabLon=0,offset = 1,lwd=1,fontsize = 0.9)

plot(st_geometry(RBs),add=T,lwd=2,border='red')
plot(st_geometry(RBsY),add=T,lwd=2,border='blue')
plot(st_geometry(RA),add=T,lwd=2,border=rgb(1,0.5,0,0.5),col=rgb(1,0.5,0,0.4))

# add_Cscale(height=60,maxVal=-1,offset = -1300,fontsize=1,width=15,lwd=2,
#            cuts = Depth_cuts2,
#            cols = Depth_cols2)

text(Labs$x,Labs$y,Labs$text,cex=1,col='darkred',font=2)
text(LabsY$x,LabsY$y,LabsY$text,cex=1,col='darkblue',font=2)
text(-290000,-2950000,"RSR Open",cex=1.3,col='black',font=2)
text(3900000,1050000,"HIMI",cex=1.3,col='black',font=2)


dev.off()






#Management area thumbnails
#Create Area Polygons
A48=ASDs[grep("48",ASDs$GAR_Short_Label),]
A48=st_union(A48)
A58=ASDs[grep("58",ASDs$GAR_Short_Label),]
A58=st_union(A58)
A88=ASDs[grep("88",ASDs$GAR_Short_Label),]
A88=st_union(A88)

#Temporary conversion to sp while xpd is fixed in sf
ASDs=as_Spatial(ASDs)
A48=as_Spatial(A48)
A58=as_Spatial(A58)
A88=as_Spatial(A88)

png(filename = 'Data/Area48.png', width = 300, height = 300, units = "px", pointsize = 12,
    bg = "transparent", res = 200)

par(mai=c(0.11,0.15,0.05,0.12),xaxs='i',yaxs='i')
plot(st_geometry(Coast[Coast$ID=='All',]),col='grey20',border='black',lwd=1)

plot(ASDs,col='white',lwd=0.75,xpd=T,add=T)
plot(A48,col=rgb(0.1,0.1,0.1,0.0),lwd=2,add=T,xpd=T)
plot(A58,col=rgb(0.1,0.1,0.1,0.5),lwd=1.25,add=T,xpd=T)
plot(A88,col=rgb(0.1,0.1,0.1,0.5),lwd=1.25,add=T,xpd=T)


text(-800000,3100000,"48",adj=c(0.5,0.5),cex=1.7,col='darkgreen',font=2)
text(3200000,1500000,"58",adj=c(0.5,0.5),cex=1.7,col='black',font=2)
text(-1240000,-2000000,"88",adj=c(0.5,0.5),cex=1.7,col='black',font=2)

dev.off()

png(filename = 'Data/Area58.png', width = 300, height = 300, units = "px", pointsize = 12,
    bg = "transparent", res = 200)

par(mai=c(0.11,0.15,0.05,0.12),xaxs='i',yaxs='i')
plot(st_geometry(Coast[Coast$ID=='All',]),col='grey20',border='black',lwd=1)

plot(ASDs,col='white',lwd=0.75,xpd=T,add=T)
plot(A48,col=rgb(0.1,0.1,0.1,0.5),lwd=1.25,add=T,xpd=T)
plot(A58,col=rgb(0.1,0.1,0.1,0.0),lwd=2,add=T,xpd=T)
plot(A88,col=rgb(0.1,0.1,0.1,0.5),lwd=1.25,add=T,xpd=T)


text(-800000,3100000,"48",adj=c(0.5,0.5),cex=1.7,col='black',font=2)
text(3200000,1500000,"58",adj=c(0.5,0.5),cex=1.7,col='darkgreen',font=2)
text(-1240000,-2000000,"88",adj=c(0.5,0.5),cex=1.7,col='black',font=2)

dev.off()

png(filename = 'Data/Area88.png', width = 300, height = 300, units = "px", pointsize = 12,
    bg = "transparent", res = 200)

par(mai=c(0.11,0.15,0.05,0.12),xaxs='i',yaxs='i')
plot(st_geometry(Coast[Coast$ID=='All',]),col='grey20',border='black',lwd=1)

plot(ASDs,col='white',lwd=0.75,xpd=T,add=T)
plot(A48,col=rgb(0.1,0.1,0.1,0.5),lwd=1.25,add=T,xpd=T)
plot(A58,col=rgb(0.1,0.1,0.1,0.5),lwd=1.25,add=T,xpd=T)
plot(A88,col=rgb(0.1,0.1,0.1,0.0),lwd=2,add=T,xpd=T)


text(-800000,3100000,"48",adj=c(0.5,0.5),cex=1.7,col='black',font=2)
text(3200000,1500000,"58",adj=c(0.5,0.5),cex=1.7,col='black',font=2)
text(-1240000,-2000000,"88",adj=c(0.5,0.5),cex=1.7,col='darkgreen',font=2)

dev.off()
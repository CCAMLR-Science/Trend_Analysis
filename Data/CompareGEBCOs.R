#Script to calculate differences between GEBCO versions
library(CCAMLRGIS)
library(terra)
library(dplyr)
ASDs=load_ASDs()
RBs=load_RBs()
SSRUs=load_SSRUs()
tmpdir="E:/Rtemp"
#Get Bathy
Bold=rast("I:/Science/Projects/GEBCO/2021/Processed/GEBCO2021_LL.tif")
Bnew=rast("I:/Science/Projects/GEBCO/2022/Processed/GEBCO2022_LL.tif")
Bdiff=Bnew-Bold #Negative means deeper

#Project extent (a single point is enough)
ExtP=CCAMLRGIS::project_data(Input=data.frame(Lat=as.numeric(ext(Bnew)$ymax),Lon=0),NamesIn = c("Lat","Lon"))$Y
#Extend a bit to avoid loss
ExtP=10000*ceiling(ExtP/10000)
#Prepare empty raster
Bp=rast(xmin=-ExtP,xmax=ExtP,ymin=-ExtP,ymax=ExtP)
crs(Bp)="epsg:6932"
res(Bp)=500
B=terra::project(Bdiff,Bp,tempdir=tmpdir)


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

Dcuts=c(-15000,-100,100,10000)
Dcols=c('red',"white",'blue')

png(filename = 'Data/Map_GEBCOComparison_2022.png', width = 2000, height = 1900, units = "px", pointsize = 12,
    bg = "white", res = 200)
par(mai=c(0,0,0,0),xaxs='i',yaxs='i')

plot(Bm,breaks=Dcuts,col=Dcols,legend=FALSE,axes=FALSE,xlim=c(-4.8e6,4.8e6),ylim=c(-4.5e6,4.5e6),xpd=T)
plot(st_geometry(ASDs),add=T,lwd=2,border="grey20",xpd=T)

plot(st_geometry(Coast[Coast$ID=='All',]),add=T,col='grey70',border='grey50',lwd=0.5)
add_RefGrid(bb=st_bbox(ASDs),ResLat=10,ResLon=20,LabLon=0,offset = 1,lwd=1,fontsize = 0.9)

plot(st_geometry(RBs),add=T,lwd=2,border='darkgreen')
plot(st_geometry(RA),add=T,lwd=2,border="orange")

text(Labs$x,Labs$y,Labs$text,cex=1,col='darkgreen',font=2)
text(-290000,-2950000,"RSR Open",cex=1.3,col='orange',font=2)
text(3900000,1050000,"HIMI",cex=1.3,col='orange',font=2)

legend('bottomright', legend=c('Deeper','Within 100m','Shallower'),
       pch=22, pt.bg=c('red','white','blue'),xpd=T,
       inset=c(-0.03,-0.04),cex=2,y.intersp=0.8)

dev.off()


bb=st_bbox(ASDs[ASDs$GAR_Short_Label=="883",])
bx=st_as_sfc(bb) #build spatial box to plot

CL=st_union(load_Coastline())
CLc=st_intersection(CL,bx)

# ASDc=st_union(ASDs)
ASDc=st_intersection(ASDs,bx)


Dc=terra::crop(Bm, vect(bx))
min(Dc)
Dp=add_col(var=c(-700,-100),cuts=10,cols=c("red","#FFDFDF"))
Ds=add_col(var=c(100,1720),cuts=10,cols=c("#DFDFFF","blue"))

Dcuts=c(Dp$cuts,Ds$cuts)
Dcols=c(Dp$cols,"white",Ds$cols)



png(filename = 'Data/Map_GEBCOComparison_883_2022.png', width = 2000, height = 2000, units = "px", pointsize = 12,
    bg = "white", res = 200)

plot(Dc,breaks=Dcuts,col=Dcols,legend=FALSE,axes=FALSE,xpd=T,xlim=c(bb[1],-1200000))
plot(st_geometry(ASDc),add=T,lwd=2,border="grey20",xpd=T)

plot(CLc,add=T,col='grey70',border='grey50',lwd=0.5)
add_RefGrid(bb,ResLat=5,ResLon=10,offset = c(10000,30000),lwd=1,fontsize = 0.9)

plot(st_geometry(RBs[grep('883',RBs$name),]),add=T,lwd=2,border='darkgreen')
plot(st_geometry(RA),add=T,lwd=2,border="orange")
plot(bx,border='black',add=T,lwd=1.2,xpd=T)
add_Cscale(height=60,offset = -275,fontsize=1,width=16,lwd=2,
           cuts = round(Dcuts),
           cols = Dcols,title = "Difference (m)")

indx=grep("88.3_",Labels$t)
text(Labels$x[indx],Labels$y[indx],Labels$t[indx],cex=0.7,col='darkgreen',font=2)

dev.off()


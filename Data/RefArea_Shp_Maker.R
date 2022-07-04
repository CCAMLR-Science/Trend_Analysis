#Script to build shapefiles for Reference areas (both projected and back-projected) #Needs update to match CCAMLRGIS v4
library(CCAMLRGIS)
library(sp)
library(rgeos)
library(rgdal)

#Load CCAMLR Areas
ASDs=load_ASDs()
EEZs=load_EEZs()
MAs=load_MAs()
MPAs=load_MPAs()

#1. RefArea HIMI (HIMI EEZ within 58.5.2)
#Take HIMI EEZ
HIMI=EEZs[EEZs$GAR_Short_Label=="HIMI",]
#Take Division 58.5.2
D5852=ASDs[ASDs$GAR_Short_Label=="5852",]
#Remove area outside 58.5.2 from HIMI EEZ
HIMI=gIntersection(HIMI,D5852)
#Add name to polygon
HIMI=SpatialPolygonsDataFrame(HIMI,data.frame(name='HIMI'))


#2. RefArea RSR_open (S70 + N70 + SRZ)
#Merge SRZ, N70 and S70
RSR_open=gUnion(MPAs[MPAs$GAR_Short_Label=='SRZ',],MAs[MAs$GAR_Short_Label%in%c('S70','N70'),])
RSR_open=SpatialPolygonsDataFrame(RSR_open,data.frame(name='RSR_open'))


#3. Merge polygons
RefAreas=rbind(RSR_open,HIMI)

#4. Export
writeOGR(obj=RefAreas,dsn="Data",layer="RefAreas",driver="ESRI Shapefile")



#Project back to Lat/Lon after cutting RSR_open at the 180 meridian
#To avoid back-projection error, the cut is made at a fraction (fr) of degrees away from the 180 meridian
fr=0.000000000001

#Cut RSR_open into East and West parts
WestCut=create_Polys(Input=data.frame(name=c(1,1,1,1),
                                      Lat=c(-85,-85,-40,-40),
                                      Lon=c(180+fr,140,140,180+fr)))
RSR_open_East=gDifference(RefAreas[RefAreas$name=="RSR_open",],WestCut,byid=T)

EastCut=create_Polys(Input=data.frame(name=c(1,1,1,1),
                                      Lat=c(-85,-85,-40,-40),
                                      Lon=c(-140,-180-fr,-180-fr,-140)))
RSR_open_West=gDifference(RefAreas[RefAreas$name=="RSR_open",],EastCut,byid=T)

plot(RefAreas[RefAreas$name=="RSR_open",],border='red',lwd=3)
plot(RSR_open_West,col='green',add=T)
plot(RSR_open_East,col='blue',add=T)

#Add names to polygons
RSR_open_West=SpatialPolygonsDataFrame(RSR_open_West,data.frame(name='RSR_open_West'),match.ID = F)
RSR_open_East=SpatialPolygonsDataFrame(RSR_open_East,data.frame(name='RSR_open_East'),match.ID = F)

#Merge HIMI to RSR_open pieces
RefAreas_tmp=RefAreas[RefAreas$name=="HIMI",]
RefAreas_tmp=rbind(RefAreas_tmp,RSR_open_East)
RefAreas_tmp=rbind(RefAreas_tmp,RSR_open_West)

plot(RefAreas_tmp,col=rainbow(3))

#Back-project RefAreas to Lat/Lon
RefAreasLL=spTransform(RefAreas_tmp,CRS("+init=epsg:4326"))

plot(RefAreasLL,col=rainbow(3))

#Export
writeOGR(obj=RefAreasLL,dsn="Data",layer="RefAreasLL",driver="ESRI Shapefile")


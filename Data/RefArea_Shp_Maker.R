#Script to build shapefiles for Reference areas (both projected and back-projected)
library(CCAMLRGIS)
library(dplyr)

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
HIMI=suppressWarnings( st_intersection(HIMI,D5852) )
#Add name to polygon
HIMI$name="HIMI"
HIMI=HIMI%>%select(name)


#2. RefArea RSR_open (S70 + N70 + SRZ)
#Merge SRZ, N70 and S70
RSR_open=suppressWarnings(st_union(MPAs[MPAs$GAR_Short_Label=='SRZ',],MAs[MAs$GAR_Short_Label=='S70',]))
RSR_open=suppressWarnings(st_union(RSR_open,MAs[MAs$GAR_Short_Label=='N70',]))

#Add name to polygon
RSR_open$name="RSR_open"
RSR_open=RSR_open%>%select(name)

#3. Merge polygons
RefAreas=rbind(RSR_open,HIMI)

#4. Export
st_write(RefAreas, "Data/RefAreas.shp",append = F,quiet = T)



#Project back to Lat/Lon after cutting RSR_open at the 180 meridian
#To avoid back-projection error, the cut is made at a fraction (fr) of degrees away from the 180 meridian
fr=0.000000000001

#Cut RSR_open into East and West parts
WestCut=create_Polys(Input=data.frame(name=c(1,1,1,1),
                                      Lat=c(-85,-85,-40,-40),
                                      Lon=c(180+fr,140,140,180+fr)))
RSR_open_East=suppressWarnings( st_difference(RefAreas[RefAreas$name=="RSR_open",],WestCut) )

EastCut=create_Polys(Input=data.frame(name=c(1,1,1,1),
                                      Lat=c(-85,-85,-40,-40),
                                      Lon=c(-140,-180-fr,-180-fr,-140)))
RSR_open_West=suppressWarnings( st_difference(RefAreas[RefAreas$name=="RSR_open",],EastCut))

plot(st_geometry(RefAreas[RefAreas$name=="RSR_open",]),border='red',lwd=3)
plot(st_geometry(RSR_open_West),col='green',add=T)
plot(st_geometry(RSR_open_East),col='blue',add=T)

#Add names to polygons
RSR_open_West$name='RSR_open_West'
RSR_open_East$name='RSR_open_East'
RSR_open_West=RSR_open_West%>%select(name)
RSR_open_East=RSR_open_East%>%select(name)

#Merge HIMI to RSR_open pieces
RefAreas_tmp=RefAreas[RefAreas$name=="HIMI",]
RefAreas_tmp=rbind(RefAreas_tmp,RSR_open_East)
RefAreas_tmp=rbind(RefAreas_tmp,RSR_open_West)

plot(st_geometry(RefAreas_tmp),col=rainbow(3))

#Back-project RefAreas to Lat/Lon
RefAreasLL=st_transform(RefAreas_tmp,crs=4326)

plot(st_geometry(RefAreasLL),col=rainbow(3))

#Export
st_write(RefAreasLL, "Data/RefAreasLL.shp",append = F,quiet = T)


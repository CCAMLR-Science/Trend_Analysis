#Script to compute fishable areas #Needs update to match CCAMLRGIS v4
library(CCAMLRGIS)
library(rgdal)
library(terra) #Package used to handle the GEBCO data

#Load research blocks
RBs=load_RBs()
#Load SSRUs (to get 882H)
SSRUs=load_SSRUs()
#simplify RBs and SSRUs and merge
RBs@data=data.frame(name=as.character(RBs$GAR_Short_Label))
SSRUs@data=data.frame(name=as.character(SSRUs$GAR_Short_Label))
SSRUs=SSRUs[SSRUs$name=="882H",]
Polys=rbind(RBs,SSRUs)
#back-Project Polys to Latitudes/Longitudes
PolysLL=sp::spTransform(Polys,CRS("+init=epsg:4326"))


#Load reference areas (created in RefArea_Shp_Maker.R)
RefAreas=readOGR(dsn=path.expand(paste0(getwd(),'/Data')), layer="RefAreasLL",verbose=F)
#Add RefAreas to PolysLL
PolysLL=rbind(PolysLL,RefAreas)


#At this point, 'PolysLL' contains all areas for which fishable areas need to be computed


#Get the unprojected GEBCO data
B=rast("I:/Science/Projects/GEBCO/2021/Processed/GEBCO2021_LL.tif")
#Convert Polys to Spatvector for the terra package
PolysLLsv=vect(PolysLL)
#Loop over polygons that are inside PolysLLsv
RawAr=data.frame(Poly=character(),Area=numeric()) #Prepare empty output
for (i in seq(1,length(PolysLLsv))){
  #Take one polygon
  pol=PolysLLsv[i,]
  #Get its name
  pname=PolysLLsv$name[i]
  #Take bathymetry data that matches the extent of the polygon
  Btmp=crop(B,ext(pol))
  #Turn GEBCO cells that are not inside the polygon into NAs
  Btmp=terra::mask(Btmp,pol)
  #Turn cells outside the fishable depth into NAs
  Btmp = classify(Btmp, cbind(-100000, -1800, NA), right=TRUE)
  Btmp = classify(Btmp, cbind(-600, 100000, NA), right=FALSE)
  #Compute the area covered by cells that are not NA
  Ar=round(expanse(Btmp, unit="km"),2)
  #Store result
  RawAr=rbind(RawAr,data.frame(Poly=pname,Area=Ar))
}
#Merge results for RSR_open (was split at the antimeridian)
RawAr=rbind(RawAr,data.frame(
  Poly="RSR_open",
  Area=sum(RawAr$Area[RawAr$Poly%in%c("RSR_open_East","RSR_open_West")])
))
#Remove RSR_open_East and RSR_open_West
RawAr=RawAr[-which(RawAr$Poly%in%c("RSR_open_East","RSR_open_West")),]
colnames(RawAr)=c("Polys","Fishable_area")
#Export
write.csv(RawAr,'Data/FishableArea2021.csv',row.names = F)

#Legend settings

#Map legend
LegOpt=list( 
  Pos = "center",
  BoxW= 50,
  BoxH= 40,
  Boxexp = c(4,0,0,0),
  PosX=-4,
  PosY=-6
)
bb=st_bbox(Coast)

LegOptZ=list( 
  Pos = "bottomleft",
  BoxW= 120,
  BoxH= 90,
  Boxexp = c(4,0,0,0),
  PosX=-130,
  PosY=-10
)
bbZ=st_bbox(RBs[RBs$ID%in%c("883_3","883_4","883_11","883_12"),])

Rectangle1=list(
  Text="Research Blocks", 
  Shape="rectangle",
  ShpFill="grey50",
  ShpBord=NA,
  Shplwd=0.5,
  fontsize=1,
  STSpace=3,
  RectW=8,
  RectH=5
)

Rectangle2=list(
  Text="5 km buffer", 
  Shape="rectangle",
  ShpFill=NA,
  ShpBord="blue",
  Shplwd=1,
  fontsize=1,
  STSpace=3,
  RectW=8,
  RectH=5
)

Rectangle3=list(
  Text="1 FSR buffer", 
  Shape="rectangle",
  ShpFill=NA,
  ShpBord="red",
  Shplwd=1,
  fontsize=1,
  STSpace=3,
  RectW=8,
  RectH=5
)

Rectangle4=list(
  Text="2 FSR buffer", 
  Shape="rectangle",
  ShpFill=NA,
  ShpBord="green",
  Shplwd=1,
  fontsize=1,
  STSpace=3,
  RectW=8,
  RectH=5
)

Items=list(Rectangle1,Rectangle2,Rectangle3,Rectangle4)

#Script to build Research Blocks buffers
library(CCAMLRGIS)
library(dplyr)

#Load vertices
Vs=read.csv("Verts_481.csv")

#Base polygons
Ps=create_Polys(Vs)

par(mai=rep(0,4),xaxs="i",yaxs="i")
plot(st_geometry(Ps))


#Load vertices
Vs2=read.csv("481clipper.csv")

#Base polygons
Ps2=create_Polys(Vs2)

par(mai=rep(0,4),xaxs="i",yaxs="i")
plot(st_geometry(Ps2))
plot(st_geometry(Ps),add=T)
plot(st_geometry(Ps2),add=T,border="blue")

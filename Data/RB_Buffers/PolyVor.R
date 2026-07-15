#Function to generate Voronoi polygons, from input polygons
PolyVor <- function(polys){
  pts = st_coordinates(polys)
  pts = pts[duplicated(pts[,"L2"]),]
  pts = st_as_sf(data.frame(pts), coords=1:2)
  vpts = do.call(c, st_geometry(pts))
  pols = st_collection_extract(st_voronoi(vpts))
  ## which voronoi polygon is each pt in?
  pi = unlist(st_intersects(pts, pols))
  ## get this in the right order
  polsL = st_as_sf(data.frame(geometry=pols[pi,]))
  ## this is the polygon that each point came from
  polsL$polygon = pts$L2
  ## merge
  v = aggregate(polsL, by=list(polsL$polygon), FUN=mean)
  v$Poly=polys$ID[v$polygon]
  return(v)
}

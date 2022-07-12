#Script to add uncertainty on plots as areas
#Input must be a dataframe with columns in order x, ymin, ymax
PlotVar=function(Input,Col){
  colnames(Input)=c("x","ymin","ymax")
  Input=dplyr::arrange(Input,x)
  if(nrow(Input)==1){
    wdth=0.1
    x=c(Input$x-wdth,Input$x+wdth,Input$x+wdth,Input$x-wdth)
    y=c(Input$ymax,Input$ymax,Input$ymin,Input$ymin)
    polygon(x,y,col=Col,border=NA)  
  }else{
    x=c(Input$x,rev(Input$x))
    y=c(Input$ymax,rev(Input$ymin))
    polygon(x,y,col=Col,border=NA)  
  }
}
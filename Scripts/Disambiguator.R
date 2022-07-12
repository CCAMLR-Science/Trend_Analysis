# Code to merge ambiguous links by fields of interest
# 'Links' is the dataframe of linked tags
# 'RecF' are the recaptures column names of the variables that must be equal within Recaptures to enable merging ambiguous links
# 'RelF' are the releases column names of the variables that must be equal within Releases to enable merging ambiguous links
# RecF and RelF must be given in the same order 
# 'Relid' record identifier of releases
# 'Recid' record identifier of recaptures
# 'append' should the merged links be added to the unambiguous ones at the end?

Disambiguator=function(Links,RecF,RelF,Relid="obs_haul_tag_release_id",Recid="obs_haul_tag_recapture_id",append="Y"){

  if(all(c(RelF,RecF)%in%colnames(Links))==F){stop("Fields not matching colnames(Links)")}
  
  #List Other Fields (OF) these field names need to be in Links
  OF=c('taglink_ambiguity_code',
       'taglink_score',
       'taglink_mismatch_taxon',
       'taglink_mismatch_serial',
       'taglink_mismatch_tag_colour',
       'taglink_mismatch_tag_type',
       'taglink_mismatch_tag_inscription',
       'taglink_mismatch_yn',
       'quarantined_release_yn',
       'quarantined_recapture_yn',
       'date_link_update'
       )

  if(all(OF%in%colnames(Links))==F){stop("Other Fields (OF) not matching colnames(Links)")}
  
  if(all(c(Relid,Recid)%in%colnames(Links))==F){stop("ID Fields not matching colnames(Links)")}
  
  
  Ls=Links[Links$taglink_ambiguity_code%in%c(2,3,4),] #Get ambiguous links
  Out=Ls[0,] #Prepare storage for merged links
  Recs=unique(Ls[[Recid]]) #Get Recapture IDs
  
  message(paste0(length(Recs)," recaptures are involved in ",dim(Ls)[1]," ambiguous links"))
  
  for(rc in Recs){ #Loop over recaptures, find all recaptures and releases involved, merge where possible (test with rc=8233)
    #Find all releases linked to that recapture
    rls=Ls[[Relid]][Ls[[Recid]]==rc]
    rls=unique(rls)
    #Find all recaptures linked to these releases
    rcs=Ls[[Recid]][Ls[[Relid]]%in%rls]
    rcs=unique(rcs)
    #Get data of interest
    irel=which(Ls[[Relid]]%in%rls)
    RelD=Ls[irel,RelF] #Release data of interest
    irec=which(Ls[[Recid]]%in%rcs)
    RecD=Ls[irec,RecF] #Release data of interest
    #Get rows involved
    ilink=unique(c(irel,irec)) #It might not be necessary to define separate irec and irel (they might always be the same thing) but it sure works
    
    if(dim(unique(RelD))[1]==1 & dim(unique(RecD))[1]==1 & all(is.na(RelD)==F) & all(is.na(RecD)==F)){ #All equal, links can be merged
    tmp=Out[0,] #empty storage
    tmp[1,RecF]=RecD[1,] #Take common recapture data
    tmp[1,RelF]=RelD[1,] #Take common release data
    
    #For some fields, take the worst case of ambiguities
    tmp$taglink_score=min(Ls$taglink_score[ilink])
    tmp$taglink_mismatch_taxon=max(Ls$taglink_mismatch_taxon[ilink])
    tmp$taglink_mismatch_serial=max(Ls$taglink_mismatch_serial[ilink])
    tmp$taglink_mismatch_tag_colour=max(Ls$taglink_mismatch_tag_colour[ilink])
    tmp$taglink_mismatch_tag_type=max(Ls$taglink_mismatch_tag_type[ilink])
    tmp$taglink_mismatch_tag_inscription=max(Ls$taglink_mismatch_tag_inscription[ilink])
    tmp$taglink_mismatch_yn=ifelse(any(Ls$taglink_mismatch_yn[ilink]=='Y'),'Y','N')
    tmp$taglink_ambiguity_code=max(Ls$taglink_ambiguity_code[ilink])
    tmp$quarantined_release_yn=ifelse(any(Ls$quarantined_release_yn[ilink]=='Y'),'Y','N')
    tmp$quarantined_recapture_yn=ifelse(any(Ls$quarantined_recapture_yn[ilink]=='Y'),'Y','N')
    
    #For some fields, take the unique
    tmp$date_link_update=unique(Ls$date_link_update[ilink])
    
    #For other fields, just concatenate
    FieldsC=colnames(tmp)[!colnames(tmp)%in%c(RecF,RelF,OF)] #Fields to concatenate
    DatC=Ls[ilink,FieldsC] #Data to concatenate
    
    for(f in names(DatC)){ #Convert dates to text to enable concatenation
      if( any(class(DatC[[f]])%in%c("POSIXct","POSIXt")) ) {
        DatC[[f]] <-  as.character(DatC[[f]])}
    } 
    #Concatenate uniques
    for (f in FieldsC){
      if(length(unique(DatC[[f]]))==1){
      tmp[[f]] = unique(DatC[[f]])
      }else{
      tmp[[f]] = paste0(unique(DatC[[f]]), collapse = ', ')
      }
    }
    
    Out=rbind(Out,tmp)
    rm(tmp)
    }
    #Remove checked records from dataframe of ambiguous links
    if(length(ilink)>0){
    Ls=Ls[-ilink,]
    }
    
  }
  message(paste0(dim(Out)[1]," links were created by merging ambiguous links based on ",paste(c(RecF,RelF),collapse=" and ")))
  
  if(append=="N"){
    return(Out)
  }else{
    
    for(f in names(Links)){ #Convert dates to text to enable append
      if(any(class(Links[[f]])%in%c("POSIXct","POSIXt"))) {
        Links[[f]] <-  as.character(Links[[f]])}
    } 
    
    Out=rbind(Links[Links$taglink_ambiguity_code%in%c(1,5),],Out)
    return(Out)
  }
}

# Example:
# Links=ccamlrtools::queryccamlr('select * from tag_link', server = "rossdev", asis = F)
# RecF=c("season_ccamlr_recapture","asd_code_recapture")
# RelF=c("season_ccamlr_release","asd_code_release")
# Amb=Disambiguator(Links=Links,RecF=RecF,RelF=RelF,append='N')

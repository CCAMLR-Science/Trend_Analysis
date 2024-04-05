#Script to extract/inspect tagging data

T_Rels=read.csv(paste0("Output_All_Releases_",Time,".csv"))
T_Recs=read.csv(paste0("Output_All_Recaptures_",Time,".csv"))
T_Catch=read.csv(paste0("Output_All_Catch_",Time,".csv"))
T_Links=read.csv(paste0("Output_Recaptures_Linked_",Time,".csv"))
# T_Links=T_Links%>%filter(taglink_mismatch_yn=="N")
#Load filtered links
Ls=read.csv(paste0("Output_Recaptures_Linked_Filtered_",Time,".csv"),check.names = F)
#Load links used in the TA
Chaps=read.csv(paste0("Output_Chapman_",Time,".csv"))
Chaps=Chaps[,c("RB","Season","N_recaptures")]
#Load results table to extract ASD/Rbs - use as tables first columns
TabDat=read.csv(paste0("Trends_",Est_Season,"_and_CLs_",Time,".csv"),check.names = F)
TabDat=TabDat[,1:4]
TabDat=TabDat[-nrow(TabDat),]


#Get counts
Tn_Rels=T_Rels%>%group_by(RESEARCH_BLOCK_CODE,season_ccamlr)%>%tally()%>%as.data.frame()
Tn_Recs=T_Recs%>%group_by(RESEARCH_BLOCK_CODE,season_ccamlr)%>%tally()%>%as.data.frame()
Tn_RecsQrel=T_Recs%>%filter(Qrel==1)%>%group_by(RESEARCH_BLOCK_CODE,season_ccamlr)%>%tally()%>%as.data.frame()
Tn_Links=T_Links%>%
  group_by(RESEARCH_BLOCK_CODE_RELEASE,RESEARCH_BLOCK_CODE_RECAPTURE,season_ccamlr_release,season_ccamlr_recapture)%>%
  dplyr::summarise(n=length(unique(obs_haul_tag_recapture_id)),
                   nmm=length(unique(obs_haul_tag_recapture_id[taglink_mismatch_yn=="Y"])),.groups = "drop")%>%
  as.data.frame()
#See where/when fishing happened
Tn_Catch=T_Catch%>%group_by(RESEARCH_BLOCK_CODE_START_SET,RESEARCH_BLOCK_CODE_END_SET,season_ccamlr)%>%tally()%>%as.data.frame()
Tn_Catch=data.frame(RB=c(Tn_Catch$RESEARCH_BLOCK_CODE_START_SET,Tn_Catch$RESEARCH_BLOCK_CODE_END_SET),
                    S=c(Tn_Catch$season_ccamlr,Tn_Catch$season_ccamlr))
Tn_Catch=Tn_Catch%>%distinct()%>%filter(is.na(RB)==F)%>%as.data.frame()
#Count within RB links (WRB) and migrants (M)
colnames(Tn_Links)=c("RBrel","RBrec","Srel","Srec","n","nmm")
Tn_Links=Tn_Links[-which(is.na(Tn_Links$RBrel)==T & is.na(Tn_Links$RBrec)==T),]
Tn_Links$RBrel[is.na(Tn_Links$RBrel)]="Out"
Tn_Links$RBrec[is.na(Tn_Links$RBrec)]="Out"
Tn_Links_WRB=Tn_Links%>%filter(RBrel==RBrec)
Tn_Links_M=Tn_Links%>%filter(RBrel!=RBrec)
if(nrow(Tn_Links)!=(nrow(Tn_Links_WRB)+nrow(Tn_Links_M))){stop("Ouch1 in TaggingData script")}

TotRelRec=Tn_Catch
colnames(Tn_Rels)=c("RB","S","Nrel")
colnames(Tn_Recs)=c("RB","S","Nrec")
colnames(Tn_RecsQrel)=c("RB","S","NrecQrel")
TotRelRec=left_join(TotRelRec,Tn_Rels,by=c("RB","S"))
TotRelRec=left_join(TotRelRec,Tn_Recs,by=c("RB","S"))
TotRelRec=left_join(TotRelRec,Tn_RecsQrel,by=c("RB","S"))


#Count migrants
TotRelRec$Emi=0
TotRelRec$Immi=0
for(i in seq(1,nrow(Tn_Links_M))){
  RBrel_m=Tn_Links_M$RBrel[i]
  RBrec_m=Tn_Links_M$RBrec[i]
  Srel_m=Tn_Links_M$Srel[i]
  Srec_m=Tn_Links_M$Srec[i]
  n_m=Tn_Links_M$n[i]
  
  #Emigrants
  iEmi=which(TotRelRec$RB==RBrel_m & TotRelRec$S==Srel_m)
  if(length(iEmi)>0){
    TotRelRec$Emi[iEmi]=TotRelRec$Emi[iEmi]+n_m
  }
  #Immigrants
  iImmi=which(TotRelRec$RB==RBrec_m & TotRelRec$S==Srec_m)
  if(length(iImmi)>0){
    TotRelRec$Immi[iImmi]=TotRelRec$Immi[iImmi]+n_m
  }
  
  rm(RBrel_m,RBrec_m,Srel_m,Srec_m,n_m)
}
TotRelRec$Emi[TotRelRec$Emi==0]=NA
TotRelRec$Immi[TotRelRec$Immi==0]=NA

#Get recaptures that are not linked
RecsNoL=T_Recs%>%filter(obs_haul_tag_recapture_id%in%unique(T_Links$obs_haul_tag_recapture_id)==F & Qrel==0)
write.csv(RecsNoL%>%filter(season_ccamlr>=Est_Season-4 & is.na(RESEARCH_BLOCK_CODE)==F),paste0("Output_Recaptures_Not_Linked_",Time,".csv"))
ttt=RecsNoL%>%group_by(RESEARCH_BLOCK_CODE)%>%tally
message("Recaptures not linked:")
for(i in seq(1,nrow(ttt))){message(paste(ttt[i,],collapse = ": "))}
ttt=RecsNoL%>%filter(season_ccamlr>=Est_Season-4 & is.na(RESEARCH_BLOCK_CODE)==F)%>%group_by(RESEARCH_BLOCK_CODE)%>%tally
message("Recaptures not linked of interest:")
for(i in seq(1,nrow(ttt))){message(paste(ttt[i,],collapse = ": "))}

RecsNoL=RecsNoL%>%group_by(RESEARCH_BLOCK_CODE,season_ccamlr)%>%tally()%>%as.data.frame()
colnames(RecsNoL)=c("RB","S","NoL")

#df of releases/recaptures
TotRelRec=left_join(TotRelRec,RecsNoL,by=c("RB","S"))

TotRelRec$Nrel[is.na(TotRelRec$Nrel)]=0
TotRelRec$Nrec[is.na(TotRelRec$Nrec)]=0

TRRtxt=TotRelRec
indx=which(is.na(TRRtxt$NrecQrel)==F)
TRRtxt$NrecQrel[indx]=paste0("QR:",TRRtxt$NrecQrel[indx])
indx=which(is.na(TRRtxt$Emi)==F)
TRRtxt$Emi[indx]=paste0("Em:",TRRtxt$Emi[indx])
indx=which(is.na(TRRtxt$Immi)==F)
TRRtxt$Immi[indx]=paste0("Im:",TRRtxt$Immi[indx])
indx=which(is.na(TRRtxt$NoL)==F)
TRRtxt$NoL[indx]=paste0("NL:",TRRtxt$NoL[indx])
TRRtxt[is.na(TRRtxt)]=""

#build collapsed numbers
TotRelRec$NrelM=NA
TotRelRec$NrecM=NA
for (i in seq(1,nrow(TotRelRec))){
  if(TRRtxt$Emi[i]==""){
    TotRelRec$NrelM[i]=TotRelRec$Nrel[i]
  }else{
    TotRelRec$NrelM[i]=paste0(TotRelRec$Nrel[i]," (",TRRtxt$Emi[i],")")
  }
  
  if(TRRtxt$Immi[i]=="" & TRRtxt$NrecQrel[i]=="" & TRRtxt$NoL[i]==""){
    TotRelRec$NrecM[i]=TotRelRec$Nrec[i]
  }else{
    tmp=TRRtxt[i,c("Immi","NrecQrel","NoL")]
    tmp=tmp[tmp!=""]
    if(length(tmp)>1){
      tmp=paste0(tmp, collapse = "|")
    }
    TotRelRec$NrecM[i]=paste0(TotRelRec$Nrec[i]," (",tmp,")")
  }
}

rm(TRRtxt)

#Convert NAs to zeros before checking numbers
TotRelRec[is.na(TotRelRec)]=0

#df of recaptures linked within RB
LinkCounts=Tn_Links_WRB

#Check sums and report
#1.Compare (recaptures linked + linked to quarantined releases + not linked +/- migrants) to all recaptures
for (i in seq(1,nrow(TotRelRec))){
  rb=TotRelRec$RB[i]
  S=TotRelRec$S[i]
  #Number of recaptures
  Nrec=TotRelRec$Nrec[i]
  #Number of recaptures not linked
  N_NoL=TotRelRec$NoL[i]
  #Number of recaptures linked to quarantined releases
  NrecQrel=TotRelRec$NrecQrel[i]
  #Number of immigrants (recaptured in this rb but came from elsewhere)
  N_Immi=TotRelRec$Immi[i]
  
  #Number of recaptures linked
  N_L=sum(LinkCounts$n[
    LinkCounts$RBrel==rb &
      LinkCounts$Srec==S
  ])
  
  if(Nrec!=(N_NoL+N_L+N_Immi+NrecQrel)){
    message(paste("NRec imbalance:",rb,S))
  }
}
#2. Compare filtered links to a) recaptures linked and b) those used in the TA as shown on plots
#Get counts of filtered links
N_Ls=Ls%>%
  group_by(RESEARCH_BLOCK_CODE_RELEASE,RESEARCH_BLOCK_CODE_RECAPTURE,season_ccamlr_release,season_ccamlr_recapture)%>%
  dplyr::summarise(n=length(unique(obs_haul_tag_recapture_id)),.groups = "drop")%>%
  as.data.frame()
colnames(N_Ls)=c("RBrel","RBrec","Srel","Srec","nf")
#Add count of unfiltered links
N_Ls=left_join(N_Ls,LinkCounts,by = c("RBrel", "RBrec", "Srel", "Srec"))
#Note discrepancy
indx=which(N_Ls$nf!=(N_Ls$n-N_Ls$nmm))
if(length(indx)>0){
  message(paste0(length(indx), " discrepancies between filtered/unfiltered links in N_Ls at rows: ", indx))
  View(N_Ls)
  #Check:
  #recaptures linked
  View(T_Links%>%filter(RESEARCH_BLOCK_CODE_RELEASE==N_Ls$RBrel[indx],
                        RESEARCH_BLOCK_CODE_RECAPTURE==N_Ls$RBrec[indx],
                        season_ccamlr_release==N_Ls$Srel[indx],
                        season_ccamlr_recapture==N_Ls$Srec[indx])
       %>%dplyr::select(tag_code_1_release,tag_code_2_release,tag_code_1_recapture,tag_code_2_recapture))
  #filtered links
  View(Ls%>%filter(RESEARCH_BLOCK_CODE_RELEASE==N_Ls$RBrel[indx],
                   RESEARCH_BLOCK_CODE_RECAPTURE==N_Ls$RBrec[indx],
                   season_ccamlr_release==N_Ls$Srel[indx],
                   season_ccamlr_recapture==N_Ls$Srec[indx])
       %>%dplyr::select(tag_code_1_release,tag_code_2_release,tag_code_1_recapture,tag_code_2_recapture))
  
  
  
}else{
  message("No discrepancies between filtered/unfiltered links")
}
#Now compare to recaptures used in the TA
N_Ls=N_Ls%>%group_by(RBrec,Srec)%>%dplyr::summarise(Nf=sum(nf),.groups = "drop")
N_Ls=left_join(N_Ls,Chaps,by=c("RBrec"="RB","Srec"="Season"))

indx=which(N_Ls$Nf!=N_Ls$N_recaptures)
if(length(indx)>0){
  message(paste0(length(indx), " discrepancies between filtered/TA links in N_Ls at rows: ", indx))
}else{
  message("No discrepancies between filtered/TA links")
}

#Get migrants details
Mig=NULL
for (i in seq(1,nrow(TotRelRec))){
  #Emigrants
  if(TotRelRec$Emi[i]!=0){
    M_S=TotRelRec$S[i]
    M_rb=TotRelRec$RB[i]
    M_links=T_Links%>%filter(RESEARCH_BLOCK_CODE_RELEASE==M_rb &
                               (RESEARCH_BLOCK_CODE_RECAPTURE!=M_rb | is.na(RESEARCH_BLOCK_CODE_RECAPTURE)) &
                               season_ccamlr_release==M_S)%>%select(
                                 taglink_tagcount,
                                 taglink_score,
                                 taglink_mismatch_yn,
                                 taglink_ambiguity_code,
                                 season_ccamlr_release,
                                 ASD_CODE_RELEASE,
                                 RESEARCH_BLOCK_CODE_RELEASE,
                                 season_ccamlr_recapture,
                                 ASD_CODE_RECAPTURE,
                                 RESEARCH_BLOCK_CODE_RECAPTURE
                               )
    if(nrow(M_links)!=TotRelRec$Emi[i]){stop("Emigrants unbalance")}
    Mig=rbind(Mig,cbind(M_rb,M_S,M_links))
  }
  
  #Immigrants
  if(TotRelRec$Immi[i]!=0){
    M_S=TotRelRec$S[i]
    M_rb=TotRelRec$RB[i]
    M_links=T_Links%>%filter(RESEARCH_BLOCK_CODE_RECAPTURE==M_rb &
                               (RESEARCH_BLOCK_CODE_RELEASE!=M_rb | is.na(RESEARCH_BLOCK_CODE_RELEASE)) &
                               season_ccamlr_recapture==M_S)%>%select(
                                 taglink_tagcount,
                                 taglink_score,
                                 taglink_mismatch_yn,
                                 taglink_ambiguity_code,
                                 season_ccamlr_release,
                                 ASD_CODE_RELEASE,
                                 RESEARCH_BLOCK_CODE_RELEASE,
                                 season_ccamlr_recapture,
                                 ASD_CODE_RECAPTURE,
                                 RESEARCH_BLOCK_CODE_RECAPTURE
                               )
    if(nrow(M_links)!=TotRelRec$Immi[i]){stop("Immigrants unbalance")}
    Mig=rbind(Mig,cbind(M_rb,M_S,M_links))
  }
}





TotRelRec=TotRelRec%>%dplyr::select(RB,S,NrelM,NrecM,NrecQrel,NoL)

#Now build per-Area tagging tables

#Load total RelRecs
Trr=TotRelRec
#Load counts of links
ULs=LinkCounts

#Loop over Rbs
RelRec=NULL
Ss=seq(Est_Season-4,Est_Season)
for(rb in TabDat$`Research Block`){
  
  tmprr=Trr%>%filter(RB==rb)
  
  if(nrow(tmprr)>0){
    #Mark duplicates with a star - if there are some, fixing is needed
    if(length(unique(tmprr$S))!=nrow(tmprr)){
      for(ss in unique(tmprr$S)){
        if(length(which(tmprr$S==ss))>1){
          dups=which(tmprr$S==ss)
          tmprr=tmprr[-dups[2:length(dups)],]
          tmprr$NrelM[dups[1]]=paste0(tmprr$NrelM[dups[1]]," *")
          message("Duplicates found in Tagging table - fix this!")
        }
      }
    }
    
    #Prepare empty df
    SRels=seq(min(tmprr$S),max(tmprr$S))
    RelRec_rb=data.frame(A=TabDat$Area[TabDat$`Research Block`==rb],
                         ASD=TabDat$`Subarea or Division`[TabDat$`Research Block`==rb],
                         RB=rb,
                         S=SRels
    )
    
    #add RelRecs
    RelRec_rb=left_join(RelRec_rb,tmprr,by=c("RB","S"))
    #add rec seasons
    Rcols=as.data.frame(matrix(NA,nrow=length(SRels),ncol=10)) #The first five are for numbers, the next five are to mark those that are used by the TA 
    Rcols[,(6:10)]="F"
    colnames(Rcols)=c(Ss,paste0(Ss,"Used"))
    RelRec_rb=cbind(RelRec_rb,Rcols)
    #Mark no fishing (per row)
    nof=which(is.na(RelRec_rb$NrelM)==T)
    if(length(nof)>0){
      RelRec_rb[nof,seq(which(colnames(RelRec_rb)=="NrelM"),which(colnames(RelRec_rb)=="NoL"))]="-"
    }
    #Mark no fishing (per cell)
    for(s_rel in SRels){
      for(s_rec in Ss){
        if(s_rec>=s_rel){
          if(RelRec_rb$NrelM[RelRec_rb$S==s_rel]=="-"){RelRec_rb[RelRec_rb$S==s_rel,as.character(s_rec)]="-"}
          if(s_rec%in%SRels==F){RelRec_rb[RelRec_rb$S==s_rel,as.character(s_rec)]="-"}
        }
      }
    }
    
    #add unfiltered links
    tmpu=ULs%>%filter(RBrec==rb)%>%dplyr::select(-c(RBrel,RBrec))
    if(nrow(tmpu)>0){
      # #constrain seasons to links
      # if(any(tmpu$Srec>=min(Ss))){
      #   RelRec_rb=RelRec_rb%>%filter(S>=min(tmpu$Srel[tmpu$Srec>=min(Ss)]))
      # }
      # else{
      #   RelRec_rb=RelRec_rb[-(1:nrow(RelRec_rb)),]
      # }
      
      for(i in seq(1,nrow(tmpu))){
        if(tmpu$Srec[i]%in%Ss){
          RelRec_rb[which(RelRec_rb$S==tmpu$Srel[i]),as.character(tmpu$Srec[i])]=tmpu$n[i]-tmpu$nmm[i]
        }
      }
    }
    #use Filtered links to constrain seasons
    tmp=Ls%>%filter(RESEARCH_BLOCK_CODE_RECAPTURE==rb)%>%dplyr::select(season_ccamlr_release,season_ccamlr_recapture)
    tmp=tmp%>%group_by(Srel=season_ccamlr_release,Srec=season_ccamlr_recapture)%>%tally()%>%as.data.frame()
    if(nrow(tmp)>0){
      RelRec_rb=RelRec_rb[RelRec_rb$S>=min(tmp$Srel) & RelRec_rb$S<=max(tmp$Srec),]
    }else{
      RelRec_rb=RelRec_rb[-(1:nrow(RelRec_rb)),]
    }
    
    #Put zeros when no linked recaptures
    for(s in Ss){
      Col=which(colnames(RelRec_rb)==s)
      Row=which(is.na(RelRec_rb[,Col])==T)
      Row=Row[which(RelRec_rb$S[Row]<=s)]
      Row=Row[which(RelRec_rb$S[Row]<=s)]
      if(length(Row)>0){RelRec_rb[Row,Col]=0}
    }
    
    # #square-bracket number of recaptures linked that are used in the TA
    # if(rb %in% RBs_1YearAtLiberty){
    #   for(i in seq(1,nrow(RelRec_rb))){
    #     Col=which(colnames(RelRec_rb)%in%as.character(Ss[Ss==(RelRec_rb$S[i]+1)]))
    #     RelRec_rb[i,Col]=paste0("[",RelRec_rb[i,Col],"]")
    #   }
    # }else{
    #   for(i in seq(1,nrow(RelRec_rb))){
    #     Col=which(colnames(RelRec_rb)%in%as.character(Ss[(Ss>=(RelRec_rb$S[i]+1) & Ss<=(RelRec_rb$S[i]+3))]))
    #     RelRec_rb[i,Col]=paste0("[",RelRec_rb[i,Col],"]")
    #   }
    # }
    
    #Mark recaptures linked that are used in the TA
    if(rb %in% RBs_1YearAtLiberty){
      for(i in seq(1,nrow(RelRec_rb))){
        Col=which(colnames(RelRec_rb)%in%as.character(Ss[Ss==(RelRec_rb$S[i]+1)]))
        if(length(Col)>0){Col=which(colnames(RelRec_rb)%in%paste0(colnames(RelRec_rb)[Col],"Used"))}
        RelRec_rb[i,Col]="T"
      }
    }else{
      for(i in seq(1,nrow(RelRec_rb))){
        Col=which(colnames(RelRec_rb)%in%as.character(Ss[(Ss>=(RelRec_rb$S[i]+1) & Ss<=(RelRec_rb$S[i]+3))]))
        if(length(Col)>0){Col=which(colnames(RelRec_rb)%in%paste0(colnames(RelRec_rb)[Col],"Used"))}
        RelRec_rb[i,Col]="T"
      }
    }
    
    
    if(nrow(RelRec_rb)>1){
      RelRec_rb$RB=c(rb,rep("",(nrow(RelRec_rb)-1)))
    }
    
    colnames(RelRec_rb)[1:8]=c("A","Subarea or Division","Research Block","Season",
                               "Releases","Recaptures","Recaptures linked to quarantined releases","Recaptures not linked")
    RelRec=rbind(RelRec,RelRec_rb)
  }
}

RelRec=RelRec%>%dplyr::select(-c("Recaptures linked to quarantined releases","Recaptures not linked"))

#Filter migrants to keep season/RB of interest
RB_S=RelRec%>%select("A","Subarea or Division",RB="Research Block",S="Season")%>%filter(nchar(RB)!=0)
for(i in seq(1,nrow(RB_S))){
  indx=which(Mig$M_rb==RB_S$RB[i] & Mig$M_S<RB_S$S[i])
  if(length(indx)>0){Mig=Mig[-indx,]}
}
Mig=Mig%>%filter(M_rb%in%RB_S$RB)
Mig=left_join(Mig,RB_S,by=c("M_rb"="RB"))
rm(RB_S)
#Format migrants table
ASDs=CCAMLRGIS::load_ASDs()
ASDs=sf::st_drop_geometry(ASDs)
ASDs=ASDs%>%select(SL=GAR_Short_Label,LL=GAR_Long_Label)
Mig$ASD_CODE_RELEASE=ASDs$LL[match(Mig$ASD_CODE_RELEASE,ASDs$SL)]
Mig$ASD_CODE_RECAPTURE=ASDs$LL[match(Mig$ASD_CODE_RECAPTURE,ASDs$SL)]

Mig=Mig%>%select(
  A,
  ASD="Subarea or Division",
  RB=M_rb,
  S=M_S,
  tagcount="taglink_tagcount",
  score="taglink_score",
  Mmatch="taglink_mismatch_yn",
  Ltype="taglink_ambiguity_code",
  RelS="season_ccamlr_release",
  RelASD="ASD_CODE_RELEASE",
  RelRB="RESEARCH_BLOCK_CODE_RELEASE",
  RecS="season_ccamlr_recapture",
  RecASD="ASD_CODE_RECAPTURE",
  RecRB="RESEARCH_BLOCK_CODE_RECAPTURE"
)
#Order RBs in Mig table
Ord_RBs=data.frame(RB=TabDat$`Research Block`[TabDat$`Research Block`%in%unique(Mig$RB)])
tmp=left_join(Ord_RBs,Mig,by="RB",multiple = "all")
Mig=tmp[,colnames(Mig)]

write.csv(Mig,paste0("Migrants_",Time,".csv"),row.names = F)

#Split by Area
L48=RelRec%>%filter(A=="48")%>%dplyr::select(-A)
L58=RelRec%>%filter(A=="58")%>%dplyr::select(-A)
L88=RelRec%>%filter(A=="88")%>%dplyr::select(-A)

#Export to be knitted
write.csv(L48,paste0("TagTable_Area_48_",Time,".csv"),row.names = F)
write.csv(L58,paste0("TagTable_Area_58_",Time,".csv"),row.names = F)
write.csv(L88,paste0("TagTable_Area_88_",Time,".csv"),row.names = F)

# rm(T_Rels,T_Recs,T_Links,T_Catch)
# rm(Tn_Rels,Tn_Recs,Tn_Links,Tn_Catch,Tn_Links_WRB,Tn_Links_M)
# rm(TotRelRec,LinkCounts,Em,Im,i)



#Map migrants
T_Links$RESEARCH_BLOCK_CODE_RECAPTURE[is.na(T_Links$RESEARCH_BLOCK_CODE_RECAPTURE)]="Out"
T_Links$RESEARCH_BLOCK_CODE_RELEASE[is.na(T_Links$RESEARCH_BLOCK_CODE_RELEASE)]="Out"
Mi=T_Links%>%filter(RESEARCH_BLOCK_CODE_RELEASE!=RESEARCH_BLOCK_CODE_RECAPTURE)
Mi=Mi%>%filter(
  ((taglink_tagcount==1 & taglink_score==5) |
     (taglink_tagcount==2 & taglink_score==9) |
     (taglink_tagcount==3 & taglink_score==13))
  & taglink_ambiguity_code==1
)
Mi=Mi%>%select(
  SRel="season_ccamlr_release" ,  
  LatRel="latitude_release"    ,             
  LonRel="longitude_release",
  ASDRel="ASD_CODE_RELEASE",
  RBRel="RESEARCH_BLOCK_CODE_RELEASE",       
  SRec="season_ccamlr_recapture",
  LatRec="latitude_recapture",               
  LonRec="longitude_recapture",
  ASDRec="ASD_CODE_RECAPTURE" ,  
  RBRec="RESEARCH_BLOCK_CODE_RECAPTURE"    
)
Mi$LatRel=as.numeric(Mi$LatRel)
Mi$LonRel=as.numeric(Mi$LonRel)

#Get RBs centers
RBcen=suppressWarnings( st_centroid(RBs_B) )
tmp=as.data.frame( st_coordinates(RBcen) )
RBcen=cbind(RBcen,tmp)
rm(tmp)
RBcen=st_drop_geometry(RBcen)
RBcen=project_data(RBcen,NamesIn = c("Y","X"),inv=T)
tmp=data.frame(RBRel=RBcen$name,LatS=RBcen$Latitude,LonS=RBcen$Longitude)
Mi=left_join(Mi,tmp,by="RBRel")
tmp=data.frame(RBRec=RBcen$name,LatE=RBcen$Latitude,LonE=RBcen$Longitude)
Mi=left_join(Mi,tmp,by="RBRec")
rm(tmp)

#Separate inter-RB migrations from outer-RB migrations
Mi_in=Mi%>%filter(RBRel!="Out" & RBRec!="Out")
Mi_out=Mi%>%filter(RBRel=="Out" | RBRec=="Out")

Mi_in=Mi_in%>%group_by(RBRel,RBRec)%>%summarise(
  LatS=unique(LatS),
  LonS=unique(LonS),
  LatE=unique(LatE),
  LonE=unique(LonE),
  n=n(),.groups = "drop")

#Group outer-RBs by regions (buffers)
Outs=data.frame(Lat=c(Mi_out$LatRel[Mi_out$RBRel=="Out"],Mi_out$LatRec[Mi_out$RBRec=="Out"]),
                Lon=c(Mi_out$LonRel[Mi_out$RBRel=="Out"],Mi_out$LonRec[Mi_out$RBRec=="Out"]))

Outs=create_Points(Outs,Buffer = 75,SeparateBuf = F)
Outs=suppressWarnings( st_cast(Outs,"POLYGON") )
Outs$ID=seq(1,nrow(Outs))
Mi_out=assign_areas(Mi_out,Polys = "Outs",AreaNameFormat = "ID",
                    NamesIn =c("LatRel","LonRel") ,NamesOut = "OutRel" )
Mi_out=assign_areas(Mi_out,Polys = "Outs",AreaNameFormat = "ID",
                    NamesIn =c("LatRec","LonRec") ,NamesOut = "OutRec" )

#Get Outs centers
Outcen=suppressWarnings( st_centroid(Outs) )
tmp=as.data.frame( st_coordinates(Outcen) )
Outcen=cbind(Outcen,tmp)
rm(tmp)
Outcen=st_drop_geometry(Outcen)
Outcen=project_data(Outcen,NamesIn = c("Y","X"),inv=T)
tmp=data.frame(OutRel=Outcen$ID,LatOutS=Outcen$Latitude,LonOutS=Outcen$Longitude)
tmp$OutRel=as.character(tmp$OutRel)
Mi_out=left_join(Mi_out,tmp,by="OutRel")
tmp=data.frame(OutRec=Outcen$ID,LatOutE=Outcen$Latitude,LonOutE=Outcen$Longitude)
tmp$OutRec=as.character(tmp$OutRec)
Mi_out=left_join(Mi_out,tmp,by="OutRec")
rm(tmp)

Mi_out$LatS[Mi_out$RBRel=="Out"]=Mi_out$LatOutS[Mi_out$RBRel=="Out"]
Mi_out$LonS[Mi_out$RBRel=="Out"]=Mi_out$LonOutS[Mi_out$RBRel=="Out"]
Mi_out$LatE[Mi_out$RBRec=="Out"]=Mi_out$LatOutE[Mi_out$RBRec=="Out"]
Mi_out$LonE[Mi_out$RBRec=="Out"]=Mi_out$LonOutE[Mi_out$RBRec=="Out"]

Mi_out$OutRel[Mi_out$RBRel!="Out"]=NA
Mi_out$OutRec[Mi_out$RBRec!="Out"]=NA


Mi_out=Mi_out%>%group_by(OutRel,OutRec,RBRel,RBRec)%>%summarise(
  LatS=unique(LatS),
  LonS=unique(LonS),
  LatE=unique(LatE),
  LonE=unique(LonE),
  n=n(),.groups = "drop")

Mi_out_Im=Mi_out%>%filter(RBRel=="Out")
Mi_out_Em=Mi_out%>%filter(RBRec=="Out")


Mi_in=arrange(Mi_in,desc(n))
Mi_out_Em=arrange(Mi_out_Em,desc(n))
Mi_out_Im=arrange(Mi_out_Im,desc(n))


ASDs=load_ASDs()
coast=load_Coastline()

plot(st_geometry(ASDs))
plot(st_geometry(RBs_B),add=T)
plot(st_geometry(Outs),col="red",add=T)

# png(filename = 'Map_Migrations.png', width = 4000, height = 3800, units = "px", pointsize = 12,
#     bg = "white", res = 200)
# par(mai=rep(0,4),xaxs="i",yaxs="i")
# plot(st_geometry(ASDs),col="grey90",lwd=2)
# plot(st_geometry(RBs_B),add=T,border="white",col="white")
# plot(st_geometry(coast[coast$surface=="Land",]),col='grey70',border='grey50',add=T,lwd=0.5)
# 
# for(i in seq(1,nrow(Mi_out_Im))){
#   asz=Mi_out_Im$n[i]
#   Inp=data.frame(lat=c(Mi_out_Im$LatS[i],Mi_out_Im$LatE[i]),
#                  lon=c(Mi_out_Im$LonS[i],Mi_out_Im$LonE[i]))
#   
#   Ld=create_Lines(cbind(name="one",Inp),Densify = T)
#   Ws=c(round(0.0012*Ld$LengthKm+0.9883),1,round(0.0012*Ld$LengthKm+0.9883))
#   Ld=st_coordinates(Ld)
#   Ld=data.frame(X=Ld[nrow(Ld)/2,1],Y=Ld[nrow(Ld)/2,2])
#   Ld=project_data(Ld,NamesIn = c("Y","X"),inv = T)
#   Inp=data.frame(lat=c(Inp$lat[1],Ld$Latitude,Inp$lat[2]),
#                  lon=c(Inp$lon[1],Ld$Longitude,Inp$lon[2]))
#   Inp$w=Ws
#   # Inp$w=c(3,1,3)
#   tmp=create_Arrow(Input=Inp,
#                    Np = 200,
#                    Pwidth = asz,
#                    Hlength = asz*3,
#                    Hwidth = asz*3)
#   plot(st_geometry(tmp),add=T,col="blue",border="darkblue")
# }
# for(i in seq(1,nrow(Mi_out_Em))){
#   asz=Mi_out_Em$n[i]
#   Inp=data.frame(lat=c(Mi_out_Em$LatS[i],Mi_out_Em$LatE[i]),
#                  lon=c(Mi_out_Em$LonS[i],Mi_out_Em$LonE[i]))
#   
#   Ld=create_Lines(cbind(name="one",Inp),Densify = T)
#   Ws=c(round(0.0012*Ld$LengthKm+0.9883),1,round(0.0012*Ld$LengthKm+0.9883))
#   Ld=st_coordinates(Ld)
#   Ld=data.frame(X=Ld[nrow(Ld)/2,1],Y=Ld[nrow(Ld)/2,2])
#   Ld=project_data(Ld,NamesIn = c("Y","X"),inv = T)
#   Inp=data.frame(lat=c(Inp$lat[1],Ld$Latitude,Inp$lat[2]),
#                  lon=c(Inp$lon[1],Ld$Longitude,Inp$lon[2]))
#   Inp$w=Ws
#   # Inp$w=c(3,1,3)
#   tmp=create_Arrow(Input=Inp,
#                    Np = 200,
#                    Pwidth = asz,
#                    Hlength = asz*3,
#                    Hwidth = asz*3)
#   plot(st_geometry(tmp),add=T,col="orange",border="darkorange")
# }
# 
# for(i in seq(1,nrow(Mi_in))){
#   asz=Mi_in$n[i]
#   Inp=data.frame(lat=c(Mi_in$LatS[i],Mi_in$LatE[i]),
#                  lon=c(Mi_in$LonS[i],Mi_in$LonE[i]))
#   
#   Ld=create_Lines(cbind(name="one",Inp),Densify = T)
#   Ws=c(round(0.0012*Ld$LengthKm+0.9883),1,round(0.0012*Ld$LengthKm+0.9883))
#   Ld=st_coordinates(Ld)
#   Ld=data.frame(X=Ld[nrow(Ld)/2,1],Y=Ld[nrow(Ld)/2,2])
#   Ld=project_data(Ld,NamesIn = c("Y","X"),inv = T)
#   Inp=data.frame(lat=c(Inp$lat[1],Ld$Latitude,Inp$lat[2]),
#                  lon=c(Inp$lon[1],Ld$Longitude,Inp$lon[2]))
#   Inp$w=Ws
#   # Inp$w=c(3,1,3)
#   
#   tmp=create_Arrow(Input=Inp,
#                    Np = 200,
#                    Pwidth = asz,
#                    Hlength = asz*3,
#                    Hwidth = asz*3)
#   plot(st_geometry(tmp),add=T,col="green",border="darkgreen")
# }
# plot(st_geometry(RBs_B),lwd=2,add=T,border="grey35")
# dev.off()


#Build arrows, then plot

Ar_Im=NULL
for(i in seq(1,nrow(Mi_out_Im))){
  asz=Mi_out_Im$n[i]
  Inp=data.frame(lat=c(Mi_out_Im$LatS[i],Mi_out_Im$LatE[i]),
                 lon=c(Mi_out_Im$LonS[i],Mi_out_Im$LonE[i]))
  
  Ld=create_Lines(cbind(name="one",Inp),Densify = T)
  Ws=c(round(0.0012*Ld$LengthKm+0.9883),1,round(0.0012*Ld$LengthKm+0.9883))
  Ld=st_coordinates(Ld)
  Ld=data.frame(X=Ld[nrow(Ld)/2,1],Y=Ld[nrow(Ld)/2,2])
  Ld=project_data(Ld,NamesIn = c("Y","X"),inv = T)
  Inp=data.frame(lat=c(Inp$lat[1],Ld$Latitude,Inp$lat[2]),
                 lon=c(Inp$lon[1],Ld$Longitude,Inp$lon[2]))
  Inp$w=Ws
  # Inp$w=c(3,1,3)
  tmp=create_Arrow(Input=Inp,
                   Np = 200,
                   Pwidth = asz,
                   Hlength = asz*3,
                   Hwidth = asz*3)
  tmp$col="blue"
  tmp$border="darkblue"
  tmp=cbind(tmp,Mi_out_Im[i,])
  Ar_Im=rbind(Ar_Im,tmp)
}


Ar_Em=NULL
for(i in seq(1,nrow(Mi_out_Em))){
  asz=Mi_out_Em$n[i]
  Inp=data.frame(lat=c(Mi_out_Em$LatS[i],Mi_out_Em$LatE[i]),
                 lon=c(Mi_out_Em$LonS[i],Mi_out_Em$LonE[i]))
  
  Ld=create_Lines(cbind(name="one",Inp),Densify = T)
  Ws=c(round(0.0012*Ld$LengthKm+0.9883),1,round(0.0012*Ld$LengthKm+0.9883))
  Ld=st_coordinates(Ld)
  Ld=data.frame(X=Ld[nrow(Ld)/2,1],Y=Ld[nrow(Ld)/2,2])
  Ld=project_data(Ld,NamesIn = c("Y","X"),inv = T)
  Inp=data.frame(lat=c(Inp$lat[1],Ld$Latitude,Inp$lat[2]),
                 lon=c(Inp$lon[1],Ld$Longitude,Inp$lon[2]))
  Inp$w=Ws
  # Inp$w=c(3,1,3)
  tmp=create_Arrow(Input=Inp,
                   Np = 200,
                   Pwidth = asz,
                   Hlength = asz*3,
                   Hwidth = asz*3)
  
  tmp$col="orange"
  tmp$border="darkorange"
  tmp=cbind(tmp,Mi_out_Em[i,])
  Ar_Em=rbind(Ar_Em,tmp)
}
Ar_Mig=NULL
for(i in seq(1,nrow(Mi_in))){
  asz=Mi_in$n[i]
  Inp=data.frame(lat=c(Mi_in$LatS[i],Mi_in$LatE[i]),
                 lon=c(Mi_in$LonS[i],Mi_in$LonE[i]))
  
  Ld=create_Lines(cbind(name="one",Inp),Densify = T)
  Ws=c(round(0.0012*Ld$LengthKm+0.9883),1,round(0.0012*Ld$LengthKm+0.9883))
  Ld=st_coordinates(Ld)
  Ld=data.frame(X=Ld[nrow(Ld)/2,1],Y=Ld[nrow(Ld)/2,2])
  Ld=project_data(Ld,NamesIn = c("Y","X"),inv = T)
  Inp=data.frame(lat=c(Inp$lat[1],Ld$Latitude,Inp$lat[2]),
                 lon=c(Inp$lon[1],Ld$Longitude,Inp$lon[2]))
  Inp$w=Ws
  # Inp$w=c(3,1,3)
  
  tmp=create_Arrow(Input=Inp,
                   Np = 200,
                   Pwidth = asz,
                   Hlength = asz*3,
                   Hwidth = asz*3)
  tmp$col="green"
  tmp$border="darkgreen"
  tmp=cbind(tmp,Mi_in[i,])
  Ar_Mig=rbind(Ar_Mig,tmp)
}

#Merge arrows that have the same starting point
if(length(unique(Ar_Im$OutRel))!=nrow(Ar_Im)){stop("Merge Ar_Im arrows")}
if(length(unique(Ar_Em$RBRel))!=nrow(Ar_Em)){
  Ar_Em=Ar_Em%>%group_by(RBRel)%>%summarise(
    col=unique(col),
    border=unique(border),  
    n=sum(n),
    geometry=st_union(geometry),
    .groups="drop"
  )
}
if(length(unique(Ar_Mig$RBRel))!=nrow(Ar_Mig)){
  Ar_Mig=Ar_Mig%>%group_by(RBRel)%>%summarise(
    col=unique(col),
    border=unique(border),  
    n=sum(n),
    geometry=st_union(geometry),
    .groups="drop"
  )
}

Ar_Mig=arrange(Ar_Mig,desc(n))
Ar_Im=arrange(Ar_Im,desc(n))
Ar_Em=arrange(Ar_Em,desc(n))


png(filename = 'Map_Migrations.png', width = 4000, height = 3800, units = "px", pointsize = 12,
    bg = "white", res = 200)
par(mai=rep(0,4),xaxs="i",yaxs="i")
plot(st_geometry(ASDs),col="grey90",lwd=2)
plot(st_geometry(RBs_B),add=T,border="white",col="white")
plot(st_geometry(coast[coast$surface=="Ice",]),col='white',add=T,lwd=0.5)
plot(st_geometry(coast[coast$surface=="Land",]),col='grey70',add=T,lwd=2)

plot(st_geometry(Ar_Im),col=Ar_Im$col,border=Ar_Im$border,add=T)
plot(st_geometry(Ar_Em),col=Ar_Em$col,border=Ar_Em$border,add=T)
plot(st_geometry(Ar_Mig),col=Ar_Mig$col,border=Ar_Mig$border,add=T)
plot(st_geometry(RBs_B),lwd=2,add=T,border="grey35")

#Add legend
#Bottom-right corner
xbr=as.numeric(st_bbox(ASDs)$xmax)-20000
ybr=as.numeric(st_bbox(ASDs)$ymin)
#Height and width
Lh=2000000
Lw=1900000
#Make box
Xs=c(xbr,xbr-Lw,xbr-Lw,xbr,xbr)
Ys=c(ybr,ybr,ybr+Lh,ybr+Lh,ybr)
Lbox=st_polygon(list(cbind(Xs,Ys)))
#Arrows butts
Yab=rev(seq(ybr,ybr+Lh,length.out=8))
Yab=Yab[2:7]
aoff=50000 #x offset
Xab=xbr-Lw+aoff
Al=500000 #Arrow length
Arlocs=data.frame(
  Xs=Xab,
  Ys=Yab,
  Xe=Xab+Al,
  Ye=Yab
)
Arlocs=project_data(Arlocs,NamesIn = c("Ys","Xs"),NamesOut = c("LatS","LonS"),inv=T)
Arlocs=project_data(Arlocs,NamesIn = c("Ye","Xe"),NamesOut = c("LatE","LonE"),inv=T)
arcol=c("green","orange","blue","white","white","white")
arbor=c("darkgreen","darkorange","darkblue","black","black","black")
aszs=c(3,3,3,1,3,7)
Ar_Leg=NULL
for(i in seq(1,6)){
  asz=aszs[i]
  Inp=data.frame(lat=c(Arlocs$LatS[i],Arlocs$LatE[i]),
                 lon=c(Arlocs$LonS[i],Arlocs$LonE[i]))
  tmp=create_Arrow(Input=Inp,
                   Np = 200,
                   Pwidth = asz,
                   Hlength = asz*3,
                   Hwidth = asz*3)
  tmp$col=arcol[i]
  tmp$border=arbor[i]
  Ar_Leg=rbind(Ar_Leg,tmp)
}
plot(Lbox,add=T,lwd=3,col="white",xpd=T)
plot(st_geometry(Ar_Leg),col=Ar_Leg$col,border=Ar_Leg$border,add=T)
text(Arlocs$Xe+aoff,Arlocs$Ye,
     c(paste0("RB to RB (n=",sum(Ar_Mig$n),")"),
       paste0("RB to Out (n=",sum(Ar_Em$n),")"),
       paste0("Out to RB (n=",sum(Ar_Im$n),")"),
       "n=1",
       "n=3",
       "n=7"
       ),
     adj=c(0,0.5),cex=2.5)
dev.off()


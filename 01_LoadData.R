library(BERT)
library(RODBC)
library(plyr)
library(CCAMLRGIS)
library(dplyr)


#Parameters preparation #####

#Get RefAreas fishable
Ref_area_seabed_areaM=RB_seabed_areaM[RB_seabed_areaM$Polys%in%c('RSR_open','HIMI'),]
#Get spatial objects
ASDs=load_ASDs()
RefAreas=st_read(dsn=path.expand(paste0(getwd(),'/Data')), layer="RefAreas", quiet = TRUE)
RBs_B=st_read(dsn=path.expand(paste0(getwd(),'/Data')), layer="BufferedRBs",quiet = TRUE)

#Biomass and CV for Reference Areas history
#2017-2018:
# HIMI_biomass_est=43993.03
# HIMI_CV_biomass_est=0.072
#2019-2020:
# HIMI_biomass_est=32917
# HIMI_CV_biomass_est=0.0308
# RSR_open_biomass_est=84658
# RSR_open_CV_biomass_est=0.0612
#2021-2023:
# HIMI_biomass_est=31111 
# HIMI_CV_biomass_est=0.0281
# RSR_open_biomass_est=84260
# RSR_open_CV_biomass_est=0.0581
#Min and max seasons for data queries
maxSeason=Est_Season
minSeason=2012
#For CPUE-based estimation: get hauls in last 3 seasons in the Reference Areas to get median CPUE
#HIMI assessment is done before the end of the fishing season so last season should be assessment season minus one
RSR_open_Ass_seasons=seq(maxSeason-2,maxSeason) #Could rename this to "CPUE seasons" or something like that
HIMI_Ass_seasons=RSR_open_Ass_seasons-1
#For Chapman: for some RBs, fish should only be 1 year at liberty. List those RBs below.
#e.g. 486_2, 486_3, 882H see WG-FSA-17 (https://www.ccamlr.org/en/system/files/e-sc-xxxvi-a07.pdf, paras 4.37(iv), 4.78, 4.80)
RBs_1YearAtLiberty=c("486_2","486_3","882H")
# expanded tag_parameters
tag_pars_TOP=list("mean_wt"=0,
                  "method"="Chapman",
                  "unit"="kg",
                  "type"=1,
                  "tag_mort"=0.1,
                  "reporting"=1,
                  "nat_mort"=0.155,
                  "chronic_shed"=0.0084,
                  "chronic_mort"=0)
tag_pars_TOA=list("mean_wt"=0,
                  "method"="Chapman",
                  "unit"="kg",
                  "type"=1,
                  "tag_mort"=0.1,
                  "reporting"=1,
                  "nat_mort"=0.13,
                  "chronic_shed"=0.0084,
                  "chronic_mort"=0)

#Source disambiguator (Used to merge ambiguous links that share season and RB)
source("Scripts/Disambiguator.R")

#################################################################################################################


#Initialize Report card####
#Open Report Card
Rcard = file(paste0('Biomass_Estimation_ReportCard_',as.character(Time),'.txt'),"w")
#Write Header 
cat("CCAMLR Secretariat | Biomass Estimation Report Card", file = Rcard, sep = "\n")
cat(paste0('Date: ',Time), file = Rcard, sep = "\n")
cat("--------------------------------------------", file = Rcard, sep = "\n")
cat("", file = Rcard, sep = "\n")
#####


#Load Data###################
LL=ccamlrtools::load_science_data(logbook_type = "longline")

#build tables
C2=LL$C2
C2=C2%>%filter(season_ccamlr>=minSeason & season_ccamlr<=maxSeason & target_species%in%c('TOP','TOA','TOT'))

LW=LL$OBS_HAUL_BIOLOGY
LW=LW%>%filter(!is.na(obs_haul_id))
LW=LW%>%filter(taxon_code%in%c("TOP","TOA"))
LW=inner_join(C2,LW,by="obs_haul_id",multiple = "all")
LW=dplyr::rename(LW, obs_logbook_id = obs_logbook_id.y)
LW=dplyr::rename(LW, haul_number = haul_number.y)

Links=LL$OBS_HAUL_TAG_LINK

Rels=LL$OBS_HAUL_TAG_RELEASE 
Rels=Rels%>%filter(taxon_code%in%c('TOA','TOP'))
Rels=inner_join(C2,Rels,by="obs_haul_id",multiple = "all")
Rels=dplyr::rename(Rels, obs_logbook_id = obs_logbook_id.y)
Rels=dplyr::rename(Rels, haul_number = haul_number.y)

Recs=LL$OBS_HAUL_TAG_RECAPTURE
Recs=Recs%>%filter(taxon_code%in%c('TOA','TOP'))
Recs=inner_join(C2,Recs,by="obs_haul_id",multiple = "all")
Recs=dplyr::rename(Recs, obs_logbook_id = obs_logbook_id.y)
Recs=dplyr::rename(Recs, haul_number = haul_number.y)

#Mark those recaptures that are linked to quarantined releases
Qlinks=Links%>%filter(quarantined_release_yn=="Y")
Recs$Qrel=0
Recs$Qrel[which(Recs$obs_haul_tag_recapture_id%in%unique(Qlinks$obs_haul_tag_recapture_id))]=1
rm(Qlinks)

Catch=LL$C2_CATCH
Catch=inner_join(C2,Catch,by="c2_id",multiple = "all")
rm(LL,C2)

#Include quarantined data?
if(IncludeQ=='N'){
  LW=LW%>%filter(quarantined_yn=="N")
  Links=Links[Links$quarantined_recapture_yn=='N',]
  Links=Links[Links$quarantined_release_yn=='N',]
  Rels=Rels%>%filter(quarantined_yn=="N")
  Recs=Recs%>%filter(quarantined_yn=="N")
  Catch=Catch%>%filter(quarantined_yn=="N")
}

#Thinout LW
LW=LW%>%select(
  asd_code,
  taxon_code,
  length_total_cm,
  greenweight_kg,
  datetime_set_start,
  datetime_set_end
)


#Remove links/releases/recaptures where minipat and p-sat are involved
NoTag=c("minipat",  "p-sat")
Tagcodes=paste(Links$tag_code_1_recapture,
               Links$tag_code_2_recapture,
               Links$tag_code_3_recapture,
               Links$tag_code_1_release,
               Links$tag_code_2_release,
               Links$tag_code_3_release,sep = "+")
indx=NULL
for(ty in NoTag){
  tmpi=grep(ty,Tagcodes)
  indx=c(indx,tmpi)
}
#now indx contains those links to be removed - use that to remove the corresponding rel/rec
Rels=Rels%>%filter(!obs_haul_tag_release_id%in%Links$obs_haul_tag_release_id[indx])
Recs=Recs%>%filter(!obs_haul_tag_recapture_id%in%Links$obs_haul_tag_recapture_id[indx])
#remove links
Links=Links[-indx,]
rm(NoTag,Tagcodes,indx,ty,tmpi)

#Assign RBs and RefAreas to linked released fish
Links=assign_areas(Input=Links,
                   NamesIn = c("latitude_release","longitude_release"),
                   Polys = c('RBs_B','RefAreas','ASDs'),
                   AreaNameFormat=c('name','name','GAR_Short_Label'),
                   Buffer=0, 
                   NamesOut=c('RESEARCH_BLOCK_CODE_RELEASE','REF_AREA_CODE_RELEASE',"ASD_CODE_RELEASE"))
#Assign RBs and RefAreas to linked recaptured fish
Links=assign_areas(Input=Links,
                   NamesIn=c("latitude_recapture","longitude_recapture"),
                   Polys=c('RBs_B','RefAreas','ASDs'),
                   AreaNameFormat=c('name','name','GAR_Short_Label'),
                   Buffer=0,
                   NamesOut=c('RESEARCH_BLOCK_CODE_RECAPTURE','REF_AREA_CODE_RECAPTURE',"ASD_CODE_RECAPTURE"))

#Assign RBs and RefAreas to released fish
Rels=assign_areas(Input=Rels,
                  NamesIn=c("latitude_release","longitude_release"),
                  Polys=c('RBs_B','RefAreas'),
                  AreaNameFormat=c('name','name'),
                  Buffer=0,
                  NamesOut=c('RESEARCH_BLOCK_CODE','REF_AREA_CODE'))

#Keep only releases in RBs and Refareas to free-up memory
Rels=Rels%>%filter(is.na(RESEARCH_BLOCK_CODE)==F | is.na(REF_AREA_CODE)==F)
#Thin out Rels
Rels=Rels%>%select(
  obs_haul_tag_release_id,
  date_release,
  longitude_release,
  latitude_release,
  length_total_cm,
  obs_logbook_id,
  taxon_code,
  asd_code,
  season_ccamlr,
  haul_number,
  REF_AREA_CODE,
  RESEARCH_BLOCK_CODE
)

#Assign RBs and RefAreas to recaptured fish
Recs=assign_areas(Input=Recs,
                  NamesIn=c("latitude_recapture","longitude_recapture"),
                  Polys=c('RBs_B','RefAreas'),
                  AreaNameFormat=c('name','name'),
                  Buffer=0,
                  NamesOut=c('RESEARCH_BLOCK_CODE','REF_AREA_CODE'))

#Thin out Recs
Recs=Recs%>%select(
  obs_haul_tag_recapture_id,
  latitude_recapture,
  longitude_recapture,
  RESEARCH_BLOCK_CODE,
  REF_AREA_CODE,
  taxon_code,
  season_ccamlr,
  Qrel,
  RESEARCH_BLOCK_CODE,
  date_recapture,
  vessel_name,
  tag_serial_number1,
  tag_type1,
  tag_inscription1,
  tag_colour1,
  tag_serial_number2,
  tag_type2,
  tag_inscription2,
  tag_colour2
)

#Assign RBs and RefAreas to catch Start of set
Catch=assign_areas(Input=Catch,
                   NamesIn=c("latitude_set_start","longitude_set_start"),
                   Polys=c('RBs_B','RefAreas'),
                   AreaNameFormat=c('name','name'),
                   Buffer=0,
                   NamesOut=c('RESEARCH_BLOCK_CODE_START_SET','REF_AREA_CODE_START_SET'))
#Assign RBs and RefAreas to catch End of set
Catch=assign_areas(Input=Catch,
                   NamesIn=c("latitude_set_end","longitude_set_end"),
                   Polys=c('RBs_B','RefAreas'),
                   AreaNameFormat=c('name','name'),
                   Buffer=0,
                   NamesOut=c('RESEARCH_BLOCK_CODE_END_SET','REF_AREA_CODE_END_SET'))

#Keep only Catch in RBs and Refareas to free-up memory
Catch=Catch%>%filter(is.na(RESEARCH_BLOCK_CODE_END_SET)==F | 
                     is.na(RESEARCH_BLOCK_CODE_START_SET)==F |  
                     is.na(REF_AREA_CODE_START_SET)==F | 
                     is.na(REF_AREA_CODE_END_SET)==F )
#Thin out catch
Catch=Catch%>%select(
  datetime_set_start,
  datetime_set_end,
  longitude_set_start,
  latitude_set_start,
  longitude_set_end,
  latitude_set_end,
  line_length_m,
  greenweight_caught_kg,
  c2_id,
  RESEARCH_BLOCK_CODE_START_SET,
  RESEARCH_BLOCK_CODE_END_SET,
  taxon_code,
  REF_AREA_CODE_START_SET,
  REF_AREA_CODE_END_SET,
  obs_logbook_id,
  haul_number,
  season_ccamlr,
  obs_haul_id,
  depth_gear_set_start_m,
  depth_gear_set_end_m
)


#Merge ambiguous links
Links=Disambiguator(Links=Links,
                    RecF=c("season_ccamlr_recapture","RESEARCH_BLOCK_CODE_RECAPTURE"),
                    RelF=c('season_ccamlr_release','RESEARCH_BLOCK_CODE_RELEASE'),
                    append='Y')


#Prep to export for post-processing
T_Rels=Rels
T_Recs=Recs
T_Links=Links
T_Catch=Catch

#filter by species/RB combo
#1.Releases
R_TOP=T_Rels[T_Rels$RESEARCH_BLOCK_CODE%in%TOP_target_RBs & T_Rels$taxon_code=='TOP',] #TOP
R_TOA=T_Rels[!T_Rels$RESEARCH_BLOCK_CODE%in%TOP_target_RBs & T_Rels$taxon_code=='TOA',] #TOA
T_Rels=rbind(R_TOP,R_TOA)
rm(R_TOP,R_TOA)
#2.Recaptures
R_TOP=T_Recs[T_Recs$RESEARCH_BLOCK_CODE%in%TOP_target_RBs & T_Recs$taxon_code=='TOP',] #TOP
R_TOA=T_Recs[!T_Recs$RESEARCH_BLOCK_CODE%in%TOP_target_RBs & T_Recs$taxon_code=='TOA',] #TOA
T_Recs=rbind(R_TOP,R_TOA)
rm(R_TOP,R_TOA)
#3.Links
L_TOP=T_Links[T_Links$RESEARCH_BLOCK_CODE_RECAPTURE%in%TOP_target_RBs & T_Links$taxon_code_recapture=='TOP',] #TOP
L_TOA=T_Links[!T_Links$RESEARCH_BLOCK_CODE_RECAPTURE%in%TOP_target_RBs & T_Links$taxon_code_recapture=='TOA',] #TOA
T_Links=rbind(L_TOP,L_TOA)
rm(L_TOP,L_TOA)

write.csv(T_Links,paste0("Output_Recaptures_Linked_",Time,".csv"),row.names = FALSE)
write.csv(T_Rels,paste0("Output_All_Releases_",Time,".csv"),row.names = FALSE)
write.csv(T_Recs,paste0("Output_All_Recaptures_",Time,".csv"),row.names = FALSE)
write.csv(T_Catch,paste0("Output_All_Catch_",Time,".csv"),row.names = FALSE)

rm(T_Rels,T_Recs,T_Links,T_Catch)


#Find releases that have been recaptured elsewhere and are therefore not available for recapture
Migrants=Links%>%filter(RESEARCH_BLOCK_CODE_RELEASE!=RESEARCH_BLOCK_CODE_RECAPTURE)
RelEm=Rels%>%filter(obs_haul_tag_release_id%in%Migrants$obs_haul_tag_release_id)
RelEm=dplyr::summarise(group_by(RelEm,RESEARCH_BLOCK_CODE),n=n())
#Report on releases that are removed for Chapman estimates (after CPUE estimates, in EstimateBiomass.R)
message("Releases that emigrated and will be excluded from Chapman estimates:")
for(i in seq(1,nrow(RelEm))){message(paste(RelEm[i,],collapse = ": "))}


#Filter Links
#No mismatches Rule
Links=Links[Links$taglink_mismatch_yn=='N',]

#Filter seasons
Links=Links%>%filter(season_ccamlr_release>=minSeason &
                       season_ccamlr_release<=maxSeason &
                       season_ccamlr_recapture>=minSeason &
                       season_ccamlr_recapture<=maxSeason)

#Keep links within buffered Research Blocks
Links=Links%>%filter(RESEARCH_BLOCK_CODE_RELEASE==RESEARCH_BLOCK_CODE_RECAPTURE)

# remove within season recaptures 
Links=Links%>%filter(season_ccamlr_recapture!=season_ccamlr_release)

#year-at-liberty filter of Links
YatL1=Links%>% #1 year at liberty
  filter(RESEARCH_BLOCK_CODE_RECAPTURE%in%RBs_1YearAtLiberty)%>%
  filter(season_ccamlr_recapture-season_ccamlr_release==1)
YatL3=Links%>% #1-3 years at liberty
  filter(!RESEARCH_BLOCK_CODE_RECAPTURE%in%RBs_1YearAtLiberty)%>%
  filter(season_ccamlr_recapture-season_ccamlr_release<=3)
Flinks=rbind(YatL1,YatL3)
rm(YatL1,YatL3)
Flinks=Flinks[Flinks$season_ccamlr_recapture>=(maxSeason-4),] #Last 5 seasons
L_TOP=Flinks[Flinks$RESEARCH_BLOCK_CODE_RECAPTURE%in%TOP_target_RBs & Flinks$taxon_code_recapture=='TOP',] #TOP
L_TOA=Flinks[!Flinks$RESEARCH_BLOCK_CODE_RECAPTURE%in%TOP_target_RBs & Flinks$taxon_code_recapture=='TOA',] #TOA
Flinks=rbind(L_TOP,L_TOA)
rm(L_TOP,L_TOA)

Lcounts=dplyr::summarise(group_by(Flinks,RESEARCH_BLOCK_CODE_RECAPTURE,season_ccamlr_recapture),n=n(),.groups = "drop")

#Export filtered Links
write.csv(Flinks,paste0("Output_Recaptures_Linked_Filtered_",Time,".csv"),row.names = FALSE)
rm(Flinks)






#Data processing ####
cat("###Data Processing start###########################", file = Rcard, sep = "\n")
cat("", file = Rcard, sep = "\n")


#1. Process catch data####
cat("#Catch data:", file = Rcard, sep = "\n")

#Missing datetime_set_start
indx=which(is.na(Catch$datetime_set_start)==T)
if(length(indx)>0){Catch=Catch[-indx,]}
cat(paste0("Missing datetime_set_start: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing datetime_set_end
indx=which(is.na(Catch$datetime_set_end)==T)
if(length(indx)>0){Catch=Catch[-indx,]}
cat(paste0("Missing datetime_set_end: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#datetime_set_end<datetime_set_start
indx=which(Catch$datetime_set_end<Catch$datetime_set_start)
if(length(indx)>0){Catch=Catch[-indx,]}
cat(paste0("datetime_set_end<datetime_set_start: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing longitude_set_start
indx=which(is.na(Catch$longitude_set_start)==T)
if(length(indx)>0){Catch=Catch[-indx,]}
cat(paste0("Missing longitude_set_start: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing latitude_set_start
indx=which(is.na(Catch$latitude_set_start)==T)
if(length(indx)>0){Catch=Catch[-indx,]}
cat(paste0("Missing latitude_set_start: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing longitude_set_end
indx=which(is.na(Catch$longitude_set_end)==T)
if(length(indx)>0){Catch=Catch[-indx,]}
cat(paste0("Missing longitude_set_end: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing latitude_set_end
indx=which(is.na(Catch$latitude_set_end)==T)
if(length(indx)>0){Catch=Catch[-indx,]}
cat(paste0("Missing latitude_set_end: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing line_length_m
indx=which(is.na(Catch$line_length_m)==T)
if(length(indx)>0){Catch=Catch[-indx,]}
cat(paste0("Missing line_length_m: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing greenweight_caught_kg
indx=which(is.na(Catch$greenweight_caught_kg)==T)
if(length(indx)>0){Catch$greenweight_caught_kg[indx]=0}
cat(paste0("Missing greenweight_caught_kg: ",length(indx),' records replaced by zero'), file = Rcard, sep = "\n")
rm(indx)

#Missing C2 ID
indx=which(is.na(Catch$c2_id)==T)
if(length(indx)>0){Catch=Catch[-indx,]}
cat(paste0("Missing c2_id: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

cat("", file = Rcard, sep = "\n")
#####

#2. Process Length-Weight data####
cat("#Length-Weight data:", file = Rcard, sep = "\n")

#Missing datetime_set_start
indx=which(is.na(LW$datetime_set_start)==T)
if(length(indx)>0){LW=LW[-indx,]}
cat(paste0("Missing datetime_set_start: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing datetime_set_end
indx=which(is.na(LW$datetime_set_end)==T)
if(length(indx)>0){LW=LW[-indx,]}
cat(paste0("Missing datetime_set_end: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#datetime_set_end<datetime_set_start
indx=which(LW$datetime_set_end<LW$datetime_set_start)
if(length(indx)>0){LW=LW[-indx,]}
cat(paste0("datetime_set_end<datetime_set_start: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing asd_code
indx=which(is.na(LW$asd_code)==T)
if(length(indx)>0){LW=LW[-indx,]}
cat(paste0("Missing asd_code: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing length_total_cm
indx=which(is.na(LW$length_total_cm)==T)
if(length(indx)>0){LW=LW[-indx,]}
cat(paste0("Missing length_total_cm: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing greenweight_kg
indx=which(is.na(LW$greenweight_kg)==T)
if(length(indx)>0){LW=LW[-indx,]}
cat(paste0("Missing greenweight_kg: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)
cat("", file = Rcard, sep = "\n")
#####

cat("", file = Rcard, sep = "\n")

#3. Process Releases data####
cat("#Releases data:", file = Rcard, sep = "\n")

#Missing date_release
indx=which(is.na(Rels$date_release)==T)
if(length(indx)>0){Rels=Rels[-indx,]}
cat(paste0("Missing date_release: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing longitude_release
indx=which(is.na(Rels$longitude_release)==T)
if(length(indx)>0){Rels=Rels[-indx,]}
cat(paste0("Missing longitude_release: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing latitude_release
indx=which(is.na(Rels$latitude_release)==T)
if(length(indx)>0){Rels=Rels[-indx,]}
cat(paste0("Missing latitude_release: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Replace missing length_total_cm by CRUISE-SPECIES average
MeanLengths=dplyr::summarise(group_by(Rels,obs_logbook_id,taxon_code),L=mean(length_total_cm,na.rm=T),.groups = "drop")
indx=which(is.na(Rels$length_total_cm)==T)
if(length(indx)>0){
  tmpdat=Rels[indx,c("obs_logbook_id","taxon_code")]
  tmpdat=left_join(tmpdat,MeanLengths,by = c("obs_logbook_id", "taxon_code"))
  Rels$length_total_cm[indx]=tmpdat$L
  rm(tmpdat)
}
cat(paste0("Missing LENGTH_CM replaced by CRUISE-SPECIES average for: ",length(indx),' records'), file = Rcard, sep = "\n")
rm(indx,MeanLengths)

#Estimate weight of released fish using LW data#### 
Rels$EST_WEIGHT_KG=est_fish_weight(length_weight_data=LW[,c("asd_code","taxon_code","length_total_cm","greenweight_kg")],
                                  length_data=Rels[,c("asd_code","season_ccamlr","obs_logbook_id","haul_number","taxon_code","length_total_cm")])

#####

cat("", file = Rcard, sep = "\n")
#####

#4. Process Recaptures data####
cat("#Recaptures data:", file = Rcard, sep = "\n")

#Missing date_release
indx=which(is.na(Links$date_release)==T)
if(length(indx)>0){Links=Links[-indx,]}
cat(paste0("Missing date_release: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing longitude_release
indx=which(is.na(Links$longitude_release)==T)
if(length(indx)>0){Links=Links[-indx,]}
cat(paste0("Missing longitude_release: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing latitude_release
indx=which(is.na(Links$latitude_release)==T)
if(length(indx)>0){Links=Links[-indx,]}
cat(paste0("Missing latitude_release: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing research_block_release
indx=which(is.na(Links$RESEARCH_BLOCK_CODE_RELEASE)==T)
if(length(indx)>0){Links=Links[-indx,]}
cat(paste0("Missing research_block_release: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing date_recapture
indx=which(is.na(Links$date_recapture)==T)
if(length(indx)>0){Links=Links[-indx,]}
cat(paste0("Missing date_recapture: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing longitude_recapture
indx=which(is.na(Links$longitude_recapture)==T)
if(length(indx)>0){Links=Links[-indx,]}
cat(paste0("Missing longitude_recapture: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing latitude_recapture
indx=which(is.na(Links$latitude_recapture)==T)
if(length(indx)>0){Links=Links[-indx,]}
cat(paste0("Missing latitude_recapture: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#taxon_code_release!=taxon_code_recapture
indx=which(Links$taxon_code_release!=Links$taxon_code_recapture)
if(length(indx)>0){Links=Links[-indx,]}
cat(paste0("taxon_code_release!=taxon_code_recapture: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)
cat("", file = Rcard, sep = "\n")
#####



cat("###Data Processing end###########################", file = Rcard, sep = "\n")
cat("", file = Rcard, sep = "\n")



# find Research blocks in which TOP or TOA were caught
Research_blocks=dplyr::summarise(group_by(Catch,RESEARCH_BLOCK_CODE_START_SET,RESEARCH_BLOCK_CODE_END_SET,taxon_code),n=n(),.groups = "drop")
Research_blocks=Research_blocks%>%filter(taxon_code%in%c("TOP","TOA") &
                                        (is.na(RESEARCH_BLOCK_CODE_START_SET)==F |
                                         is.na(RESEARCH_BLOCK_CODE_END_SET)==F))
Research_blocks=sort(unique(c(Research_blocks$RESEARCH_BLOCK_CODE_START_SET,Research_blocks$RESEARCH_BLOCK_CODE_END_SET)))

#Report if RBs don't have data or fishable area
AllRBs=RB_seabed_areaM$Polys
AllRBs=AllRBs[-which(AllRBs%in%c("HIMI","RSR_open"))]
RBnodat=sort(AllRBs[AllRBs%in%Research_blocks==F])
RBnoFA=sort(RB_seabed_areaM$Polys[RB_seabed_areaM$Fishable_area==0])

if(length(RBnodat)>0){
  cat(paste0(length(RBnodat)," RBs do not have catch data: ",paste0(RBnodat,collapse = ", "),"."), file = Rcard, sep = "\n")
  cat(paste0(length(RBnodat)," RBs do not have catch data: ",paste0(RBnodat,collapse = ", "),"."), sep = "\n")
}

if(length(RBnoFA)>0){
  cat(paste0(length(RBnoFA)," RBs do not have fishable area: ",paste0(RBnoFA,collapse = ", "),"."), file = Rcard, sep = "\n")
  cat(paste0(length(RBnoFA)," RBs do not have fishable area: ",paste0(RBnoFA,collapse = ", "),"."), sep = "\n")
}

if(Output=="Y"){
  write.csv(Catch,"Output_Catch.csv")
  write.csv(Rels,"Output_Rels.csv")
  write.csv(LW,"Output_LW.csv")
  write.csv(Links,"Output_Links.csv")
}


cat("Data loaded", sep = "\n")

close(Rcard)
rm(Rcard)
rm(LW,Recs)
a=gc()
rm(a)

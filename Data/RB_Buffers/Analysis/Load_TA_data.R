#Load TA data
LL=ccamlrtools::load_science_data(logbook_type = "longline")

#build tables
C2=LL$C2
C2=C2%>%filter(season_ccamlr>=2012 & target_species%in%c('TOP','TOA','TOT'))

Catch=LL$C2_CATCH
Catch=inner_join(C2,Catch,by="c2_id",multiple = "all")

C2=C2%>%filter(!is.na(obs_haul_id))

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

rm(LL,C2)

#Exclude quarantined
Rels=Rels%>%filter(quarantined_yn=="N")
Recs=Recs%>%filter(quarantined_yn=="N")
Catch=Catch%>%filter(quarantined_yn=="N")


#Assign RBs and RefAreas to released fish
Rels=assign_areas(Input=Rels,
                  NamesIn=c("latitude_release","longitude_release"),
                  Polys=c('RBs','B5k',"B1F",'B2F'),
                  AreaNameFormat=c('ID','name','name','name'),
                  Buffer=0,
                  NamesOut=c('RB','B5k',"B1F",'B2F'))
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
  RB,
  B5k,
  B1F,
  B2F
)
#Assign RBs and RefAreas to recaptured fish
Recs=assign_areas(Input=Recs,
                  NamesIn=c("latitude_recapture","longitude_recapture"),
                  Polys=c('RBs','B5k',"B1F",'B2F'),
                  AreaNameFormat=c('ID','name','name','name'),
                  Buffer=0,
                  NamesOut=c('RB','B5k',"B1F",'B2F'))

#Thin out Recs
Recs=Recs%>%select(
  obs_haul_tag_recapture_id,
  latitude_recapture,
  longitude_recapture,
  taxon_code,
  season_ccamlr,
  date_recapture,
  vessel_name,
  tag_serial_number1,
  tag_type1,
  tag_inscription1,
  tag_colour1,
  tag_serial_number2,
  tag_type2,
  tag_inscription2,
  tag_colour2,
  RB,
  B5k,
  B1F,
  B2F
)

#Assign RBs and RefAreas to catch Start of set
Catch=assign_areas(Input=Catch,
                   NamesIn=c("latitude_set_start","longitude_set_start"),
                   Polys=c('RBs','B5k',"B1F",'B2F'),
                   AreaNameFormat=c('ID','name','name','name'),
                   Buffer=0,
                   NamesOut=c('RB_s','B5k_s',"B1F_s",'B2F_s'))
#Assign RBs and RefAreas to catch End of set
Catch=assign_areas(Input=Catch,
                   NamesIn=c("latitude_set_end","longitude_set_end"),
                   Polys=c('RBs','B5k',"B1F",'B2F'),
                   AreaNameFormat=c('ID','name','name','name'),
                   Buffer=0,
                   NamesOut=c('RB_e','B5k_e',"B1F_e",'B2F_e'))

#temp fix for line_length_m name change###############################################################################################
colnames(Catch)[grep("line_length",colnames(Catch))]="line_length_m"
######################################################################################################################################

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
  taxon_code,
  obs_logbook_id,
  haul_number,
  season_ccamlr,
  obs_haul_id,
  depth_gear_set_start_m,
  depth_gear_set_end_m,
  'RB_s','B5k_s',"B1F_s",'B2F_s',
  'RB_e','B5k_e',"B1F_e",'B2F_e'
)


#filter Catch by species
Catch=Catch[which(Catch$taxon_code%in%c("TOP","TOA")),]


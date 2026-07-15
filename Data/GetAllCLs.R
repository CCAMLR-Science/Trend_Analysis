#Script to get all past catch limits
library(dplyr)
library(stringr)


#Get them
Catch_limits_old = ccamlrtools::queryccamlr("SELECT [clID]
,[Season]
,[ASD]
,[MA]
,[ManagedSpecies]
,[LimitTonnes]
,[ConservationMeasure]
,[Comment]
FROM [cdb].[dbo].[TAC_CATCH_LIMITS]
where MA like '%[_]%' or  MA = '882H' ")

Catch_limits_old = Catch_limits_old %>% dplyr::filter(ManagedSpecies %in% c('TOA' , 'TOP', 'TOT'))
Catch_limits_old = Catch_limits_old %>% dplyr::filter(!(ASD == '486' & ManagedSpecies == 'TOP')) # filter out TOP in 486_1 and 486_2
Catch_limits_old = Catch_limits_old %>% dplyr::select(Season, MA, Lim = LimitTonnes)

# Catch_limits_old=Catch_limits_old%>%group_by(Season, MA)%>%summarise(Lim=max(as.numeric(LimitTonnes))) 


Catch_limits = ccamlrtools::queryccamlr(
  paste0(" SELECT distinct 	
                  CSN_Code,
                  dfa_name as 'RB',
              	  tg.taxon_group_cd as 'Taxon Group',
                  [DFL_Target_Bycatch] as 'Target Bycatch',
                  [DFL_Catch_Limit] as 'Catch limit'
                  FROM [cdb].[dbo].[DIRECTED_FISHING_CATCH_LIMIT] DFCL
                  inner join [DIRECTED_FISHING_AREA_COMPOSITION] DFAC on DFAC.DFA_ID = DFCL.DFA_ID
                  inner join GEOGRAPHICAL_AREA GAR on GAR.GAR_ID = DFAC.GAR_ID
                  inner join  [DIRECTED_FISHING_AREA] DFA_FLAT on DFA_FLAT.DFA_ID = DFCL.DFA_ID
                  inner join [CCAMLR].[dbo].[FISHERY_SEASONAL_REGULATION] FSR on FSR.FSR_ID = DFA_FLAT.FSR_ID
                  inner join CCAMLR.dbo.[CCAMLR_SEASON] on [CCAMLR_SEASON].CSN_ID = FSR.CSN_ID
                  inner join [ANTARCTICA].[dbo].[TAXON_GROUP] tg on tg.taxon_group_legacy_cd = TGR_ID
                  where (dfa_name like 'Research%' or DFA_Name like 'SSRU%')
                          "), db = "cdb")


Catch_limits=Catch_limits%>%filter(`Target Bycatch`=="Target")
Catch_limits$RB=str_remove_all(Catch_limits$RB, "\\.")
Catch_limits$RB=str_remove_all(Catch_limits$RB, "Research Block ")
Catch_limits$RB=str_remove_all(Catch_limits$RB, "SSRU ")
Catch_limits$RB=str_remove_all(Catch_limits$RB, " ")



AllCLs_old=data.frame(
  Season=as.integer(Catch_limits_old$Season),
  RB=Catch_limits_old$MA,
  CL=as.numeric(Catch_limits_old$Lim)
)


AllCLs=data.frame(
  Season=as.integer(Catch_limits$CSN_Code),
  RB=Catch_limits$RB,
  CL=as.numeric(Catch_limits$`Catch limit`)
  )

AllCLs=rbind(AllCLs_old,AllCLs)

write.csv(AllCLs,"Data/AllCLs.csv",row.names = F)

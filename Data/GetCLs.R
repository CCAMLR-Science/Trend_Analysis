#Script to get catch limits
#Run it, but always check against the catch limits table of the SC.
library(dplyr)
library(stringr)

#Set season, then run
Season = 2024



CLs=read.csv("Data/CLs.csv",check.names = F)



Catch_limits = ccamlrtools::queryccamlr(
  paste0(" SELECT distinct 	  
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
                  where [CSN_Code] =  " , Season , " and (dfa_name like 'Research%' or DFA_Name like 'SSRU%')
                          "), db = "cdb")


Catch_limits=Catch_limits%>%filter(`Target Bycatch`=="Target")
Catch_limits$RB=str_remove_all(Catch_limits$RB, "\\.")
Catch_limits$RB=str_remove_all(Catch_limits$RB, "Research Block ")
Catch_limits$RB=str_remove_all(Catch_limits$RB, "SSRU ")
Catch_limits$RB=str_remove_all(Catch_limits$RB, " ")

NewCLs=data.frame(RB=Catch_limits$RB,L=as.numeric(Catch_limits$`Catch limit`))
colnames(NewCLs)[2]=Season

CLs=left_join(CLs,NewCLs,by="RB")

write.csv(CLs,"Data/CLs.csv",row.names = F)

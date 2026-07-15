#This is the script to run - it calls the other scripts
#Some parameters have to be set here, others within the subscripts
# Install BERT:
# devtools::install_github("ccamlr/BERT", build_vignettes = TRUE)


#Timestamp to be added to file names
Time=Sys.time() 
Time=format(Time,"%d-%b-%Y")
Time=paste0(Time,"_V4") #Add suffix to files if desired (e.g. to compare results between GEBCO versions)
#V1: using FishableArea2024 and 5km buffers
#V2: using FishableArea2025 and 5km buffers
#V3: using FishableArea2025 and 1FSR buffers
#V4: using FishableArea2025 and 2FSR buffers

Exclc="883_12" #For V4 exclude them right before CPUE in 02_EstimateBiomass.R (@l.7)
Excl="5843a_1" #For V4 exclude them right before Chapman in 02_EstimateBiomass.R (@l.158)


#Get fishable areas 
RB_seabed_areaM=read.csv('Data/FishableArea2025.csv')

#Set Season of estimation
Est_Season=2025

#Set biomass and CV for Reference Areas 
HIMI_biomass_est=23485
HIMI_CV_biomass_est=0.0435
RSR_open_biomass_est=88594
RSR_open_CV_biomass_est=0.057

#List RBs in the proper order
RBsToDo=c("481_1","481_2","481_3",
          "482_N","482_S",
          "486_2","486_3","486_4","486_5",
          "5841_1","5841_2","5841_3","5841_4","5841_5","5841_6",
          "5842_1","5842_2",
          "5843a_1",
          "5844b_1","5844b_2",
          "882_1","882_2","882_3","882_4","882H",
          "883_1","883_2","883_3","883_4","883_5","883_6","883_7","883_8","883_9","883_10","883_11","883_12")



#List RBs that require catch advice (Wait until 1-June when all notifications are up)
RBsCAdv=c("482_N","482_S",
          "486_2","486_3","486_4","486_5",
          "5841_1","5841_2","5841_3","5841_4","5841_5","5841_6",
          "5842_1","5842_2",
          "882_1","882_2","882_3","882_4","882H",
          "883_1","883_2","883_3","883_4","883_6","883_11","883_12")

#Set number of bootstrap iterations
n_boot=10000

#Compare to GIS database
RBcheck=CCAMLRGIS::load_RBs()
if(all(RBsToDo%in%c(RBcheck$GAR_Short_Label,"882H"))==F){stop("Missing RB in RBsToDo")}
rm(RBcheck)

#Set RBs that are TOP target (all other RBS are TOA target)
TOP_target_RBs=c("5843a_1","5844b_1","5844b_2")

#Chose whether to include Quarantined data ("Y") or not ("N")
IncludeQ="N"

#Chose whether to output data extracts ("Y") or not ("N")
Output="N"

#1. Load data
source("01_LoadData.R")

#3. Estimate Biomasses
source("02_EstimateBiomass.R")

#4. Trend analysis
source("03_AnalyseTrends.R")

#5. Trend analysis history
source("04_TrendsHistory.R")

#6. Tagging data post-processing
source("05_TaggingData.R")

#7. Full CPUE estimates time series
source("06_CPUE_history.R")

#Optional: send notification of completion
PBtext="Trend Analysis Completed."
source("C:/Users/stephane/Desktop/CCAMLR/CODES/99 - PushBullet/PushBullet.R")

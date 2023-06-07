#This is the script to run - it calls the other scripts
#Some parameters have to be set here, others within the subscripts
# Install BERT:
# devtools::install_github("ccamlr/BERT", build_vignettes = TRUE)


#Timestamp to be added to file names
Time=Sys.time() 
Time=format(Time,"%d-%b-%Y")
Time=paste0(Time,"_V2") #Add suffix to files if desired (e.g. to compare results between GEBCO versions)
#V1: using FishableArea2021
#V2: using FishableArea2023

#Get fishable areas 
RB_seabed_areaM=read.csv('Data/FishableArea2023.csv')

#Set number of bootstrap iterations
n_boot=10000

#Set Season of estimation
Est_Season=2023

#Set biomass and CV for Reference Areas 
HIMI_biomass_est=31111 
HIMI_CV_biomass_est=0.0281
RSR_open_biomass_est=84260
RSR_open_CV_biomass_est=0.0581

#List RBs in the proper order
RBsToDo=c("481_1","481_2","481_3","482_N","482_S",
          "486_2","486_3","486_4","486_5",
          "5841_1","5841_2","5841_3","5841_4","5841_5","5841_6",
          "5842_1","5842_2",
          "5843a_1",
          "5844b_1","5844b_2",
          "882_1","882_2","882_3","882_4","882H",
          "883_1","883_2","883_3","883_4","883_5","883_6","883_7","883_8","883_9","883_10")
#List RBs that require catch advice
RBsCAdv=c("486_2","486_3","486_4","486_5",
          "5841_1","5841_2","5841_3","5841_4","5841_5","5841_6",
          "5842_1","5842_2",
          "882_1","882_2","882_3","882_4","882H",
          "883_1","883_2","883_3","883_4","883_5","883_6","883_7","883_8","883_9","883_10")

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

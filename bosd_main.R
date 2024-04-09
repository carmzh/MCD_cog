# Import data ----
# This is the master R script which is used to obtain main results for BOSD paper
rm(list=ls()) # clear the workspace

library(dplyr)
library(lubridate)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(effsize)
library(RColorBrewer)
library(tidyverse)
library(psych)
library(lsr)
library(rmarkdown)
library(knitr)
library(pandoc)


# work out which machine we are working from
if ( file.exists("/Users/ctailby/Library/CloudStorage/OneDrive-TheUniversityofMelbourne/CogLab_Projects/Manuscripts/BOSD/Datasets/BOSD Master Oct Edits DATES CORRECTED_sz freq-ASM_newAEPs_dyslexia_oldASMandFreq.csv") ) {
  # we are working from Chris's machine
  BOSD_folder = "/Users/ctailby/Library/CloudStorage/OneDrive-TheUniversityofMelbourne/CogLab_Projects/Manuscripts/BOSD/"
} else if ( file.exists("/Users/carmen/Library/CloudStorage/OneDrive-SharedLibraries-TheUniversityofMelbourne/Chris Tailby - CogLab_Projects/Manuscripts/BOSD") ) {
  # we are working from Carmen's machine
  BOSD_folder = "/Users/carmen/Library/CloudStorage/OneDrive-SharedLibraries-TheUniversityofMelbourne/Chris Tailby - CogLab_Projects/Manuscripts/BOSD"
}
# master_data = "Datasets/BOSD Master Oct Edits DATES CORRECTED.csv"
# master_data = "Datasets/BOSD Master Oct Edits DATES CORRECTED_sz freq-ASM.csv"
# master_data = "Datasets/BOSD Master Oct Edits DATES CORRECTED_sz freq-ASM_newAEPs.csv"
master_data = "Datasets/BOSD Master Oct Edits DATES CORRECTED_sz freq-ASM_newAEPs_dyslexia_oldASMandFreq.csv"

fname = file.path(BOSD_folder,master_data)
# note: Oct Edits master file: prior to this duration of epilepsy was calculated incorrectly, in this file is calculated 
#       as age at assessment minus age at onset, and therefore varies by assessment
# data = read.csv("/Users/Alana/The University of Melbourne/CogLab_Projects/BOSD/Datasets/BOSD Master 9 Sept 2020.csv")
data = read.csv(fname)

# Exclusion ----
# patients to exclude completely because they have an Intellectual Disability: 381790, 676625, 659192, 697080
data = data[-which(data$UR == '381790'),]
data = data[-which(data$UR == '676625'),]
data = data[-which(data$UR == '659192'),]
data = data[-which(data$UR == '697080'),]
# and exclude one case with no data
data = data[-which(data$UR == '2112652'),]
# exclude one case due to patient having had SAH which led to verbal memory drop before BOSD resection
data = data[-which(data$UR == "2172094"),]

delete.dyslexia=FALSE # set to TRUE if you want to omit dyslexia cases from the data set
if ( delete.dyslexia ) {
  dyslex.UR = unique(data[which(data$Dx_Dyslexia==1),"UR"])
  for ( UR in dyslex.UR) {
    dyslex.inds = which(data$UR==UR)
    data = data[-dyslex.inds,]
  }
} 

# convert date strings to date format using as.Date
# date of birth
data$DoB = as.Date(data$DoB,"%d/%m/%Y")
# date of assessment
data$Date_of_Ax = as.Date(data$Date_of_Ax,"%d/%m/%Y")
# date of surgeries
data$Date_of_surgery_1 = as.Date(data$Date_of_surgery_1,"%d/%m/%Y")
data$Date_of_surgery_2 = as.Date(data$Date_of_surgery_2,"%d/%m/%Y")
data$Date_of_surgery_3 = as.Date(data$Date_of_surgery_3,"%d/%m/%Y")
# dates medication and Sz frequency estimates sourced from
data$Sz_freq_souce_date = as.Date(data$Sz_freq_souce_date,"%d/%m/%Y")
data$ASM_source_date = as.Date(data$ASM_source_date,"%d/%m/%Y")

# calculate age at Ax
data$Age_at_Ax = time_length(difftime(data$Date_of_Ax,data$DoB),"years") 
data$Duration_of_epilepsy = data$Age_at_Ax-data$Age_at_epilepsy_onset

# now delete outliers on cognitive tests:
# UR 525628 has sig psychomotor retardation (e.g.) TMT A score of 165") but will leave in and use nonparametric
# Neuropsych report at time notes appropriate engagement and non-ID so retain
# data$Trails_A[which(data$Trails_A==165)] = NaN



# Norming ----
# start by sourcing the norming script, calls on the script that norms the raw scores
if ( file.exists("/Users/ctailby/Library/CloudStorage/OneDrive-TheUniversityofMelbourne/CogLab_Projects/Norms/raw_scores_to_norm_functions_v4.R") ) {
  # Chris's computer
  source("/Users/ctailby/Library/CloudStorage/OneDrive-TheUniversityofMelbourne/CogLab_Projects/Norms/raw_scores_to_norm_functions_v4.R")
  norm.path = "/Users/ctailby/OneDrive - The University of Melbourne/CogLab_Projects/Norms"
} else if ( file.exists("/Users/carmen/The University of Melbourne/Chris Tailby - CogLab_Projects/Norms/raw_scores_to_norm_functions_v4.R") ) {
  # Carmen's computer
  source("/Users/carmen/Library/CloudStorage/OneDrive-SharedLibraries-TheUniversityofMelbourne/Chris Tailby - CogLab_Projects/Norms/raw_scores_to_norm_functions_v4.R")
  norm.path = "/Users/carmen/Library/CloudStorage/OneDrive-SharedLibraries-TheUniversityofMelbourne/Chris Tailby - CogLab_Projects/Norms"
}
## RAVLT
# assign an empty B1 score (the norm function requires a list B score to be passed in; 
#   we don't have one so just set to NaN for everybody)
data$RAVLT_B1 = NaN
# define RAVLT columns in the data frame to pass to the norm function
ravlt.fields = list(A1_field = "RAVLT_T1", 
                    A2_field = "RAVLT_T2", 
                    A3_field = "RAVLT_T3", 
                    A4_field = "RAVLT_T4", 
                    A5_field = "RAVLT_T5", 
                    A1toA5_field = "RAVLT_1to5", 
                    B1_field = "RAVLT_B1", 
                    A6_field = "RAVLT_A6", 
                    A7_field = "RAVLT_A7", 
                    age_field = "Age_at_Ax", 
                    id_field = "UR", 
                    A1_field_z = "RAVLT_A1_z", 
                    A2_field_z = "RAVLT_A2_z", 
                    A3_field_z = "RAVLT_A3_z", 
                    A4_field_z = "RAVLT_A4_z", 
                    A5_field_z = "RAVLT_A5_z", 
                    A1toA5_field_z = "RAVLT_1to5_z", 
                    B1_field_z = "RAVLT_B1_z", 
                    A6_field_z = "RAVLT_A6_z", 
                    A7_field_z = "RAVLT_Delay_z")
data = RAVLT_raw_to_norm_schmidt(data,ravlt.fields,norm.path)
# convert RAVLT z-score to age scaled score
data$RAVLT_1to5_SS = data$RAVLT_1to5_z*3+10
data$RAVLT_Delay_SS = data$RAVLT_Delay_z*3+10
# and rename the RAVLT raw score column with the suffix RS 
#   (i.e. our naming convention will be to use the suffix '_RS' for raw scores
#   and'_SS' for age scaled scores)
names(data)[names(data) == 'RAVLT_1to5'] <- 'RAVLT_1to5_RS'
names(data)[names(data) == 'RAVLT_A7'] <- 'RAVLT_Delay_RS'

## TMT
# assume level of education is 12 years (for norming purposes)
data$level_of_ed = NaN
tmt.a.fields = list(raw_score_field = "Trails_A", 
                    id_field = "UR",
                    age_field = "Age_at_Ax", 
                    edu_field = "level_of_ed", 
                    z_score_field = "Trails_A_z")
# run the norm function
data = written_tmta_raw_to_norm_tombaugh(data,tmt.a.fields,norm.path)
# convert TMT A z-score to age scaled score
data$Trails_A_SS = data$Trails_A_z*3+10
# and rename the Trails_A raw score column with the suffix RS
names(data)[names(data) == 'Trails_A'] <- 'Trails_A_RS'
# and now norm TMT B
tmt.b.fields = list(raw_score_field = "Trails_B", 
                    id_field = "UR",
                    age_field = "Age_at_Ax", 
                    edu_field = "level_of_ed", 
                    z_score_field = "Trails_B_z")
data = written_tmtb_raw_to_norm_tombaugh(data,tmt.b.fields,norm.path)
# convert TMT B z-score to age scaled score
data$Trails_B_SS = data$Trails_B_z*3+10
# and rename the Trails_B raw score column with the suffix RS
names(data)[names(data) == 'Trails_B'] <- 'Trails_B_RS'

## FAS
fas.fields = list(raw_score_field = "FAS_total", 
                  id_field = "UR",
                  age_field = "Age_at_Ax", 
                  z_score_field = "FAS_total_z")
data = cowat_raw_to_norm_mitrushina(data,fas.fields,norm.path)
# convert FAS z-score to age scaled score
data$FAS_total_SS = data$FAS_total_z*3+10
# and rename the FAS raw score column with the suffix RS
names(data)[names(data) == 'FAS_total'] <- 'FAS_total_RS'

## Animals
animals.fields = list(raw_score_field = "Animals_RS",
                      id_field = "UR",
                      age_field = "Age_at_Ax",
                      edu_field = 12,
                      z_score_field = "Animals_z")
data = animals_raw_to_norm_mitrushina(data,animals.fields,norm.path)

# RCF Copy
rcf.copy.fields = list(raw_score_field = "RCF_Copy_RS", 
                       id_field = "UR",
                       age_field = "Age_at_Ax", 
                       z_score_field = "RCF_Copy_z")
data = RCF_copy_to_norm_spreen(data,rcf.copy.fields,norm.path)
# convert RCF copy z-score to age scaled score
data$RCF_Copy_SS = data$RCF_Copy_z*3+10

# and norm the RCF Delay
rcf.delay.fields = list(raw_score_field = "RCF_Delay_RS", 
                        id_field = "UR",
                        age_field = "Age_at_Ax", 
                        z_score_field = "RCF_Delay_z")
data = RCF_delay_to_norm_spreen(data,rcf.delay.fields,norm.path)
# convert RCF copy z-score to age scaled score
data$RCF_Delay_SS = data$RCF_Delay_z*3+10

# BNT
bnt.fields = list(raw_score_field = "BNT_RS", 
                  id_field = "UR",
                  age_field = "Age_at_Ax", 
                  z_score_field = "BNT_z")
data = bnt_raw_to_norm_mitrushina(data,bnt.fields,norm.path)
# convert BNT z-score to age scaled score
data$BNT_SS = data$BNT_z*3+10


# convert WTAR scores
data$WTAR_IQ = data$WTAR_SS
# convert WTAR FSIQ (mean = 100, SD = 15) score to WTAR SS
data$WTAR_SS = ((data$WTAR_IQ-100)/15)*3+10
# convert WTAR FSIQ (mean = 100, SD = 15) score to WTAR z-score
data$WTAR_z = ((data$WTAR_IQ-100))/15

# convert DSpan to z
data$Dspan_z = (data$Dspan_SS-10)/3

# convert DSCd to z
data$DSCd_z = (data$DSCd_SS-10)/3

# convert BD to z
data$BD_z = (data$BD_SS-10)/3

# convert MR to z
data$MR_z = (data$MR_SS-10)/3

# convert Pcomp to z
data$Pcomp_z = (data$Pcomp_SS-10)/3

# convert Vocab to z
data$Vocab_z = (data$Vocab_SS-10)/3

# convert Simi to z
data$Simi_z = (data$Simi_SS-10)/3

# convert Info to z
data$Info_z = (data$Info_SS-10)/3

# convert Comp to z
data$Comp_z = (data$Comp_SS-10)/3

# convert Arith to z
data$Arith_z = (data$Arith_SS-10)/3

# convert SS to z
data$SS_z = (data$SS_SS-10)/3

# convert Cod to z
data$Cod_z = (data$Cod_SS-10)/3

# convert LN to z
data$LN_z = (data$LN_SS-10)/3

# convert VCI to z
data$VCI_z = (data$VCI-100)/15

# convert POI to z
data$POI_z = (data$POI-100)/15

# convert PRI to z
data$PRI_z = (data$PRI-100)/15

# convert PSI to z
data$PSI_z = (data$PSI-100)/15

# convert WMI to z
data$WMI_z = (data$WMI-100)/15

# convert VR1 to z
data$VR1_z = (data$VR1_SS-100)/15

# convert VR2 to z
data$VR2_z = (data$VR2_SS-100)/15

# convert LM1 to z
data$LM1_z = (data$LM1_SS-100)/15

# convert LM2 to z
data$LM2_z = (data$LM2_SS-100)/15

# get each unique UR and Last name
unique_UR = unique(data$UR)
unique_Lastname = unique(data$Last_Name)

# Specify tests ----
# these are the test names without the _RS or _SS suffix
tests_to_get = c("DSCd","Dspan","BD","Simi", "Trails_B","FAS_total", "BNT","RAVLT_1to5","RAVLT_Delay","RCF_Delay","WTAR")

# additional_tests = c("Vocab", "Simi", "Info", "Comp", "Arith", "SS", "Cod", "LN", "LM1") # will need to add to this list
#tests_to_get = c(tests_to_get,additional_tests)



# Develop init.data ----
# initialise empty data frame to store single instance of initial assessment data in
init.data = data.frame()

# loop through each participant and extract pre-op scores
for (UR in unique_UR) {
  # initialise empty data frame for this participant
  t_data = data.frame()
  # find all instances of this UR in the full data set
  UR.inds = which(data$UR == UR)
  # fill some info
  t_data[1,'UR'] = data$UR[UR.inds[1]]
  t_data[1,'First_Name'] = data$First_Name[UR.inds[1]]
  t_data[1,'Last_Name'] = data$Last_Name[UR.inds[1]]
  t_data[1,'Date_of_birth'] = data$DoB[UR.inds[1]]
  t_data[1,'Age_at_epilepsy_onset'] = data$Age_at_epilepsy_onset[UR.inds[1]]
  t_data[1,'Hemisphere_BOSD'] = data$Hemisphere_BOSD[UR.inds[1]]
  t_data[1,'Lobe_of_BOSD'] = data$Lobe_of_BOSD[UR.inds[1]]
  t_data[1,'Gender'] = data$Gender[UR.inds[1]]
  t_data[1,'Date_of_surgery_1'] = data$Date_of_surgery_1[UR.inds[1]]
  t_data[1,'Dx_Dyslexia'] = data$Dx_Dyslexia[UR.inds[1]]
  
  # now fill the rest of the init.data data frame with data from the tests we are interested in
  for (test in tests_to_get) {
    # initialise SS and RS var names
    SS_test = paste(test,"_SS",sep="") 
    z_test = paste(test,"_z",sep="") 
    RS_test = paste(test,"_RS",sep="") 
    # initialise age, duration and date at assessment var names
    age_test = paste(test,"_Age_at_Ax",sep="") 
    dur_test = paste(test,"_Dur_at_Ax",sep="")
    date_ax_test = paste(test,"_Date_of_Ax",sep="")
    asm_num_test = paste(test,"_ASM_num",sep="")
    asm_date_test = paste(test,"_ASM_date",sep="")
    sz_freq_test = paste(test,"_Sz_freq_num",sep="")
    sz_date_test = paste(test,"_Sz_freq_date",sep="")
    
    # find instances of this test that have scaled scores that are not NaN (and therefore the test was administered)
    use_ind = which(!is.na(data[UR.inds,SS_test])) 
    
    if ( length(use_ind) > 0 ) {
      # find the earliest assessment, based on the "Age_at_Ax" variable
      use_ind = use_ind[which.min(data$Age_at_Ax[UR.inds[use_ind]])]
      
      # now check that this assessment date is prior to the first recorded surgery
      if ( is.na(data[UR.inds[use_ind], "Date_of_surgery_1"]) ) {
        # person has not had surgery, so empty use_ind
        # use_ind = c() # originally we deleted a case if they had not had surgery, but this is inappropriate - line commented out on 17 March 2021 [ct]
      }  else {
        if ( data[UR.inds[use_ind],"Date_of_surgery_1"] < data[UR.inds[use_ind],"Date_of_Ax"] ) {
          # this is a post-op case, so empty use_ind
          use_ind = c()
        }
      }
    }
    
    if ( length(use_ind) > 0 ) {
      if ( length(use_ind) == 1 ) {
        # store the scaled score
        t_data[1,SS_test] = data[UR.inds[use_ind],SS_test]
        # store the z score
        t_data[1,z_test] = data[UR.inds[use_ind],z_test]
        # copy in the raw score too
        t_data[1,RS_test] = data[UR.inds[use_ind],RS_test]
        # and store age at test, duration of epilepsy at test and date of assessment
        t_data[1,age_test] = data[UR.inds[use_ind],"Age_at_Ax"]
        t_data[1,dur_test] = data[UR.inds[use_ind],"Duration_of_epilepsy"]
        t_data[1,date_ax_test] = data[UR.inds[use_ind],"Date_of_Ax"]
        t_data[1,asm_num_test] = data[UR.inds[use_ind],"ASM_number"]
        t_data[1,asm_date_test] = data[UR.inds[use_ind],"ASM_source_date"]
        t_data[1,sz_freq_test] = data[UR.inds[use_ind],"Sz_freq_per_month"]
        t_data[1,sz_date_test] = data[UR.inds[use_ind],"Sz_freq_souce_date"]
      } 
    } else {
      t_data[1,SS_test] = NaN
      t_data[1,z_test] = NaN
      t_data[1,RS_test] = NaN
      t_data[1,age_test] = NaN
      t_data[1,dur_test] = NaN
      t_data[1,date_ax_test] = as.Date(NaN,origin = "0001-01-01")
      t_data[1,asm_num_test] = NaN
      t_data[1,asm_date_test] = as.Date(NaN,origin = "0001-01-01")
      t_data[1,sz_freq_test] = NaN
      t_data[1,sz_date_test] = as.Date(NaN,origin = "0001-01-01")
    }
  }
  if ( length(init.data) == 0 ) {
    init.data = t_data } else {
      init.data = rbind(init.data,t_data)
    }
}


df.cols = names(init.data)
tests_SS = df.cols[grep('_SS',df.cols)]
tests_z = df.cols[grep('_z',df.cols)]
# get columns used to calculate epileptological variables, e.g. Age at assessment, duration of epilepsy at assessment, etc.
tests_Age_at_Ax = df.cols[grep('Age_at_Ax',df.cols)]
tests_Dur_at_Ax = df.cols[grep('Dur_at_Ax',df.cols)]
tests_ASM_at_Ax = df.cols[grep('ASM_num',df.cols)]
tests_Sz_freq_num = df.cols[grep('Sz_freq_num', df.cols)]


# Develop pre.post.data----
# initialise empty data frame to store single instance of preop assessment data in
pre.post.data = data.frame()

# loop through each participant and extract pre-op scores
# calculate age at surgery
data$Age_surgery_1 = time_length(difftime(data$Date_of_surgery_1,data$DoB),"years") 
data$Age_surgery_2 = time_length(difftime(data$Date_of_surgery_2,data$DoB),"years") 
data$Age_surgery_3 = time_length(difftime(data$Date_of_surgery_3,data$DoB),"years")

for (UR in unique_UR) {
  # initialise empty data frame for this (or ith) participant at pre-op
  t_data_prepost = data.frame()
  # find all instances of this UR in the full data set, to be used in preop.data
  UR.inds = which(data$UR == UR)
  # UR.inds now stores position (row, col) of cells which satisfy the condition (data$UR == UR being looped)
  
  # fill some info
  t_data_prepost[1,'UR'] = data$UR[UR.inds[1]]
  t_data_prepost[1,'First_Name'] = data$First_Name[UR.inds[1]]
  t_data_prepost[1,'Last_Name'] = data$Last_Name[UR.inds[1]]
  t_data_prepost[1,'Date_of_birth'] = data$DoB[UR.inds[1]]
  t_data_prepost[1,'Age_at_epilepsy_onset'] = data$Age_at_epilepsy_onset[UR.inds[1]]
  t_data_prepost[1,'Hemisphere_BOSD'] = data$Hemisphere_BOSD[UR.inds[1]]
  t_data_prepost[1,'Lobe_of_BOSD'] = data$Lobe_of_BOSD[UR.inds[1]]
  t_data_prepost[1,'Gender'] = data$Gender[UR.inds[1]]
  t_data_prepost[1,'Date_of_surgery_1'] = data$Date_of_surgery_1[UR.inds[1]]
  t_data_prepost[1,'Date_of_surgery_2'] = data$Date_of_surgery_2[UR.inds[1]]
  t_data_prepost[1,'Date_of_surgery_3'] = data$Date_of_surgery_3[UR.inds[1]]
  t_data_prepost[1,'Dx_Dyslexia'] = data$Dx_Dyslexia[UR.inds[1]]
  
  # now fill the rest of the preop.data data frame with data from the tests we are interested in
  for (test in c(tests_to_get)) {
    # clear the use.pre.op.ax and use.post.op.ax variables
    use.pre.op.ax = vector()
    use.post.op.ax = vector()
    
    # initialise SS_test_pre-op, z_test_preop, and RS_test_preop var names
    SS_test = paste0(test,"_SS") 
    z_test = paste0(test,"_z") 
    RS_test = paste0(test,"_RS") 
    # initialise age, duration and date at assessment var names
    age_test = paste0(test,"_Age_at_Ax") 
    dur_test = paste0(test,"_Dur_at_Ax")
    date_ax_test = paste0(test,"_Date_of_Ax")
    
    # create pre and post variables of the above
    pre_SS_test = paste0("pre_",SS_test) 
    pre_z_test = paste0("pre_",z_test) 
    pre_RS_test = paste0("pre_",RS_test) 
    pre_age_test = paste0("pre_",age_test) 
    pre_dur_test = paste0("pre_",dur_test)
    pre_date_ax_test = paste0("pre_",date_ax_test)
    pre_sz_freq_test = paste0("pre_",test,"_sz_freq")
    pre_n_meds_test = paste0("pre_",test,"_n_meds")
    
    post_SS_test = paste0("post_",SS_test) 
    post_z_test = paste0("post_",z_test) 
    post_RS_test = paste0("post_",RS_test) 
    post_age_test = paste0("post_",age_test) 
    post_dur_test = paste0("post_",dur_test)
    post_date_ax_test = paste0("post_",date_ax_test)
    post_sz_freq_test = paste0("post_",test,"_sz_freq")
    post_n_meds_test = paste0("post_",test,"_n_meds")
    
    # and difference score data
    post_min_pre_SS_test = paste0("post_min_pre_",SS_test) 
    post_min_pre_z_test = paste0("post_min_pre_",z_test) 
    post_min_pre_RS_test = paste0("post_min_pre_",RS_test) 
    post_min_pre_age_test = paste0("post_min_pre_",age_test) 
    
    
    
    # find instances of this test that have scaled scores that are not NaN (and therefore the test was administered)
    has.test = which(!is.na(data[UR.inds,SS_test])) 
    
    # initialise empty date columns
    t_data_prepost[1,pre_date_ax_test] = as.Date("1000-01-01")
    t_data_prepost[1,post_date_ax_test] = as.Date("1000-01-01")
    
    if ( length(has.test) == 0 ) {
      # this test was not administered on any assessment, so skip to next iteration of test loop
      t_data_prepost[1,pre_SS_test] = NA
      t_data_prepost[1,pre_z_test] = NA
      t_data_prepost[1,pre_RS_test] = NA
      t_data_prepost[1,pre_age_test] = NA
      t_data_prepost[1,pre_dur_test] = NA
      t_data_prepost[1,pre_date_ax_test] = NA
      t_data_prepost[1,pre_sz_freq_test] = NA
      t_data_prepost[1,pre_n_meds_test] = NA
      
      t_data_prepost[1,post_SS_test] = NA
      t_data_prepost[1,post_z_test] = NA
      t_data_prepost[1,post_RS_test] = NA
      t_data_prepost[1,post_age_test] = NA
      t_data_prepost[1,post_dur_test] = NA
      t_data_prepost[1,post_date_ax_test] = NA
      t_data_prepost[1,post_sz_freq_test] = NA
      t_data_prepost[1,post_n_meds_test] = NA
      
      t_data_prepost[1,post_min_pre_SS_test] = NA
      t_data_prepost[1,post_min_pre_z_test] = NA
      t_data_prepost[1,post_min_pre_RS_test] = NA
      t_data_prepost[1,post_min_pre_age_test] = NA
      next
    } else {
      ## pre-op ----
      ## (relative to first surgery) 
      yrs.re.1st.surg = time_length(difftime(data$Date_of_Ax[UR.inds[has.test]],data$Date_of_surgery_1[UR.inds[1]]),"years") 
      # pre-op Ax is stored as negative time, find Ax that are pre-op
      pre.op.axs = which(yrs.re.1st.surg < 0)
      
      if (length(pre.op.axs) > 0) {
        # find the pre-op Ax closest to the surgery 
        t.ind = which.max(yrs.re.1st.surg[pre.op.axs])
        use.pre.op.ax = UR.inds[has.test[pre.op.axs[t.ind]]] # use this index to populate pre-op data
      }
      
      # now override for certain cases for which we don't have a surgically naive Npsych Ax
      # but for which we do have neuropsych Axs before and after the last surgery
      if ( t_data_prepost[1,'UR'] == 2172094 ) {
        use.pre.op.ax = UR.inds[has.test[which(time_length(difftime(data$Date_of_Ax[UR.inds[has.test]],data$Date_of_surgery_2[UR.inds[1]]),"years")  < 0 )]]
      }
      
      ## post-op ----
      # (relative to last surgery) 
      # first check if three surgeries
      if (!is.na(data$Date_of_surgery_3[UR.inds[1]])) {
        # participant had three surgeries, record date of the third (last surgery)
        date.last.surg = data$Date_of_surgery_3[UR.inds[1]]
      } else if (!is.na(data$Date_of_surgery_2[UR.inds[1]])) {
        # participant had two surgeries, record date of the second (last surgery)
        date.last.surg = data$Date_of_surgery_2[UR.inds[1]]
      } else if (!is.na(data$Date_of_surgery_1[UR.inds[1]])) {
        # participant had one surgery, record date of the first (last surgery)
        date.last.surg = data$Date_of_surgery_1[UR.inds[1]]
      } else { date.last.surg = NA }
      
      days.re.last.surg = time_length(difftime(data$Date_of_Ax[UR.inds[has.test]],date.last.surg),"days") 
      # post-op Ax is stored as postive time, find Ax that are post-op
      post.op.axs = which(days.re.last.surg > 60) # at least 60 days post-op
      
      if (length(post.op.axs) > 0) {
        # find the first post-op Ax provided 60 days post surgery minimum 
        t.ind = which.min(days.re.last.surg[post.op.axs])
        use.post.op.ax = UR.inds[has.test[post.op.axs[t.ind]]] # use this index to populate post-op data
      }
      
      # now start populating pre and post data
      # first populate the pre-op data
      if ( length(use.pre.op.ax) == 1 ) {
        t_data_prepost[1,pre_SS_test] = data[use.pre.op.ax,SS_test]
        t_data_prepost[1,pre_z_test] = data[use.pre.op.ax,z_test]
        t_data_prepost[1,pre_RS_test] = data[use.pre.op.ax,RS_test]
        t_data_prepost[1,pre_age_test] = data[use.pre.op.ax,"Age_at_Ax"]
        t_data_prepost[1,pre_dur_test] = data[use.pre.op.ax,"Duration_of_epilepsy"]
        t_data_prepost[1,pre_date_ax_test] = data[use.pre.op.ax,"Date_of_Ax"]
        t_data_prepost[1,pre_sz_freq_test] = data[use.pre.op.ax,"Sz_freq_per_month"]
        t_data_prepost[1,pre_n_meds_test] = data[use.pre.op.ax,"ASM_number"]
      } else {
        # no pre-op data so fill with NA
        t_data_prepost[1,pre_SS_test] = NA
        t_data_prepost[1,pre_z_test] = NA
        t_data_prepost[1,pre_RS_test] = NA
        t_data_prepost[1,pre_age_test] = NA
        t_data_prepost[1,pre_dur_test] = NA
        t_data_prepost[1,pre_date_ax_test] = NA
        t_data_prepost[1,pre_sz_freq_test] = NA
        t_data_prepost[1,pre_n_meds_test] = NA
      }
      
      # now populate the post-op data
      if ( length(use.post.op.ax) == 1 ) {
        t_data_prepost[1,post_SS_test] = data[use.post.op.ax,SS_test]
        t_data_prepost[1,post_z_test] = data[use.post.op.ax,z_test]
        t_data_prepost[1,post_RS_test] = data[use.post.op.ax,RS_test]
        t_data_prepost[1,post_age_test] = data[use.post.op.ax,"Age_at_Ax"]
        t_data_prepost[1,post_dur_test] = data[use.post.op.ax,"Duration_of_epilepsy"]
        t_data_prepost[1,post_date_ax_test] = data[use.post.op.ax,"Date_of_Ax"]
        t_data_prepost[1,post_sz_freq_test] = data[use.post.op.ax,"Sz_freq_per_month"]
        t_data_prepost[1,post_n_meds_test] = data[use.post.op.ax,"ASM_number"]
      } else {
        # no post-op data so fill with NA
        t_data_prepost[1,post_SS_test] = NA
        t_data_prepost[1,post_z_test] = NA
        t_data_prepost[1,post_RS_test] = NA
        t_data_prepost[1,post_age_test] = NA
        t_data_prepost[1,post_dur_test] = NA
        t_data_prepost[1,post_date_ax_test] = NA
        t_data_prepost[1,post_sz_freq_test] = NA
        t_data_prepost[1,post_n_meds_test] = NA
      }
    }
    # calculate post minus pre values
    t_data_prepost[1,post_min_pre_SS_test] = t_data_prepost[1,post_SS_test] - t_data_prepost[1,pre_SS_test]
    t_data_prepost[1,post_min_pre_z_test] = t_data_prepost[1,post_z_test] - t_data_prepost[1,pre_z_test]
    t_data_prepost[1,post_min_pre_RS_test] = t_data_prepost[1,post_RS_test] - t_data_prepost[1,pre_RS_test]
    t_data_prepost[1,post_min_pre_age_test] = t_data_prepost[1,post_age_test] - t_data_prepost[1,pre_age_test]
  }
  
  
  # now concatenate pre- & post-op data
  pre.post.data = rbind(pre.post.data,t_data_prepost)
}

# RESULTS ----
## Demographics ----
# by subject mean first: use this in table
# (ie. when calculating average age at baseline, calculate average age across all test administered & analysed[horizontal] within each patient, then average such subject-level mean[vertical])
demographics.check.cols = list(tests_Age_at_Ax,
                               tests_Dur_at_Ax,
                               tests_ASM_at_Ax,
                               tests_Sz_freq_num)
# store demographics in a data frame, initialised on the next line
dem.df = data.frame(matrix(NA, ncol = 7, nrow = 4))
rownames(dem.df) = c("Age","Dur","ASM","SzFreq")
colnames(dem.df) = c("mean","median","SD","IQR","min","max","count")
counter = 0
for ( this.var in demographics.check.cols ) {
  counter = counter + 1
  dem.df[counter,1] = (mean(rowMeans(init.data[,this.var], na.rm=TRUE), na.rm=TRUE))
  dem.df[counter,2] = (median(rowMeans(init.data[,this.var], na.rm=TRUE), na.rm=TRUE))
  dem.df[counter,3] = (sd(rowMeans(init.data[,this.var], na.rm=TRUE), na.rm=TRUE))
  dem.df[counter,4] = (IQR(rowMeans(init.data[,this.var], na.rm=TRUE), na.rm=TRUE))
  dem.df[counter,5] = (min(rowMeans(init.data[,this.var], na.rm=TRUE), na.rm=TRUE))
  dem.df[counter,6] = (max(rowMeans(init.data[,this.var], na.rm=TRUE), na.rm=TRUE))
  dem.df[counter,7] = length(which(!is.na(rowMeans(init.data[,this.var], na.rm=TRUE))))
}
print(dem.df)

# Age at epilepsy onset
mean((init.data$Age_at_epilepsy_onset), na.rm=TRUE)
median((init.data$Age_at_epilepsy_onset), na.rm=TRUE)
sd((init.data$Age_at_epilepsy_onset), na.rm=TRUE)
IQR((init.data$Age_at_epilepsy_onset), na.rm=TRUE)
min((init.data$Age_at_epilepsy_onset), na.rm=TRUE)
max((init.data$Age_at_epilepsy_onset), na.rm=TRUE)

# Total count of Hemisphere by Lobe (irrespective of test)
table(init.data$Lobe_of_BOSD,init.data$Hemisphere_BOSD)



## Boxplot: baseline cog z-scores----
tests_z = df.cols[grep('_z',df.cols)]
init.data.melt.z = melt(init.data,
                        id.vars = c("UR","Lobe_of_BOSD","Hemisphere_BOSD"), 
                        measure.vars = tests_z, 
                        variable.name = "measure",
                        value.name = "z.score")
# set measure order for plotting
init.data.melt.z$measure = factor(init.data.melt.z$measure, 
                                  levels = (c("WTAR_z", 
                                              "Simi_z", 
                                              "BD_z", 
                                              "DSCd_z", 
                                              "Trails_B_z", 
                                              "Dspan_z", 
                                              "FAS_total_z", 
                                              "BNT_z", 
                                              "RAVLT_1to5_z", 
                                              "RAVLT_Delay_z", 
                                              "RCF_Delay_z" )))

# since cannot make boxplot do single sample t-test against zero, using SS scores instead
# https://www.rdocumentation.org/packages/ggpubr/versions/0.4.0/topics/stat_compare_means
tests_SS = df.cols[grep('_SS',df.cols)]
init.data.melt.SS = melt(init.data,
                         id.vars = c("UR","Lobe_of_BOSD","Hemisphere_BOSD"), 
                         measure.vars = tests_SS, 
                         variable.name = "measure",
                         value.name = "standard.score")
# re-order the levels
init.data.melt.SS$measure = factor(init.data.melt.SS$measure, 
                                   levels = (c("WTAR_SS", 
                                               "Simi_SS", 
                                               "BD_SS", 
                                               "DSCd_SS", 
                                               "Trails_B_SS", 
                                               "Dspan_SS", 
                                               "FAS_total_SS", 
                                               "BNT_SS", 
                                               "RAVLT_1to5_SS", 
                                               "RAVLT_Delay_SS", 
                                               "RCF_Delay_SS" )))
new.x.axis = gsub("_SS","",levels(init.data.melt.SS$measure))

ggplot(data = subset(init.data.melt.SS, !is.na(standard.score)), 
       aes(x = measure, y = standard.score, fill = measure)) + 
  geom_boxplot(show.legend = FALSE, outlier.shape = NA, width = 0.35) + 
  scale_fill_brewer(palette = "Set3")+
  theme(axis.text.x = element_text(angle = 60)) +
  geom_jitter(shape = 20, width = 0.15,show.legend = FALSE, size = 2) +
  theme(panel.background = element_blank(),
        panel.grid = element_line(size = 0.2, linetype = 2, color = "grey"))+
  scale_y_continuous(limits=c(-10,20))+
  geom_hline(yintercept = 10) +
  stat_compare_means(mapping = NULL, # stat compare means doesn't work with ref.group = 0 so have to operate on SS scores
                     # note also stat compare doesn't correct for multiple comparisons (this was done separately for Table 3)
                     method = "t.test",
                     paired = FALSE,
                     method.args = list(alternative = "two.sided"),
                     ref.group = 10,
                     hide.ns = TRUE,
                     label = "p.signif") + 
  scale_x_discrete("measure", labels = c("WTAR", "Simi", "BD", "DSCd", "Trails_B", "Dspan", "FAS_total", "BNT", "RAVLT_1to5", "RAVLT_Delay", "RCF_Delay")) +
  scale_y_continuous("z-score", breaks = c(-14, -11, -8, -5, -2, 1, 4, 7, 10, 13, 16), 
                     labels = (c(-14, -11, -8,-5,-2, 1, 4, 7, 10, 13, 16)-10)/3) # hack to convert from SS to z

# and for z-score inputs (with y-axis clipped)
ggplot(data = subset(init.data.melt.z, !is.na(z.score)), 
       aes(x = measure, y = z.score, fill = measure)) + 
  geom_boxplot(show.legend = FALSE, outlier.shape = NA, width = 0.35) + 
  scale_fill_brewer(palette="Set3") +
  theme(axis.text.x = element_text(angle = 60)) +
  geom_jitter(shape = 20, width = 0.15,show.legend = FALSE, size = 2) +
  theme(panel.background = element_blank(),
        panel.grid = element_line(size = 0.2, linetype = 2, color = "grey"))+
  coord_cartesian(ylim = c(-4,2.5)) +
  # scale_y_continuous(limits=c(-4,2.5)) + # this line will introduce NaNs for y-values outside this range, which in turn will induce a non-finite values error in ggplot2
  geom_hline(yintercept = 0) +
  scale_x_discrete("measure", labels = c("WTAR", "Simi", "BD", "DSCd", "Trails_B", "Dspan", "FAS_total", "BNT", "RAVLT_1to5", "RAVLT_Delay", "RCF_Delay"))

## ASM & Sz freq effects on impaired domains----
### DSCd----
ggscatter(init.data, x = "DSCd_ASM_num", y = "DSCd_z",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman")

ggscatter(init.data, x = "DSCd_Sz_freq_num", y = "DSCd_z",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman")

### BNT----
ggscatter(init.data, x = "BNT_ASM_num", y = "BNT_z",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman")

ggscatter(init.data, x = "BNT_Sz_freq_num", y = "BNT_z",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman")

### TMT-b----
ggscatter(init.data, x = "Trails_B_ASM_num", y = "Trails_B_z",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman")

ggscatter(init.data, x = "Trails_B_Sz_freq_num", y = "Trails_B_z",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman")

### Dspan----
ggscatter(init.data, x = "Dspan_ASM_num", y = "Dspan_z",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman")

ggscatter(init.data, x = "Dspan_Sz_freq_num", y = "Dspan_z",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman")

### FAS----
ggscatter(init.data, x = "FAS_total_ASM_num", y = "FAS_total_z",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman")

ggscatter(init.data, x = "FAS_total_Sz_freq_num", y = "FAS_total_z",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman")
glm(FAS_total_z ~ FAS_total_ASM_num, data = init.data, family = "gaussian")

### RAVLT immediate----
ggscatter(init.data, x = "RAVLT_1to5_ASM_num", y = "RAVLT_1to5_z",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman")

ggscatter(init.data, x = "RAVLT_1to5_Sz_freq_num", y = "RAVLT_1to5_z",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman")


## Admin: Lat & lobe----
# get counts for each test, by left and right cases, and test whether administration of the test
# depended upon laterality of the BOSD
lat.by.test.admin.depend = data.frame(test = rep(NaN,length(tests_z)), 
                                      p.chisq = rep(NaN,length(tests_z)), 
                                      p.chisq.fdr = rep(NaN,length(tests_z)), 
                                      p.fisher = rep(NaN,length(tests_z)),
                                      p.fisher.fdr = rep(NaN,length(tests_z)))
counter = 0
for ( this.test in tests_z ) {
  counter = counter + 1
  tmp.var.name = paste0("has.",this.test)
  init.data[,tmp.var.name] = !is.na(init.data[,this.test])
  print(tmp.var.name)
  print(table(init.data[,c("Hemisphere_BOSD",tmp.var.name)]))
  print(tmp.var.name)
  print(chisq.test(table(init.data[,c("Hemisphere_BOSD",tmp.var.name)])))
  lat.by.test.admin.depend$p.chisq[counter] = chisq.test(table(init.data[,c("Hemisphere_BOSD",tmp.var.name)]))$p.value
  lat.by.test.admin.depend$p.fisher[counter] = fisher.test(table(init.data[,c("Hemisphere_BOSD",tmp.var.name)]))$p.value
  lat.by.test.admin.depend$test[counter] = this.test
}
# and calculate adjusted p-values
lat.by.test.admin.depend$p.fisher.fdr = p.adjust(lat.by.test.admin.depend$p.fisher, method = "fdr")
lat.by.test.admin.depend$p.chisq.fdr = p.adjust(lat.by.test.admin.depend$p.chisq, method = "fdr")


# get counts for each test, by frontal and parietal cases, and test whether administration of the test
# depended upon lobe of the BOSD
lobe.by.test.admin.depend = data.frame(test = rep(NaN,length(tests_z)), 
                                       p.chisq = rep(NaN,length(tests_z)), 
                                       p.chisq.fdr = rep(NaN,length(tests_z)), 
                                       p.fisher = rep(NaN,length(tests_z)),
                                       p.fisher.fdr = rep(NaN,length(tests_z)))
counter = 0
for ( this.test in tests_z ) {
  counter = counter + 1
  tmp.var.name = paste0("has.",this.test)
  init.data[,tmp.var.name] = !is.na(init.data[,this.test])
  print(tmp.var.name)
  print(table(init.data[which(init.data$Lobe_of_BOSD=="Frontal"|init.data$Lobe_of_BOSD=="Parietal"),c("Lobe_of_BOSD",tmp.var.name)]))
  print(tmp.var.name)
  # print(chisq.test(table(init.data[which(init.data$Lobe_of_BOSD=="Frontal"|init.data$Lobe_of_BOSD=="Parietal"),c("Hemisphere_BOSD",tmp.var.name)])))
  print(fisher.test(table(init.data[which(init.data$Lobe_of_BOSD=="Frontal"|init.data$Lobe_of_BOSD=="Parietal"),c("Hemisphere_BOSD",tmp.var.name)])))
  lobe.by.test.admin.depend$p.chisq[counter] = chisq.test(table(init.data[which(init.data$Lobe_of_BOSD=="Frontal"|init.data$Lobe_of_BOSD=="Parietal"),c("Hemisphere_BOSD",tmp.var.name)]))$p.value
  lobe.by.test.admin.depend$p.fisher[counter] = fisher.test(table(init.data[which(init.data$Lobe_of_BOSD=="Frontal"|init.data$Lobe_of_BOSD=="Parietal"),c("Hemisphere_BOSD",tmp.var.name)]))$p.value
  lobe.by.test.admin.depend$test[counter] = this.test
}
# and calculate adjusted p-values
lobe.by.test.admin.depend$p.fisher.fdr = p.adjust(lobe.by.test.admin.depend$p.fisher, method = "fdr")
lobe.by.test.admin.depend$p.chisq.fdr = p.adjust(lobe.by.test.admin.depend$p.chisq, method = "fdr")

## Table: baseline cog stats----
## Descriptive statistics of z scores by surgically naïve BOSD cohort at baseline
# Number of female/male at baseline, out of n=40
length(init.data[which(init.data$Gender=="F")])

# Cog measures and respective counts analysed at baseline 
colSums(!is.na(init.data[,tests_z]))

# Descriptive statistics of z scores by surgically naive BOSD cohort at baseline
sapply(init.data[,tests_z],mean,na.rm=TRUE)
sapply(init.data[,tests_z],median,na.rm=TRUE)
sapply(init.data[,tests_z],sd,na.rm=TRUE)
sapply(init.data[,tests_z],IQR,na.rm=TRUE)
sapply(init.data[,tests_z],min,na.rm=TRUE)
sapply(init.data[,tests_z],max,na.rm=TRUE)

# normality check for each test at baseline
for ( tests in tests_z) {
  if (shapiro.test(init.data[,tests])$p.value>=0.05){
    print(paste(tests,": Normal"))
  } else {
    print(paste(tests,": Not Normal"))
  }
} 


# writing a function that gives one-sample t-test with FDR-adjusted p-values
return_t_p_value_list = function(input_df, input_chr_vector) {
  t_vector = vector(length = length(input_chr_vector))
  p_vector = vector(length = length(input_chr_vector))
  for (i in 1:length(input_chr_vector)) {
    mytTest = t.test(x = input_df[, input_chr_vector[i] ], y = NULL, alternative = "two.sided", mu = 0)
    t_vector[i] = mytTest$stat 
    p_vector[i] = p.adjust(mytTest$p.value, method = "fdr")
  }
  data.frame(colname = input_chr_vector, t_stat = t_vector, p_value = p_vector)
}
return_t_p_value_list(init.data, tests_z)
# disregard "DSCd_z", "BNT_z", "RAVLT_Delay_z": 
# non-normal distributions analysed non-parametrically below

# Wilcox's tests for non-normal distributions
wilcox.test(x = init.data$DSCd_z, 
            y = NULL,
            paired = FALSE,
            alternative = "two.sided")

wilcox.test(x = init.data$RAVLT_Delay_z,
            y = NULL,
            paired = FALSE,
            alternative = "two.sided")

wilcox.test(x = init.data$BNT_z,
            y = NULL,
            paired = FALSE,
            alternative = "two.sided")

# first write a between-samples t-test fx
return_t_p_value_between_samples = function(input_df_1, input_df_2, input_chr_vector) {
  t_vector = vector(length = length(input_chr_vector))
  p_vector = vector(length = length(input_chr_vector))
  for (i in 1:length(input_chr_vector)) {
    mytTest = t.test(x = input_df_1[, input_chr_vector[i]], 
                     y = input_df_2[, input_chr_vector[i]], 
                     alternative = "two.sided")
    t_vector[i] = mytTest$stat 
    p_vector[i] = p.adjust(mytTest$p.value, method = "fdr")
  }
  data.frame(colname = input_chr_vector, t_stat = t_vector, p_value = p_vector)
}

## Fig: Lobe analyses----
# F vs P lobes (baseline)
# select frontal and parietal BOSD cases
init.frontal.cases = subset(init.data, Lobe_of_BOSD == "Frontal")
init.parietal.cases = subset(init.data, init.data$Lobe_of_BOSD == "Parietal")

# frequency table: laterality x lobe
table(init.data$Hemisphere_BOSD, init.data$Lobe_of_BOSD) # too few a left-parietal cases, cannot run 2x2 ANOVA

### Normality check
# frontal baseline cog
for (tests in tests_z) {
  if (shapiro.test(init.frontal.cases[,tests])$p.value>=0.05){
    print(paste(tests,": Normal"))
  } else {
    print(paste(tests,": Not Normal"))
  }
} #frontal: BNT and RAVLT_Delay not normal

# parietal baseline cog
# create a vector with tests N>3 for shapiro to work i.e., removing FAS
parietal.shapiro.z = c("DSCd_z",
                       "Dspan_z",
                       "BD_z",
                       "Simi_z",
                       "Trails_B_z",
                       "BNT_z",
                       "RAVLT_1to5_z",
                       "RAVLT_Delay_z",
                       "RCF_Delay_z",
                       "WTAR_z")
for (tests in parietal.shapiro.z) {
  if (shapiro.test(init.parietal.cases[,tests])$p.value>=0.05){
    print(paste(tests,": Normal"))
  } else {
    print(paste(tests,": Not Normal"))
  }
} #parietal: DSCd, Dspan, RAVLT_Delay not normal, FAS low N

#conclusion: RAVLT_Delay, FAS, BNT, DSCd, Dspan to be analysed non-parametrically in F vs P
wilcox.test(init.frontal.cases$RAVLT_Delay_z, 
            init.parietal.cases$RAVLT_Delay_z,
            paired = FALSE,
            alternative = "two.sided") #ns

wilcox.test(init.frontal.cases$FAS_total_z, 
            init.parietal.cases$FAS_total_z,
            paired = FALSE,
            alternative = "two.sided") #ns

wilcox.test(init.frontal.cases$BNT_z, 
            init.parietal.cases$BNT_z,
            paired = FALSE,
            alternative = "two.sided") #ns

wilcox.test(init.frontal.cases$DSCd_z, 
            init.parietal.cases$DSCd_z,
            paired = FALSE,
            alternative = "two.sided") #ns

wilcox.test(init.frontal.cases$Dspan_z, 
            init.parietal.cases$Dspan_z,
            paired = FALSE,
            alternative = "two.sided") #ns

# t-test: compare frontal vs parietal
return_t_p_value_between_samples(init.frontal.cases, init.parietal.cases, tests_z) # only RAVLT_1to5 was sig: frontal < parietal
# find group means for RAVLT_1to5
t.test(init.frontal.cases$RAVLT_1to5_z, 
       init.parietal.cases$RAVLT_1to5_z,
       alternative = "two.sided")
# effect size for RAVLT_1to5
effsize::cohen.d(init.frontal.cases$RAVLT_1to5_z, init.parietal.cases$RAVLT_1to5_z,
                 paired = FALSE, 
                 na.rm = TRUE,
                 mu = 0, 
                 hedges.correction = TRUE,
                 conf.level = 0.95, 
                 noncentral = FALSE) 

### factorial boxplot: RAVLT1-5, F vs P (L/R)----
# create an appropriate dataframe
factorial_boxplot_ravlt1to5_df <- init.data[, c("UR", "Hemisphere_BOSD", "Lobe_of_BOSD", "RAVLT_1to5_z")]
factorial_boxplot_ravlt1to5_df_frontal <- filter(factorial_boxplot_ravlt1to5_df, Lobe_of_BOSD == "Frontal")
factorial_boxplot_ravlt1to5_df_parietal <- filter(factorial_boxplot_ravlt1to5_df, Lobe_of_BOSD == "Parietal")
factorial_boxplot_ravlt1to5_df <- bind_rows(factorial_boxplot_ravlt1to5_df_frontal, factorial_boxplot_ravlt1to5_df_parietal)

# make hemisphere and lobe factors
factorial_boxplot_ravlt1to5_df$Lobe_of_BOSD <- factor(factorial_boxplot_ravlt1to5_df$Lobe_of_BOSD, 
                                                      levels = c("Frontal", "Parietal"),
                                                      labels = c("Frontal", "Parietal"))

factorial_boxplot_ravlt1to5_df$Hemisphere_BOSD <- factor(factorial_boxplot_ravlt1to5_df$Hemisphere_BOSD,
                                                         levels = c("Left", "Right"),
                                                         labels = c("Left", "Right"))

# boxplot
factorial_boxplot_ravlt1to5_df %>% 
  ggplot( aes(x = Lobe_of_BOSD, y = RAVLT_1to5_z, fill = Lobe_of_BOSD)) +
  geom_boxplot(alpha = 1, width = 0.35) + 
  scale_fill_brewer(palette="Dark2") +
  geom_jitter(aes(color = Hemisphere_BOSD), size = 5, position = position_jitter(width = 0.15), shape = 20) + 
  scale_color_brewer(palette = "Pastel1")+
  theme(panel.background = element_blank(),
        panel.grid = element_line(size = 0.2, linetype = 2, color = "grey"))


# #### Boxplot: F vs P in RAVLT_1to5
# ggplot(subset(init.data,(Lobe_of_BOSD=="Frontal"|Lobe_of_BOSD=="Parietal")),
#        aes(x = Lobe_of_BOSD,y=RAVLT_1to5_z,color = Hemisphere_BOSD)) +
#   geom_boxplot() + 
#   geom_jitter(position = position_jitterdodge(jitter.width=0.25)) +
#   scale_color_brewer(palette = "Dark2")
# 
# # stats: only main effect of lobe remains
# t.test(init.data[which(init.data$Lobe_of_BOSD=="Frontal"),"RAVLT_1to5_z"],
#        init.data[which(init.data$Lobe_of_BOSD=="Parietal"),"RAVLT_1to5_z"],
#        var.equal=FALSE)
# # stats: ANOVA
# summary(aov(RAVLT_1to5_z ~ Lobe_of_BOSD,data = subset(init.data,(Lobe_of_BOSD=="Frontal"|Lobe_of_BOSD=="Parietal"))))
# summary(aov(RAVLT_1to5_z ~ Hemisphere_BOSD,data = subset(init.data,(Lobe_of_BOSD=="Frontal"|Lobe_of_BOSD=="Parietal"))))
# summary(aov(RAVLT_1to5_z ~ Hemisphere_BOSD*Lobe_of_BOSD,data = subset(init.data,(Lobe_of_BOSD=="Frontal"|Lobe_of_BOSD=="Parietal"))))
# summary(aov(RAVLT_1to5_z ~ Hemisphere_BOSD+Lobe_of_BOSD,data = subset(init.data,(Lobe_of_BOSD=="Frontal"|Lobe_of_BOSD=="Parietal"))))
# # run type 2 anova due to unbalanced design with no sig interaction effect
# anova_ravlt1to5_lobe <-aov(RAVLT_1to5_z ~ Hemisphere_BOSD*Lobe_of_BOSD,data = subset(init.data,(Lobe_of_BOSD=="Frontal"|Lobe_of_BOSD=="Parietal")))
# Anova(anova_ravlt1to5_lobe, type = 2)

# Count of Hemisphere by Lobe for RAVLT_1to5_z
# table(init.data[which(!is.na(init.data$RAVLT_1to5_z)),"Lobe_of_BOSD"],init.data[which(!is.na(init.data$RAVLT_1to5_z)),"Hemisphere_BOSD"])

## Fig: Lat analyses----
# Boxplot of baseline z-scores on RCF Delay for Left and Right hemisphere BOSD
# ggplot(subset(init.data),
#        aes(x = Hemisphere_BOSD,y=RCF_Delay_z,color = Hemisphere_BOSD)) +
#   geom_boxplot() + 
#   geom_jitter(position = position_jitterdodge(jitter.width=0.5))


### L vs R (baseline)
# Table 6a of: BOSD_manuscript_v4_newAEPs
# no longer in use (24/5/23)
# hem.to.get = c("Left","Right")
# for ( this_test in tests_to_get ) {
#   for ( this_hem in hem.to.get ) {
#     init.test = paste0(this_test,"_z")
#     init.test.inds = which(!is.na(init.data[,init.test]) & (init.data[,"Hemisphere_BOSD"]==this_hem))
#     cat("\n\n")
#     print(paste0(this_test, " ", this_hem))
#     if ( length(init.test.inds) > 0 ) {
#       print(describe(init.data[init.test.inds,c(init.test)]))
#     } else {
#       print(paste0('No data for ',this_test, "; hemisphere: ", this_hem))
#     }
#   }
# }

# Left vs Right hemisphere
init.left.cases = subset(init.data, Hemisphere_BOSD == "Left")
init.right.cases = subset(init.data, Hemisphere_BOSD == "Right")

# L vs R normality
for (tests in tests_z) {
  if (shapiro.test(init.left.cases[,tests])$p.value>=0.05){
    print(paste(tests,": Normal"))
  } else {
    print(paste(tests,": Not Normal"))
  }
} #left: BNT not normal

for (tests in tests_z) {
  if (shapiro.test(init.right.cases[,tests])$p.value>=0.05){
    print(paste(tests,": Normal"))
  } else {
    print(paste(tests,": Not Normal"))
  }
} #right: DSCd, RAVLT_delay not normal

#conclusion: BNT & RAVLT_delay analysed non-parametrically in L vs R
wilcox.test(init.left.cases$BNT_z, 
            init.right.cases$BNT_z,
            paired = FALSE,
            alternative = "two.sided")

wilcox.test(init.left.cases$RAVLT_Delay_z, 
            init.right.cases$RAVLT_Delay_z,
            paired = FALSE,
            alternative = "two.sided")

wilcox.test(init.left.cases$DSCd_z, 
            init.right.cases$DSCd_z,
            paired = FALSE,
            alternative = "two.sided")


# t-tests
return_t_p_value_between_samples(init.left.cases, init.right.cases, tests_z)
# only RCF_Delay was sig: left < right
# find group means
t.test(init.left.cases$RCF_Delay_z,
       init.right.cases$RCF_Delay_z,
       alternative = "two.sided")
# effect size for RCF delay
library(effsize)
effsize::cohen.d(init.left.cases$RCF_Delay_z, init.right.cases$RCF_Delay_z,
                 paired = FALSE, 
                 na.rm = TRUE,
                 mu = 0, 
                 hedges.correction = TRUE,
                 conf.level = 0.95, 
                 noncentral = FALSE)

### factorial boxplot: RCF-delay, L vs R (F/P)----
# create an appropriate dataframe
factorial_boxplot_rcfdelay_df <- init.data[, c("UR", "Hemisphere_BOSD", "Lobe_of_BOSD", "RCF_Delay_z")]
factorial_boxplot_rcfdelay_df_frontal <- filter(factorial_boxplot_rcfdelay_df, Lobe_of_BOSD == "Frontal")
factorial_boxplot_rcfdelay_df_parietal <- filter(factorial_boxplot_rcfdelay_df, Lobe_of_BOSD == "Parietal")
factorial_boxplot_rcfdelay_df <- bind_rows(factorial_boxplot_rcfdelay_df_frontal, factorial_boxplot_rcfdelay_df_parietal)

# make hemisphere and lobe factors
factorial_boxplot_rcfdelay_df$Lobe_of_BOSD <- factor(factorial_boxplot_rcfdelay_df$Lobe_of_BOSD, 
                                                     levels = c("Frontal", "Parietal"),
                                                     labels = c("Frontal", "Parietal"))

factorial_boxplot_rcfdelay_df$Hemisphere_BOSD <- factor(factorial_boxplot_rcfdelay_df$Hemisphere_BOSD,
                                                        levels = c("Left", "Right"),
                                                        labels = c("Left", "Right"))

# boxplot
factorial_boxplot_rcfdelay_df %>% 
  ggplot( aes(x = Hemisphere_BOSD, y = RCF_Delay_z, fill = Hemisphere_BOSD)) +
  geom_boxplot(alpha = 1, width = 0.35) + 
  scale_fill_brewer(palette="Pastel1") +
  geom_jitter(aes(color = Lobe_of_BOSD), size = 5, position = position_jitter(width = 0.15), shape = 20, fill = 'white') + 
  scale_color_brewer(palette = "Dark2") +
  theme(panel.background = element_blank(), 
        panel.grid = element_line (size = 0.2, linetype = 2, color = "grey"))


### F, P, L, R descriptive stats----
# Frontal: mean, sd, min, max
sapply(init.frontal.cases[, tests_z], mean, na.rm=TRUE)
sapply(init.frontal.cases[, tests_z], sd, na.rm=TRUE)
sapply(init.frontal.cases[, tests_z], min, na.rm=TRUE)
sapply(init.frontal.cases[, tests_z], max, na.rm=TRUE)

# Parietal: mean, sd, min, max
sapply(init.parietal.cases[, tests_z], mean, na.rm=TRUE)
sapply(init.parietal.cases[, tests_z], sd, na.rm=TRUE)
sapply(init.parietal.cases[, tests_z], min, na.rm=TRUE)
sapply(init.parietal.cases[, tests_z], max, na.rm=TRUE)

# Left: mean, sd, min, max
sapply(init.left.cases[, tests_z], mean, na.rm=TRUE)
sapply(init.left.cases[, tests_z], sd, na.rm=TRUE)
sapply(init.left.cases[, tests_z], min, na.rm=TRUE)
sapply(init.left.cases[, tests_z], max, na.rm=TRUE)

# Right: mean, sd, min, max
sapply(init.right.cases[, tests_z], mean, na.rm=TRUE)
sapply(init.right.cases[, tests_z], sd, na.rm=TRUE)
sapply(init.right.cases[, tests_z], min, na.rm=TRUE)
sapply(init.right.cases[, tests_z], max, na.rm=TRUE)

## Table: pre-post freq/ASM----
# Pre- and post-operative seizure frequency (seizures per month) and number of antiseizure medications
## eliminate UR 2172094
# pre.post.data <- subset(pre.post.data, pre.post.data$UR != "2172094") <<- already eliminated since baseline analysis

### Had surgery----
# number of people who had surgery
had.surgery = filter(pre.post.data, !is.na(Date_of_surgery_1))
print(nrow(had.surgery))

#### Pre-surg sz freq (cross-sectional) ----
pre.post.df.cols = names(pre.post.data)
sz_freq = pre.post.df.cols[grep('_sz_freq', pre.post.df.cols)]
pre_sz_freq = sz_freq[grep('pre_', sz_freq)]
# N for pre-surgery sz freq
length(which(!is.na(rowMeans(pre.post.data[, pre_sz_freq],na.rm=TRUE))))
# Pre-surgery sz freq: by subject
rowMeans(pre.post.data[, pre_sz_freq],na.rm=TRUE)
# mean(within subject means first)
#mean(rowMeans(pre.post.data[, pre_sz_freq],na.rm=TRUE),na.rm=TRUE)
#sd(rowMeans(pre.post.data[, pre_sz_freq],na.rm=TRUE),na.rm=TRUE)
median(rowMeans(pre.post.data[, pre_sz_freq],na.rm=TRUE),na.rm=TRUE)
IQR(rowMeans(pre.post.data[, pre_sz_freq],na.rm=TRUE),na.rm=TRUE)
min(rowMeans(pre.post.data[, pre_sz_freq],na.rm=TRUE),na.rm=TRUE)
max(rowMeans(pre.post.data[, pre_sz_freq],na.rm=TRUE),na.rm=TRUE)

how.many.had.pre.sz.freq <- (rowMeans(pre.post.data[, pre_sz_freq],na.rm=TRUE))

#### Post-surg sz freq (cross-sectional) ----
post_sz_freq = sz_freq[grep('post_', sz_freq)]
# N for post-surgery sz freq
length(which(!is.na(rowMeans(pre.post.data[, post_sz_freq],na.rm=TRUE))))
# by subject
# mean(within subject means)
#mean(rowMeans(pre.post.data[, post_sz_freq],na.rm=TRUE),na.rm=TRUE)
#sd(rowMeans(pre.post.data[, post_sz_freq],na.rm=TRUE),na.rm=TRUE)
median(rowMeans(pre.post.data[, post_sz_freq],na.rm=TRUE),na.rm=TRUE)
IQR(rowMeans(pre.post.data[, post_sz_freq],na.rm=TRUE),na.rm=TRUE)
min(rowMeans(pre.post.data[, post_sz_freq],na.rm=TRUE),na.rm=TRUE)
max(rowMeans(pre.post.data[, post_sz_freq],na.rm=TRUE),na.rm=TRUE)


how.many.had.post.sz.freq <- (rowMeans(pre.post.data[, post_sz_freq],na.rm=TRUE))
length(which(how.many.had.post.sz.freq == 0))

#### Post-surg sz freq change categories (longitudinal) 
# Find those who have BOTH pre and post surgical Sz frequency data
pre.sz.freq.means = rowMeans(pre.post.data[, pre_sz_freq],na.rm=TRUE)
post.sz.freq.means = rowMeans(pre.post.data[, post_sz_freq],na.rm=TRUE)
post.minus.pre.sz.freq = post.sz.freq.means - pre.sz.freq.means # total change in Sz freq
post.under.pre.sz.freq = post.sz.freq.means / pre.sz.freq.means # proportional change in Sz freq
length(which(!is.na(post.minus.pre.sz.freq))) # n with pre post data
length(which(post.sz.freq.means == 0)) # n with post-op seizure free 

length(which(post.minus.pre.sz.freq < 0 & post.sz.freq.means == 0)) # n with reduction and seizure free
length(which(post.minus.pre.sz.freq < 0 & post.sz.freq.means != 0)) # n with reduction
length(which(post.minus.pre.sz.freq == 0)) # n unchanged
length(which(post.minus.pre.sz.freq > 0)) # n increased

# find those with unchanged seizure frequency
which(post.minus.pre.sz.freq == 0)
pre.post.data[c(16,26,33,35), pre_sz_freq]


# report portion change in those with Sz reduction but not Sz free
#median(post.under.pre.sz.freq[post.under.pre.sz.freq != 0],na.rm=TRUE)
#IQR(post.under.pre.sz.freq[post.under.pre.sz.freq != 0],na.rm=TRUE)
min(post.under.pre.sz.freq[post.under.pre.sz.freq != 0],na.rm=TRUE)
max(post.under.pre.sz.freq[post.under.pre.sz.freq != 0],na.rm=TRUE)
#extent of seizure reduction (not included in table)
median(post.minus.pre.sz.freq,na.rm=TRUE)
IQR(post.minus.pre.sz.freq,na.rm=TRUE)
min(post.minus.pre.sz.freq,na.rm=TRUE)
max(post.minus.pre.sz.freq,na.rm=TRUE)


#### Pre-surg ASM (cross-sectional)
ASM_number = pre.post.df.cols[grep('_n_meds', pre.post.df.cols)]
pre_ASM_number = ASM_number[grep('pre_', ASM_number)]
length(which(!is.na(rowMeans(pre.post.data[, pre_ASM_number],na.rm=TRUE))))
# by subject first
# mean, sd, median, IQR, min, max
#mean(rowMeans(pre.post.data[, pre_ASM_number],na.rm=TRUE),na.rm=TRUE)
#sd(rowMeans(pre.post.data[, pre_ASM_number],na.rm=TRUE),na.rm=TRUE)
median(rowMeans(pre.post.data[, pre_ASM_number],na.rm=TRUE),na.rm=TRUE)
IQR(rowMeans(pre.post.data[, pre_ASM_number],na.rm=TRUE),na.rm=TRUE)
min(rowMeans(pre.post.data[, pre_ASM_number],na.rm=TRUE),na.rm=TRUE)
max(rowMeans(pre.post.data[, pre_ASM_number],na.rm=TRUE),na.rm=TRUE)

#### Post-surg ASM (cross-sectional)
post_ASM_number = ASM_number[grep('post_', ASM_number)]
length(which(!is.na(rowMeans(pre.post.data[, post_ASM_number], na.rm=TRUE))))
# by subject first
# mean, sd, median, IQR, min, mas
#mean(rowMeans(pre.post.data[, post_ASM_number],na.rm=TRUE),na.rm=TRUE)
#sd(rowMeans(pre.post.data[, post_ASM_number],na.rm=TRUE),na.rm=TRUE)
median(rowMeans(pre.post.data[, post_ASM_number],na.rm=TRUE),na.rm=TRUE)
IQR(rowMeans(pre.post.data[, post_ASM_number],na.rm=TRUE),na.rm=TRUE)
min(rowMeans(pre.post.data[, post_ASM_number],na.rm=TRUE),na.rm=TRUE)
max(rowMeans(pre.post.data[, post_ASM_number],na.rm=TRUE),na.rm=TRUE)

#### Post-surg ASM change categories (longitudinal) 
# Find those who have BOTH pre and post surgical ASM data
pre.asm.num.means = rowMeans(pre.post.data[, pre_ASM_number],na.rm=TRUE)
post.asm.num.means = rowMeans(pre.post.data[, post_ASM_number],na.rm=TRUE)
post.minus.pre.asm.num = post.asm.num.means - pre.asm.num.means
length(which(!is.na(post.minus.pre.asm.num))) # n with pre post data

length(which(post.minus.pre.asm.num < 0 & post.asm.num.means == 0)) # n with reduction and now on no meds
length(which(post.minus.pre.asm.num < 0 & post.asm.num.means != 0)) # n with reduction in meds (though still on meds)
length(which(post.minus.pre.asm.num == 0)) # n unchanged
length(which(post.minus.pre.asm.num > 0)) # n increased

median(post.minus.pre.asm.num,na.rm=TRUE)
IQR(post.minus.pre.asm.num,na.rm=TRUE)
min(post.minus.pre.asm.num,na.rm=TRUE)
max(post.minus.pre.asm.num,na.rm=TRUE)

#### Interval between pre- post-ax
# calculate within subject mean first, then mean across subjects
post_min_pre_age_at_ax = pre.post.df.cols[grep('post_min_pre.*Age_at_Ax', pre.post.df.cols)]
post_min_pre_tests_z = pre.post.df.cols[grep('post_min_pre.*_z', pre.post.df.cols)]
365*(median(rowMeans(pre.post.data[,post_min_pre_age_at_ax], na.rm=TRUE), na.rm=TRUE))
365*(IQR(rowMeans(pre.post.data[,post_min_pre_age_at_ax], na.rm=TRUE), na.rm=TRUE))
365*(min(rowMeans(pre.post.data[,post_min_pre_age_at_ax], na.rm=TRUE), na.rm=TRUE))
365*(max(rowMeans(pre.post.data[,post_min_pre_age_at_ax], na.rm=TRUE), na.rm=TRUE))
# count how many had pre-post assessment interval data
how.many.had.prepost.interval.data <- rowMeans(pre.post.data[,post_min_pre_age_at_ax], na.rm = TRUE)
length(which(!is.na(how.many.had.prepost.interval.data)))

#### Interval between surg & post-ax
# define last surgery date 
pre.post.data$Date_last_surgery = as.Date(apply(pre.post.data[, c("Date_of_surgery_1",
                                                                  "Date_of_surgery_2",
                                                                  "Date_of_surgery_3")], 1,max, na.rm = TRUE))
# calculate interval between last surgery and post op Ax, in days
Ax.days.since.surg = pre.post.data[,names(pre.post.data)[grep('post.*Date_of_Ax',names(pre.post.data))]] - 
  cbind(as.data.frame(pre.post.data$Date_last_surgery),
        rep(as.data.frame(pre.post.data$Date_last_surgery),length(grep('post.*Date_of_Ax',names(pre.post.data)))-1))
for ( column in names(Ax.days.since.surg) ) {
  Ax.days.since.surg[,column] = as.numeric(Ax.days.since.surg[,column])
}
# Ax.days.since.surg$UR = pre.post.data$UR
# calculate within subject mean first, then mean across subjects
#mean(rowMeans(Ax.days.since.surg,na.rm=TRUE),na.rm=TRUE)
#sd(rowMeans(Ax.days.since.surg,na.rm=TRUE),na.rm=TRUE)
median(rowMeans(Ax.days.since.surg,na.rm=TRUE),na.rm=TRUE)
IQR(rowMeans(Ax.days.since.surg,na.rm=TRUE),na.rm=TRUE)
min(rowMeans(Ax.days.since.surg,na.rm=TRUE),na.rm=TRUE)
max(rowMeans(Ax.days.since.surg,na.rm=TRUE),na.rm=TRUE)
# count how many had surgery-post ax interval data
# count how many had pre-post assessment interval data
how.many.had.surgpost.interval.data <- rowMeans(Ax.days.since.surg, na.rm = TRUE)
length(which(!is.na(how.many.had.surgpost.interval.data)))



# for those that didn't have surgery
not.had.surgery = filter(init.data, is.na(Date_of_surgery_1))
nrow(not.had.surgery)
# sz freq
no.surg.df.cols = names(not.had.surgery)
no.surg.sz_freq = no.surg.df.cols[grep('_Sz_freq_num', no.surg.df.cols)]
# by subject
length(which(!is.na(rowMeans(not.had.surgery[,no.surg.sz_freq],na.rm=TRUE))))
# mean(within subject means)
#mean(rowMeans(not.had.surgery[,no.surg.sz_freq],na.rm=TRUE),na.rm=TRUE)
#sd(rowMeans(not.had.surgery[,no.surg.sz_freq],na.rm=TRUE),na.rm=TRUE)
median(rowMeans(not.had.surgery[,no.surg.sz_freq],na.rm=TRUE),na.rm=TRUE)
IQR(rowMeans(not.had.surgery[,no.surg.sz_freq],na.rm=TRUE),na.rm=TRUE)
min(rowMeans(not.had.surgery[,no.surg.sz_freq],na.rm=TRUE),na.rm=TRUE)
max(rowMeans(not.had.surgery[,no.surg.sz_freq],na.rm=TRUE),na.rm=TRUE)
# ASMs
no.surg.ASM = no.surg.df.cols[grep('ASM_num', no.surg.df.cols)]
# by subject
length(which(!is.na(rowMeans(not.had.surgery[,no.surg.ASM],na.rm=TRUE))))
# mean(within subject means)
#mean(rowMeans(not.had.surgery[,no.surg.ASM],na.rm=TRUE),na.rm=TRUE)
#sd(rowMeans(not.had.surgery[,no.surg.ASM],na.rm=TRUE),na.rm=TRUE)
median(rowMeans(not.had.surgery[,no.surg.ASM],na.rm=TRUE),na.rm=TRUE)
IQR(rowMeans(not.had.surgery[,no.surg.ASM],na.rm=TRUE),na.rm=TRUE)
min(rowMeans(not.had.surgery[,no.surg.ASM],na.rm=TRUE),na.rm=TRUE)
max(rowMeans(not.had.surgery[,no.surg.ASM],na.rm=TRUE),na.rm=TRUE)


## Post-pre cog----
# find post minus pre cognitive score measures
post.min.pre.cog = names(pre.post.data)[grep('post_min_pre.*_z', names(pre.post.data))]
# don't include RCF copy
if ( length(which(post.min.pre.cog=="post_min_pre_RCF_Copy_z")) > 0 ) {
  post.min.pre.cog = post.min.pre.cog[-which(post.min.pre.cog=="post_min_pre_RCF_Copy_z")]
}

# get count of number of unique cases with post-vs-pre cog data
tmp.hasPostVsPre = !is.na(pre.post.data[,post.min.pre.cog]) # has post and pre op scores
tmp.numPostVsPreCogScoresInGivenRow = rowSums(tmp.hasPostVsPre>0) # number of tests for which each participant has post vs pre op scores: 0 means no post vs pre data
n.unique.individuals.PostVsPreCogScores = length(which(tmp.numPostVsPreCogScoresInGivenRow>0)) # identify those which have one or more post vs pre-op cog scores, then take the length to know how many unique cases there are

### Boxplot: diff z-scores----
# create new labels to keep test names only
new.labels = gsub("post_min_pre_","",post.min.pre.cog)
new.labels = gsub("_z","",new.labels)

# get total number of pre- vs post-op comparisons
length(which(!is.na(as.matrix(pre.post.data[,post.min.pre.cog]))))

# and get number of change scores in excess of a 1.5 z-score unit decline
length(which(as.matrix(pre.post.data[,post.min.pre.cog]) < -1.5))

# get list of unique individuals and tests on which such a change observed
sig.cog.change = which(as.matrix(pre.post.data[,post.min.pre.cog]) < -1.5,arr.ind = TRUE)
pre.post.data[unique(sig.cog.change[,1]),c("UR","Lobe_of_BOSD","Hemisphere_BOSD")] # get list of unique individuals
sort(post.min.pre.cog[unique(sig.cog.change[,2])]) # get list of unique tests

post.vs.pre.cog.df.mel = reshape2::melt(pre.post.data,
                                        id.vars = c("UR","Hemisphere_BOSD","Lobe_of_BOSD"),
                                        measure.vars = post.min.pre.cog,
                                        variable.name = "cog.test",
                                        value.name = "z.score.diff")
for ( ind in 1:length(post.min.pre.cog) ) {
  # and mirror the post minus pre column name with just the test name, to simplify the axis label in the subsequent plot
  post.vs.pre.cog.df.mel$cog.test.name[which(post.vs.pre.cog.df.mel$cog.test==post.min.pre.cog[ind])] = new.labels[ind]
}

# post.vs.pre.cog.df.mel$cog.test <- factor(post.vs.pre.cog.df.mel$cog.test, 
#                                     levels = (c("WTAR_SS", "Simi_SS", "BD_SS", "DSCd_SS", "Trails_B_SS", "Dspan_SS", "FAS_total_SS", "BNT_SS", "RAVLT_1to5_SS", "RAVLT_Delay_SS", "RCF_Delay_SS" )))

post.vs.pre.cog.df.mel$cog.test.name = factor(post.vs.pre.cog.df.mel$cog.test.name, 
                                              levels = c("WTAR", "Simi", "BD", "DSCd", "Trails_B", "Dspan", "FAS_total", "BNT", "RAVLT_1to5", "RAVLT_Delay", "RCF_Delay" ))
ggplot(data = subset(post.vs.pre.cog.df.mel,!is.na(z.score.diff)), 
       aes(x = cog.test.name, 
           y = z.score.diff)) + 
  geom_boxplot(show.legend = FALSE, outlier.shape=NA) + 
  theme(axis.text.x = element_text(angle = 60)) + 
  geom_abline(slope = 0, intercept = 0) + 
  geom_jitter(width = 0.2, height = 0.1)

# and do another version, fudging in values for WTAR and Simi (for which we have essentially no data), to assign a fill color in 
# accordance with the baseline fill colors
post.vs.pre.cog.df.mel.2 = post.vs.pre.cog.df.mel
post.vs.pre.cog.df.mel.2[which(post.vs.pre.cog.df.mel.2$cog.test=="post_min_pre_WTAR_z"),"z.score.diff"] = rnorm(length(which(post.vs.pre.cog.df.mel.2$cog.test=="post_min_pre_WTAR_z")), mean = 0, sd = 0.01)
post.vs.pre.cog.df.mel.2[which(post.vs.pre.cog.df.mel.2$cog.test=="post_min_pre_Simi_z"),"z.score.diff"] = rnorm(length(which(post.vs.pre.cog.df.mel.2$cog.test=="post_min_pre_Simi_z")), mean = 0, sd = 0.01)
ggplot(data = subset(post.vs.pre.cog.df.mel.2,!is.na(z.score.diff)), 
       aes(x = cog.test.name, y = z.score.diff, fill = cog.test.name)) + 
  geom_boxplot(show.legend = FALSE, outlier.shape=NA, width = 0.35) + 
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 60)) + 
  geom_abline(slope = 0, intercept = 0) + 
  geom_jitter(width = 0.15, height = 0.1,show.legend = FALSE) +
  theme(panel.background = element_blank(),
        panel.grid = element_line(size = 0.2, linetype = 2, color= "grey"))+
  coord_cartesian(xlim = c(3,11))

# find the two cases with a significant drop in BNT (*one case after removing Joseph WHITE)
pre.post.data[which(pre.post.data$post_min_pre_BNT_z < -1.5),
              c("UR","First_Name","Last_Name","Lobe_of_BOSD","Hemisphere_BOSD",
                "pre_BNT_Date_of_Ax", "pre_BNT_RS","post_BNT_Date_of_Ax","post_BNT_RS")]
# and the cases with a significant drop in RAVLT 1 to 5
pre.post.data[which(pre.post.data$post_min_pre_RAVLT_1to5_z < -1.5),
              c("UR","First_Name","Last_Name","Lobe_of_BOSD","Hemisphere_BOSD",
                "pre_RAVLT_1to5_Date_of_Ax", "pre_RAVLT_1to5_RS","post_RAVLT_1to5_Date_of_Ax","post_RAVLT_1to5_RS")]

pre.post.data[which(pre.post.data$post_min_pre_RAVLT_1to5_z < -1.5),
              c("UR","First_Name","Last_Name","Lobe_of_BOSD","Hemisphere_BOSD",
                "pre_RAVLT_1to5_Date_of_Ax", "pre_RAVLT_1to5_z","post_RAVLT_1to5_Date_of_Ax","post_RAVLT_1to5_z")]

# and the single case with a significant drop in RCF_Delay
pre.post.data[which(pre.post.data$post_min_pre_RCF_Delay_z < -1.5),
              c("UR","First_Name","Last_Name","Lobe_of_BOSD","Hemisphere_BOSD",
                "pre_RCF_Delay_Date_of_Ax", "pre_RCF_Delay_RS","post_RCF_Delay_Date_of_Ax", "post_RCF_Delay_RS")]

pre.post.data[which(pre.post.data$post_min_pre_RCF_Delay_z < -1.5),
              c("UR","First_Name","Last_Name","Lobe_of_BOSD","Hemisphere_BOSD",
                "pre_RCF_Delay_Date_of_Ax", "pre_RCF_Delay_z","post_RCF_Delay_Date_of_Ax", "post_RCF_Delay_z")]



# obs: this was when RCF_copy_RS was included in tests_to_get while developing pre.post.data): 
# RCF_copy RS normal pre-surgically, therefore drop in delay z-score likely due to memory dysfx

### Scatter: DSCd_z vs ASM----
# scatter plot of post-surg vs pre-surg scores, classified by ASM change
# only DSCd was of interest
# colour-coded by increased, unchanged, reduced ASM count compared to pre-surg
for ( this_test in tests_to_get ) {
  pre.test = paste0("pre_",this_test,"_z")
  post.test = paste0("post_",this_test,"_z")
  pre.test.meds = paste0("pre_",this_test,"_n_meds")
  post.test.meds = paste0("post_",this_test,"_n_meds")
  diff.test.meds = paste0("post.minus.pre_",this_test,"_n_meds")
  diff.test.meds.qual = paste0("post.minus.pre_",this_test,"_med_change")
  # find participants with pre and post data on this test
  matched.test.inds = which(!is.na(pre.post.data[,pre.test]) & !is.na(pre.post.data[,post.test]))
  # create column that contains the change in number of meds between pre and post Ax
  pre.post.data[,diff.test.meds] = pre.post.data[,post.test.meds] - pre.post.data[,pre.test.meds]
  # create factor column, qualitatively describing change in meds (reduced or not reduced), to use this as a plot aesthetic
  pre.post.data[,diff.test.meds.qual] = NA
  # identify those with a reduction
  pre.post.data[which(pre.post.data[,diff.test.meds] < 0),diff.test.meds.qual] = "Reduced"
  # those with no change
  pre.post.data[which(pre.post.data[,diff.test.meds] == 0),diff.test.meds.qual] = "No change"
  # those with an increase
  pre.post.data[which(pre.post.data[,diff.test.meds] > 0),diff.test.meds.qual] = "Increased"
  print(ggplot(data = pre.post.data[matched.test.inds,],
               aes(x = get(pre.test), y = get(post.test), color = get(diff.test.meds.qual))) + 
          # geom_point(size = 3) + 
          geom_jitter(shape = 20, size = 5, width = 0.15, height = 0.1) + # need this to reveal overlapping data points
          scale_color_manual(values = c("brown3", "coral",  "deepskyblue3", "lightgrey" )) + 
          theme(panel.background = element_blank(),
                panel.grid = element_line(size = 0.2, linetype = 2, color = "grey")) +
          geom_abline(slope = 1, intercept = 0) +
          ggtitle(print(this_test)) +
          labs(x = print(pre.test), y = print(post.test))
  )
}

### Scatter: DSCd_z vs Sz freq ----
# find participants with pre and post data on DSCd
matched.test.inds = which(!is.na(pre.post.data[,"pre_DSCd_z"]) & !is.na(pre.post.data[,"post_DSCd_z"]))
# create column that contains the change in Sz frequency between pre and post DSCd Ax
pre.post.data[,"post.minus.pre_DSCd_sz_freq"] = pre.post.data[,"post_DSCd_sz_freq"] - pre.post.data[,"pre_DSCd_sz_freq"]
# create factor column, qualitatively describing change in sz freq (reduced or not reduced), to use this as a plot aesthetic
pre.post.data[,"post.minus.pre_DSCd_sz_freq_reduced"] = NA
# identify those with a reduction
pre.post.data[which(pre.post.data[,"post.minus.pre_DSCd_sz_freq"] < 0),"post.minus.pre_DSCd_sz_freq_reduced"] = "Reduced"
# those without a reduction
pre.post.data[which(pre.post.data[,"post.minus.pre_DSCd_sz_freq"] >= 0),"post.minus.pre_DSCd_sz_freq_reduced"] = "Not-reduced"
ggplot(data = pre.post.data[matched.test.inds,],aes(x = pre_DSCd_z, y = post_DSCd_z, color = post.minus.pre_DSCd_sz_freq_reduced)) + 
  geom_jitter(size = 3, width = 0.1, height = 0.1) + 
  scale_y_continuous(limits=c(-2,2)) +
  geom_abline(slope = 1, intercept = 0)

## Table: pre/post cog stats----
# Pre- and post-op cognitive test scores, and change scores, 
# with repeated measures statistical tests
# get descriptive data for pre-, post-, post.minus.pre scores
for ( this_test in tests_to_get ) {
  pre.test = paste0("pre_",this_test,"_z")
  post.test = paste0("post_",this_test,"_z")
  diff.test = paste0("post.minus.pre_",this_test,"_z")
  matched.test.inds = which(!is.na(pre.post.data[,pre.test]) & !is.na(pre.post.data[,post.test]))
  if ( length(matched.test.inds) > 0 ) {
    pre.post.data[matched.test.inds, diff.test] = 
      pre.post.data[matched.test.inds,post.test] - 
      pre.post.data[matched.test.inds,pre.test] 
    print(describe(pre.post.data[matched.test.inds,c(pre.test,post.test,diff.test)]))
  } else {
    pre.post.data[, diff.test] = NA
    print(paste0('No matched pre post data for ',this_test))
  }
}

# t-test of post.minus.pre diff scores
# normality check
# removing tests (simi, WTAR) with N < 3 which fails shapiro requirement
post.minus.pre_z_shapiro = c("post.minus.pre_DSCd_z",
                             "post.minus.pre_Dspan_z",
                             "post.minus.pre_BD_z",
                             "post.minus.pre_Trails_B_z",
                             "post.minus.pre_FAS_total_z",
                             "post.minus.pre_BNT_z",
                             "post.minus.pre_RAVLT_1to5_z",
                             "post.minus.pre_RAVLT_Delay_z",
                             "post.minus.pre_RCF_Delay_z")

for ( tests in post.minus.pre_z_shapiro) {
  if (shapiro.test(pre.post.data[,tests])$p.value>=0.05){
    print(paste(tests,": Normal"))
  } else {
    print(paste(tests,": Not Normal"))
  }
}
# DSCd, BNT not normal

# find t-stats and p-values for pre- vs post-op diff z scores
post.min.pre.cog.low.N.removed = c("post_min_pre_DSCd_z",
                                   "post_min_pre_Dspan_z",
                                   "post_min_pre_BD_z",
                                   "post_min_pre_Trails_B_z",
                                   "post_min_pre_FAS_total_z",
                                   "post_min_pre_BNT_z",
                                   "post_min_pre_RAVLT_1to5_z",
                                   "post_min_pre_RAVLT_Delay_z",
                                   "post_min_pre_RCF_Delay_z")
return_t_p_value_list(pre.post.data, post.min.pre.cog.low.N.removed)
#only Trails B was sig (DSCd too but not normal)

# effect size for TMT B (the only test that was sig)
# install.packages("lsr")
cohensD(pre.post.data$post.minus.pre_Trails_B_z, mu=0)

# Wilcox's tests for tests with non-normal distribution
# DSCd
wilcox.test(x = pre.post.data$post.minus.pre_DSCd_z, 
            alternative = "two.sided")
# effect size = test.stats/rt N


# BNT
wilcox.test(x = pre.post.data$post.minus.pre_BNT_z, 
            alternative = "two.sided")
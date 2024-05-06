# Import data ----
# script to analyse BOSD data, specifically re. IC-CoDE analysis

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
master_data = "Datasets/BOSD Master Oct Edits DATES CORRECTED_sz freq-ASM_newAEPs_dyslexia_oldASMandFreq_IC-code.csv"

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
## RAVLT----
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

## TMT----
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

## FAS----
fas.fields = list(raw_score_field = "FAS_total", 
                  id_field = "UR",
                  age_field = "Age_at_Ax", 
                  z_score_field = "FAS_total_z")
data = cowat_raw_to_norm_mitrushina(data,fas.fields,norm.path)
# convert FAS z-score to age scaled score
data$FAS_total_SS = data$FAS_total_z*3+10
# and rename the FAS raw score column with the suffix RS
names(data)[names(data) == 'FAS_total'] <- 'FAS_total_RS'

## Animals----
data$level_of_ed = NaN
animals.fields = list(raw_score_field = "Animals_RS",
                      id_field = "UR",
                      age_field = "Age_at_Ax",
                      edu_field = "level_of_ed",
                      z_score_field = "Animals_z")
data = animals_raw_to_norm_mitrushina(data,animals.fields,norm.path)

## RCF Copy----
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

## BNT----
bnt.fields = list(raw_score_field = "BNT_RS", 
                  id_field = "UR",
                  age_field = "Age_at_Ax", 
                  z_score_field = "BNT_z")
data = bnt_raw_to_norm_mitrushina(data,bnt.fields,norm.path)
# convert BNT z-score to age scaled score
data$BNT_SS = data$BNT_z*3+10

## Stroop----
stroop.dots.fields = list(raw_score_field = "Stroop_D_RS",
                          id_field = "UR",
                          age_field = "Age_at_Ax",
                          z_score_field = "Stroop_D_z")
data = stroop_dots_raw_to_norm_troyer(data,stroop.dots.fields,norm.path)

stroop.neurtral.fields = list(raw_score_field = "Stroop_W_RS",
                              id_field = "UR",
                              age_field = "Age_at_Ax",
                              z_score_field = "Stroop_W_z")
data = stroop_neutral_raw_to_norm_troyer(data,stroop.neurtral.fields,norm.path)

stroop.colour.fields = list(raw_score_field = "Stroop_C_RS",
                            id_field = "UR",
                            age_field = "Age_at_Ax",
                            z_score_field = "Stroop_C_z")
data = stroop_colours_raw_to_norm_troyer(data,stroop.colour.fields,norm.path)


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
data$VR1_z = (data$VR1_SS-10)/3

# convert VR2 to z
data$VR2_z = (data$VR2_SS-10)/3

# convert LM1 to z
data$LM1_z = (data$LM1_SS-10)/3

# convert LM2 to z
data$LM2_z = (data$LM2_SS-10)/3

# get each unique UR and Last name
unique_UR = unique(data$UR)
unique_Lastname = unique(data$Last_Name)

# Specify tests ----
# these are the test names without the _RS or _SS suffix
tests_to_get = c("DSCd","Dspan","BD","Simi", "Trails_B","FAS_total", "BNT","RAVLT_1to5","RAVLT_Delay","RCF_Delay","WTAR")
additional_tests = c("Vocab", "Simi", "Info", "Comp", "Pcomp", "MR", "Arith", "SS", "Cod", "LN", "VR1", "VR2", "LM1", "LM2", "Animals", "RCF_Copy", "Trails_A", "Stroop_D", "Stroop_W", "Stroop_C") # will need to add to this list
tests_to_get = c(tests_to_get,additional_tests)



# Develop init.data.iccode ----
# initialise empty data frame to store single instance of initial assessment data in
init.data.iccode = data.frame()

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
  
  # now fill the rest of the init.data.iccode data frame with data from the tests we are interested in
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
    
    # find instances of this test that have raw or scaled scores that are not NaN (and therefore the test was administered)
    use_ind = which((!is.na(data[UR.inds,RS_test])) | (!is.na(data[UR.inds,SS_test])))
    
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
  if ( length(init.data.iccode) == 0 ) {
    init.data.iccode = t_data } else {
      init.data.iccode = rbind(init.data.iccode,t_data)
    }
}


df.cols = names(init.data.iccode)
tests_SS = df.cols[grep('_SS',df.cols)]
tests_z = df.cols[grep('_z',df.cols)]
# get columns used to calculate epileptological variables, e.g. Age at assessment, duration of epilepsy at assessment, etc.
tests_Age_at_Ax = df.cols[grep('Age_at_Ax',df.cols)]
tests_Dur_at_Ax = df.cols[grep('Dur_at_Ax',df.cols)]
tests_ASM_at_Ax = df.cols[grep('ASM_num',df.cols)]
tests_Sz_freq_num = df.cols[grep('Sz_freq_num', df.cols)]

# IC-CoDE (domain)----
## language----
# create variable for to include tests
lang.cols.iccode = c("FAS_total_z", "BNT_z", "Animals_z")

# make new column to indicate eligibility for cognitive phenotyping, ie. at least two tests
init.data.iccode$eligibility.lang = NaN

# how many participants' language domain were eligible for cognitive phenotyping?
for (UR in unique_UR) {
  # pull the data for this record
  t.record = which(init.data.iccode$UR == UR)
  # get language tests for this participant
  t.lang.data = init.data.iccode[t.record, lang.cols.iccode]
  # get eligibility column for lang
  t.eli.data = init.data.iccode[t.record, "eligibility.lang"]
  
  # if there are at least two language tests administered in this domain
  if (length(which(!is.na(t.lang.data))) >=2) {
    # the participant is eligible for cognitive phenotyping
    init.data.iccode[t.record, "eligibility.lang"] = 1
  } else {
    # the participant is not eligible for cognitive phenotyping
    init.data.iccode[t.record, "eligibility.lang"] = NaN
  }
}

n.lang.eligible = length(which(init.data.iccode[, "eligibility.lang"] == 1))
print(n.lang.eligible)

# how many were impaired in language domain (ie. impaired on at least two measures)?
# make new column to store IC Code classification
init.data.iccode$iccode.lang.2.imp = NaN
init.data.iccode$iccode.lang.1.imp = NaN
# define threshold considered impairment, in z units
impairment.threshold = -1.5 
 
for ( UR in unique_UR )  {
  # pull the data for this record
  t.record = which(init.data.iccode$UR == UR)
  # get language columns for this participant
  t.lang.data = init.data.iccode[t.record,lang.cols.iccode]
  if ( length(which(!is.na(t.lang.data))) >= 2 )  {
    # there are at least two language tests
    # check how many are impaired
    if ( length(which(t.lang.data <= impairment.threshold)) >= 2 ) {
      # impaired on two tests in the language domain
      init.data.iccode[t.record,"iccode.lang.2.imp"] = 1
      init.data.iccode[t.record,"iccode.lang.1.imp"] = 0
    } else if ( length(which(t.lang.data <= impairment.threshold)) == 1 ) {
      # impaired on one test in the language domain
      init.data.iccode[t.record,"iccode.lang.2.imp"] = 0
      init.data.iccode[t.record,"iccode.lang.1.imp"] = 1
    } else {
      # not impaired on any language task administered
      init.data.iccode[t.record,"iccode.lang.2.imp"] = 0
      init.data.iccode[t.record,"iccode.lang.1.imp"] = 0
    }
  }
}

n.lang.impaired = length(which(init.data.iccode[, "iccode.lang.2.imp"] ==1))
print(n.lang.impaired)

## memory----
# create variable for to include tests
mem.cols.iccode = c("RAVLT_1to5_z", "RAVLT_Delay_z", "VR1_z", "VR2_z", "LM1_z", "LM2_z", "RCF_Delay_z")

# make new column to indicate eligibility for cognitive phenotyping, ie. at least two tests
init.data.iccode$eligibility.mem = NaN

# how many participants' memory domain were eligible for cognitive phenotyping?
for (UR in unique_UR) {
  # pull the data for this record
  t.record = which(init.data.iccode$UR == UR)
  # get memory tests for this participant
  t.mem.data = init.data.iccode[t.record, mem.cols.iccode]
  # get eligibility column for mem
  t.eli.data = init.data.iccode[t.record, "eligibility.mem"]
  
  # if there are at least two memory tests administered in this domain
  if (length(which(!is.na(t.mem.data))) >=2) {
    # the participant is eligible for cognitive phenotyping
    init.data.iccode[t.record, "eligibility.mem"] = 1
  } else {
    # the participant is not eligible for cognitive phenotyping
    init.data.iccode[t.record, "eligibility.mem"] = NaN
  }
}

n.mem.eligible = length(which(init.data.iccode[, "eligibility.mem"] == 1))
print(n.mem.eligible)

# how many were impaired in memory domain (ie. impaired on at least two measures)?
# make new column to store IC Code classification
init.data.iccode$iccode.mem.2.imp = NaN
init.data.iccode$iccode.mem.1.imp = NaN
# define threshold considered impairment, in z units
impairment.threshold = -1.5 

for ( UR in unique_UR )  {
  # pull the data for this record
  t.record = which(init.data.iccode$UR == UR)
  # get memory columns for this participant
  t.mem.data = init.data.iccode[t.record,mem.cols.iccode]
  if ( length(which(!is.na(t.mem.data))) >= 2 )  {
    # there are at least two memory tests
    # check how many are impaired
    if ( length(which(t.mem.data <= impairment.threshold)) >= 2 ) {
      # impaired on two tests in the memory domain
      init.data.iccode[t.record,"iccode.mem.2.imp"] = 1
      init.data.iccode[t.record,"iccode.mem.1.imp"] = 0
    } else if ( length(which(t.mem.data <= impairment.threshold)) == 1 ) {
      # impaired on one test in the memory domain
      init.data.iccode[t.record,"iccode.mem.2.imp"] = 0
      init.data.iccode[t.record,"iccode.mem.1.imp"] = 1
    } else {
      # not impaired on any memory task administered
      init.data.iccode[t.record,"iccode.mem.2.imp"] = 0
      init.data.iccode[t.record,"iccode.mem.1.imp"] = 0
    }
  }
}

n.mem.impaired = length(which(init.data.iccode[, "iccode.mem.2.imp"] ==1))
print(n.mem.impaired)

## executive function----
# create variable for to include tests
ef.cols.iccode = c("LN_z", "Arith_z", "Trails_B_z", "Stroop_C_z") 

# make new column to indicate eligibility for cognitive phenotyping, ie. at least two tests
init.data.iccode$eligibility.ef = NaN

# how many participants' executive function domain were eligible for cognitive phenotyping?
for (UR in unique_UR) {
  # pull the data for this record
  t.record = which(init.data.iccode$UR == UR)
  # get executive function tests for this participant
  t.ef.data = init.data.iccode[t.record, ef.cols.iccode]
  # get eligibility column for ef
  t.eli.data = init.data.iccode[t.record, "eligibility.ef"]
  
  # if there are at least two executive function tests administered in this domain
  if (length(which(!is.na(t.ef.data))) >=2) {
    # the participant is eligible for cognitive phenotyping
    init.data.iccode[t.record, "eligibility.ef"] = 1
  } else {
    # the participant is not eligible for cognitive phenotyping
    init.data.iccode[t.record, "eligibility.ef"] = NaN
  }
}

n.ef.eligible = length(which(init.data.iccode[, "eligibility.ef"] == 1))
print(n.ef.eligible)

# how many were impaired in executive function domain (ie. impaired on at least two measures)?
# make new column to store IC Code classification
init.data.iccode$iccode.ef.2.imp = NaN
init.data.iccode$iccode.ef.1.imp = NaN
# define threshold considered impairment, in z units
impairment.threshold = -1.5 

for ( UR in unique_UR )  {
  # pull the data for this record
  t.record = which(init.data.iccode$UR == UR)
  # get executive function columns for this participant
  t.ef.data = init.data.iccode[t.record,ef.cols.iccode]
  if ( length(which(!is.na(t.ef.data))) >= 2 )  {
    # there are at least two executive function tests
    # check how many are impaired
    if ( length(which(t.ef.data <= impairment.threshold)) >= 2 ) {
      # impaired on two tests in the executive function domain
      init.data.iccode[t.record,"iccode.ef.2.imp"] = 1
      init.data.iccode[t.record,"iccode.ef.1.imp"] = 0
    } else if ( length(which(t.ef.data <= impairment.threshold)) == 1 ) {
      # impaired on one test in the executive function domain
      init.data.iccode[t.record,"iccode.ef.2.imp"] = 0
      init.data.iccode[t.record,"iccode.ef.1.imp"] = 1
    } else {
      # not impaired on any executive function task administered
      init.data.iccode[t.record,"iccode.ef.2.imp"] = 0
      init.data.iccode[t.record,"iccode.ef.1.imp"] = 0
    }
  }
}

n.ef.impaired = length(which(init.data.iccode[, "iccode.ef.2.imp"] ==1))
print(n.ef.impaired)

## attention/speed----
# create variable for to include tests
att.cols.iccode = c("DSCd_z", "Trails_A_z", "Dspan_z", "SS_z", "Cod_z", "Stroop_D_z", "Stroop_W_z") 

# make new column to indicate eligibility for cognitive phenotyping, ie. at least two tests
init.data.iccode$eligibility.att = NaN

# how many participants' attention/speed domain were eligible for cognitive phenotyping?
for (UR in unique_UR) {
  # pull the data for this record
  t.record = which(init.data.iccode$UR == UR)
  # get attention/speed tests for this participant
  t.att.data = init.data.iccode[t.record, att.cols.iccode]
  # get eligibility column for attention/speed
  t.eli.data = init.data.iccode[t.record, "eligibility.att"]
  
  # if there are at least two attention/speed tests administered in this domain
  if (length(which(!is.na(t.att.data))) >=2) {
    # the participant is eligible for cognitive phenotyping
    init.data.iccode[t.record, "eligibility.att"] = 1
  } else {
    # the participant is not eligible for cognitive phenotyping
    init.data.iccode[t.record, "eligibility.att"] = NaN
  }
}

n.att.eligible = length(which(init.data.iccode[, "eligibility.att"] == 1))
print(n.att.eligible)

# how many were impaired in attention/speed domain (ie. impaired on at least two measures)?
# make new column to store IC Code classification
init.data.iccode$iccode.att.2.imp = NaN
init.data.iccode$iccode.att.1.imp = NaN
# define threshold considered impairment, in z units
impairment.threshold = -1.5 

for ( UR in unique_UR )  {
  # pull the data for this record
  t.record = which(init.data.iccode$UR == UR)
  # get attention/speed columns for this participant
  t.att.data = init.data.iccode[t.record,att.cols.iccode]
  if ( length(which(!is.na(t.att.data))) >= 2 )  {
    # there are at least two attention/speed tests
    # check how many are impaired
    if ( length(which(t.att.data <= impairment.threshold)) >= 2 ) {
      # impaired on two tests in the attention/speed domain
      init.data.iccode[t.record,"iccode.att.2.imp"] = 1
      init.data.iccode[t.record,"iccode.att.1.imp"] = 0
    } else if ( length(which(t.att.data <= impairment.threshold)) == 1 ) {
      # impaired on one test in the attention/speed domain
      init.data.iccode[t.record,"iccode.att.2.imp"] = 0
      init.data.iccode[t.record,"iccode.att.1.imp"] = 1
    } else {
      # not impaired on any attention/speed task administered
      init.data.iccode[t.record,"iccode.att.2.imp"] = 0
      init.data.iccode[t.record,"iccode.att.1.imp"] = 0
    }
  }
}

n.att.impaired = length(which(init.data.iccode[, "iccode.att.2.imp"] ==1))
print(n.att.impaired)

## visuospatial----
# create variable for to include tests
vis.cols.iccode = c("Pcomp_z", "MR_z", "BD_z", "RCF_Copy_z") 

# make new column to indicate eligibility for cognitive phenotyping, ie. at least two tests
init.data.iccode$eligibility.vis = NaN

# how many participants' visuospatial domain were eligible for cognitive phenotyping?
for (UR in unique_UR) {
  # pull the data for this record
  t.record = which(init.data.iccode$UR == UR)
  # get visuospatial tests for this participant
  t.vis.data = init.data.iccode[t.record, vis.cols.iccode]
  # get eligibility column for att
  t.eli.data = init.data.iccode[t.record, "eligibility.vis"]
  
  # if there are at least two visuospatial tests administered in this domain
  if (length(which(!is.na(t.vis.data))) >=2) {
    # the participant is eligible for cognitive phenotyping
    init.data.iccode[t.record, "eligibility.vis"] = 1
  } else {
    # the participant is not eligible for cognitive phenotyping
    init.data.iccode[t.record, "eligibility.vis"] = NaN
  }
}

n.vis.eligible = length(which(init.data.iccode[, "eligibility.vis"] == 1))
print(n.vis.eligible)

# how many were impaired in visuospatial domain (ie. impaired on at least two measures)?
# make new column to store IC Code classification
init.data.iccode$iccode.vis.2.imp = NaN
init.data.iccode$iccode.vis.1.imp = NaN
# define threshold considered impairment, in z units
impairment.threshold = -1.5 

for ( UR in unique_UR )  {
  # pull the data for this record
  t.record = which(init.data.iccode$UR == UR)
  # get visuospatial columns for this participant
  t.vis.data = init.data.iccode[t.record,vis.cols.iccode]
  if ( length(which(!is.na(t.vis.data))) >= 2 )  {
    # there are at least two visuospatial tests
    # check how many are impaired
    if ( length(which(t.vis.data <= impairment.threshold)) >= 2 ) {
      # impaired on two tests in the visuospatial domain
      init.data.iccode[t.record,"iccode.vis.2.imp"] = 1
      init.data.iccode[t.record,"iccode.vis.1.imp"] = 0
    } else if ( length(which(t.vis.data <= impairment.threshold)) == 1 ) {
      # impaired on one test in the visuospatial domain
      init.data.iccode[t.record,"iccode.vis.2.imp"] = 0
      init.data.iccode[t.record,"iccode.vis.1.imp"] = 1
    } else {
      # not impaired on any visuospatial task administered
      init.data.iccode[t.record,"iccode.vis.2.imp"] = 0
      init.data.iccode[t.record,"iccode.vis.1.imp"] = 0
    }
  }
}

n.vis.impaired = length(which(init.data.iccode[, "iccode.vis.2.imp"] ==1))
print(n.vis.impaired)

# IC-CoDE (phenotype)----
# only people with at least four domains assessed are eligible for IC-CoDE phenotyping
# number of eligible domains
eli.cols.iccode = c("eligibility.lang", "eligibility.mem", "eligibility.ef", "eligibility.att", "eligibility.vis")

# number of impaired domain
imp.cols.iccode = c("iccode.lang.2.imp", "iccode.mem.2.imp", "iccode.ef.2.imp", "iccode.att.2.imp",  "iccode.vis.2.imp")

# column to indicate overall eligibility for phenotyping
init.data.iccode$eligibility.pheno = NaN

# make new cols to store impairment category
init.data.iccode$iccode.sing.imp = NaN
init.data.iccode$iccode.bi.imp = NaN
init.data.iccode$iccode.gen.imp = NaN
init.data.iccode$iccode.nonimpaired = NaN


for (UR in unique_UR) {
  # pull the data for this record
  s.record = which(init.data.iccode$UR == UR)
  # get impairment columns for this participant
  s.imp.data = init.data.iccode[s.record, imp.cols.iccode]
  # get eligibility column for this participant too
  s.eli.data = init.data.iccode[s.record, eli.cols.iccode]
  
  if (length(which(!is.na(s.eli.data))) < 4) {
    # there are less than four domains measured
    # the person is not eligible for IC-CoDE classification, coded as NaN
    init.data.iccode[s.record, "eligibility.pheno"] = NaN
    # all possible classification outcomes coded as NaN
    init.data.iccode[s.record,"iccode.sing.imp"] = NaN
    init.data.iccode[s.record,"iccode.bi.imp"] = NaN
    init.data.iccode[s.record,"iccode.gen.imp"] = NaN
    init.data.iccode[s.record,"iccode.nonimpaired"] = NaN
  }
  else if (length(which(!is.na(s.eli.data))) >= 4) {
    # there are at least four domains measured
    init.data.iccode[s.record,"eligibility.pheno"] = 1
    # check how many cases with at least three domains impaired (ie. gen)
    if ( length(which(s.imp.data == 1)) >= 3) {
      init.data.iccode[s.record,"iccode.sing.imp"] = 0
      init.data.iccode[s.record,"iccode.bi.imp"] = 0
      init.data.iccode[s.record,"iccode.gen.imp"] = 1
      init.data.iccode[s.record,"iccode.nonimpaired"] = 0
    } else if ( length(which(s.imp.data == 1)) == 2) {
      # there are two impaired domains (ie. bi)
      init.data.iccode[s.record,"iccode.sing.imp"] = 0
      init.data.iccode[s.record,"iccode.bi.imp"] = 1
      init.data.iccode[s.record,"iccode.gen.imp"] = 0
      init.data.iccode[s.record,"iccode.nonimpaired"] = 0
    } else if ( length(which(s.imp.data == 1)) == 1) {
      # there is one impaired domain (ie. singular)
      init.data.iccode[s.record,"iccode.sing.imp"] = 1
      init.data.iccode[s.record,"iccode.bi.imp"] = 0
      init.data.iccode[s.record,"iccode.gen.imp"] = 0
      init.data.iccode[s.record,"iccode.nonimpaired"] = 0
    } else if ( length(which(s.imp.data == 1)) == 0) {
      # there is no impaired domain (ie. not impaired)
      init.data.iccode[s.record,"iccode.sing.imp"] = 0
      init.data.iccode[s.record,"iccode.bi.imp"] = 0
      init.data.iccode[s.record,"iccode.gen.imp"] = 0
      init.data.iccode[s.record,"iccode.nonimpaired"] = 1
    } 
  }
}

n.eligible.pheno = length(which(init.data.iccode[, "eligibility.pheno"] == 1))
print(n.eligible.pheno)
# how many were generalised impaired?
n.gen.imp = length(which(init.data.iccode[,"iccode.gen.imp"] == 1))
print(n.gen.imp)
# how many were bi impaired?
n.bi.imp = length(which(init.data.iccode[, "iccode.bi.imp"] == 1))
print(n.bi.imp)
# how many were sing impaired?
n.sing.imp = length(which(init.data.iccode[, "iccode.sing.imp"] == 1))
print(n.sing.imp)
# how many were not impaired?
n.non.imp = length(which(init.data.iccode[, "iccode.nonimpaired"] == 1))
print(n.non.imp)

# code to check all the above work
init.data.iccode[, c(eli.cols.iccode, "eligibility.pheno", imp.cols.iccode, "iccode.sing.imp", "iccode.bi.imp", "iccode.gen.imp", "iccode.nonimpaired")]

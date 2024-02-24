# this is the master R script which is used to obtain results for chapter 1 - 'cognitive features associated with MCD'
rm(list=ls())

# libraries
library(tidyverse)
library(ggplot2)

# import data----
setwd("/Users/carmen/Library/CloudStorage/OneDrive-TheUniversityofMelbourne/PhD/Study-1/data")
data <- read.csv("AustralianEpilepsyPr-MCDPatientCharacteri_DATA_2024-02-20_0953.csv")

# data cleaning----
## remove meaningless columns
data <- data[-c(2:4)] # removed redcap generated event indices

## rename columns
data <- data %>%
  rename(
    pt_sex = aepref_pt_sex,
    pt_age = npsyts_age_at_npsych_ax,
    pt_ref = aepref_pt_ref_cat, 
    # imaging outcome classification status:
    pt_abnorm_status = mridat_rad_rep_sum_class,
    pt_ep_lesn_status = mridat_epileptogenic_lesn, 
    pt_motion_status = mridat_mri_rad_motion_aff,
    pt_repeat_status = mridat_mri_rad_more_img,
    # MCD lesion types:
    MCD_BOSD = mridat_rad_class_mcd___1,
    MCD_FCD = mridat_rad_class_mcd___2,
    MCD_tuber = mridat_rad_class_mcd___3,
    MCD_GMH_PVNH = mridat_rad_class_mcd___4,
    MCD_GMH_subnod = mridat_rad_class_mcd___5,
    MCD_GMH_band = mridat_rad_class_mcd___6,
    MCD_polymicrogyria = mridat_rad_class_mcd___7,
    MCD_hh = mridat_rad_class_mcd___8,
    MCD_meningocele = mridat_rad_class_mcd___9,
    MCD_encephalocele = mridat_rad_class_mcd___10,
    MCD_CC_dysgenesis = mridat_rad_class_mcd___11,
    MCD_chiari = mridat_rad_class_mcd___12,
    MCD_other = mridat_rad_class_mcd___13,
    MCD_none = mridat_rad_class_mcd___14,
    MCD_unknown = mridat_rad_class_mcd___15
  )

# make categorical variables factors
## sex
data$pt_sex <- factor(data$pt_sex,
                      levels = c(1,2,3),
                      labels = c("M", "F", "O"))

## referral category
data$pt_ref <- factor(data$pt_ref,
                      levels = c(1,2,3),
                      labels = c("FUS", "New.Dx", "DRE"))

## imaging statuses
### abnormality
data$pt_abnorm_status <- factor(data$pt_abnorm_status,
                                levels = c(1,2,3),
                                labels = c("normal", "abnormal", "incidental"))

### epileptogenic lesion
data$pt_ep_lesn_status <- factor(data$pt_ep_lesn_status,
                                 levels = c(1,2,3),
                                 labels = c("yes", "no", "uncertain"))

### motion
data$pt_motion_status <- factor(data$pt_motion_status,
                                levels = c(1,2,3),
                                labels = c("not.mentioned", "mild", "degraded"))

### repeat scan recommended
data$pt_repeat_status <- factor(data$pt_repeat_status,
                                levels = c(1,2),
                                labels = c("yes", "no"))

# descriptive stats----
# > create variable containing all unique id
unique_id = unique(data$record_id)
# > create variable containing all MCD columns
mcd_types = c("MCD_BOSD",
              "MCD_FCD",
              "MCD_tuber",
              "MCD_GMH_PVNH",
              "MCD_GMH_subnod",
              "MCD_GMH_band",
              "MCD_polymicrogyria",
              "MCD_hh",
              "MCD_meningocele",
              "MCD_encephalocele",
              "MCD_CC_dysgenesis",
              "MCD_chiari",
              "MCD_other")

## identify patients with varying numbers of MCD
## > make a new column to store info on lesion count for each patient
data$num_mcd = NaN

for (id in unique_id) {
  # pull the data for this record
  t.record = which(data$record_id == id)
  # get MCD columns for this patient
  t.mcd.data = data[t.record, mcd_types] 
  if (rowSums(t.mcd.data, na.rm = TRUE) == 3) {
    # there are exactly three MCDs 
    data[t.record, "num_mcd"] = 3
  } else if (rowSums(t.mcd.data, na.rm = TRUE) == 2) {
    # there are exactly two MCDs 
    data[t.record, "num_mcd"] = 2
  } else if (rowSums(t.mcd.data, na.rm = TRUE) == 1) {
    # there is exactly one MCD 
    data[t.record, "num_mcd"] = 1
  } else {
    # no relevant MCD identified
    data[t.record, "num_mcd"] = 0
  }
} 

## counts of people with varying numbers of MCD
num_mcd <- factor(data$num_mcd, 
                  levels = c(0,1,2,3))

barplot(table(data$num_mcd),
        xlab = "Number of MCD",
        ylab = "Frequency")

# load in the TeleNP data
telenp.data = load("/Users/carmen/Library/CloudStorage/OneDrive-SharedLibraries-TheUniversityofMelbourne/Chris Tailby - CogLab_Projects/AEP/R_files/AEPTeleNP_json_read/allPtntCtrlData.RData")

# testing
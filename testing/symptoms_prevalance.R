# Symptom trajectories

## set working directory 

setwd("~/Documents/Projects/COVID_app")

## ---------------------------------------------------------------------------------------------------
# Loading required packages

library(rio) 
library(scales)
#library(plotly)
library(stringr)
library(ggrepel)
library(tidyverse)

## ---------------------------------------------------------------------------------------------------
# Importing files

data.path <- list.files("/Volumes/G-DRIVE mobile USB-C/covid19/Raw_data/ulfs_prediction_model_CURRENT/source_files/data_files", pattern = ".gz")[1]
latest_file <- as.character(readr::parse_number(data.path))

# Importing files
patients <- rio::import(paste0("/Volumes/G-DRIVE mobile USB-C/covid19/Raw_data/ulfs_prediction_model_CURRENT/source_files/data_files/patients_export_geocodes_", latest_file, ".csv"))
assessments <- rio::import(paste0("/Volumes/G-DRIVE mobile USB-C/covid19/Raw_data/ulfs_prediction_model_CURRENT/source_files/data_files/assessments_export_", latest_file, ".csv"))

# Subseting for yesterday's assessments
assessments$date <- as.Date(assessments$updated_at, format = "%Y-%m-%d")
assessments_recent <- assessments %>%
  filter(date == Sys.Date() - 2)

assessments$date <- NULL


## ---------------------------------------------------------------------------------------------------
# Merging Loss of smell with altered smell

# Convert empty strings to NAs
assessments_recent[assessments_recent == ""] <- NA

# Pre-process to accommodate to smell variable changes
# Create a new variable
assessments_recent$OR_losses <- 0

# Recode a bit - change NAs to 0s, logicals to integers
assessments_recent[is.na(assessments_recent$loss_of_smell),]$loss_of_smell <- 0
assessments_recent$loss_of_smell <- as.numeric(assessments_recent$loss_of_smell)

assessments_recent[is.na(assessments_recent$altered_smell),]$altered_smell <- 0
assessments_recent$altered_smell <- as.numeric(assessments_recent$altered_smell)

# Update OR_losses, R will hangle the logicals here
assessments_recent$OR_losses <- assessments_recent$loss_of_smell | assessments_recent$altered_smell

# "Archive" the variable
assessments_recent$loss_of_smell_old <- assessments_recent$loss_of_smell

# Assign the new variable to loss_of_smell
# Then drop the redundant variable
assessments_recent$loss_of_smell <- assessments_recent$OR_losses
assessments_recent$OR_losses <- NULL

# Check
table(assessments_recent$loss_of_smell)

## ---------------------------------------------------------------------------------------------------

# Subseting symptoms
assessments_symp <- assessments_recent[,c('patient_id','updated_at','health_status','persistent_cough', 'fatigue', 'delirium', 'shortness_of_breath', 'fever', 'diarrhoea', 
                                          'abdominal_pain', 'chest_pain', 'hoarse_voice', 'skipped_meals', 'loss_of_smell', 
                                          'sore_throat', 'unusual_muscle_pains', 'chills_or_shivers', 'eye_soreness')]

#assessments_symp[assessments_symp == ""] <- NA

# Recoding symptoms
assessments_symp[which(assessments_symp$fatigue == "mild"),]$fatigue <- FALSE
assessments_symp[which(assessments_symp$fatigue == "severe"),]$fatigue <- TRUE
assessments_symp[which(assessments_symp$fatigue == "no"),]$fatigue <- FALSE

assessments_symp[which(assessments_symp$shortness_of_breath == "mild"),]$shortness_of_breath <- FALSE
assessments_symp[which(assessments_symp$shortness_of_breath == "severe"),]$shortness_of_breath <- TRUE
assessments_symp[which(assessments_symp$shortness_of_breath == "significant"),]$shortness_of_breath <- TRUE
assessments_symp[which(assessments_symp$shortness_of_breath == "no"),]$shortness_of_breath <- FALSE

assessments_symp$fatigue <- as.logical(assessments_symp$fatigue)
assessments_symp$shortness_of_breath <- as.logical(assessments_symp$shortness_of_breath)

assessments_symp$updated_at <- as.Date(assessments_symp$updated_at, format = "%Y-%m-%d")

# Getting number of assessments per day
symptoms_obs <- assessments_symp %>%
  group_by(updated_at) %>%
  dplyr::tally()

names(symptoms_obs) <- c('date', 'total')

# Getting the number of symptoms 

symptoms_df <- assessments_symp %>%
  group_by(updated_at) %>%
  dplyr::summarize(sum(fever, na.rm = TRUE), sum(loss_of_smell, na.rm = TRUE), sum(skipped_meals, na.rm = TRUE), sum(persistent_cough, na.rm = TRUE),
                   sum(diarrhoea, na.rm = TRUE), sum(chest_pain, na.rm = TRUE), sum(hoarse_voice, na.rm = TRUE),  
                   sum(delirium, na.rm = TRUE), sum(fatigue, na.rm = TRUE), sum(shortness_of_breath, na.rm = TRUE), sum(abdominal_pain, na.rm = TRUE),
                   sum(sore_throat, na.rm = TRUE), sum(unusual_muscle_pains, na.rm = TRUE), sum(chills_or_shivers, na.rm = TRUE), sum(eye_soreness, na.rm = TRUE))
names(symptoms_df) <- c('date', 'fever', 'loss_of_smell', 'skipped_meals', 'persistent_cough', 'diarrhoea',
                        'chest_pain', 'hoarse_voice', 'delirium', 'fatigue', 'shortness_of_breath', 'abdominal_pain',
                        'sore_throat', 'unusual_muscle_pains', 'chills_or_shivers', 'eye_soreness')

symptoms_df <- symptoms_df %>%
  merge(symptoms_obs, by = "date")

symptoms_df2 <-symptoms_df %>%
  mutate(fever= fever/total, loss_of_smell = loss_of_smell/total, 
         skipped_meals = skipped_meals/total, persistent_cough = persistent_cough/total, diarrhoea = diarrhoea/total,
         chest_pain = chest_pain/total, hoarse_voice = hoarse_voice/total, delirium = delirium/total, 
         fatigue = fatigue/total, shortness_of_breath = shortness_of_breath/total, abdominal_pain=abdominal_pain/total,
         sore_throat = sore_throat/total, unusual_muscle_pains = unusual_muscle_pains/total, chills_or_shivers = chills_or_shivers/ total,
         eye_soreness = eye_soreness/total)

date_to_upd <- Sys.Date()
symptoms_df2 <- symptoms_df2 %>%
  filter(date >= "2020-04-29" & date < date_to_upd)

symptoms_df3 <- symptoms_df2[,1:16]

symptoms_df3[,2:16] <- symptoms_df3[,2:16] * 100 
# names(symptoms_df3) <- c('date','Feber', 'Nedsatt lukt eller smaksinne', 'Aptitlöshet', 'Ihållande hosta', 'Diarré', 'Bröstsmärta',
#                          'Heshet', 'Förvirring, desorientering, dåsighet', 'Svår trötthet', 'Måttliga/allvarliga andningsbesvär', 
#                          'Buksmärta')

# 
symptoms_saved <- rio::import("symptoms.csv")
tail(symptoms_saved)
# symptoms_saved <- symptoms_saved %>%
#   filter(date < "2021-03-08")
#symptoms_df4 <- symptoms_df4[1:291,]
symptoms_df4 <- rbind(symptoms_saved, symptoms_df3)
tail(symptoms_df4)

rio::export(symptoms_df4, "~Desktop/symptoms.csv")

# Daily numbers on patients and assessments
patients_df <- patients %>%
  group_by(id) %>%
  slice(1L)

patients_df <- patients[patients$reported_by_another == FALSE,]

patient_nrow <- nrow(patients_df) 
assessments_nrow <- nrow(assessments)

daily_numbers <- data.frame(patients_total = patient_nrow, assessments_total = assessments_nrow)
rio::export(daily_numbers, "~Desktop/daily_numbers.csv")

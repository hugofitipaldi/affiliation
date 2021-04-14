# data-raw/mydataset.R
# Data import and processing pipeline

countries_list <- rio::import("/Volumes/G-DRIVE mobile USB-C/Bibliometrics/extra_files/countries.csv")
universities_list <- rio::import("/Volumes/G-DRIVE mobile USB-C/Bibliometrics/extra_files/universities.csv")
states_list <- rio::import("/Volumes/G-DRIVE mobile USB-C/Bibliometrics/extra_files/states.csv")

# Data update
usethis::use_data(countries_list, universities_list, states_list, overwrite = TRUE)

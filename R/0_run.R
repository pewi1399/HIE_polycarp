rm(list = ls())

source("R/1_get_data.R", encoding = "utf-8")
source("R/2_diagnoses.R", encoding = "utf-8")
source("R/3_derivations.R", encoding = "utf-8")

analysdata <- readRDS("Output/3_derivations.rds")

mfr_vars<- c("FODKL", 
"EPILEPSI", 
"DIABETES")

snq_vars <- c("Antal_foster", 
"art_Glukos")

diagnosis_vars <- c("obstekat", 
"ruptur", 
"skulderdy", 
"Eklam", 
"prolaps", 
"abl")
  
derived_vars <- c("K_BJUD", 
"Framstupa", 
"vidoppet", 
"sate"
)

testfil <- cbind(
                analysdata[sample(1:nrow(analysdata), 10000, replace = TRUE), mfr_vars],
                analysdata[sample(1:nrow(analysdata), 10000, replace = TRUE), snq_vars],
                analysdata[sample(1:nrow(analysdata), 10000, replace = TRUE), diagnosis_vars],
                analysdata[sample(1:nrow(analysdata), 10000, replace = TRUE), derived_vars]
)

haven::write_sav(testfil, "Output/0_testfil.sav")

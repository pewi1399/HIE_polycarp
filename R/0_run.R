rm(list = ls())

system.time({
  source("R/1_get_data.R", encoding = "utf-8")
  })

system.time({
  source("R/2_diagnoses.R", encoding = "utf-8")
  })

system.time({
  source("R/3_derivations.R", encoding = "utf-8")
  })


# ------------------------------- write data -----------------------------------
analysdata <- readRDS("Output/3_derivations.rds")

haven::write_sav(analysdata, "Output/0_analysdata.sav")
write.csv2(analysdata, "Output/0_analysdata.csv")


keep_vars <-grep("DIAG|FLOP|_par", names(analysdata), value = TRUE, invert = TRUE)
analysdata_mini <- analysdata[,keep_vars]

haven::write_sav(analysdata_mini, "Output/0_analysdata_mini.sav")
haven::write_sav(analysdata_mini[10000, ], "Output/0_analysdata_mini_10000.sav")




#--------------------------------- testfil -------------------------------------

#mfr_vars<- c("FODKL", 
#"EPILEPSI", 
#"DIABETES")
#
#snq_vars <- c("Antal_foster", 
#"art_Glukos")
#
#diagnosis_vars <- c("obstekat", 
#"ruptur", 
#"skulderdy", 
#"Eklam", 
#"prolaps", 
#"abl")
#  
#derived_vars <- c("K_BJUD", 
#"Framstupa", 
#"vidoppet", 
#"sate"
#)
#
#robson_vars <- grep("robson", names(analysdata), value = TRUE)
#
#testfil <- cbind(
#                #analysdata[sample(1:nrow(analysdata), 10000, replace = TRUE), mfr_vars],
#                #analysdata[sample(1:nrow(analysdata), 10000, replace = TRUE), snq_vars],
#                #analysdata[sample(1:nrow(analysdata), 10000, replace = TRUE), diagnosis_vars],
#                #analysdata[sample(1:nrow(analysdata), 10000, replace = TRUE), derived_vars],
#                 analysdata[sample(1:nrow(analysdata), 10000, replace = TRUE),robson_vars]
#)
#
#
#haven::write_sav(testfil, "Output/0_testfil_robson.sav")

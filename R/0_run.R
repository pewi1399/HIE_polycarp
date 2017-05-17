rm(list = ls())

library(dplyr)

if(FALSE){
  system.time({
    source("R/1_get_data.R", encoding = "utf-8") #158.24s
    })
  
  system.time({
    source("R/2_diagnoses.R", encoding = "utf-8") # 85.94s
    })
  
  system.time({
    source("R/3_derivations.R", encoding = "utf-8") #160.45s
    })
}

# ------------------------------- write data -----------------------------------
analysdata <- readRDS("Output/3_derivations.rds")

analysdata$MFODLAND <- gsub("å", "a", analysdata$MFODLAND)
analysdata$MFODLAND <- gsub("ä", "a", analysdata$MFODLAND)
analysdata$MFODLAND <- gsub("ö", "o", analysdata$MFODLAND)
analysdata$MFODLAND <- gsub("Å", "A", analysdata$MFODLAND)
analysdata$MFODLAND <- gsub("Ä", "A", analysdata$MFODLAND)
analysdata$MFODLAND <- gsub("Ö", "O", analysdata$MFODLAND)


analysdata$YRKE <- gsub("å", "a", analysdata$YRKE)
analysdata$YRKE <- gsub("ä", "a", analysdata$YRKE)
analysdata$YRKE <- gsub("ö", "o", analysdata$YRKE)
analysdata$YRKE <- gsub("Å", "A", analysdata$YRKE)
analysdata$YRKE <- gsub("Ä", "A", analysdata$YRKE)
analysdata$YRKE <- gsub("Ö", "O", analysdata$YRKE)

analysdata$SJUKHUSNAMN <- gsub("å", "a", analysdata$SJUKHUSNAMN)
analysdata$SJUKHUSNAMN <- gsub("ä", "a", analysdata$SJUKHUSNAMN)
analysdata$SJUKHUSNAMN <- gsub("ö", "o", analysdata$SJUKHUSNAMN)
analysdata$SJUKHUSNAMN <- gsub("Å", "A", analysdata$SJUKHUSNAMN)
analysdata$SJUKHUSNAMN <- gsub("Ä", "A", analysdata$SJUKHUSNAMN)
analysdata$SJUKHUSNAMN <- gsub("Ö", "O", analysdata$SJUKHUSNAMN)


keep_vars <-grep("DIAG|FLOP|_par|par_|SJUKHUSNAMN|YRKE", names(analysdata), value = TRUE, invert = TRUE)
priovars <- openxlsx::read.xlsx("Indata/variabel_prioritering.xlsx")
prio <- priovars %>% 
  filter(primary == 1)


keep_vars <- keep_vars[keep_vars %in% prio$Variabel]


analysdata_mini <- analysdata[, keep_vars]
#table(duplicated(toupper(names(analysdata))))
#chars = sapply(analysdata, class) == "character"
#characterdata <- analysdata[,chars]




#---------------------------- write full data ---------------------------------- 

if(FALSE){
  #system.time({
  #    haven::write_sav(analysdata, "Output/0_analysdata.sav") #68.66 
  #  })
  
 # system.time({
 #   haven::write_dta (analysdata, "Output/0_analysdata.dta") #ERROR string to long
 #   })
  
 # system.time({
 #   openxlsx::write.xlsx(analysdata, "Output/0_analysdata.xlsx") # Probable Memory Error
 #   })
  
  system.time({
    write.csv2(analysdata, "Output/0_analysdata.csv", 
               na = "", 
               row.names = FALSE)
    }) #175.44
  
  system.time({
    write.table(analysdata, "Output/0_analysdata.txt",
                sep = "\t",
                row.names = FALSE,
                na = "") 
  }) # 186.27 
  
}
#-------------------------------------------------------------------------------

#------------------------------- write minidata --------------------------------
if(FALSE){
  
  #system.time({
  #  haven::write_sav(analysdata_mini, "Output/0_analysdata_mini.sav") #25.35
  #  })
  #
  #system.time({
  #  haven::write_dta(analysdata_mini, "Output/0_analysdata_mini.dta") #25.09s 
  #  })
  
  #system.time({
  #  openxlsx::write.xlsx(analysdata_mini, "Output/0_analysdata_mini.xlsx") # Memory Error
  #  })
  
  system.time({
    write.csv2(analysdata_mini, "Output/0_analysdata_mini.csv", 
               na = "", 
               row.names = FALSE) 
    }) #101.15 
  
  system.time({
    write.table(analysdata_mini, "Output/0_analysdata_mini.txt",
                sep = "\t",
                row.names = FALSE,
                na = "") 
  }) # 93.39 

  
}

#write kompletteringsdata
if(FALSE){
  
  kompletteringVars <- c("lopnr_barn",
                         "O903",
                         "P50",
                         "P500",
                         "P501",
                         "P502",
                         "P503",
                         "P504",
                         "P505",
                         "P508",
                         "P509",
                         "BVIKT",
                         "S_Bvikt",
                         "B_vikt",
                         "TSECAR",
                         "TSECTIO",
                         "antalsectio")
  
  analysdata_komplettering <- analysdata[, kompletteringVars]
  
  system.time({
    write.table(analysdata_komplettering, "Output/0_analysdata_komplettering.txt",
                sep = "\t",
                row.names = FALSE,
                na = "") 
  })
  
  
  
  
}

if(FALSE){
  viktVars <- c("lopnr_barn",
                         "BVIKT",
                         "S_Bvikt",
                         "B_vikt")
  
  analysdata_vikt <- analysdata[, viktVars]
  
  system.time({
    write.table(analysdata_vikt, "Output/0_analysdata_vikt.txt",
                sep = "\t",
                row.names = FALSE,
                na = "") 
  })
}


#--------------------------------- testfil -------------------------------------
# system.time({
#   haven::write_sav(analysdata_mini[1:1000,], "Output/tmp.sav") #25.35
# })
# 
# system.time({
#   write.csv2(analysdata_mini[1:1000,], "Output/tmp.csv", 
#              na = "", 
#              row.names = FALSE) #25.35
# })

# mini1000 <- analysdata_mini[1:10000, ]
# haven::write_sav(mini1000, "Output/0_analysdata_mini_10000.sav")


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

rm(list = ls())
library(dplyr)
library(testthat)
library(data.table)
library(tidyr)
library(openxlsx)

# read data
dat <- readRDS("Output/1_get_data.rds")

#--------------------------- calculate cases per year --------------------------
table(dat$AR)
table(dat$KON)



#-------------------------------------------------------------------------------
snq_vars <- c("Antal_foster", "BARN_Ordn_nr", "G___v_", "K_n", "Vattenavg_ng", 
             "art_pH", "art_PO2", "art_PCO2", "art_BE", "art_Laktat", "art_Glukos", 
             "Ven_pH", "Ven_pO_2", "Ven_pCO_2", "Ven_BE", "Ven_Laktat", "Ven_Glukos", 
             "Post_pH", "Post_BE", "HLR__tg_rder", "Extra_O_2", "Hj_rtmassage", 
             "CPAP_via_mask", "Adrenalin", "Ventilation_via_mask", "Acidoskorrektion", 
             "Intubation", "Neonatolog", "Barn__specialist", "Barn__ej_specialist", 
             "Narkos_l_kare", "OB_GYN_l_kare", "Annan_l_kare", "Ingen_l_kare", 
             "Ink_fr_sjh__vr", "Avdelning", "X_1a_inskrivning__datum", "X_1a_inskrivning___lder__dagar_", 
             "X_1a_inskrivning__sjukhus", "L_gsta_MABP", "L_gsta_temp_vid_inl_ggning", 
             "L_gsta_pH", "L_gsta_PaO2_FiO2_kvot", "Multipla_kramper", "Diures__0_12_tim", 
             "SNAP_II_score", "Ingen_andningsst_rning", "NAS", "PAS", "MAS", 
             "RDS", "Pneumothorax", "Thoraxdr_nage", "PPHN", "Kramper", "HIE", 
             "H_gst_HIE", "Antiepileptisk_beh_vid_utskrivni", "Behandlad_med_hypotermi", 
             "Hypoglukemi___2_6_efter_3_tim_", "Insulinbehandling", "Ljusbeh", 
             "Hyperbilirubinemi", "H_gsta_bilirubin_v_rde", "Missb__kromosom_avv_", 
             "Alla_diagnoser", "Alla_missbildningar", "Alla_andra__tg_rder", 
             "V_rdtid__inneliggande", "Utskriven_till", "Avliden_enl_SNQ", 
             "Avliden_datum", "Avliden_tid", "X_lder_vid_d_dsfall__dagar_", 
             "Obduktion", "Missbildning", "Perinatal_asfyxi", "Andningssjd", 
             "Intrakran__bl_dning", "Pneumothorax_0001", "Infektion", "Kromosomavvikelse", 
             "Annan_faktor", "Avliden_enl_DOR_datum", "D_dsorsaken_bed_ms_vara_relatera", 
             "lopnr_mamma_snq") 
  
# create temporary dataset
tmp <- dat[1:10000,snq_vars]

# split variables by class
snq_classes <- sapply(dat[,snq_vars], class)

table(snq_classes)

snq_numeric <- names(snq_classes[snq_classes == "numeric"])
snq_integer <- names(snq_classes[snq_classes == "integer"])
snq_factor <- names(snq_classes[snq_classes == "factor"])


test_that({"No vars dropped, length of pieces is equal to length of total"},
          expect_equal(length(snq_numeric) + length(snq_integer) + length(snq_factor)
                       , expect = length(snq_vars)
                       )
)

# list all factors
factor_table_list_before <- lapply(tmp[,snq_factor], function(x){ table(x, useNA = "always")})

# correct all factors converting dots to NA
tmp[,snq_factor] <- lapply(tmp[,snq_factor], as.character)
tmp[,snq_factor] <- lapply(tmp[,snq_factor], function(x){ifelse(x == "", NA, x)})

# list all corrected factors
factor_table_list_after <- lapply(tmp[,snq_factor], function(x){ table(x, useNA = "always")})

wb <- openxlsx::createWorkbook()
for(variable in snq_factor){
  print(variable)
  
  # max length for sheetname is 31 characters
  openxlsx::addWorksheet(wb = wb, sheetName = substr(variable,1,31))
  
  oldData <- data.frame(factor_table_list_before[variable])
  names(oldData) <- c("Notering", "Antal")
  
  newData <- data.frame(factor_table_list_after[variable])
  names(newData) <- c("Notering", "Antal")
  
  addStyle(wb, 
           sheet = substr(variable,1,31), 
           createStyle(fgFill = "#FF6666"),
           rows = 1:(nrow(oldData)+1),
           cols = 1:2,
           gridExpand = TRUE
           )
  
  addStyle(wb, 
           sheet = substr(variable,1,31), 
           createStyle(fgFill = "#99e699"),
           rows = 1:(nrow(newData)+1),
           cols = 4:5,
           gridExpand = TRUE
  )
  
  
  openxlsx::writeData(wb = wb, 
                      sheet = substr(variable, 1,31),
                      startCol = 1,
                      oldData
  )
  openxlsx::writeData(wb = wb, 
                      sheet = substr(variable, 1,31),
                      startCol = 4,
                      newData
  )
}

openxlsx::saveWorkbook(wb, "Output/1_snq_tables.xlsx", overwrite = TRUE)

#testfile temporary export
out <- tmp[,c("Hj_rtmassage", "Vattenavg_ng")]
names(out) <- c("Hjartmassage", "Vattenavgang")

haven::write_dta(out, "Output/test_snq.dta")
haven::write_sav(out, "Output/test_snq.sav")


t1 <- haven::read_dta("Output/test_snq.dta")
t2 <- haven::read_sav("Output/test_snq.sav")
#-------------------------------------------------------------------------------



tmp[,snq_vars] <- lapply(tmp[,snq_vars], as.character)

str(tmp)
# check values
lapply(tmp[,snq_vars], table)
lapply(tmp[,snq_vars], function(x){ table(x == "")})

tmp[,snq_vars] <- lapply(tmp[,snq_vars], function(x){ ifelse(x == "", NA, x)})

lapply(tmp[,snq_vars], function(x){ table(x == "")})


rm(list = ls())
library(dplyr)
library(testthat)
library(data.table)
library(tidyr)

# read data
dat <- readRDS("Output/2_diagnoses.rds")
dat <- data.frame(dat)

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

snq <- dat[, snq_vars]

snq_classes <- sapply(dat[,snq_vars], class)
snq_factor <- names(snq_classes[snq_classes == "factor"])

# correct all factors converting dots to NA
snq[,snq_factor] <- lapply(snq[,snq_factor], as.character)
snq[,snq_factor] <- lapply(snq[,snq_factor], function(x){ifelse(x == "", NA, x)})

# create factor ensuring no unused levels are inherited
snq[,snq_factor] <- lapply(snq[,snq_factor], as.factor)

# create function that creates a dictionary
dictionary_builder <- function(fvar, data){
  
  print(fvar)
  
  labels <- levels(data[, fvar])
  out <- data.frame(variable = fvar,
                    value = 1:length(labels), 
                    label = labels)
  return(out)
}

factor_dictionary <- lapply(snq_factor, dictionary_builder, data = snq)
factor_dictionary <- do.call("rbind", factor_dictionary)


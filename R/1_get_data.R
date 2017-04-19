rm(list = ls())
library(dplyr)
library(testthat)
library(data.table)

# print delivered files
list.files("Indata/Leverans_April_2017")

#-------------------------- read in .txt files ---------------------------------
snq <- read.table("Indata/Leverans_April_2017/KVALREG_SNQ_8467_2016.txt", 
                  sep = "\t", 
                  header = TRUE
                  )

mfr <- read.table("Indata/Leverans_April_2017/UT_MFR_8467_2016.txt", 
                  sep = "\t", 
                  header = TRUE
)

par_ov <- read.table("Indata/Leverans_April_2017/UT_PAR_OV_8467_2016.txt", 
                  sep = "\t", 
                  header = TRUE
)

par_sv <- read.table("Indata/Leverans_April_2017/UT_PAR_SV_8467_2016.txt", 
                  sep = "\t", 
                  header = TRUE
)
#-------------------------------------------------------------------------------

#-------------------------------- snq validation -------------------------------
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
snq <- dat[1:10000,snq_vars]

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
factor_table_list_before <- lapply(snq[,snq_factor], function(x){ table(x, useNA = "always")})

# correct all factors converting dots to NA
snq[,snq_factor] <- lapply(snq[,snq_factor], as.character)
snq[,snq_factor] <- lapply(snq[,snq_factor], function(x){ifelse(x == "", NA, x)})

# list all corrected factors
factor_table_list_after <- lapply(snq[,snq_factor], function(x){ table(x, useNA = "always")})

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
#-------------------------------------------------------------------------------



#-------------------------- apply tests and merge ------------------------------
mfr_n_barn <- sum(table(unique(mfr$lopnr_barn)))
mfr_n_mamma <- sum(table(unique(mfr$lopnr_mamma)))
par_n_ov <- sum(table(unique(par_ov$lopnr)))
par_n_sv <- sum(table(unique(par_sv$lopnr)))
snq_n_barn <- sum(table(unique(snq$lopnr_barn)))
snq_n_mamma <- sum(table(unique(snq$lopnr_mamma)))

overlapp_snq_mfr_barn <- sum(snq$lopnr_barn %in% mfr$lopnr_barn)
overlapp_snq_mfr_mamma <- sum(snq$lopnr_mamma %in% mfr$lopnr_mamma)

mamma_lista <- unique(c(mfr$lopnr_barn, mfr$lopnr_mamma))
barn_lista <- unique(c(snq$lopnr_barn, mfr$lopnr_barn))

# create summary dataset 
listing_dataframe <- data.frame(
  "Källa" = c("mfr", "snq", "par_ov", "par_sv"),
  "Antal_rader" = c(nrow(mfr), nrow(snq), nrow(par_ov), nrow(par_sv)), 
  "Antal_mor" = c(mfr_n_mamma, snq_n_mamma, sum(mamma_lista %in% par_ov$lopnr), sum(mamma_lista %in% par_sv$lopnr)),
  "Antal_barn" = c(mfr_n_barn, snq_n_barn, sum(barn_lista %in% par_ov$lopnr), sum(barn_lista %in% par_sv$lopnr))
  )

overlap_dataframe <- data.frame(
  mammor_i_snq_som_finns_i_mfr = overlapp_snq_mfr_mamma,
  barn_i_snq_som_finns_i_mfr = overlapp_snq_mfr_barn,
  totalt_antal_mammor = length(mamma_lista),
  totalt_antal_barn = length(barn_lista)
  )


test_that({"no duplicated mfr kids"},
          expect_equal(nrow(mfr), expect = mfr_n_barn)
)

test_that({"no duplicated snq kids"},
          expect_equal(nrow(snq), expect = snq_n_barn)
)

# check that only kids are present in par
test_that({"no par data from mothers"},
 expect_equal(
   sum(mfr$lopnr_mamma %in% par_ov$lopnr) + sum(mfr$lopnr_mamma %in% par_sv$lopnr),
   expect = 0
   )
)

# create diagnose variables for mfr
# create singular column for diagnosis
test_that({"mfr is a data.frame and not a data.table"},
          expect_equal(
            class(mfr),
            expect = "data.frame"
          )
)


mfr$MDIAG <- do.call(paste, c(mfr[,grep("MDIAG", names(mfr))]))
# format
mfr$MDIAG <- gsub(" NA.*$", "", mfr$MDIAG)
mfr$MDIAG <- paste(" ", mfr$MDIAG, sep = "")
# create singular column for diagnosis
mfr$BDIAG <- do.call(paste, c(mfr[,grep("BDIAG", names(mfr))]))
# format
mfr$BDIAG <- gsub(" NA.*$", "", mfr$BDIAG)
mfr$BDIAG <- paste(" ", mfr$BDIAG, sep = "")


# create personal variable for diagnosis from par
par <- setDT(rbind_list(par_ov, par_sv))

par <- par[,
    list(par_diagnoses = paste(DIAGNOS, collapse = " "),
         par_op = paste0(OP, collapse = " ")
         )
    , by = "lopnr"]

# merge par data with mfr
###########################
###########################
###########################
# Antag att mfr är personvektor och att vi endast tar med den första 
# förekomsten av en individ både i snq oc i mfr. Detta är endast en temporär lösning!

mfr <- mfr[!duplicated(mfr$lopnr_barn),]

snq <- 
  snq %>% 
  filter(!duplicated(lopnr_barn)) %>% 
  rename(lopnr_mamma_snq = lopnr_mamma)



# merge analysis files
tmp <- merge(mfr, snq, by = "lopnr_barn", all.x = TRUE)
###########################
###########################
###########################

# merge par once for kids and once for mothers
par_mamma <-
  par %>% 
  mutate(par_op_mamma = par_op,
         par_diagnoses_mamma = par_diagnoses,
         lopnr_mamma = lopnr) %>% 
  select(lopnr_mamma, par_op_mamma, par_diagnoses_mamma)

par_barn <-
  par %>% 
  mutate(par_op_barn = par_op,
         par_diagnoses_barn = par_diagnoses,
         lopnr_barn = lopnr) %>% 
  select(lopnr_barn, par_op_barn, par_diagnoses_barn)

tmp <- merge(tmp, par_barn, by = "lopnr_barn", all.x = TRUE)




# --------------------------- create out dataset -------------------------------
out <- tmp
test_that({"expect same ids in person vector as in outfile"},
          expect_true(all(out$lopnr_barn %in% mfr$lopnr_barn) & all(mfr$lopnr_barn %in% out$lopnr_barn))
          )

# outdata
saveRDS(out, "Output/1_get_data.rds")
write.csv2(out, "Output/1_get_data.csv")

# write log files 
write.csv2(listing_dataframe, "Output/1_listings.csv")
write.csv2(overlap_dataframe, "Output/1_overlap.csv")
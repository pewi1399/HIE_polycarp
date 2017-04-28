rm(list = ls())
library(dplyr)
library(testthat)
library(data.table)
library(openxlsx)

# print delivered files
list.files("Indata/Leverans_April_2017")

#-------------------------- read in .txt files ---------------------------------
snq <- read.table("Indata/Leverans_April_2017/KVALREG_SNQ_8467_2016.txt", 
                  sep = "\t",
                  quote = "",
                  header = TRUE
                  )

mfr <- read.table("Indata/Leverans_April_2017/UT_MFR_8467_2016.txt", 
                  sep = "\t",
                  quote = "",
                  header = TRUE
)
#mfr <- haven::read_sas("Indata/Leverans_April_2017/UT_MFR_8467_2016.sas7bdat")

par_ov <- read.table("Indata/Leverans_April_2017/UT_PAR_OV_8467_2016.txt", 
                  sep = "\t",
                  quote = "",
                  header = TRUE
)

par_sv <- read.table("Indata/Leverans_April_2017/UT_PAR_SV_8467_2016.txt", 
                  sep = "\t",
                  quote = "",
                  header = TRUE
)
#-------------------------------------------------------------------------------

#--------------------------------- mfr validation ------------------------------
#mfr <- mfr[1:1000, ]
mfr_vars <- names(mfr)

# create temporary dataset
#mfr <- dat[,mfr_vars]

# split variables by class
mfr_classes <- sapply(mfr, class)

#table(mfr_classes)

mfr_numeric <- names(mfr_classes[mfr_classes == "numeric"])
mfr_integer <- names(mfr_classes[mfr_classes == "integer"])
mfr_factor <- names(mfr_classes[mfr_classes == "factor"])
mfr_logical <- names(mfr_classes[mfr_classes == "logical"])


test_that({"No vars dropped, length of pieces is equal to length of total"},
          expect_equal(length(mfr_numeric) + length(mfr_integer) + 
                         length(mfr_factor)+ length(mfr_logical)
                       , expect = length(mfr_vars)
          )
)

# list all factors
table_list_before_mfr <- lapply(mfr[,c(mfr_integer, mfr_logical)], function(x){ table(x, useNA = "always")})


# correct all factors converting dots to NA
mfr[,mfr_factor] <- lapply(mfr[,mfr_factor], as.character)
mfr[,mfr_factor] <- lapply(mfr[,mfr_factor], function(x){ifelse(x == "", NA, x)})

# integers are most problematic
singular_mfr <- sapply(mfr[,mfr_integer], function(x){ length(table(x)) <= 1})


# replace singulars NA with 0
mfr[,mfr_integer[singular_mfr]] <- lapply(mfr[,mfr_integer[singular_mfr]], function(x){ifelse(is.na(x), 0, x)})


# list all factors
table_list_after_mfr <- lapply(mfr[,c(mfr_integer, mfr_logical)], function(x){ table(x, useNA = "always")})

wb <- openxlsx::createWorkbook()
for(variable in c(mfr_integer, mfr_logical)){

  
  if(variable %in% c("lopnr_mamma", "lopnr_barn")){ next }
  
  print(variable)
  
  # max length for sheetname is 31 characters
  openxlsx::addWorksheet(wb = wb, sheetName = substr(variable,1,31))
  
  oldData <- data.frame(table_list_before_mfr[variable])
  names(oldData) <- c("Notering", "Antal")
  
  newData <- data.frame(table_list_after_mfr[variable])
  names(newData) <- c("Notering", "Antal")

  
  
  openxlsx::addStyle(wb, 
                     sheet = substr(variable,1,31), 
                     createStyle(fgFill = "#FF6666"),
                     rows = 1:(nrow(oldData)+1),
                     cols = 1:2,
                     gridExpand = TRUE
  )
  
  openxlsx::addStyle(wb, 
                     sheet = substr(variable,1,31), 
                     createStyle(fgFill = "#99e699"),
                     rows = 1:(nrow(newData)+1),
                     cols = 4:6,
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

openxlsx::saveWorkbook(wb, "Output/1_mfr_tables.xlsx", overwrite = TRUE)


#-------------------------------------------------------------------------------

#-------------------------------- snq validation -------------------------------
snq_vars <- names(snq)

# create temporary dataset
#snq <- dat[,snq_vars]

# split variables by class
snq_classes <- sapply(snq, class)

#table(snq_classes)

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

# create factor ensuring no unused levels are inherited
snq[,snq_factor] <- lapply(snq[,snq_factor], as.factor)

# create function that creates a dictionary
dictionary_builder <- function(fvar, data){
  
  print(fvar)
  
  labels <- levels(data[, fvar])
  out <- data.frame(var = fvar,
                    value = 1:length(labels), 
                    label = labels)
  return(out)
}

factor_dictionary <- lapply(snq_factor, dictionary_builder, data = snq)
factor_dictionary <- do.call("rbind", factor_dictionary)

# convert factors back to numeric 
snq[,snq_factor] <- lapply(snq[,snq_factor], as.numeric)


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
  names(newData) <- c("value", "Antal")
  
 newData <- merge(newData, subset(factor_dictionary, var == variable), by = "value", all.x = TRUE) %>% 
   select(value, label, Antal)
  
  
  openxlsx::addStyle(wb, 
           sheet = substr(variable,1,31), 
           createStyle(fgFill = "#FF6666"),
           rows = 1:(nrow(oldData)+1),
           cols = 1:2,
           gridExpand = TRUE
  )
  
  openxlsx::addStyle(wb, 
           sheet = substr(variable,1,31), 
           createStyle(fgFill = "#99e699"),
           rows = 1:(nrow(newData)+1),
           cols = 4:6,
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
openxlsx::write.xlsx(factor_dictionary, "Output/1_snq_factordictionary.xlsx", overwrite = TRUE)
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


# test_that({"no duplicated mfr kids"},
#           expect_equal(nrow(mfr), expect = mfr_n_barn)
# )
# 
# test_that({"no duplicated snq kids"},
#           expect_equal(nrow(snq), expect = snq_n_barn)
# )

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

# create singular column for diagnosis
mfr$MFLOP <- do.call(paste, c(mfr[,grep("^FLOP", names(mfr))]))
# format
mfr$MFLOP <- gsub(" NA.*$", "", mfr$MFLOP)
mfr$MFLOP <- paste(" ", mfr$MFLOP, sep = "")

# create singular column for diagnosis
mfr$BFLOP <- do.call(paste, c(mfr[,grep("^BFLOP", names(mfr))]))
# format
mfr$BFLOP <- gsub(" NA.*$", "", mfr$BFLOP)
mfr$BFLOP <- paste(" ", mfr$BFLOP, sep = "")


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

#dupes <- mfr[duplicated(mfr$lopnr_barn),"lopnr_barn"]
#tmp <- mfr %>% filter(lopnr_barn %in% dupes)

mfr <- mfr[!duplicated(mfr$lopnr_barn),]


#dupes <- snq[duplicated(snq$lopnr_barn),"lopnr_barn"]
#tmp <- snq %>% filter(lopnr_barn %in% dupes)


snq$finns_i_snq <- 1

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
#write.csv2(out, "Output/1_get_data.csv")

raw_dataframe <- data.frame(
  antal_rader = nrow(out),
  antal_barn = sum(table(unique(out$lopnr_barn))),
  antal_mor = sum(table(unique(out$lopnr_mamma))),
  antal_snq = sum(out$finns_i_snq, na.rm = TRUE)
  )

# write log files 
openxlsx::write.xlsx(listing_dataframe, "Output/1_listings.xlsx")
openxlsx::write.xlsx(overlap_dataframe, "Output/1_overlap.xlsx")
openxlsx::write.xlsx(raw_dataframe, "Output/1_info_analysfil.xlsx")

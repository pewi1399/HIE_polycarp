rm(list=ls())
path = "/home/per/Polycarp/KBH/P104_HIE"

library(data.table)
library(XLConnect)
library(testthat)
library(haven)
#--------------------------- read in datafiles ---------------------------------
dat0 <- data.table(readRDS(file.path(path,"Output","analysdataHIE_1_0.rds")))
dat0 <- dat0[1:1000,]

regions <- readWorksheetFromFile(file.path(path, "Indata", "regioner.xlsx"), sheet = 1)
countries <- readWorksheetFromFile(file.path(path,"Indata", "fodland.xlsx"), sheet = 1)
#----------------------- Read in diagnosis files -------------------------------
# Diagnoses
metadata <- readWorksheetFromFile("/home/per/Desktop/dataDictionary_160330.xlsx", sheet = 1)
# collapse all code variables
diags <- grep("kod", names(metadata), value = TRUE)
#insert leading spaces
metadata[, diags] <- lapply(metadata[, diags], function(x) paste(" ",x, sep = ""))
# assemble search phrases
metadata$search <- do.call(paste, c(metadata[,grep("kod", names(metadata))], sep="|"))
# searches should start with an exact match but may end on any string
metadata$search <- gsub("\\|\\ NA.*$", "", metadata$search)
# drop "kod" vars
metadata <- metadata[,grep("^kod", names(metadata), invert = TRUE)]
metadataMor <- subset(metadata, Group == "mor")
metadataBarn <- subset(metadata, Group == "barn")
# split singles and factors
metadataMorSingle <- subset(metadataMor, factor == "nej")
metadataMorFactor <- subset(metadataMor, factor == "ja")
# test that all diags are still there
test_that("rows add up",
          expect_equal(nrow(subset(metadata, Group == "mor" | Group == "barn")), nrow(metadataMorSingle) +
                         nrow(metadataMorFactor) + nrow(metadataBarn))
)
# if all child diagnoses are not of factor type then no need to split "metadatabarn"
test_that("no factor variables among kids",
          expect_false(any(metadataBarn$factor!="nej"))
)
# create singular column for diagnosis
dat0$MDIAG <- do.call(paste, c(dat0[,grep("MDIAG", names(dat0)), with = FALSE]))
# format
dat0$MDIAG <- gsub(" NA.*$", "", dat0$MDIAG)
dat0$MDIAG <- paste(" ", dat0$MDIAG, sep = "")
# create singular column for diagnosis
dat0$BDIAG <- do.call(paste, c(dat0[,grep("BDIAG", names(dat0)), with = FALSE]))
# format
dat0$BDIAG <- gsub(" NA.*$", "", dat0$BDIAG)
dat0$BDIAG <- paste(" ", dat0$BDIAG, sep = "")
# apply search phrase on these
applySearch <- function(variable, phrase){
  ## variable: variable containing ICD diag
  ## phrase: a string containing string or regex to be used for matching
  out<- ifelse(grepl(phrase, variable), 1,0)
  return(out)
}
# create diagnosis variables
dat0[,(metadataBarn$variable):=lapply(metadataBarn$search, applySearch, variable = dat0$BDIAG),]
dat0[,(metadataMorSingle$variable):=lapply(metadataMorSingle$search, applySearch, variable = dat0$MDIAG),]
# loop in factor variables
metadataMorFactor$factorLevel <- ifelse(is.na(metadataMorFactor$factorLevel),
                                        metadataMorFactor$variable,metadataMorFactor$factorLevel)
diags <-unique(metadataMorFactor$variable)
dat0 <- data.frame(dat0)
for(i in 1:length(diags)){
  dat0[,diags[i]] <- 0
  sub <- subset(metadataMorFactor, variable == diags[i])
  for(j in 1:nrow(sub)){
    dat0[,diags[i]] <- ifelse(grepl(sub[j, "search"], dat0$MDIAG), paste0(j,". ", sub[j , "factorLevel"]), dat0[,diags[i]])
  }
}
setDT(dat0)
#-------------------------- derivation of regions ------------------------------
# Hospital levels and regions
sjukhusniva <- structure(regions$Sjukhusnivå, .Names = regions$Sjukhus)
dat0$sjukhusniva <- sjukhusniva[dat0$sjukhusnamn]
sjukvardsregion <- structure(regions$Region, .Names = regions$Sjukhus)
dat0$sjukhvardsregion <- sjukvardsregion[dat0$sjukhusnamn]
# Countries
landsgrupp <- structure(countries$kod, .Names = countries$land)
dat0$landsgrupp <- landsgrupp[dat0$MFODLAND]
varldsdel <- structure(countries$grupp, .Names = countries$land)
dat0$varldsdel <- varldsdel[dat0$MFODLAND]
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# derivation from mfr variables
dat0$MLANGD <- ifelse(dat0$MLANGD>200 | dat0$MLANGD < 130, NA, dat0$MLANGD)
test_that("no values are outside of range",
          expect_true(max(dat0$MLANGD, na.rm=TRUE)<=200 & min(dat0$MLANGD, na.rm=TRUE)>=130)
)
# apgar no higher than 10 or lower than 1
apgarVars <- grep("^APGAR", names(dat0), value = TRUE)
dat0[, (apgarVars):=lapply(.SD, function(x) ifelse(x > 10 | x < 1, NA, x)), .SDcols = apgarVars]
# Age of mother at birth
# mothers birthdates are not always known
# assume born in the middle of each year/month if that data is not present
dat0$MFODDAT <- gsub(" ", "", dat0$MFODDAT)
dat0$MFODDAT <- ifelse(nchar(dat0$MFODDAT) == 4, paste0(dat0$MFODDAT, "0615"), dat0$MFODDAT)
dat0$MFODDAT <- ifelse(nchar(dat0$MFODDAT) == 6, paste0(dat0$MFODDAT, "15"), dat0$MFODDAT)
test_that("all dates are of valid length",
          expect_true(max(nchar(dat0$MFODDAT))==8)
)
dat0$MFODDAT <- as.Date(dat0$MFODDAT, format = "%Y%m%d")
dat0$BFODDAT <- as.character(dat0$BFODDAT)
dat0$BFODDAT <- as.Date(dat0$BFODDAT, format = "%Y%m%d")
dat0$MALDER <- as.numeric(difftime(dat0$BFODDAT, dat0$MFODDAT, units = "days"))/365.24
dat0$MALDER <- round(dat0$MALDER)
# table(dat0$MALDER>60)
# table(dat0$MALDER<10)
dat0$MALDER <- ifelse(dat0$MALDER>10 & dat0$MALDER<60, dat0$MALDER, NA)
test_that("ages within reasonable limits", {
  expect_less_than(max(dat0$MALDER, na.rm=TRUE), 60)
  expect_more_than(min(dat0$MALDER, na.rm=TRUE), 10)
}
)
# New age variable
dat0$K_Malder <- ifelse(dat0$MALDER <= 20, "1. <= 20 yrs",
                        ifelse(dat0$MALDER <= 34, "2. 21-24 yrs",
                               ifelse(dat0$MALDER >= 35, ">= 35 yrs", NA)
                        )
)
test_that("no new NAs",
          expect_equal(sum(is.na(dat0$MALDER)), sum(is.na(dat0$K_Malder)))
)
# create BMI
dat0$BMI <- dat0$MVIKT/(dat0$MLANGD/100)^2
# create BMI class
dat0$K_BMI <- cut(dat0$BMI, breaks = c(-Inf, 18.5, 24.9, 29.9, 34.9, 39.9 , Inf), include.lowest = TRUE)
# ROK derivation
dat0$ROK <- ifelse(dat0$ROK1==1, 0,
                   ifelse(dat0$ROK1 == 2 | dat0$ROK1 == 3, 1, NA)
)
# "Samboende"
dat0$Sambo <- ifelse(dat0$FAMSIT == 1, 1,
                     ifelse(dat0$FAMSIT == 2 | dat0$FAMSIT ==3, 2, NA)
)
# aggregate infertility treatments
aggVars <- c("OFRIABEF","OFRISTIM","OFRIKIRU","OFRIICSI","OFRIANN")
dat0$INFBEH <- ifelse(rowSums(dat0[, aggVars, with = FALSE],na.rm=TRUE)>1, 1, 0)
# length classification
#quantile(dat0$MLANGD, na.rm = TRUE, probs = c(0, 0.03, 0.10, 0.90, 0.97, 1))
dat0$K_MLANGD <- cut(dat0$MLANGD, quantile(dat0$MLANGD, na.rm = TRUE, probs = c(0, 0.03, 0.10, 0.90, 0.97, 1),
                                           labels = c("1. <3%", "2. 3-10%", "3. 10-90%", "4. 90-97%", "5. >97%")
)
)
# "Bjudning"
dat0$K_BJUD <- ifelse(dat0$BJUDNING == 1, "1. Framstupa",
                      ifelse(dat0$BJUDNING == 4, "2. Vidoppet",
                             ifelse(dat0$BJUDNING == 6, "3. Sate",
                                    ifelse(dat0$BJUDNING == 0, "4. Annat", NA)
                             )
                      )
)
# "Jourtid"
dat0$FFJOUR <- ifelse(dat0$FODKL >= 1700 | dat0$FODKL <=800, 1, 0)
# "Semestertid"
# numeric month day
dat0$temp <- as.numeric(gsub("^....-|-", "", as.character(dat0$BFODDAT)))
dat0$FSEM <- ifelse(dat0$temp > 615 & dat0$temp < 815, 1, 0)
# Sven sandin/Marsal weight
dat0$marsalVikt =ifelse(dat0$KON==1,
                        -(1.907345*10**(-6))*dat0$GRDBS**4 +
                          (1.140644*10**(-3))*dat0$GRDBS**3-
                          1.336265*10**(-1)*dat0$GRDBS**2+
                          1.976961*10**(0)*dat0$GRDBS+
                          2.410053*10**(2),
                        -(2.761948*10**(-6))*dat0$GRDBS**4+
                          (1.744841*10**(-3))*dat0$GRDBS**3-
                          2.893626*10**(-1)*dat0$GRDBS**2+
                          1.891197*10**(1)*dat0$GRDBS-
                          4.135122*10**(2)
)
dat0$avvikelseMarsal=(dat0$BVIKTBS-dat0$marsalVikt)/(dat0$marsalVikt*0.12)
dat0$S_Bvikt <- as.character(cut(dat0$avvikelseMarsal,
                                 quantile(dat0$avvikelseMarsal, na.rm = TRUE,
                                          probs = c(0, 0.03, 0.10, 0.90, 0.97, 1)
                                 ),
                                 labels = c("1. <3%", "2. 3-10%", "3. 10-90%", "4. 90-97%", "5. >97%")
))
# weight classification
dat0$B_vikt <- as.character(cut(dat0$BVIKTBS, c(0, 2500, 3000, 4000, 4500, 5000, Inf),dig.lab = 5))
# APGAR classification
dat0$APGAR5_class <- as.character(cut(dat0$APGAR5, c(0,4,7,10)))
dat0$APGAR10_class <- as.character(cut(dat0$APGAR10, c(0,4,7,10)))
#-------------------------------------------------------------------------------
snq0 <- read.csv2(file.path(path,"Output","snq.csv"), stringsAsFactors = TRUE)


snq1<- snq0[]


names(snq2) <- gsub("ö", "o", names(snq2))

# Börja här.....
# setDT(snq2)
# setkey(snq2, lpnr_snq, fdat_barn_snq)
# 
# tmp <- dat0[snq2]
dat0 <- merge(dat0, snq2, 
      by.x = c("lpnr_mor", "BFODDAT"), 
      by.y = c("lpnr_snq", "fdat_barn_snq"), all.x=TRUE)


#-------------------------------------------------------------------------------
# filter
names(dat0)

dat0$GRVBSfilter <- ifelse(is.na(dat0$GRVBS), dat0$Grav.langd..v._snq, dat0$GRVBS)


dat0$filterTidig <- ifelse(dat0$GRVBSfilter<36,1,0)
dat0$filter <- rowSums(dat0[,c("DODFOD","MISSB","filterTidig"), with = FALSE], na.rm=TRUE)
dat0$filterTidig <- NULL
dat0$GRVBSfilter <- NULL
#-------------------------------------------------------------------------------
out <- data.frame(dat0) 

out[,] <- lapply(out, function(col) {
  if (sum(is.na(as.numeric(as.character(col))))==sum(is.na(col))) {
    as.numeric(as.character(col))
  } else {
    col
  }
})


table(sapply(out[,], class))

out$BFODDAT <- as.character(out$BFODDAT)
out$MFODDAT <- as.character(out$MFODDAT)

table(sapply(out[,], class))

saveRDS(out, file.path(path,"Output","analysdataHIE_2_0.rds"))
write.csv2(out,file.path(path,"Output","analysdataHIE_2_0.csv"), row.names=F)
write_dta(out, file.path(path,"Output","analysdataHIE_2_0.dta"))

# 
# extra <- out[,c("lpnr_mor", "S_Bvikt", "B_vikt","APGAR5", "APGAR10","K_BJUD", "K_MLANGD", "APGAR5_class", "APGAR10_class")]
# str(extra)
# 
# write_dta(extra, file.path(path,"Output","komplettering.dta"))
# 
# table(duplicated(out[,c("lpnr_mor", "lpnr_BARN")]))
#EOF
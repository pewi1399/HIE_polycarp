rm(list = ls())
library(dplyr)
library(testthat)
library(data.table)
library(tidyr)
library(openxlsx)

# read data
dat <- readRDS("Output/2_diagnoses.rds")
labelDictionary <- openxlsx::read.xlsx("Diagnoskoder/labelDictionary.xlsx", sheet = 1)
fodelseLand <- openxlsx::read.xlsx("Indata/fodland.xlsx", sheet = 1)
sjukhusRegioner <- openxlsx::read.xlsx("Indata/regioner.xlsx", sheet = 1)
sjukhusKoder <- openxlsx::read.xlsx("Indata/Sjukhuskoder 81 st.xlsx")
names(sjukhusKoder) <- c("SJUKHUS", "SJUKHUSNAMN")

sjukhusKoder <- sjukhusKoder[!duplicated(sjukhusKoder),]

# spara namn innan bearbetningar
names_before <- names(dat)

# lägg till klartext för sjukhus
dat <- merge(dat, sjukhusKoder, by = "SJUKHUS", all.x = TRUE)

# merge on region and kod
sjukhusRegioner$EGEN <- NULL
dat <- merge(dat, sjukhusRegioner, by = "SJUKHUSNAMN", all.x = TRUE)

# merge on birthregion
fodelseLand <- fodelseLand[!duplicated(fodelseLand),]
dat <- merge(dat, fodelseLand, by = "MFODLAND", all.x = TRUE)


#table(is.na(tmp$SJUKHUSNAMN))
#table(is.na(tmp$SJUKHUSNIVA))
#table(is.na(tmp$REGION))

#test unique(dat$SJUKHUSNAMN)[!(unique(dat$SJUKHUSNAMN) %in% unique(sjukhusRegioner$SJUKHUSNAMN))]
#test unique(dat$SJUKHUS)[!(unique(dat$SJUKHUS) %in% unique(sjukhusKoder$SJUKHUS))]

# dat$K_Sjukniva <- 
dat$MFOD_NORDEN <- ifelse(dat$MFODLAND %in% c("SVERIGE", "ISLAND", "DANMARK", "FINLAND", "NORGE"), 1, 0)

# derivation from mfr variables
dat$MLANGD <- ifelse(dat$MLANGD>200 | dat$MLANGD < 130, NA, dat$MLANGD)
test_that("no values are outside of range",
          expect_true(max(dat$MLANGD, na.rm=TRUE)<=200 & min(dat$MLANGD, na.rm=TRUE)>=130)
)
# apgar no higher than 10 or lower than 1
apgarVars <- grep("^APGAR", names(dat), value = TRUE)
dat[, (apgarVars):=lapply(.SD, function(x) ifelse(x > 10 | x < 1, NA, x)), .SDcols = apgarVars]

# Age of mother at birth
# mothers birthdates are not always known
# assume born in the middle of each year/month if that data is not present
#dat$MFODDAT <- gsub(" ", "", dat$MFODDAT)
#dat$MFODDAT <- ifelse(nchar(dat$MFODDAT) == 4, paste0(dat$MFODDAT, "0615"), dat$MFODDAT)
#dat$MFODDAT <- ifelse(nchar(dat$MFODDAT) == 6, paste0(dat$MFODDAT, "15"), dat$MFODDAT)

#test_that("all dates are of valid length",
#          expect_true(max(nchar(dat$MFODDAT))==8)
#)

#dat$MFODDAT <- as.Date(dat$MFODDAT, format = "%Y%m%d")
#dat$BFODDAT <- as.character(dat$BFODDAT)
#dat$BFODDAT <- as.Date(dat$BFODDAT, format = "%Y%m%d")
#dat$MALDER <- as.numeric(difftime(dat$BFODDAT, dat$MFODDAT, units = "days"))/365.24
#dat$MALDER <- round(dat$MALDER)
# table(dat$MALDER>60)
# table(dat$MALDER<10)
#dat$MALDER <- ifelse(dat$MALDER>10 & dat$MALDER<60, dat$MALDER, NA)
test_that("ages within reasonable limits", {
  expect_less_than(max(dat$MALDER, na.rm=TRUE), 60)
  expect_more_than(min(dat$MALDER, na.rm=TRUE), 10)
}
)
# New age variable
dat$K_Malder <- ifelse(dat$MALDER <= 20, 1,
                        ifelse(dat$MALDER <= 34, 2,
                               ifelse(dat$MALDER >= 35, 3, NA)
                        )
)
#labels
# 1 "1. <= 20 yrs"
# 2 "2. 21-24 yrs"
# 3 "3. >= 35 yrs"

test_that("no new NAs",
          expect_equal(sum(is.na(dat$MALDER)), sum(is.na(dat$K_Malder)))
)
# create BMI
dat$BMI <- dat$MVIKT/(dat$MLANGD/100)^2
# create BMI class
dat$K_BMI <- as.numeric(cut(dat$BMI, breaks = c(-Inf, 18.5, 24.9, 29.9, 34.9, 39.9 , Inf), include.lowest = TRUE))

#labels
# 1 "1. BMI <18.5"
# 2 "2. 18.5-24.9"
# 3 "3. 25-29.9"
# 4 "4. 30-34.9"
# 5 "5. 35-39.9"
# 6 "6. >40"

# create BMI class
dat$K_BMI2 <- ifelse(dat$BMI<25, 0, 1)

test_that("no new NA compared to BMI",
          expect_equal(sum(is.na(dat$BMI)), sum(is.na(dat$K_BMI2)))
)

#labels
# 0 "0. BMI <18.5- 24.9 (under/normalvikt)"
# 1 "1. BMI 25-29.99 (value: övervikt)"


# ROK derivation
dat$ROK <- ifelse(dat$ROK1==1, 0,
                   ifelse(dat$ROK1 == 2 | dat$ROK1 == 3, 1, NA)
)

# "Samboende"
dat$Sambo <- ifelse(dat$FAMSIT == 1, 1,
                     ifelse(dat$FAMSIT == 2 | dat$FAMSIT ==3, 2, NA)
)

# aggregate infertility treatments
#aggVars <- c("OFRIABEF","OFRISTIM","OFRIKIRU","OFRIICSI","OFRIANN")
#dat$INFBEH <- ifelse(rowSums(dat[, aggVars, with = FALSE],na.rm=TRUE)>1, 1, 0)


# length classification
#quantile(dat$MLANGD, na.rm = TRUE, probs = c(0, 0.03, 0.10, 0.90, 0.97, 1))
dat$K_MLANGD <- as.numeric(cut(dat$MLANGD, 
                               quantile(dat$MLANGD, 
                                        na.rm = TRUE, 
                                        probs = c(0, 0.03, 0.10, 0.90, 0.97, 1)
                                        ),
                               include.lowest = TRUE
                               )
                           )

test_that("no new NAs in length class",
          expect_equal(sum(is.na(dat$MLANGD)), sum(is.na(dat$K_MLANGD)))
)

#labels
# 1 "1. <3%"
# 2 "2. 3-10%"
# 3 "3. 10-90%"
# 4 "4. 90-97%"
# 5 "5. >97%"

dat$K_MLANGD2 <- as.numeric(cut(dat$MLANGD, 
                                quantile(dat$MLANGD, na.rm = TRUE, probs = c(0, 0.10, 1)),
                                include.lowest = TRUE
                                )
                            )

test_that("no new NAs in length class 2",
          expect_equal(sum(is.na(dat$MLANGD)), sum(is.na(dat$K_MLANGD2)))
)

#labels
# 1 "1. <10 percentilen"
# 2 "2 >=10% percentilen"

dat$K_MLANGD3 <- as.numeric(cut(dat$MLANGD, 
                               c(0, 160, 172, Inf),
                                include.lowest = TRUE
                               )
)

test_that("no new NAs in length class 3",
          expect_equal(sum(is.na(dat$MLANGD)), sum(is.na(dat$K_MLANGD3)))
)

#labels
# 0 "0. < 161 cm"
# 1 "1 161 -172"
# 2 "2 >172"
#tapply( dat$MLANGD, dat$K_MLANGD3, summary)

dat$K_MLANGD4 <- ifelse(dat$MLANGD<=155, 1, 0)

test_that("no new NAs in length class 4",
          expect_equal(sum(is.na(dat$MLANGD)), sum(is.na(dat$K_MLANGD4)))
)

#labels
# 0 "0. > 155 cm"
# 1 "1 <= 155 cm"





# "Bjudning"
dat$K_BJUD <- ifelse(dat$BJUDNING == 1, 0,
                      ifelse(dat$BJUDNING == 4, 1,
                             ifelse(dat$BJUDNING == 6, 2,
                                    ifelse(dat$BJUDNING == 0, 3, NA)
                             )
                      )
)


#labels
# 0 "0. Framstupa" 
# 1 "1. Vidoppet"
# 2 "2. Sate"
# 3 "3. Annat"

# "Framstupa"
dat$Framstupa <- ifelse(dat$BJUDNING == 1, 1,
                         ifelse(dat$BJUDNING == 4, 0,
                                ifelse(dat$BJUDNING == 6, 0,
                                       ifelse(dat$BJUDNING == 0, 0, NA)
                                )
                         )
)


# "vidoppet"
dat$vidoppet <- ifelse(dat$BJUDNING == 1, 0,
                        ifelse(dat$BJUDNING == 4, 1,
                               ifelse(dat$BJUDNING == 6, 0,
                                      ifelse(dat$BJUDNING == 0, 0, NA)
                               )
                        )
)


# "sate"
dat$sate <- ifelse(dat$BJUDNING == 1, 0,
                    ifelse(dat$BJUDNING == 4, 0,
                           ifelse(dat$BJUDNING == 6, 1,
                                  ifelse(dat$BJUDNING == 0, 0, NA)
                           )
                    )
)

# "Jourtid"
dat$FJOUR <- ifelse(dat$FODKL >= 1700 | dat$FODKL <=800, 1, 0)

# "Jourtid2"
dat$FJOUR_02_07 <- ifelse(dat$FODKL >= 200 & dat$FODKL <=700, 1, 0)

# "Semestertid"
# numeric month day
#dat$temp <- as.numeric(gsub("^....-|-", "", as.character(dat$BFODDAT)))
dat$FSEM <- ifelse(substr(dat$BFODDAT, 5,6) %in% c("06", "07", "08"), 1, 0)

# Sven sandin/Marsal weight
#dat$marsalVikt =ifelse(dat$KON==1,
#                        -(1.907345*10**(-6))*dat$GRVBS**4 +
#                          (1.140644*10**(-3))*dat$GRVBS**3-
#                          1.336265*10**(-1)*dat$GRVBS**2+
#                          1.976961*10**(0)*dat$GRVBS+
#                          2.410053*10**(2),
#                        -(2.761948*10**(-6))*dat$GRVBS**4+
#                          (1.744841*10**(-3))*dat$GRVBS**3-
#                          2.893626*10**(-1)*dat$GRVBS**2+
#                          1.891197*10**(1)*dat$GRVBS-
#                          4.135122*10**(2)
#)

#dat$avvikelseMarsal=(dat$BVIKTBS-dat$marsalVikt)/(dat$marsalVikt*0.12)

#dat$S_Bvikt <- as.numeric(cut(dat$BVIKTBS,
#                               quantile(dat$BVIKTBS, na.rm = TRUE,
#                                        probs = c(0, 0.03, 0.10, 0.90, 0.97, 1)
#                               ),
#                              include.lowest = TRUE
#                              )
#                          )

#labels
# 1 "1. <3%"
# 2 "2. 3-10%"
# 3 "3. 10-90%"
# 4 "4. 90-97%"
# 5 "5. >97%"



# weight classification
#dat$B_vikt <- as.numeric(cut(dat$BVIKTBS, c(0, 2500, 3000, 4000, 4500, 5000, Inf),dig.lab = 5))

#labels
# 1 "1. < 2500 g"
# 2 "2. 2500-  <3000g"
# 3 "3. 3000-4000g"
# 4 "4. >4000 - <4500g"
# 5 "5. >4500 - <5000g"
# 6 "6. >5000g"

# Hypertoni 
dat$HT <- ifelse(rowSums(dat[,c("KroHT", "GravHT", "prekl", "Eklam")], na.rm = TRUE)>0, 1, 0)

#labels
# 0. Nej
# 1. Ja (KroHT+ gravHT+ precla, + eclamsia  )

# Diabetes
dat$DM <- ifelse(rowSums(dat[,c("preDM", "gestDM")], na.rm = TRUE)>0, 1, 0)

#labels
# 0. Nej
# 1. Ja

# APGAR classification
dat$APGAR5_class <- as.numeric(cut(dat$APGAR5, c(0,4,7,10),
                                   include.lowest = TRUE))

#labels
# 1 "1. Apgar5 <4"
# 2 "2. Apgar5 <7-4"
# 3 "3.  Apgar5 >4"

dat$K_APGAR57 <- ifelse(dat$APGAR5<7, 1, 0)
dat$K_APGAR54 <- ifelse(dat$APGAR5<4, 1, 0)

dat$K_HOMF35 <- ifelse(dat$HOMF>=35, 1, 0)
dat$K_HOMF37 <- ifelse(dat$HOMF>=37, 1, 0)

dat$K_APGAR10 <- as.numeric(cut(dat$APGAR10, c(0,4,7,10),
                                include.lowest = TRUE))

test_that("no new NAs in apgar 10 class",
          expect_equal(sum(is.na(dat$APGAR10)), sum(is.na(dat$K_APGAR10)))
)


#labels
# 1 "1. Apgar10 >7"
# 2 "2. Apgar10 <7-4"
# 3 "3.  Apgar10 <4"

dat$K_GRVBS <- ifelse(dat$GRVBS >= 37 & dat$GRVBS <=40,1,
                       ifelse(dat$GRVBS < 37, 2, 
                         ifelse(dat$GRVBS == 41, 3,
                              ifelse(dat$GRVBS >=42, 4, NA)))
                      )

test_that("no new NAs in GRVBS class",
          expect_equal(sum(is.na(dat$GRVBS)), sum(is.na(dat$K_GRVBS)))
)

# labels
# 1 "1. Vecka 36-39+9"
# 2 "2. Vecka 40-41+6"
# 3 "3. >vecka 42"

# Robson class

# Variabler ###

#sätt vecka = 35 om prematur sätt vecka = 42 om överburen
dat$week_robson <- ifelse(is.na(dat$GRVBS) & dat$prematur_robson == 1, 36,
                          ifelse(is.na(dat$GRVBS) & dat$overburen_robson == 1, 42, dat$GRVBS)
)


dat$parous_robson <- ifelse(dat$PARITET_F == 1, 1, 0)

dat$simplex_robson <- ifelse(dat$BORDF2 == 1, 1, 0)

dat$tidsect_robson <- ifelse(dat$TSECTIO == 1, 1, 0)


dat$fstart_robson <- ifelse(dat$fstart_spontan_robson == 1, 3,
                            ifelse(dat$fstart_induktion_diag_robson == 1 | dat$fstart_induktion_op_robson == 1 | dat$FLINDUKT == 1, 1 ,
                                   ifelse(dat$fstart_elektiv_robson == 1 | dat$ELEKAKUT == 1, 2,
                                          ifelse(dat$fstart_spontan_robson == 1, 3, NA)
                                          )))

#om fstart = missing antag  = 3 = spontan
dat$fstart_robson[is.na(dat$fstart_robson)] <- 3

# Levels
# 1. Induktion
# 2. Elektiv sectio
# 3. Spontan

dat$bjsect_robson <- ifelse(dat$bjsect_huvud_robson == 1, 1,
                            ifelse(dat$bjsect_tvar_robson == 1, 2, 
                                   ifelse(dat$bjsect_sate_robson == 1, 3, 
                                          ifelse(dat$BJUDNING == 1 | dat$BJUDNING == 4, 1, 
                                                 ifelse(dat$BJUDNING == 6, 3,
                                                        ifelse(dat$TANG == 1, 1, NA)
                                                 )))))

# Levels
# 1. Huvud
# 2. Tvar
# 3. Sate


dat$sectio_robson <- ifelse(dat$fstart_robson == 2 | dat$sectio_robson == 1 | dat$SECAVSL == 1, 1, 0)
###############

  
# Klassificering
dat$robson_number <- ifelse(dat$simplex_robson==1 & dat$parous_robson==0 & dat$week_robson>36 & dat$bjsect_robson==1 & dat$fstart_robson==3,
      "1",
    ifelse(dat$simplex_robson==1 & dat$parous_robson==0 & dat$week_robson>36 & dat$bjsect_robson==1 & dat$fstart_robson !=3,
      "2",  
    ifelse(dat$simplex_robson==1 & dat$parous_robson==1 & dat$week_robson>36 & dat$tidsect_robson==0 & dat$bjsect_robson==1 & dat$fstart_robson==3,
      "3",
    ifelse(dat$simplex_robson==1 & dat$parous_robson==1 & dat$week_robson>36 & dat$tidsect_robson==0 & dat$bjsect_robson==1 & dat$fstart_robson != 3,
      "4",
    ifelse(dat$simplex_robson==1 & dat$parous_robson==1 & dat$week_robson>36 & dat$tidsect_robson==1 & dat$bjsect_robson==1,
      "5",
    ifelse(dat$simplex_robson==1 & dat$parous_robson==0 & dat$bjsect_robson== 3,
      "6",
    ifelse(dat$simplex_robson==1 & dat$parous_robson==1 & dat$bjsect_robson== 3,
      "7",
    ifelse(dat$simplex_robson==0,
      "8",
    ifelse(dat$simplex_robson==1 & dat$bjsect_robson== 2,
      "9",
    ifelse(dat$simplex_robson==1 & dat$week_robson<37,
      "10", NA))))))))))


dat$robson_letter <- ifelse(dat$fstart_robson == 3, 1, 
                            ifelse(dat$fstart_robson == 1, 2,
                                   ifelse(dat$fstart_robson == 2, 3, NA)
                                   )
                            )

dat$robson_class <- as.integer(ifelse(dat$robson_number %in% c("2", "4", "5", "8", "10"), paste0(dat$robson_number, dat$robson_letter), dat$robson_number))


#----------------------------- print file --------------------------------------
dat <- data.frame(dat) 
saveRDS(dat, "Output/3_derivations.rds")

#------------------------------ log file ---------------------------------------

new_names <- names(dat)[!(names(dat) %in% names_before)]

wb <- openxlsx::createWorkbook()
for(variable in new_names){
  print(variable)
  
  # max length for sheetname is 31 characters
  openxlsx::addWorksheet(wb = wb, sheetName = substr(variable,1,31))
  
  
  
  newData <- data.frame(table(dat[, variable], useNA = "always"))
  names(newData) <- c("value", "Antal")

  
  newData <- merge(newData, subset(labelDictionary, Variabel == variable), by = "value", all.x = TRUE) %>% 
    select(value, label, Antal)
  
  
  #openxlsx::addStyle(wb, 
  #                   sheet = substr(variable,1,31), 
  #                   createStyle(fgFill = "#FF6666"),
  #                   rows = 1:(nrow(oldData)+1),
  #                   cols = 1:2,
  #                   gridExpand = TRUE
  #)
  
  
  openxlsx::writeData(wb = wb, 
                      sheet = substr(variable, 1,31),
                      startCol = 1,
                      newData
  )

}

openxlsx::saveWorkbook(wb, "Output/3_derived_variables.xlsx", overwrite = TRUE)


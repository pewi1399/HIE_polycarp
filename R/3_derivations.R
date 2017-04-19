rm(list = ls())
library(dplyr)
library(testthat)
library(data.table)
library(tidyr)

# read data
dat <- readRDS("Output/2_diagnoses.rds")


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
dat$MFODDAT <- gsub(" ", "", dat$MFODDAT)
dat$MFODDAT <- ifelse(nchar(dat$MFODDAT) == 4, paste0(dat$MFODDAT, "0615"), dat$MFODDAT)
dat$MFODDAT <- ifelse(nchar(dat$MFODDAT) == 6, paste0(dat$MFODDAT, "15"), dat$MFODDAT)

test_that("all dates are of valid length",
          expect_true(max(nchar(dat$MFODDAT))==8)
)

dat$MFODDAT <- as.Date(dat$MFODDAT, format = "%Y%m%d")
dat$BFODDAT <- as.character(dat$BFODDAT)
dat$BFODDAT <- as.Date(dat$BFODDAT, format = "%Y%m%d")
dat$MALDER <- as.numeric(difftime(dat$BFODDAT, dat$MFODDAT, units = "days"))/365.24
dat$MALDER <- round(dat$MALDER)
# table(dat$MALDER>60)
# table(dat$MALDER<10)
dat$MALDER <- ifelse(dat$MALDER>10 & dat$MALDER<60, dat$MALDER, NA)
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
dat$K_BMI2 <- as.numeric(cut(dat$BMI, breaks = c(-Inf, 18.5, 24.9, 29.9, Inf), include.lowest = TRUE))
#stop("check this out, strange class rules...")
#labels
# 1 "1. BMI <18.5- 24.9 (under/normalvikt)"
# 2 "2. BMI 25-29.99 (value: övervikt)"
# 3 "3. BMI >30 (value övervikt)"

# ROK derivation
dat$ROK <- ifelse(dat$ROK1==1, 0,
                   ifelse(dat$ROK1 == 2 | dat$ROK1 == 3, 1, NA)
)
# "Samboende"
dat$Sambo <- ifelse(dat$FAMSIT == 1, 1,
                     ifelse(dat$FAMSIT == 2 | dat$FAMSIT ==3, 2, NA)
)
# aggregate infertility treatments
aggVars <- c("OFRIABEF","OFRISTIM","OFRIKIRU","OFRIICSI","OFRIANN")
dat$INFBEH <- ifelse(rowSums(dat[, aggVars, with = FALSE],na.rm=TRUE)>1, 1, 0)
# length classification
#quantile(dat$MLANGD, na.rm = TRUE, probs = c(0, 0.03, 0.10, 0.90, 0.97, 1))
dat$K_MLANGD <- as.numeric(cut(dat$MLANGD, quantile(dat$MLANGD, na.rm = TRUE, probs = c(0, 0.03, 0.10, 0.90, 0.97, 1)
)))

#labels
# 1 "1. <3%"
# 2 "2. 3-10%"
# 3 "3. 10-90%"
# 4 "4. 90-97%"
# 5 "5. >97%"

dat$K_MLANGD2 <- as.numeric(cut(dat$MLANGD, quantile(dat$MLANGD, na.rm = TRUE, probs = c(0, 0.10, 1))))
#labels
# 1 "1. <10 percentilen"
# 2 "2 >=10% percentilen"

# "Bjudning"
dat$K_BJUD <- ifelse(dat$BJUDNING == 1, 1,
                      ifelse(dat$BJUDNING == 4, 2,
                             ifelse(dat$BJUDNING == 6, 3,
                                    ifelse(dat$BJUDNING == 0, 4, NA)
                             )
                      )
)


#labels
# 1 "1. Framstupa" 
# 2 "2. Vidoppet"
# 3 "3. Sate"
# 4 "4. Annat"

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
dat$FFJOUR <- ifelse(dat$FODKL >= 1700 | dat$FODKL <=800, 1, 0)

# "Jourtid2"
dat$FFJOUR_02_07 <- ifelse(dat$FODKL >= 200 & dat$FODKL <=700, 1, 0)

# "Semestertid"
# numeric month day
dat$temp <- as.numeric(gsub("^....-|-", "", as.character(dat$BFODDAT)))
dat$FSEM <- ifelse(dat$temp > 615 & dat$temp < 815, 1, 0)
# Sven sandin/Marsal weight
dat$marsalVikt =ifelse(dat$KON==1,
                        -(1.907345*10**(-6))*dat$GRDBS**4 +
                          (1.140644*10**(-3))*dat$GRDBS**3-
                          1.336265*10**(-1)*dat$GRDBS**2+
                          1.976961*10**(0)*dat$GRDBS+
                          2.410053*10**(2),
                        -(2.761948*10**(-6))*dat$GRDBS**4+
                          (1.744841*10**(-3))*dat$GRDBS**3-
                          2.893626*10**(-1)*dat$GRDBS**2+
                          1.891197*10**(1)*dat$GRDBS-
                          4.135122*10**(2)
)
dat$avvikelseMarsal=(dat$BVIKTBS-dat$marsalVikt)/(dat$marsalVikt*0.12)
dat$S_Bvikt <- as.numeric(cut(dat$avvikelseMarsal,
                               quantile(dat$avvikelseMarsal, na.rm = TRUE,
                                        probs = c(0, 0.03, 0.10, 0.90, 0.97, 1)
                               )
))
#labels
# 1 "1. <3%"
# 2 "2. 3-10%"
# 3 "3. 10-90%"
# 4 "4. 90-97%"
# 5 "5. >97%"



# weight classification
dat$B_vikt <- as.numeric(cut(dat$BVIKTBS, c(0, 2500, 3000, 4000, 4500, 5000, Inf),dig.lab = 5))

#labels
# 1 "1. < 2500 g"
# 2 "2. 2500-  <3000g"
# 3 "3. 3000-4000g"
# 4 "4. >4000 - <4500g"
# 5 "5. >4500 - <5000g"
# 6 "6. >5000g"

# APGAR classification
dat$APGAR5_class <- as.numeric(cut(dat$APGAR5, c(0,4,7,10)))

#labels
# 1 "1. Apgar5 >7"
# 2 "2. Apgar5 <7-4"
# 3 "3.  Apgar5 <4"

dat$K_APGAR57 <- ifelse(dat$APGAR5<7, 1, 0)
dat$K_APGAR54 <- ifelse(dat$APGAR5<4, 1, 0)

dat$K_HOMF35 <- ifelse(dat$HOMF>=35, 1, 0)
dat$K_HOMF37 <- ifelse(dat$HOMF>=37, 1, 0)

dat$APGAR10_class <- as.numeric(cut(dat$APGAR10, c(0,4,7,10)))

#labels
# 1 "1. Apgar10 >7"
# 2 "2. Apgar10 <7-4"
# 3 "3.  Apgar10 <4"

dat$GRVBS_K <- ifelse(dat$GRVBS >36 & dat$GRVBS <=39,1,
                       ifelse(dat$GRVBS >36 & dat$GRVBS < 42,2,
                              ifelse(dat$GRVBS >=42, 3, NA)))
# labels
# 1 "1. Vecka 36-39+9"
# 2 "2. Vecka 40-41+6"
# 3 "3. >vecka 42"
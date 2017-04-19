rm(list = ls())
library(dplyr)
library(testthat)
library(data.table)
library(tidyr)

# read data
dat <- readRDS("Output/1_get_data.rds")
setDT(dat)
metadata <- readxl::read_excel("Diagnoskoder/dataDictionary.xlsx", sheet = 1)
#fix this by changing input method or by deleting columns in excel file
metadata <- metadata[,1:117]

# ---------------------------- prepare metadata --------------------------------
# collapse all code variables
diags <- grep("kod", names(metadata), value = TRUE)
#insert leading spaces
metadata[, diags] <- lapply(metadata[, diags], function(x) paste(" ",x, sep = ""))

# assemble search phrases
metadata$search <- do.call(paste, c(metadata[,grep("kod", names(metadata))], sep="|"))

# searches should start with an exact match but may end on any string
metadata$search <- gsub("\\|\\ NA.*$", "", metadata$search)

# drop "kod" vars
metadata <- metadata %>% 
  select(Group, 
         variabel, 
         label,
         factor,
         factorLevel, 
         level,
         source,
         search
         )

metadataMor <- subset(metadata, Group == "mor")
metadataBarn <- subset(metadata, Group == "barn")

# split singles and factors
metadataMorSingle <- subset(metadataMor, factor == "nej" & source == "mfr")
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

# apply search phrase on these
applySearch <- function(variable, phrase){
  ## variable: variable containing ICD diag
  ## phrase: a string containing string or regex to be used for matching
  out<- ifelse(grepl(phrase, variable), 1,0)
  return(out)
}

# create diagnosis variables
dat[,(metadataBarn$variabel):=lapply(metadataBarn$search, applySearch, variable = dat$BDIAG),]
dat[,(metadataMorSingle$variabel):=lapply(metadataMorSingle$search, applySearch, variable = dat$MDIAG),]

dat$obstekat <- ifelse((dat$ruptur + 
                          dat$skulderdy + 
                          dat$Eklam + 
                          dat$prolaps + 
                          dat$abl) > 0, 1, 0)

saveRDS(dat, "Output/2_diagnoses.rds")
#-------------------------------------------------------------------------------


#-------------- create summary tables for eventual troubleshoot ----------------
vars <- c(metadataBarn$variabel,  "lopnr_barn")

barn <- 
  dat[, vars, with = FALSE] %>% 
  gather(key, variabel, -lopnr_barn) %>% 
  group_by(key) %>% 
  summarise(n = n(),
            antal = sum(variabel),
            andel = mean(variabel),
            per_100000 = round(100000*mean(variabel))
            )

vars <- c(metadataMorSingle$variabel, "obstekat", "lopnr_mamma")

mamma <- 
  dat[, vars, with = FALSE] %>% 
  gather(key, variabel, -lopnr_mamma) %>% 
  group_by(key) %>% 
  summarise(n = n(),
            antal = sum(variabel),
            andel = mean(variabel),
            per_100000 = round(100000*mean(variabel))
  )

write.csv2(barn, "Output/2_diagnoses_barn.csv")
write.csv2(mamma, "Output/2_diagnoses_mamma.csv")
#-------------------------------------------------------------------------------
# loop in factor variables
# metadataMorFactor$factorLevel <- ifelse(is.na(metadataMorFactor$factorLevel),
#                                         metadataMorFactor$variable,metadataMorFactor$factorLevel)
# diags <-unique(metadataMorFactor$variable)
# dat0 <- data.frame(dat0)
# for(i in 1:length(diags)){
#   dat0[,diags[i]] <- 0
#   sub <- subset(metadataMorFactor, variable == diags[i])
#   for(j in 1:nrow(sub)){
#     dat0[,diags[i]] <- ifelse(grepl(sub[j, "search"], dat0$MDIAG), paste0(j,". ", sub[j , "factorLevel"]), dat0[,diags[i]])
#   }
# }
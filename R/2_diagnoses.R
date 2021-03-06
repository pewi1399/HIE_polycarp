rm(list = ls())
library(dplyr)
library(testthat)
library(data.table)
library(tidyr)
library(openxlsx)

# read data
dat <- readRDS("Output/1_get_data.rds")
setDT(dat)
metadata <- openxlsx::read.xlsx("Diagnoskoder/dataDictionary.xlsx", sheet = 1)
#fix this by changing input method or by deleting columns in excel file
#metadata <- metadata[,1:117]

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
metadataMorOp <-  subset(metadataMor, factor == "nej" & source == "mfrop")
metadataBarnOp <-  subset(metadataBarn, factor == "nej" & source == "mfrop")

# test that all diags are still there
test_that("rows add up",
          expect_equal(nrow(subset(metadata, Group == "mor" | Group == "barn")), nrow(metadataMorSingle) +
                         nrow(metadataMorFactor) + nrow(metadataBarn) + nrow(metadataMorOp))
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

dat[,(metadataBarnOp$variabel):=lapply(metadataBarnOp$search, applySearch, variable = dat$BDIAG),]
dat[,(metadataMorOp$variabel):=lapply(metadataMorOp$search, applySearch, variable = dat$MDIAG),]

dat$obstekat <- ifelse((dat$ruptur + 
                          dat$skulderdy + 
                          dat$Eklam + 
                          dat$prolaps + 
                          dat$abl) > 0, 1, 0)



# loop in factor variables
metadataMorFactor$factorLevel <- ifelse(is.na(metadataMorFactor$factorLevel),
                                        metadataMorFactor$variabel, metadataMorFactor$factorLevel)

diags <-unique(metadataMorFactor$variabel)

dat <- data.frame(dat)

for(i in 1:length(diags)){
  dat[,diags[i]] <- 0
  
  sub_var <- subset(metadataMorFactor, variabel == diags[i])
  
  for(j in 1:nrow(sub_var)){
    
    criteria <- (dat[,diags[i]] == 0 |  dat[,diags[i]] >= j)
    
    dat[criteria, diags[i]] <- ifelse(
                                                grepl(sub_var[j, "search"], dat[criteria,"MDIAG"]), 
                                                sub_var[j , "level"], 
                                                dat[criteria,diags[i]]
                             )
  }
}

setDT(dat)

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

openxlsx::write.xlsx(barn, "Output/2_diagnoses_barn.xlsx")
openxlsx::write.xlsx(mamma, "Output/2_diagnoses_mamma.xlsx")
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
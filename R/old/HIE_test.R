library(XLConnect)
library(testthat)
# Diagnoses
metadata <- readWorksheetFromFile("/home/per/Polycarp/KBH/P104_HIE/Diagnoskoder/dataDictionary.xlsx", sheet = 1)

# collapse all code variables
diags <- grep("kod", names(metadata), value = TRUE)

#insert leading spaces
metadata[, diags] <- lapply(metadata[, diags], function(x) paste(" ",x, sep = ""))

# assemble search phrases
metadata$search <- do.call(paste, c(metadata[,grep("kod", names(metadata))], sep="|"))

# searches should start with an exact match but may end on any string 
metadata$search <- gsub("\\|\\ NA.*$", "", metadata$search)

# create dataframe  
mfr = data.frame(id = 1:4, diag = c(" O300 O14 ", " O814 O815 ", " P909 ", " E02 O363 O13 O10 O266"))

#-------------------------------------------------------------------------------
# this is the common format for diagnoses
library(data.table)
setDT(mfr)
#mfr[,new:= paste(diag, collapse=" "), by = id]
#mfr$diag <- paste(" ", mfr$diag, sep = "")
metadataSingle <- subset(metadata, factor == "nej")
metadataFactor <- subset(metadata, factor == "ja")
test_that("rows of split datasets are add up",
         expect_equal(nrow(metadata), nrow(metadataSingle)+nrow(metadataFactor))
         )
  
# apply search phrase on these
applySearch <- function(variable, phrase){
  ## variable: variable containing ICD diag
  ## phrase: a string containing string or regex to be used for matching
  out<- ifelse(grepl(phrase, variable), 1,0)  
  return(out)
}

# create diagnosis variables
mfr[,(metadataSingle$variabel):=lapply(metadataSingle$search, applySearch, variable = mfr$diag),]

#-------------------------------------------------------------------------------
metadataFactor$factorLevel <- ifelse(is.na(metadataFactor$factorLevel),
                                     paste0(metadataFactor$variabel, metadataFactor$level),
                                     metadataFactor$factorLevel
                                     )

# derive factor levels one by one
mfr[,(metadataFactor$factorLevel):=lapply(metadataFactor$search, applySearch, variable = mfr$diag),]

# 

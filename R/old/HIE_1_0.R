rm(list=ls())

path = "/home/per/Polycarp/KBH/P104_HIE"

# packages
#library(data.table)
#library(plyr)
library(haven)
library(epiR)
library(data.table)
mfr.1973 <- readRDS(file.path(path, "Indata","SOS_","__MFR","mfr_1973_2013.rds"))
mfr.2007 <- readRDS(file.path(path, "Indata","SOS_","__MFR","mfr_2007_2013.rds"))
snq <- readRDS(file.path(path, "Indata","SOS_","__MFR","din_fil_snq.rds"))
mfr.snq <- readRDS(file.path(path, "Indata","SOS_","__MFR","mfr_snq_kivinnor.rds"))

# mfr.1973 <-mfr.1973[1:100,]
# mfr.2007 <-mfr.2007[1:100,]
# snq <-snq[1:100,]
# mfr.snq <-mfr.snq[1:100,]
# 
# save(mfr.1973, file = file.path(path, "Indata","SOS_","__MFR","mfr_1973_2013TEST.rds"))
# save(mfr.2007, file = file.path(path, "Indata","SOS_","__MFR","mfr_2007_2013TEST.rds"))
# save(snq, file = file.path(path, "Indata","SOS_","__MFR","din_fil_snqTEST.rds"))
# save(mfr.snq, file = file.path(path, "Indata","SOS_","__MFR","mfr_snq_kivinnorTEST.rds"))



# write.csv2(mfr.1973, file.path(path, "Indata","SOS_","__MFR","mfr_1973_2013.csv"), row.names=F, na="")
# write.csv2(mfr.2007, file.path(path, "Indata","SOS_","__MFR","mfr_2007_2013.csv"), row.names=F, na="")
# write.csv2(snq, file.path(path, "Indata","SOS_","__MFR","din_fil_snq.csv"), row.names=F, na="")
# write.csv2(mfr.snq, file.path(path, "Indata","SOS_","__MFR","mfr_snq_kivinnor.csv"), row.names=F, na="")

# remove dupes
#snq <- snq[!duplicated(snq),]
#cases <- merge(mfr.snq, snq, by = )

snq$fodselFore_v36 <- ifelse(snq$`Grav.längd (v)`<36,1,0)
snq$HIE_1to3 <- ifelse(snq$`Högst HIE`>=1 & snq$`Högst HIE`<=3,1,0)
snq$hypotermi <- ifelse(snq$`Behandlad med hypotermi`=="Ja",1,0)
snq$multiplaKramper <- ifelse(snq$`Multipla kramper` =="Ja",1,0)
#snq$cnsKramper <- ifelse(snq$)
snq$antiEpileptisk <- ifelse(snq$`Antiepileptisk beh vid utskrivni`=="Ja",1,0)
snq$P909 <- ifelse(grepl("P909", snq$`Diagnoser ICD-10 kod`),1,0)
snq$P916 <- ifelse(grepl("P916", snq$`Diagnoser ICD-10 kod`),1,0)
snq$P210 <- ifelse(grepl("P210", snq$`Diagnoser ICD-10 kod`),1,0)
snq$P219 <- ifelse(grepl("P219", snq$`Diagnoser ICD-10 kod`),1,0)

snq$missbildning <- ifelse(snq$Missbildning=="Ja",1,0)
snq$Kromosomfel <- ifelse(snq$`Missb./kromosom avv.`=="Ja",1,0)
#snq$fosterdod
mfr.snq$case <- "yes"
mfr.2007$case <- "no"


vars = c("fodselFore_v36","HIE_1to3", "hypotermi", "multiplaKramper", "antiEpileptisk", 
         "P909", "P916", "P210", "P219", "fodselFore_v36")

#remove cases that's not in mfr
snq = subset(snq, snq$lpnr %in% mfr.snq$lpnr_mor)
snq1 = data.table(snq[,c("lpnr","fdat_barn", vars)])
snq1$AR <- substr(snq1$fdat_barn, 1, 4) 

snq2 <- subset(snq1,AR<2014)
snq2$fdat_barn = NULL
snq2$AR = NULL

snq3 <- snq2[,lapply(.SD, function(x){ifelse(sum(x,na.rm=T)>0,1,0)}),by=lpnr]

#tt = snq1[,list(sum(table(unique(fdat_barn)))),by=lpnr]
#table(tt$V1)

mfr.cases <- merge(mfr.snq, snq3, by.x = "lpnr_mor", by.y="lpnr", all.x=T)

mfr <- plyr::rbind.fill(mfr.cases , mfr.2007)
mfr[mfr==""] = NA

# covariate patterns
pattern <- epi.cp(snq3[,vars,with=F])
pattern <- pattern$cov.pattern
pattern <- pattern[order(-pattern$n),]
pattern$id = NULL


#change type to numeric for valid numerics
out <- mfr
out[,] <- lapply(out, function(col) {
  if (sum(is.na(as.numeric(as.character(col))))==sum(is.na(col))) {
    as.numeric(as.character(col))
  } else {
    col
  }
})
str(out)

snq[,] <- lapply(snq, function(col) {
  if (sum(is.na(as.numeric(as.character(col))))==sum(is.na(col))) {
    as.numeric(as.character(col))
  } else {
    col
  }
})

write.csv2(snq, file.path(path,"Output","snq.csv"), row.names=F)
write_dta(snq, file.path(path,"Output","snq.dta"))

# saveRDS(out, file.path(path,"Output","analysdataHIE_1_0.rds"))
# write.csv2(pattern,file.path(path,"Output","covariatePatterns.csv"), row.names=F)
# write_dta(out, file.path(path,"Output","analysdatabas.dta"))
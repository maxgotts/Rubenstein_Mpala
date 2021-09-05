rm(list=ls())

library(dplyr)
library(xlsx)


PATH <- readline("Paste $PATH here> ")#"/Users/maxgotts/Desktop/MPALA/Photos/Photo Datasets/12th June 2021, Loop 2/individuals"

if (FALSE) { ###################################################################
  source('~/Desktop/MPALA/Photos/GenerateWildbook.R')  #### Run this line!! ####
}###############################################################################


wildbook <- read.csv(paste0(PATH,"/WildbookSheet.csv"))
wildbook <- filter(wildbook,Encounter.mediaAsset0!="")
white <- read.csv("~/Desktop/MPALA/Whitesheets/Gotts_TransformedWhitesheets.csv")
white <- filter(white,!is.na(Photos.begin))


for (photo_row in 1:nrow(wildbook)) {
  filename <- wildbook[photo_row,"Encounter.mediaAsset0"]
  filenumber <- strtoi(strsplit(strsplit(filename,"DSCN")[[1]][2],".JPG")[[1]])
  the.dazzle <- 0
  for (dazzle in 1:nrow(white)) {
    if (is.na(white[dazzle,2])) { next }
    if (filenumber >= white[dazzle,"Photos.begin"]) {
      the.dazzle <- dazzle
    }
  }
  if (white[the.dazzle,"Multispecies"] == 1) {
    cat("Warning: Species entry in `",filename,"` needs to be handled manually.\n",sep="")
    wildbook[photo_row,2:48] <- white[the.dazzle,4:50]
    wildbook[photo_row,13] <- "" # Remove species designation
  } else {
    wildbook[photo_row,2:48] <- white[the.dazzle,4:50]
  }
}

wildbook$Encounter.submitterID <- "Max Gotts"
wildbook$Encounter.submitter0.emailAddress <- "mgotts@princeton.edu"
wildbook$Encounter.submitter0.fullName <- "Max Gotts"
wildbook$Encounter.submitter0.affiliation <- "Princeton University"
wildbook$Encounter.project0.researchProjectName <- "Cattle-Zebra Interactions (Princeton University)"


for (coln in colnames(wildbook)) {
  column <- wildbook[,coln]
  column[is.na(column)]<-""
  wildbook[,coln] <- column
}


write.xlsx(wildbook,paste0(PATH,"/WildbookSheet.xlsx"), row.names=FALSE)
file.remove(paste0(PATH,"/WildbookSheet.csv"))

# write.csv(wildbook,paste0(PATH,"/WildbookSheet.csv"), row.names=FALSE)



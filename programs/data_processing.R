#' # Data entry and processing
#' 
#' ### Preamble
#' 
#' Load Libraries
#+ libraries, messages=FALSE, warning=FALSE
library(ezknitr)
library(knitr)
library(metafor)
library(doBy)

#' Clear environment
#+ clear
remove(list=ls())

#' Document settings
#+ settings
opts_chunk$set(fig.width = 6, fig.height = 4)

#' ### Load data
#+ data
raw.data <- read.csv("data/workspace.csv")



#' ### Processing data Steps
#' 
#' 
#' 1. New dataset with only selected fields
#' 2. Standardize categorical moderators
#' 3. Partition data
#' 4. Standardize effect size information
#' 5. Make sure categorical moderators have n > 5
#' 6. Calculate effect sizes
#' 7. Save files for analysis
#' 
#' ### 1. New dataset with only selected fields
#+ selectFields
data.reduced <- raw.data[,c(1:7,10:16,19:48)]
# take out three data points with information on control treatments only
data.reduced <- data.reduced[data.reduced$FID!=105 &
                               data.reduced$FID!=154 &
                               data.reduced$FID!=211,]
#' Table of all data
#+ datatable
summaryBy(FID~Reference+ReferenceNumb+studyYear+State, data = data.reduced, FUN = length)
#summaryBy(FID~ReferenceNumb+Reference+studyYear+State, data = data.reduced, FUN = length)

#' ### 2. Standardize categorical moderators
#' 
#' Active ingredients: define mixed and 2+ applications of mixed (if 2+ applications of same, just use that active ingredient as is.)
#+ standardizeActive
# Active ingredients - fix typos in original dataset
data.reduced$active.ingredient.coded <- as.character(data.reduced$active.ingredient.coded)
data.reduced$active.ingredient.coded[data.reduced$trade.name=="ACT Plus"] <- "other"
data.reduced$active.ingredient.coded[data.reduced$trade.name=="Domark"] <- "TETR"
data.reduced$active.ingredient.coded[data.reduced$trade.name=="Folicur"] <- "TEBU"
data.reduced$active.ingredient.coded[data.reduced$trade.name=="Folicur fb Headline"] <- "tebu fb pyra"
data.reduced$active.ingredient.coded[data.reduced$trade.name=="Folicur + Headline"] <- "tebu + pyra"
data.reduced$active.ingredient.coded[data.reduced$trade.name=="Laredo"] <- "MYC"
data.reduced$active.ingredient.coded[data.reduced$trade.name=="Punch fb Punch"] <- "FLUS"
data.reduced$active.ingredient.coded[data.reduced$trade.name=="Stratego"] <- "prop + trif"


# Active ingredients - new code for analysis (e.g. >5 obs)
#     Start by making new field and storing the old code
data.reduced$activeIngClean <- "empty"
#     Mixed
mixed.indices <- grep("+", data.reduced$active.ingredient.coded, fixed=TRUE)
data.reduced$activeIngClean[mixed.indices] <- "mixed"
#     Dual applications
dual.apps <- grep("fb", data.reduced$active.ingredient.coded, fixed=TRUE)
data.reduced$activeIngClean[dual.apps] <- "dual"
data.reduced$activeIngClean[grep("azo fb azo", data.reduced$active.ingredient.coded, fixed=F)] <- "AZO"
data.reduced$activeIngClean[grep("myc fb myc", data.reduced$active.ingredient.coded, fixed=T)] <- "MYC"
data.reduced$activeIngClean[grep("tetr fb tetr", data.reduced$active.ingredient.coded, fixed=T)] <- "TETR"
data.reduced$activeIngClean[grep("tetra fb tetra", data.reduced$active.ingredient.coded, fixed=T)] <- "TETR"
data.reduced$activeIngClean[grep("tebu fb tebu", data.reduced$active.ingredient.coded, fixed=T)] <- "TEBU"
data.reduced$activeIngClean[grep("prop fb prop", data.reduced$active.ingredient.coded, fixed=T)] <- "PROP"
data.reduced$activeIngClean[grep("pyra fb pyra", data.reduced$active.ingredient.coded, fixed=T)] <- "PYR"
data.reduced$activeIngClean[grep("pyra fb prya", data.reduced$active.ingredient.coded, fixed=T)] <- "PYR"
data.reduced$activeIngClean[grep("flut fb flut", data.reduced$active.ingredient.coded, fixed=T)] <- "FLUT"

#' Rewrite combinations so that they are alphabetical
data.reduced$combo_complete <- NA
data.reduced$combo_complete[data.reduced$activeIngClean!="mixed"&
                              data.reduced$activeIngClean!="dual"] <- "single"

head(data.reduced[is.na(data.reduced$combo_complete),c(10:12,45:46)])

data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="tebu+thi"] <- 
  "TEBU + THIO"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="fluz+famo"] <- 
  "FAMO + FLUZ"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="tebu+sur"] <-
  "SURF + TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="tebu+trif+oth"] <-
  "TEBU + TRIF + OTH"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="tebu+trif"] <-
  "TEBU + TRIF"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prop+azo"] <- 
  "AZO + PROP"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="flux+pyra"] <- 
  "FLUX + PYR"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prop+azo"|
                           data.reduced$active.ingredient.coded=="pro+azo"] <-
  "AZO + PROP"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="flus+pic"] <- 
  "FLUS + PICO"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prop+trif+prot"] <-
  "PROP + PROT + TRIF"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="azo+cypr"] <-
  "AZO + CYPR"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="pyra+tebu"] <-
  "PYR + TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prop+trif"] <- 
  "PROP + TRIF"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prop+trif+tebu"] <-
  "PROP + TEBU + TRIF"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="oth+flus"] <-
  "FLUS + OTH"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="flus+pyra"] <-
  "FLUS + PYR"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="pyra+myc"] <- 
  "MYC + PYR"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="pyra+flut"] <- 
  "FLUT + PYR"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prop+trif+myc"] <-
  "MYC + PROP + TRIF"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prop+azo+tebu"] <- 
  "AZO + PROP + TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prop+azo+myc"] <- 
  "AZO + MYC + PROP"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prop+azo+flut"] <- 
  "AZO + FLUT + PROP"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prop+trif+flut"] <- 
  "FLUT + PROP + TRIF"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="oth+flut"] <- 
  "FLUT + OTH"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="azo+prop"] <- 
  "AZO + PROP"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="azo+dife"] <-
  "AZO + DIF"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prot+trif"] <- 
  "PROT + TRIF"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="flux+flut"] <- 
  "FLUT + FLUX"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="flut+azo"] <- 
  "AZO + FLUT"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="tebu + pyra"] <- 
  "PYR + TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="azo+flut"] <- 
  "AZO + FLUT"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="chl+tebu"] <- 
  "CHL + TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="azo+tebu"] <- 
  "AZO + TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="azo+myc"] <- 
  "AZO + MYC"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="azo+pyra"] <- 
  "AZO + PYR"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="myc+pyra"] <- 
  "MYC + PYR"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="fenb+pyra"] <-
  "FEN + PYR"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="tebu+sul"] <-
   "SUL + TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="teb+cop"] <-
  "COPPER + TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="teb+eth"] <- 
  "ETH + TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="teb+thio"] <- 
  "TEBU + THIO"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="tetr+azo"] <- 
  "AZO + TETR"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="tetr+ort"] <- 
  "ORTH + TETR"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="tetr+cro"] <- 
  "CROP + TETR"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="cop+oth"] <- 
  "COPPER + OTH"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="eth+oth"] <- 
  "ETH + OTH"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="sul+oth"] <- 
  "SUL + OTH"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="thio+oth"] <- 
  "OTH + THIO"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="org+azo"] <- 
  "AZO + ORG" 
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="pyra fb tebu"] <- 
  "PYR fb TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="flut+eth"] <- 
  "ETH + FLUT"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="sul+ unknown"] <- 
  "SUL + OTH"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prop+azo fb teb"] <-
  "AZO + PROP fb TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="pyra + other fb tebu"] <- 
  "OTH + PYR fb TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prop+trif fb tebu"] <- 
  "PROP + TRIF fb TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="flut+pyra"] <- 
  "FLUT + PYR"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prop+azo fb tebu"] <- 
  "AZO + PROP fb TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="azo fb tebu"] <- 
  "AZO fb TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="azo fb myc"] <- 
  "AZO fb MYC"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="azo fb pyro"] <- 
  "AZO fb PYR"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="azo fb pyro + tebu"] <- 
  "AZO fb PYR + TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="azo fb flut"] <- 
  "AZO fb FLUT"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="chl fb tebu+chl"] <- 
  "CHL fb CHL + TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="chl fb tebu"|
                           data.reduced$active.ingredient.coded=="chl fb tebu "] <- 
  "CHL fb TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="pyra+met"] <- 
  "MET + PYR"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="pyra+teb"] <- 
  "PYR + TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="myc + pyra"] <- 
  "MYC + PYR"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="fenb+other"] <- 
  "FEN + OTH"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="unknown + flus"] <- 
  "FLUS + OTH"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="pyra + flus"] <- 
  "FLUS + PYR"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="cypr + azo"] <- 
  "AZO + CYPR"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="pyra + met"] <- 
  "MET + PYR"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="flus + fam"] <-
  "FAMO + FLUS"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="flus + pyra"] <- 
  "FLUS + PYR"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="flus + azo"] <- 
  "AZO + FLUS"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="pyra + tebu fb pyra"] <-
  "PYR + TEBU fb PYR"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="pyra fb met"] <- 
  "PYR fb MET"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="flus + carb"] <- 
  "CARB + FLUS" 
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="cyp + azo"] <- 
  "AZO + CYPR"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="thio + other"] <- 
  "OTH + THIO"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="chlo fb tebu"] <- 
  "CHL fb TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prop + azo fb prop + azo"] <- 
  "AZO + PROP"
data.reduced$activeIngClean[data.reduced$active.ingredient.coded=="tria fb tria"] <- 
  "cypr"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="tria fb tria"] <- "single"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prop + azo"] <- 
  "AZO + PROP"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="tebu + thio"] <- 
  "TEBU + THIO"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="tebu + pyra + pyra"] <- 
  "PYR + TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="tebu + adj"] <- 
  "ADJ + TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prop + trif"] <- 
  "PROP + TRIF"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="pyra + adj"] <- 
  "ADJ + PYR"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="flus + carb + picoxy"] <- 
  "CARB + FLUS + PICO"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="unknown + myc"] <- 
  "MYC + OTH"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="cyp + azo fb cyp"] <- 
  "AZO + CYPR fb CYPR"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="flut + pyra"] <- 
  "FLUT + PYR"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="flut + thio"] <- 
  "FLUT + THIO"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="thio fb tebu"] <- 
  "THIO fb TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="thio + tebu"] <- 
  "TEBU + THIO"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="pico + cypr"] <- 
  "CYPR + PICO"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prot + trif"] <- 
  "PROT + TRIF"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="tebu + pyra fb tebu + pyra"] <- 
  "PYR + TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="tebu + pyra fb myc"] <- 
  "PYR + TEBU fb MYC"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prop +trif fb tebu"] <- 
  "PROP + TRIF fb TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prop + trif fb prya"] <- 
  "PROP + TRIF fb PYR"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prop +azo fb tebu"] <- 
  "AZO + PROP fb TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prop + azo fb myc"] <-
  "AZO + PROP fb MYC"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prop + azy"] <- 
  "AZO + PROP"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="tebu + chlor"] <- 
  "CHL + TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="tebu + pyra fb tebu"] <- 
  "PYR + TEBU fb TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="tebu +pyra fb flut"] <- 
  "PYR + TEBU fb FLUT"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prop + trif fb tebu"] <- 
  "PROP + TRIF fb TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prop + trif fb myc"] <- 
  "PROP + TRIF fb MYC"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prop fb tebu"] <- 
  "PROP fb TEBU"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prop fb myc"] <- 
  "PROP fb MYC"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prop fb flut"] <- 
  "PROP fb FLUT"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="prop + trif fb flut"] <- 
  "PROP + TRIF fb FLUT"
data.reduced$alphaIngred[data.reduced$active.ingredient.coded=="tebu fb pyra"] <- 
  "TEBU fb PYR"
data.reduced$combo_complete[!is.na(data.reduced$alphaIngred)] <- "complete"

#' Look at results, do any have n >= 15?
sort(table(data.reduced$alphaIngred))
#  only pyr + tebu and azo + prop have >15


#     Single applications
data.reduced$activeIngClean[data.reduced$activeIngClean=="empty"] <- 
  as.character(data.reduced$active.ingredient.coded[data.reduced$activeIngClean=="empty"])
#     Clean up so labels are consistent (e.g., "pyr" vs "pyra")
data.reduced$activeIngClean[data.reduced$activeIngClean=="pyr"] <- "PYR"
data.reduced$activeIngClean[data.reduced$activeIngClean=="pyraclostrobin"] <- "PYR"
data.reduced$activeIngClean[data.reduced$activeIngClean=="pyro"] <- "PYR"
data.reduced$activeIngClean[data.reduced$activeIngClean=="teb"] <- "TEBU"
data.reduced$activeIngClean[data.reduced$activeIngClean=="tetr"] <- "TETR"
data.reduced$activeIngClean[data.reduced$activeIngClean=="thi"] <- "THIO"
data.reduced$activeIngClean[data.reduced$activeIngClean==""] <- "unknown"
data.reduced$activeIngClean[data.reduced$activeIngClean=="cyp"] <- "CYPR"
data.reduced$activeIngClean[data.reduced$activeIngClean=="tebu "] <- "TEBU"
data.reduced$activeIngClean[data.reduced$active.ingredient.coded=="tria"] <- "TEBU"
#     Check progress
sort(table(data.reduced$activeIngClean))

#' Convert to capital letters
data.reduced$activeIngClean <- toupper(data.reduced$activeIngClean) 
sort(table(data.reduced$activeIngClean))

data.reduced$activeIngClean[data.reduced$activeIngClean=="PYRA"] <- "PYR"
data.reduced$activeIngClean[data.reduced$activeIngClean=="FEBU"] <- "FENB"



#' Class of fungicide: clean up labels to be consistent and if <5, relabel as "oth"
#' 
#+ standardizeClass
# Table to see how it looks as is
table(data.reduced$class.code)
# Start with "empty"
data.reduced$classClean <- "empty"
# Clean up labels
data.reduced$classClean[data.reduced$class.code==""] <- "unknown"
data.reduced$classClean[data.reduced$class.code=="other "] <- "other"
data.reduced$classClean[data.reduced$class.code=="strobilurin "] <- "strobilurin"
data.reduced$classClean[data.reduced$class.code=="strobulurin"] <- "strobilurin"
data.reduced$classClean[data.reduced$class.code=="triaz+AL384:AM384ole"] <- "triazole"
data.reduced$classClean[data.reduced$class.code=="tebu"] <- "strobilurin"
# Fill in the rest
data.reduced$classClean[data.reduced$classClean=="empty"] <- 
  as.character(data.reduced$class.code[data.reduced$classClean=="empty"])
# Check with table
sort(table(data.reduced$classClean))

# View relation between classes and active ingredients
summaryBy(FID~activeIngClean+classClean,data = data.reduced,FUN=length)

# Fix the classes
data.reduced$classClean[data.reduced$activeIngClean=="AZO"] <- "strobilurin"
data.reduced$classClean[data.reduced$activeIngClean=="COP"] <- "copper sulfate"
data.reduced$classClean[data.reduced$activeIngClean=="MYC"] <- "myc"
data.reduced$classClean[data.reduced$activeIngClean=="PYR"] <- "strobilurin"
data.reduced$classClean[data.reduced$activeIngClean=="TETR"] <- "triazole"
data.reduced$classClean[data.reduced$activeIngClean=="TEBU"] <- "triazole"
data.reduced$classClean[data.reduced$activeIngClean=="THIO"] <- "thiophanate"

# View relation between classes and active ingredients
summaryBy(FID~classClean + activeIngClean,data = data.reduced,FUN=length)

# Instead of "mma", specify the mixed classes
head(data.reduced[data.reduced$classClean=="mma",c(10:12,45:48)])
data.reduced$classClean[data.reduced$alphaIngred=="TEBU + THIO"] <- "triaz + thio"
data.reduced$classClean[data.reduced$alphaIngred=="PROP + TRIF"] <- "triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="TEBU + TRIF + OTH"] <- "triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="AZO + PROP"] <- "triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="AZO + CYPR"] <- "triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="PYR + TEBU"] <- "triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="TEBU + TRIF"] <- "triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="PROP + TEBU + TRIF"] <- "triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="FLUT + PYR"] <- "triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="FLUS + PYR"] <- "triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="AZO + PROP + TRIF"] <- "triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="AZO + PROP + TEBU"] <- "triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="AZO + FLUT + PROP"] <- "triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="FLUS + PICO"] <- "triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="MYC + PYR"] <- "triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="FLUT + PROP + TRIF"] <- "triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="AZO + DIF"] <- "triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="PROT + TRIF"] <- "triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="AZO + MYC + PROP"] <- "triaz + strob"
data.reduced$classClean[data.reduced$classClean=="copper sulfate"] <- "inorganic"
data.reduced$classClean[data.reduced$classClean=="mancozeb"] <- "dithiocarbamates"
data.reduced$classClean[data.reduced$classClean=="metconazole"] <- "triazole"
data.reduced$classClean[data.reduced$classClean=="myc"] <- "triazole"
data.reduced$classClean[data.reduced$alphaIngred=="PROP + PROT + TRIF"] <- "triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="FLUX + PYR"] <- "strobilurin"
data.reduced$classClean[data.reduced$alphaIngred=="FLUT + FLUX"] <- "triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="AZO + FLUT"] <- "triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="AZO + PYR"] <- "strobilurin"
data.reduced$classClean[data.reduced$alphaIngred=="AZO + MYC"] <- "triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="AZO + TEBU"] <- "triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="AZO + PYR"] <- "strobilurin"
data.reduced$classClean[data.reduced$alphaIngred=="FEN + PYR"] <- "triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="SUL + TEBU"] <- "inorg + triaz"
data.reduced$classClean[data.reduced$alphaIngred=="MYC + PROP + TRIF"] <- "triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="CHL + TEBU"] <- "chlor + triaz"
data.reduced$classClean[data.reduced$alphaIngred=="AZO + TETR"] <- "triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="ORTH + TETR"] <- "triaz + other"
data.reduced$classClean[data.reduced$alphaIngred=="CROP + TETR"] <- "triaz + other"
data.reduced$classClean[data.reduced$alphaIngred=="SURF + TEBU"] <- "triaz + other"
data.reduced$classClean[data.reduced$alphaIngred=="COPPER + OTH"] <- "inorg + other"
data.reduced$classClean[data.reduced$alphaIngred=="COPPER + TEBU"] <- "inorg + triaz"
data.reduced$classClean[data.reduced$alphaIngred=="AZO + ORG"] <- "organic + strob"
data.reduced$classClean[data.reduced$alphaIngred=="PYR fb TEBU"] <- "series strob fb triaz"
data.reduced$classClean[data.reduced$alphaIngred=="TEBU fb PYR"] <- "series triaz fb strob"
data.reduced$classClean[data.reduced$alphaIngred=="AZO + PROP fb TEBU"] <- "series triaz + strob fb triaz"
data.reduced$classClean[data.reduced$alphaIngred=="OTH + PYR fb TEBU"] <- "series strob + other fb triaz"
data.reduced$classClean[data.reduced$alphaIngred=="PROP + TRIF fb TEBU"] <- "series triaz + strob fb triaz"
data.reduced$classClean[data.reduced$alphaIngred=="AZO fb TEBU"] <- "series strob fb triaz"
data.reduced$classClean[data.reduced$alphaIngred=="AZO fb MYC"] <- "series strob fb triaz"
data.reduced$classClean[data.reduced$alphaIngred=="AZO fb PYR"] <- "strobilurin"
data.reduced$classClean[data.reduced$alphaIngred=="AZO fb FLUT"] <- "series strob fb triaz"
data.reduced$classClean[data.reduced$alphaIngred=="AZO fb PYR + TEBU"] <- "series strob fb triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="CHL fb CHL + TEBU"] <- "series chlor fb chlor + triaz"
data.reduced$classClean[data.reduced$alphaIngred=="ETH + OTH"] <- "other"
data.reduced$classClean[data.reduced$alphaIngred=="ETH + TEBU"] <- "triaz + other"
data.reduced$classClean[data.reduced$alphaIngred=="MET + PYR"] <- "triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="CHL fb TEBU"] <- "series chlor fb triaz"
data.reduced$classClean[data.reduced$alphaIngred=="SUL + OTH"] <- "inorg + other"
data.reduced$classClean[data.reduced$alphaIngred=="OTH + THIO"] <- "thio + other"
data.reduced$classClean[data.reduced$alphaIngred=="FLUS + OTH"] <- "triaz + other"
data.reduced$classClean[data.reduced$alphaIngred=="ETH + FLUT"] <- "triaz + other"
data.reduced$classClean[data.reduced$alphaIngred=="PYR + TEBU fb PYR"] <- "series triaz + strob fb strob"
data.reduced$classClean[data.reduced$alphaIngred=="ADJ + TEBU"] <- "triaz + other"
data.reduced$classClean[data.reduced$alphaIngred=="ADJ + PYR"] <- "strob + other"
data.reduced$classClean[data.reduced$alphaIngred=="CARB + FLUS + PICO"] <- "triaz + strob + thio"
data.reduced$classClean[data.reduced$alphaIngred=="MYC + OTH"] <- "triaz + other"
data.reduced$classClean[data.reduced$alphaIngred=="CYPR + PICO"] <- "triaz + strob"
data.reduced$classClean[data.reduced$alphaIngred=="PYR + TEBU fb MYC"] <- "series triaz + strob fb triaz"
data.reduced$classClean[data.reduced$alphaIngred=="THIO fb TEBU"] <- "series thio fb triaz"
data.reduced$classClean[data.reduced$alphaIngred=="AZO + CYPR fb CYPR"] <- "series triaz + strob fb triaz"
data.reduced$classClean[data.reduced$alphaIngred=="PROP + TRIF fb PYR"] <- "series triaz + strob fb strob"
data.reduced$classClean[data.reduced$alphaIngred=="PROP + TRIF fb MYC"] <- "series triaz + strob fb triaz"
data.reduced$classClean[data.reduced$alphaIngred=="PYR + TEBU fb TEBU"] <- "series triaz + strob fb triaz"
data.reduced$classClean[data.reduced$alphaIngred=="PYR + TEBU fb FLUT"] <- "series triaz + strob fb triaz"
data.reduced$classClean[data.reduced$alphaIngred=="AZO + PROP fb MYC"] <- "series triaz + strob fb triaz"
data.reduced$classClean[data.reduced$alphaIngred=="PROP fb MYC"] <- "triazole"
data.reduced$classClean[data.reduced$alphaIngred=="FLUT + THIO"] <- "triaz + thio"
data.reduced$classClean[data.reduced$alphaIngred=="FLUT + OTH"] <- "triaz + other"
data.reduced$classClean[data.reduced$alphaIngred=="PROP fb TEBU"] <- "triazole"
data.reduced$classClean[data.reduced$alphaIngred=="PROP fb FLUT"] <- "triazole"
data.reduced$classClean[data.reduced$alphaIngred=="PROP + TRIF fb FLUT"] <- "series triaz + strob fb triaz"
data.reduced$classClean[data.reduced$trade.name=="Charisma"] <- "triaz + strob"
data.reduced$classClean[data.reduced$classClean=="dithiocarbamates"] <- "other"

# Checking data
head(data.reduced[data.reduced$classClean=="other",c(10:12,45:48)],32)
sort(table(data.reduced$classClean))
sort(table(data.reduced$activeIngClean))

# Final clean up of specific cases
data.reduced$activeIngClean[data.reduced$active.ingredient.coded=="sul"] <- "SUL"
data.reduced$classClean[data.reduced$active.ingredient.coded=="sul"] <- "inorganic"
data.reduced$classClean[data.reduced$trade.name=="Ballad"] <- "organic"
data.reduced$activeIngClean[data.reduced$active.ingredient.coded=="eth"] <- "OTH"
data.reduced$activeIngClean[data.reduced$activeIngClean=="OTH"] <- "other"
data.reduced$activeIngClean[data.reduced$activeIngClean=="OTHER"] <- "other"
data.reduced$activeIngClean[data.reduced$activeIngClean=="UNKNOWN"] <- "other"
data.reduced$classClean[data.reduced$classClean=="unknown"] <- "other"





#' Growth stages of application
#' 
#+ standardizeRclass
# Examine original data
sort(table(data.reduced$Growth.stage.applied))
data.reduced$growthStateClean <- "empty"

#   Applied during R1 plus another stage
data.reduced$growthStateClean[grep("^1", data.reduced$Growth.stage.applied)] <- "1+"
data.reduced$growthStateClean[grep("^r1", data.reduced$Growth.stage.applied)] <- "1+"
#   Applied during R1 only
data.reduced$growthStateClean[data.reduced$Growth.stage.applied=="1"|
                                data.reduced$Growth.stage.applied=="r1"|
                                data.reduced$Growth.stage.applied=="R1"] <- "1"
#   Applied during R2 initially plus another stage
data.reduced$growthStateClean[grep("^r2", data.reduced$Growth.stage.applied)] <- "2+"
#   Applied during R2 only
data.reduced$growthStateClean[data.reduced$Growth.stage.applied=="r2"] <- "2"
#   Applied during R3 initially plus another stage
data.reduced$growthStateClean[grep("^3", data.reduced$Growth.stage.applied)] <- "3"
data.reduced$growthStateClean[grep("^r3", data.reduced$Growth.stage.applied)] <- "3"
#   Applied during R3 only
data.reduced$growthStateClean[data.reduced$Growth.stage.applied=="r3"|
                                data.reduced$Growth.stage.applied=="R3"] <- "3"
#   Applied during R4 initially plus another stage (none were 4 only)
data.reduced$growthStateClean[grep("^r4", data.reduced$Growth.stage.applied)] <- "4"
#   Applied during R5
data.reduced$growthStateClean[grep("^r5", data.reduced$Growth.stage.applied)] <- "5"
data.reduced$growthStateClean[grep("^R5", data.reduced$Growth.stage.applied)] <- "5"
data.reduced$growthStateClean[grep("^5", data.reduced$Growth.stage.applied)] <- "5"
#   Applied during V-stage
data.reduced$growthStateClean[grep("^v", data.reduced$Growth.stage.applied)] <- "V"
#   Unknown stage of application
data.reduced$growthStateClean[data.reduced$growthStateClean=="empty"] <- "unknown"
# Check with table
#sort(table(data.reduced$Growth.stage.applied[data.reduced$growthStateClean=="empty"]))
table(data.reduced$growthStateClean)

#' Number of applications
#' 
#+ standardizeNumbApps
# Check with table
table(data.reduced$applicationsNumb)

#' ### 3. Partition data
#' 
#' Rust data
#+ rustData
# Data that had rust severity in percentage originally
    # Column #15 is rustSeverPerc
rust.perc.orig <- data.reduced[!is.na(data.reduced$rustSeverPerc),]
rust.perc.orig$scale <- "Percent"
# Data that had rust severity on scale originally
    # Column #19 is rustSever1-8
rust.scale.orig <- data.reduced[!is.na(data.reduced$rustSever1.8),]
# Data from Lawrence references are actually on 0-10 Scale
scale.1to10.refs <- c(3,4,53,55,62,68,16,41)
rust.scale.orig$scale[rust.scale.orig$ReferenceNumb %in% scale.1to10.refs] <- "Scale 0-10"
# Rest are on 1-8 scale
rust.scale.orig$scale[!rust.scale.orig$ReferenceNumb %in% scale.1to10.refs] <- "Scale 0-8"

# Combine back together
rust.data <- rbind(rust.perc.orig, rust.scale.orig)

#' Yield data
#+ yieldData
# Data that had yield data
    # Column #35 is yield
yield.data <- rust.data[!is.na(rust.data$yield),] 

#' 100sw data
#+ seedData
# Data that had 100sw data
    # Column #39 is 100sw
seedwt.data <- rust.data[!is.na(rust.data$seedWt),] 

#' Cercospora data
#+ cercosporaData
# Data that had Cercospora data
cerco.data <- data.reduced[!is.na(data.reduced$CercoSever),]
# Define the scale used
cerco.data$scale <- "empty"
# Data from Padgett et al. studies used scale 0-10
cerco.data$scale[grep("^Pa", cerco.data$Reference)] <- "Scale 0-10"
# Data from Price et al. studies used scale 0-8
cerco.data$scale[grep("^Pr", cerco.data$Reference)] <- "Scale 0-8"
# Data from others used 0-5 scale
cerco.data$scale[cerco.data$scale=="empty"] <- "Scale 0-5"

#' Target spot data
#+ targetSpotData
# Data that had Target Spot data
target.spot.data <- data.reduced[!is.na(data.reduced$Tsseverity)|
                                   !is.na(data.reduced$Tsincidence),]
# Define the scale used
target.spot.data$scale <- "empty"
# Percentage originally
target.spot.data$scale[target.spot.data$ReferenceNumb==55 | 
                         target.spot.data$ReferenceNumb==21] <- "Percent"
# Data with scale 0-10
target.spot.data$scale[target.spot.data$ReferenceNumb==30] <- "Scale 0-10"
# Data with scale 0-5
target.spot.data$scale[target.spot.data$scale=="empty"] <- "Scale 0-5"

#' ### 4. Standardize effect size information
#' 
#' * Convert group means, n, and std into form needed for metafor package
#' 
#' For rust severity on scale 0-8, convert to percent
#' 
#' * if scale is less than or equal to 2; 2.5*scale
#' * if scale is greater than 2, less than or equal to 4; 5 + 5*(scale-2)
#' * if scale is greater than 4, less than or equal to 6; 15 + 10*(scale-4)
#' * if scale is greater than 6; 35 + 32.5*(scale-6)
#' 
#' For rust severity on scale 0-10, convert to percent
#' 
#' * if scale is <= 3, (0.25/3)*scale
#' * if scale is >3, <=4, 2.5 + 2.5(scale-3)
#' * if scale is >4, <=6, 5 + 5(scale-4)
#' * if scale is >6, <=8, 15 + 10(scale-6)
#' * if scale is >8, 35 + 32.5(scale-8)
#' 
#+ rustScalePerc
# For treatment groups
rust.data$m1i[rust.data$scale=="Scale 0-8" & 
                rust.data$rustSever1.8 <= 2] <-
                      rust.data$rustSever1.8[rust.data$scale=="Scale 0-8"&
                                               rust.data$rustSever1.8 <= 2]*2.5
rust.data$m1i[rust.data$scale=="Scale 0-8" & 
                rust.data$rustSever1.8 > 2 &
                rust.data$rustSever1.8 <= 4] <- 5 +
                      (rust.data$rustSever1.8[rust.data$scale=="Scale 0-8"&
                                               rust.data$rustSever1.8 > 2 &
                                               rust.data$rustSever1.8 <= 4]-2)*5
rust.data$m1i[rust.data$scale=="Scale 0-8" & 
                rust.data$rustSever1.8 > 4 &
                rust.data$rustSever1.8 <= 6] <- 15 +
                      (rust.data$rustSever1.8[rust.data$scale=="Scale 0-8"&
                                                rust.data$rustSever1.8 > 4 &
                                                rust.data$rustSever1.8 <= 6]-4)*10
rust.data$m1i[rust.data$scale=="Scale 0-8" & 
                rust.data$rustSever1.8 > 6] <- 35 +
                      (rust.data$rustSever1.8[rust.data$scale=="Scale 0-8"&
                                                rust.data$rustSever1.8 > 6]-6)*32.5
# For control groups
rust.data$m2i[rust.data$scale=="Scale 0-8" & 
                rust.data$rustSever1.8Cont <= 2] <-
                      rust.data$rustSever1.8Cont[rust.data$scale=="Scale 0-8"&
                                               rust.data$rustSever1.8Cont <= 2]*2.5
rust.data$m2i[rust.data$scale=="Scale 0-8" & 
                rust.data$rustSever1.8Cont > 2 &
                rust.data$rustSever1.8Cont <= 4] <- 5 +
                      (rust.data$rustSever1.8Cont[rust.data$scale=="Scale 0-8"&
                                                rust.data$rustSever1.8Cont > 2 &
                                                rust.data$rustSever1.8Cont <= 4]-2)*5
rust.data$m2i[rust.data$scale=="Scale 0-8" & 
                rust.data$rustSever1.8Cont > 4 &
                rust.data$rustSever1.8Cont <= 6] <- 15 +
                      (rust.data$rustSever1.8Cont[rust.data$scale=="Scale 0-8"&
                                                rust.data$rustSever1.8Cont > 4 &
                                                rust.data$rustSever1.8Cont <= 6]-4)*10
rust.data$m2i[rust.data$scale=="Scale 0-8" & 
                rust.data$rustSever1.8Cont > 6] <- 35 +
                      (rust.data$rustSever1.8Cont[rust.data$scale=="Scale 0-8"&
                                                rust.data$rustSever1.8Cont > 6]-6)*32.5
# For rust severity on 0-10 scale
rust.data$m1i[rust.data$scale=="Scale 0-10" & 
                rust.data$rustSever1.8 <= 3] <-
  rust.data$rustSever1.8[rust.data$scale=="Scale 0-10"&
                           rust.data$rustSever1.8 <= 3]*(2.5/3)
rust.data$m1i[rust.data$scale=="Scale 0-10" & 
                rust.data$rustSever1.8 > 3 &
                rust.data$rustSever1.8 <= 4] <- 2.5 +
  (rust.data$rustSever1.8[rust.data$scale=="Scale 0-10" & 
                           rust.data$rustSever1.8 > 3 &
                           rust.data$rustSever1.8 <= 4]-3)*2.5
rust.data$m1i[rust.data$scale=="Scale 0-10" & 
                rust.data$rustSever1.8 > 4 &
                rust.data$rustSever1.8 <= 6] <- 5 +
  (rust.data$rustSever1.8[rust.data$scale=="Scale 0-10" & 
                           rust.data$rustSever1.8 > 4 &
                           rust.data$rustSever1.8 <= 6]-4)*5
rust.data$m1i[rust.data$scale=="Scale 0-10" & 
                rust.data$rustSever1.8 > 6 &
                rust.data$rustSever1.8 <= 8] <- 15 +
  (rust.data$rustSever1.8[rust.data$scale=="Scale 0-10" & 
                           rust.data$rustSever1.8 > 6 &
                           rust.data$rustSever1.8 <= 8]-6)*10
rust.data$m1i[rust.data$scale=="Scale 0-10" & 
                rust.data$rustSever1.8 > 8] <- 35 +
  (rust.data$rustSever1.8[rust.data$scale=="Scale 0-10" & 
                            rust.data$rustSever1.8 > 8]-8)*32.5
# Control
rust.data$m2i[rust.data$scale=="Scale 0-10" & 
                rust.data$rustSever1.8Cont <= 3] <-
  rust.data$rustSever1.8Cont[rust.data$scale=="Scale 0-10"&
                           rust.data$rustSever1.8Cont <= 3]*(2.5/3)
rust.data$m2i[rust.data$scale=="Scale 0-10" & 
                rust.data$rustSever1.8Cont > 3 &
                rust.data$rustSever1.8Cont <= 4] <- 2.5 +
  (rust.data$rustSever1.8Cont[rust.data$scale=="Scale 0-10" & 
                            rust.data$rustSever1.8Cont > 3 &
                            rust.data$rustSever1.8Cont <= 4]-3)*2.5
rust.data$m2i[rust.data$scale=="Scale 0-10" & 
                rust.data$rustSever1.8Cont > 4 &
                rust.data$rustSever1.8Cont <= 6] <- 5 +
  (rust.data$rustSever1.8Cont[rust.data$scale=="Scale 0-10" & 
                            rust.data$rustSever1.8Cont > 4 &
                            rust.data$rustSever1.8Cont <= 6]-4)*5
rust.data$m2i[rust.data$scale=="Scale 0-10" & 
                rust.data$rustSever1.8Cont > 6 &
                rust.data$rustSever1.8Cont <= 8] <- 15 +
  (rust.data$rustSever1.8Cont[rust.data$scale=="Scale 0-10" & 
                            rust.data$rustSever1.8Cont > 6 &
                            rust.data$rustSever1.8Cont <= 8]-6)*10
rust.data$m2i[rust.data$scale=="Scale 0-10" & 
                rust.data$rustSever1.8Cont > 8] <- 35 +
  (rust.data$rustSever1.8Cont[rust.data$scale=="Scale 0-10" & 
                            rust.data$rustSever1.8Cont > 8]-8)*32.5
# For rust severity in percent originally
rust.data$m1i[rust.data$scale=="Percent"] <- rust.data$rustSeverPerc[rust.data$scale=="Percent"]
rust.data$m2i[rust.data$scale=="Percent"] <- rust.data$rustSeverPercCont[rust.data$scale=="Percent"]
# Sample sizes
rust.data$n1i <- rust.data$n2i <- rust.data$replications

#' Yield data
#+ yieldDatametafor
yield.data$yield.kg.ha <- yield.data$yield*67.25 # convert to kg ha-1
yield.data$yieldCont.kg.ha <- yield.data$yieldCont*67.25 # convert to kg ha-1
  # conversion info from https://www.extension.iastate.edu/agdm/wholefarm/html/c6-80.html
yield.data$m1i <- yield.data$yield.kg.ha # Yield for treatment group
yield.data$m2i <- yield.data$yieldCont.kg.ha # Yield for control group
yield.data$n1i <- yield.data$n2i <- yield.data$replications

#' 100 seed weight data
#+ seedDatametafor
seedwt.data$m1i <- seedwt.data$seedWt # Yield for treatment group
seedwt.data$m2i <- seedwt.data$seedWtCont # Yield for control group
seedwt.data$n1i <- seedwt.data$n2i <- seedwt.data$replications

#' Cercospora data
#+ cercoMetafor
# Scale 0-10
cerco.data$m1i[cerco.data$scale=="Scale 0-10"] <- 
  cerco.data$CercoSever[cerco.data$scale=="Scale 0-10"]*10
cerco.data$m2i[cerco.data$scale=="Scale 0-10"] <- 
  cerco.data$CercoSeverCont[cerco.data$scale=="Scale 0-10"]*10
# Scale 0-8
cerco.data$m1i[cerco.data$scale=="Scale 0-8" & 
                cerco.data$CercoSever <= 2] <-
  cerco.data$CercoSever[cerco.data$scale=="Scale 0-8"&
                           cerco.data$CercoSever <= 2]*2.5
cerco.data$m1i[cerco.data$scale=="Scale 0-8" & 
                cerco.data$CercoSever > 2 &
                cerco.data$CercoSever <= 4] <- 5 +
  (cerco.data$CercoSever[cerco.data$scale=="Scale 0-8"&
                            cerco.data$CercoSever > 2 &
                            cerco.data$CercoSever <= 4]-2)*5
cerco.data$m1i[cerco.data$scale=="Scale 0-8" & 
                cerco.data$CercoSever > 4 &
                cerco.data$CercoSever <= 6] <- 15 +
  (cerco.data$CercoSever[cerco.data$scale=="Scale 0-8"&
                            cerco.data$CercoSever > 4 &
                            cerco.data$CercoSever <= 6]-4)*10
cerco.data$m1i[cerco.data$scale=="Scale 0-8" & 
                cerco.data$CercoSever > 6] <- 35 +
  (cerco.data$CercoSever[cerco.data$scale=="Scale 0-8"&
                            cerco.data$CercoSever > 6]-6)*32.5
# Control
cerco.data$m2i[cerco.data$scale=="Scale 0-8" & 
                 cerco.data$CercoSeverCont <= 2] <-
  cerco.data$CercoSeverCont[cerco.data$scale=="Scale 0-8"&
                          cerco.data$CercoSeverCont <= 2]*2.5
cerco.data$m2i[cerco.data$scale=="Scale 0-8" & 
                 cerco.data$CercoSeverCont > 2 &
                 cerco.data$CercoSeverCont <= 4] <- 5 +
  (cerco.data$CercoSeverCont[cerco.data$scale=="Scale 0-8"&
                           cerco.data$CercoSeverCont > 2 &
                           cerco.data$CercoSeverCont <= 4]-2)*5
cerco.data$m2i[cerco.data$scale=="Scale 0-8" & 
                 cerco.data$CercoSeverCont > 4 &
                 cerco.data$CercoSeverCont <= 6] <- 15 +
  (cerco.data$CercoSeverCont[cerco.data$scale=="Scale 0-8"&
                           cerco.data$CercoSeverCont > 4 &
                           cerco.data$CercoSeverCont <= 6]-4)*10
cerco.data$m2i[cerco.data$scale=="Scale 0-8" & 
                 cerco.data$CercoSeverCont > 6] <- 35 +
  (cerco.data$CercoSeverCont[cerco.data$scale=="Scale 0-8"&
                           cerco.data$CercoSeverCont > 6]-6)*32.5
#' Scale 0-5
#' 
#' * if scale is less than or equal to 1; 10*scale
#' * if scale is greater than 1, less than or equal to 2; 10 + 15*(scale-1)
#' * if scale is greater than 2; 25 + 25*(scale-2)
#' 
#+ scale05cerco
cerco.data$m1i[cerco.data$scale=="Scale 0-5" & 
                 cerco.data$CercoSever <= 1] <-
  cerco.data$CercoSever[cerco.data$scale=="Scale 0-5"&
                          cerco.data$CercoSever <= 1]*10
cerco.data$m1i[cerco.data$scale=="Scale 0-5" & 
                 cerco.data$CercoSever > 1 &
                 cerco.data$CercoSever <= 2] <- 10 +
  (cerco.data$CercoSever[cerco.data$scale=="Scale 0-5"& 
                           cerco.data$CercoSever > 1 &
                           cerco.data$CercoSever <= 2]-1)*15
cerco.data$m1i[cerco.data$scale=="Scale 0-5" & 
                 cerco.data$CercoSever > 2 ] <- 25 +
  (cerco.data$CercoSever[cerco.data$scale=="Scale 0-5"& 
                           cerco.data$CercoSever > 2]-2)*25
# Control
cerco.data$m2i[cerco.data$scale=="Scale 0-5" & 
                 cerco.data$CercoSeverCont <= 1] <-
  cerco.data$CercoSeverCont[cerco.data$scale=="Scale 0-5"&
                          cerco.data$CercoSeverCont <= 1]*10
cerco.data$m2i[cerco.data$scale=="Scale 0-5" & 
                 cerco.data$CercoSeverCont > 1 &
                 cerco.data$CercoSeverCont <= 2] <- 10 +
  (cerco.data$CercoSeverCont[cerco.data$scale=="Scale 0-5"& 
                           cerco.data$CercoSeverCont > 1 &
                           cerco.data$CercoSeverCont <= 2]-1)*15
cerco.data$m2i[cerco.data$scale=="Scale 0-5" & 
                 cerco.data$CercoSeverCont > 2 ] <- 25 +
  (cerco.data$CercoSeverCont[cerco.data$scale=="Scale 0-5"& 
                           cerco.data$CercoSeverCont > 2]-2)*25
cerco.data$n1i <- cerco.data$n2i <- cerco.data$replications

#' Target spot data
#+ targetSpotMetafor
# Scale 0-10
target.spot.data$m1i[target.spot.data$scale=="Scale 0-10"] <- 
  target.spot.data$Tsseverity[target.spot.data$scale=="Scale 0-10"]*10
target.spot.data$m2i[target.spot.data$scale=="Scale 0-10"] <- 
  target.spot.data$TSseverityCont[target.spot.data$scale=="Scale 0-10"]*10
# Percentage
target.spot.data$m1i[target.spot.data$scale=="Percent"] <- 
  target.spot.data$Tsincidence[target.spot.data$scale=="Percent"]
target.spot.data$m2i[target.spot.data$scale=="Percent"] <- 
  target.spot.data$TSincidenceCont[target.spot.data$scale=="Percent"]
# Scale 0-5
target.spot.data$m1i[target.spot.data$scale=="Scale 0-5" & 
                 target.spot.data$Tsseverity <= 1] <-
  target.spot.data$Tsseverity[target.spot.data$scale=="Scale 0-5"&
                          target.spot.data$Tsseverity <= 1]*10
target.spot.data$m1i[target.spot.data$scale=="Scale 0-5" & 
                 target.spot.data$Tsseverity > 1 &
                 target.spot.data$Tsseverity <= 2] <- 10 +
  (target.spot.data$Tsseverity[target.spot.data$scale=="Scale 0-5"& 
                           target.spot.data$Tsseverity > 1 &
                           target.spot.data$Tsseverity <= 2]-1)*15
target.spot.data$m1i[target.spot.data$scale=="Scale 0-5" & 
                 target.spot.data$Tsseverity > 2 ] <- 25 +
  (target.spot.data$Tsseverity[target.spot.data$scale=="Scale 0-5"& 
                           target.spot.data$Tsseverity > 2]-2)*25
# Control
target.spot.data$m2i[target.spot.data$scale=="Scale 0-5" & 
                 target.spot.data$TSseverityCont <= 1] <-
  target.spot.data$TSseverityCont[target.spot.data$scale=="Scale 0-5"&
                              target.spot.data$TSseverityCont <= 1]*10
target.spot.data$m2i[target.spot.data$scale=="Scale 0-5" & 
                 target.spot.data$TSseverityCont > 1 &
                 target.spot.data$TSseverityCont <= 2] <- 10 +
  (target.spot.data$TSseverityCont[target.spot.data$scale=="Scale 0-5"& 
                               target.spot.data$TSseverityCont > 1 &
                               target.spot.data$TSseverityCont <= 2]-1)*15
target.spot.data$m2i[target.spot.data$scale=="Scale 0-5" & 
                 target.spot.data$TSseverityCont > 2 ] <- 25 +
  (target.spot.data$TSseverityCont[target.spot.data$scale=="Scale 0-5"& 
                               target.spot.data$TSseverityCont > 2]-2)*25
target.spot.data$n1i <- target.spot.data$n2i <- target.spot.data$replications

#' ### 5. Take out categorical moderators with n < 5
#' 
#' Rust data
#+ removeSmallRst
# Active ingredients
sort(table(rust.data$activeIngClean))
analyze.ai <- c("DUAL", "PYR","TEBU","FLUT","MIXED")
rust.data$category_ai[rust.data$activeIngClean %in% analyze.ai] <- 
  rust.data$activeIngClean[rust.data$activeIngClean %in% analyze.ai]
table(rust.data$category_ai)

# Class
sort(table(rust.data$classClean))
analyze.class <- c("strobilurin","triaz + strob","triazole")
rust.data$category_class[rust.data$classClean %in% analyze.class] <- 
  rust.data$classClean[rust.data$classClean %in% analyze.class]
table(rust.data$category_class)

# R-stage
sort(table(rust.data$growthStateClean))
analyze.rstage <- c("4","5","1+","2+","3")
rust.data$category_rstage[rust.data$growthStateClean %in% analyze.rstage] <- 
  rust.data$growthStateClean[rust.data$growthStateClean %in% analyze.rstage]
table(rust.data$category_rstage)

# Applications 
sort(table(rust.data$applicationsNumb))
rust.data$number_applications[rust.data$applicationsNumb!=5] <- 
  rust.data$applicationsNumb[rust.data$applicationsNumb!=5]
table(rust.data$number_applications)

#' Yield data
#+ removeSmallYield
# Applications
sort(table(yield.data$applicationsNumb))
yield.data$number_applications[yield.data$applicationsNumb==1|yield.data$applicationsNumb==2] <- 
  yield.data$applicationsNumb[yield.data$applicationsNumb==1|yield.data$applicationsNumb==2]
table(yield.data$number_applications)

# R-stage
sort(table(yield.data$growthStateClean))
yield.data$category_rstage[yield.data$growthStateClean %in% analyze.rstage] <- 
  yield.data$growthStateClean[yield.data$growthStateClean %in% analyze.rstage]
table(yield.data$category_rstage)

# Active ingredients
sort(table(yield.data$activeIngClean))
analyze.ai <- c("PYR","TEBU","FLUT","MIXED")
yield.data$category_ai[yield.data$activeIngClean %in% analyze.ai] <- 
  yield.data$activeIngClean[yield.data$activeIngClean %in% analyze.ai]
sort(table(yield.data$category_ai))

# Class
sort(table(yield.data$classClean))
yield.data$category_class[yield.data$classClean %in% analyze.class] <- 
  yield.data$classClean[yield.data$classClean %in% analyze.class]
sort(table(yield.data$category_class))

#' 100-seed weight data
#+ removeSmall100sw
# Applications 
sort(table(seedwt.data$applicationsNumb))
seedwt.data$number_applications[seedwt.data$applicationsNumb!=3] <-
  seedwt.data$applicationsNumb[seedwt.data$applicationsNumb!=3]
table(seedwt.data$number_applications)

# growth stage
sort(table(seedwt.data$growthStateClean))
analyze.rstage <- c("4","1+","2+","3")
seedwt.data$category_rstage[seedwt.data$growthStateClean %in% analyze.rstage] <-
  seedwt.data$growthStateClean[seedwt.data$growthStateClean %in% analyze.rstage]
table(seedwt.data$category_rstage)

# Active ingredients
sort(table(seedwt.data$activeIngClean))
analyze.ai <- c("TEBU","FLUT","MIXED")
seedwt.data$category_ai[seedwt.data$activeIngClean %in% analyze.ai] <- 
  seedwt.data$activeIngClean[seedwt.data$activeIngClean %in% analyze.ai]
table(seedwt.data$category_ai)

# Class
sort(table(seedwt.data$classClean))
seedwt.data$category_class[seedwt.data$classClean %in% analyze.class] <- 
  seedwt.data$classClean[seedwt.data$classClean %in% analyze.class]
table(seedwt.data$category_class)

#' ### 6. Calculate effect sizes
#' 
#' Using log response ratio, cannot have 0s in data; so changing 0 to 0.0001
rust.data$m1i[rust.data$m1i==0] <- 0.0001
rust.data$m2i[rust.data$m2i==0] <- 0.0001

yield.data$m1i[yield.data$m1i==0] <- 0.0001
yield.data$m2i[yield.data$m2i==0] <- 0.0001

seedwt.data$m1i[seedwt.data$m1i==0] <- 0.0001
seedwt.data$m2i[seedwt.data$m2i==0] <- 0.0001

cerco.data$m1i[cerco.data$m1i==0] <- 0.0001
cerco.data$m2i[cerco.data$m2i==0] <- 0.0001

target.spot.data$m1i[target.spot.data$m1i==0] <- 0.0001
target.spot.data$m2i[target.spot.data$m2i==0] <- 0.0001
#' 
#' Overall means (raw mean difference)
#+ effectSizeMD
rust.data.ROM <- escalc(measure = "ROM", m1i = m1i, m2i = m2i, 
                    sd1i = sd1i, sd2i = sd2i, n1i = n1i, n2i = n2i,
                    data = rust.data)
yield.data.ROM <- escalc(measure = "ROM", m1i = m1i, m2i = m2i, 
                     sd1i = sd1i, sd2i = sd2i, n1i = n1i, n2i = n2i,
                     data = yield.data)
seedwt.data.ROM <- escalc(measure = "ROM", m1i = m1i, m2i = m2i, 
                      sd1i = sd1i, sd2i = sd2i, n1i = n1i, n2i = n2i,
                      data = seedwt.data)
cerco.data.ROM <- escalc(measure = "ROM", m1i = m1i, m2i = m2i, 
                              sd1i = sd1i, sd2i = sd2i, n1i = n1i, n2i = n2i,
                              data = cerco.data)
target.spot.data.ROM <- escalc(measure = "ROM", m1i = m1i, m2i = m2i, 
                              sd1i = sd1i, sd2i = sd2i, n1i = n1i, n2i = n2i,
                              data = target.spot.data)

#' ### 7. Save files for analysis
#+ save
save(rust.data.ROM, yield.data.ROM, seedwt.data.ROM,
     cerco.data.ROM, target.spot.data.ROM,
     file="data/output_data/data_cleaned.R")

#' ### Footer
#' 
#' Spun with ezspin("programs/data_processing.R", out_dir="output", fig_dir="figures", keep_md=FALSE)
#' 
#' Session Info:
#+ sessionInfo
sessionInfo()
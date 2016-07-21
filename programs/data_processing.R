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
data.reduced <- raw.data[,c(1:5,10:11,13:17,20:49)]
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
data.reduced$active.ingredient.coded[data.reduced$trade.name=="ACT Plus"] <- "oth"
data.reduced$active.ingredient.coded[data.reduced$trade.name=="Domark"] <- "tetra"
data.reduced$active.ingredient.coded[data.reduced$trade.name=="Folicur"] <- "tebu"
data.reduced$active.ingredient.coded[data.reduced$trade.name=="Folicur fb Headline"] <- "tebu + pyra"
data.reduced$active.ingredient.coded[data.reduced$trade.name=="Folicur + Headline"] <- "tebu + pyra"
data.reduced$active.ingredient.coded[data.reduced$trade.name=="Laredo"] <- "myc"
data.reduced$active.ingredient.coded[data.reduced$trade.name=="Punch fb Punch"] <- "flus"
data.reduced$active.ingredient.coded[data.reduced$trade.name=="Stratego"] <- "mixed"


# Active ingredients - new code for analysis (e.g. >5 obs)
#     Start by making new field and storing the old code
data.reduced$activeIngClean <- "empty"
#     Mixed
mixed.indices <- grep("+", data.reduced$active.ingredient.coded, fixed=TRUE)
data.reduced$activeIngClean[mixed.indices] <- "mixed"
#     Dual applications
dual.apps <- grep("fb", data.reduced$active.ingredient.coded, fixed=TRUE)
data.reduced$activeIngClean[dual.apps] <- "dual"
data.reduced$activeIngClean[grep("azo fb azo", data.reduced$active.ingredient.coded, fixed=F)] <- "azo"
data.reduced$activeIngClean[grep("myc fb myc", data.reduced$active.ingredient.coded, fixed=T)] <- "myc"
data.reduced$activeIngClean[grep("tetr fb tetr", data.reduced$active.ingredient.coded, fixed=T)] <- "tetra"
data.reduced$activeIngClean[grep("tetra fb tetra", data.reduced$active.ingredient.coded, fixed=T)] <- "tetra"
data.reduced$activeIngClean[grep("tebu fb tebu", data.reduced$active.ingredient.coded, fixed=T)] <- "tebu"
data.reduced$activeIngClean[grep("prop fb prop", data.reduced$active.ingredient.coded, fixed=T)] <- "prop"
data.reduced$activeIngClean[grep("pyra fb pyra", data.reduced$active.ingredient.coded, fixed=T)] <- "pyra"
data.reduced$activeIngClean[grep("pyra fb prya", data.reduced$active.ingredient.coded, fixed=T)] <- "pyra"
data.reduced$activeIngClean[grep("flut fb flut", data.reduced$active.ingredient.coded, fixed=T)] <- "flut"
#     Single applications
data.reduced$activeIngClean[data.reduced$activeIngClean=="empty"] <- 
  as.character(data.reduced$active.ingredient.coded[data.reduced$activeIngClean=="empty"])
#     Clean up so labels are consistent (e.g., "pyr" vs "pyra")
data.reduced$activeIngClean[data.reduced$activeIngClean=="pyr"] <- "pyra"
data.reduced$activeIngClean[data.reduced$activeIngClean=="pyraclostrobin"] <- "pyra"
data.reduced$activeIngClean[data.reduced$activeIngClean=="pyro"] <- "pyra"
data.reduced$activeIngClean[data.reduced$activeIngClean=="teb"] <- "tebu"
data.reduced$activeIngClean[data.reduced$activeIngClean=="tetr"] <- "tetra"
data.reduced$activeIngClean[data.reduced$activeIngClean=="thi"] <- "thio"
data.reduced$activeIngClean[data.reduced$activeIngClean==""] <- "unknown"
data.reduced$activeIngClean[data.reduced$activeIngClean=="cyp"] <- "cypr"
data.reduced$activeIngClean[data.reduced$activeIngClean=="tebu "] <- "tebu"
#     Check progress
table(data.reduced$activeIngClean)
#' When sample size is < 5, make "oth"
#+ standardizeActive2
less.than.five <- c("chlo", "cop", "eth", "febu", "met ", "org", "pico", "prop",
                    "prot", "sul", "tria", "trif", "unknown")
data.reduced$activeIngClean[data.reduced$activeIngClean %in% less.than.five] <- "oth"
# Check with table
table(data.reduced$activeIngClean)

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
# Fill in the rest
data.reduced$classClean[data.reduced$classClean=="empty"] <- 
  as.character(data.reduced$class.code[data.reduced$classClean=="empty"])
# Check with table
table(data.reduced$classClean)
#' When sample size is < 5, make "oth"
#+ standardizeClass2
less.than.five <- c("chloronitrile", "copper sulfate", "mancozeb", "metconazole",
                    "myc", "tebu", "unknown")
data.reduced$classClean[data.reduced$classClean %in% less.than.five] <- "other"
# Check with table
table(data.reduced$classClean)

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
#' When sample size is < 5, delete
#+ standardizeNumbApps2
data.reduced <- data.reduced[data.reduced$applicationsNumb!=8,]

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
yield.data <- data.reduced[!is.na(data.reduced$yield),] 

#' 100sw data
#+ seedData
# Data that had 100sw data
    # Column #39 is 100sw
seedwt.data <- data.reduced[!is.na(data.reduced$seedWt),] 

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
# Assume 20% as std deviation
rust.data$sd1i <- rust.data$sd2i <- 20
# Sample sizes
rust.data$n1i <- rust.data$n2i <- rust.data$replications

#' Yield data
#+ yieldDatametafor
yield.data$yield.kg.ha <- yield.data$yield*67.25 # convert to kg ha-1
yield.data$yieldCont.kg.ha <- yield.data$yieldCont*67.25 # convert to kg ha-1
  # conversion info from https://www.extension.iastate.edu/agdm/wholefarm/html/c6-80.html
yield.data$m1i <- yield.data$yield.kg.ha # Yield for treatment group
yield.data$sd1i <- yield.data$yield.kg.ha*0.2 # Std is 20% of mean
yield.data$m2i <- yield.data$yieldCont.kg.ha # Yield for control group
yield.data$sd2i <- yield.data$yieldCont.kg.ha*0.2 # Std is 20% of the mean
yield.data$n1i <- yield.data$n2i <- yield.data$replications

#' 100 seed weight data
#+ seedDatametafor
seedwt.data$m1i <- seedwt.data$seedWt # Yield for treatment group
seedwt.data$sd1i <- seedwt.data$seedWt*0.2 # Std is 20% of mean
seedwt.data$m2i <- seedwt.data$seedWtCont # Yield for control group
seedwt.data$sd2i <- seedwt.data$seedWtCont*0.2 # Std is 20% of the mean
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
# SD and replications
cerco.data$sd1i <- cerco.data$sd2i <- 20
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
# SD and replications
target.spot.data$sd1i <- target.spot.data$sd2i <- 20
target.spot.data$n1i <- target.spot.data$n2i <- target.spot.data$replications

#' ### 5. Take out categorical moderators with n < 5
#' 
#' Rust data
#+ removeSmallRst
# Active ingredients
sort(table(rust.data$activeIngClean))
small.ing <- c("cypr", "myc")
rust.data$activeIngClean[rust.data$activeIngClean %in% small.ing] <- "oth"
table(rust.data$activeIngClean)
# Class
sort(table(rust.data$classClean))
rust.data$classClean[rust.data$classClean=="thiophanate"] <- "other"
table(rust.data$classClean)
# R-stage
sort(table(rust.data$growthStateClean))
# Applications 
sort(table(rust.data$applicationsNumb))

#' Yield data
#+ removeSmallYield
# Applications
sort(table(yield.data$applicationsNumb))
yield.data <- yield.data[yield.data$applicationsNumb!=5,]
# R-stage
sort(table(yield.data$growthStateClean))
yield.data <- yield.data[yield.data$growthStateClean!="V",]
# Active ingredients
sort(table(yield.data$activeIngClean))
yield.data$activeIngClean[yield.data$activeIngClean=="gly"] <- "oth"
sort(table(yield.data$activeIngClean))
# Class
sort(table(yield.data$classClean))
yield.data$classClean[yield.data$classClean=="herbicide"] <- "other"
sort(table(yield.data$classClean))

#' 100-seed weight data
#+ removeSmall100sw
# Applications and growth stage
sort(table(seedwt.data$applicationsNumb))
sort(table(seedwt.data$growthStateClean))
seedwt.data <- seedwt.data[seedwt.data$applicationsNumb!=3 & 
                             seedwt.data$growthStateClean!=1,]
# Active ingredients
sort(table(seedwt.data$activeIngClean))
small.ing.100 <- c("gly", "thio" ,"cypr", "myc", "dual")
seedwt.data$activeIngClean[seedwt.data$activeIngClean %in% small.ing.100] <- "oth"
sort(table(seedwt.data$activeIngClean))
# Class
sort(table(seedwt.data$classClean))
small.class.100 <- c("herbicide", "thiophanate")
seedwt.data$classClean[seedwt.data$classClean %in% small.class.100] <- "other"
sort(table(seedwt.data$classClean))

#' ### 6. Calculate effect sizes
#' 
#' Overall means (raw mean difference)
#+ effectSizeMD
rust.data.MD <- escalc(measure = "MD", m1i = m1i, m2i = m2i, 
                    sd1i = sd1i, sd2i = sd2i, n1i = n1i, n2i = n2i,
                    data = rust.data)
yield.data.MD <- escalc(measure = "MD", m1i = m1i, m2i = m2i, 
                     sd1i = sd1i, sd2i = sd2i, n1i = n1i, n2i = n2i,
                     data = yield.data)
seedwt.data.MD <- escalc(measure = "MD", m1i = m1i, m2i = m2i, 
                      sd1i = sd1i, sd2i = sd2i, n1i = n1i, n2i = n2i,
                      data = seedwt.data)
cerco.data.MD <- escalc(measure = "MD", m1i = m1i, m2i = m2i, 
                              sd1i = sd1i, sd2i = sd2i, n1i = n1i, n2i = n2i,
                              data = cerco.data)
target.spot.data.MD <- escalc(measure = "MD", m1i = m1i, m2i = m2i, 
                              sd1i = sd1i, sd2i = sd2i, n1i = n1i, n2i = n2i,
                              data = target.spot.data)
#' Standardized mean difference
#+ effectSizeStd
rust.data.SMD <- escalc(measure = "SMD", m1i = m1i, m2i = m2i, 
              sd1i = sd1i, sd2i = sd2i, n1i = n1i, n2i = n2i,
              data = rust.data)
yield.data.SMD <- escalc(measure = "SMD", m1i = m1i, m2i = m2i, 
                     sd1i = sd1i, sd2i = sd2i, n1i = n1i, n2i = n2i,
                     data = yield.data)
seedwt.data.SMD <- escalc(measure = "SMD", m1i = m1i, m2i = m2i, 
                             sd1i = sd1i, sd2i = sd2i, n1i = n1i, n2i = n2i,
                             data = seedwt.data)

#' ### 7. Save files for analysis
#+ save
save(rust.data.SMD, yield.data.SMD, seedwt.data.SMD,
     rust.data.MD, yield.data.MD, seedwt.data.MD,
     cerco.data.MD, target.spot.data.MD,
     file="data/output_data/data_cleaned.R")

#' ### Footer
#' 
#' Spun with ezspin("programs/data_processing.R", out_dir="output", fig_dir="figures", keep_md=FALSE)
#' 
#' Session Info:
#+ sessionInfo
sessionInfo()
#' # Data analysis using bootstrapping
#' 
#' Rust severity as dependent variable
#' 
#' ### Preamble
#' 
#' Load Libraries
#+ libraries, messages=FALSE, warning=FALSE
library(ezknitr)
library(knitr)
library(metafor)

#' Clear environment
#+ clear
remove(list=ls())

#' Document settings
#+ settings
opts_chunk$set(fig.width = 6, fig.height = 4)

#' ### Load data
#+ loadData
load(file="data/output_data/data_cleaned.R")
remove(list=c(#"rust.data.ROM",
              "cerco.data.ROM",
              "seedwt.data.ROM",
              "target.spot.data.ROM",
              "yield.data.ROM"
              ))

#' Number of bootstrap simulations: 
nsims <- 5

#' Empty data frame to hold results
results.rust <- as.data.frame(matrix(NA,ncol=14,nrow=nsims))
colnames(results.rust) <- c("OVERALL",
                            "DUAL","FLUT","MIXED","PYR","TEBU",
                            "Strobilurin","Triaz_Strob","Triazole",
                            "category_rstage1+", "category_rstage2+", 
                            "category_rstage3","category_rstage4",
                            "category_rstage5")

#' Running the bootstraps
for(ii in 1:nsims){
  studyIDS <- data.frame(ReferenceNumb = sample(rust.data.ROM$ReferenceNumb, 
                                                length(unique(rust.data.ROM$ReferenceNumb)),
                                                replace=T))
  newdata <- merge(studyIDS, rust.data.ROM)
  # Overall analysis
  meta <- rma.uni(yi = yi,
                  vi = (n1i + n2i)/(n1i*n2i),
                  data = newdata,
                  method = "ML")
  results.rust$OVERALL[ii] <- meta$b
  
  # Active Ingredients
  ai <- rma.uni(yi = yi,
                vi = (n1i + n2i)/(n1i*n2i),
                data = newdata,
                method = "REML",
                mods = ~category_ai-1)
  results.rust[ii,2:6] <- ai$b
  
  # Classes
  class <- rma.uni(yi = yi,
                   vi = (n1i + n2i)/(n1i*n2i),
                   data = newdata,
                   method = "REML",
                   mods = ~category_class-1)
  results.rust[ii,7:9] <- class$b
  
  # Growth stage
  rstage <- rma.uni(yi = yi,
                    vi = (n1i + n2i)/(n1i*n2i),
                    data = newdata,
                    method = "REML",
                    mods = ~category_rstage-1)
  results.rust$`category_rstage1+`[ii] <- rstage$b[rownames(rstage$b)=="category_rstage1+"]
  results.rust$`category_rstage2+`[ii] <- rstage$b[rownames(rstage$b)=="category_rstage2+"]
  results.rust$`category_rstage3`[ii] <- rstage$b[rownames(rstage$b)=="category_rstage3"]
  results.rust$`category_rstage4`[ii] <- rstage$b[rownames(rstage$b)=="category_rstage4"]
  results.rust$`category_rstage5`[ii] <- rstage$b[rownames(rstage$b)=="category_rstage5"]
  
}

class <- rma.uni(yi = yi,
                 vi = (n1i + n2i)/(n1i*n2i),
                 data = newdata,
                 method = "REML",
                 mods = ~category_rstage-1)

#' Active ingredients

test 

#' ### Footer
#' 
#' Spun with ezspin("programs/data_analysis.R", out_dir="output", fig_dir="figures", keep_md=FALSE)
#' 
#' Session Info:
#+ sessionInfo
sessionInfo()
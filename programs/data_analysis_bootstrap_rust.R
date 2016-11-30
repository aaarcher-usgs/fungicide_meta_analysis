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
library(devtools)

#' Clear environment and set seed
#+ clear
remove(list=ls())
set.seed(18837)

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
                            "FLUT","MIXED","PYR","TEBU",
                            "Strobilurin","Triaz_Strob","Triazole",
                            "R1+", "R2+", 
                            "R3","R5",
                            "Application Intrcpt","Application Slope")

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
  results.rust$FLUT[ii] <- ai$b[rownames(ai$b)=="category_aiFLUT"]
  results.rust$MIXED[ii] <- ai$b[rownames(ai$b)=="category_aiMIXED"]
  results.rust$PYR[ii] <- ai$b[rownames(ai$b)=="category_aiPYR"]
  results.rust$TEBU[ii] <- ai$b[rownames(ai$b)=="category_aiTEBU"]
  
  # Classes
  class <- rma.uni(yi = yi,
                   vi = (n1i + n2i)/(n1i*n2i),
                   data = newdata,
                   method = "REML",
                   mods = ~category_class-1)
  results.rust$Strobilurin[ii] <- 
    class$b[rownames(class$b)=="category_classstrobilurin"]
  results.rust$Triaz_Strob[ii] <- 
    class$b[rownames(class$b)=="category_classtriaz + strob"]
  results.rust$Triazole[ii] <- 
    class$b[rownames(class$b)=="category_classtriazole"]
  
  # Growth stage
  rstage <- rma.uni(yi = yi,
                    vi = (n1i + n2i)/(n1i*n2i),
                    data = newdata,
                    method = "REML",
                    mods = ~category_rstage-1)
  results.rust$`R1+`[ii] <- 
    rstage$b[rownames(rstage$b)=="category_rstage1+"]
  results.rust$`R2+`[ii] <- 
    rstage$b[rownames(rstage$b)=="category_rstage2+"]
  results.rust$R3[ii] <- 
    rstage$b[rownames(rstage$b)=="category_rstage3"]
  results.rust$R5[ii] <- 
    rstage$b[rownames(rstage$b)=="category_rstage5"]
  
  # Number of applications
  applications <- rma.uni(yi = yi,
                          vi = (n1i + n2i)/(n1i*n2i),
                          data = newdata,
                          method = "REML",
                          mods = ~number_applications)
  results.rust$`Application Intrcpt`[ii] <- 
    applications$b[rownames(applications$b)=="intrcpt"]
  results.rust$`Application Slope`[ii] <- 
    applications$b[rownames(applications$b)=="number_applications"]
}

class <- rma.uni(yi = yi,
                 vi = (n1i + n2i)/(n1i*n2i),
                 data = newdata,
                 method = "REML",
                 mods = ~category_rstage-1)


#' ### Footer
#' 
#' Spun with ezspin("programs/data_analysis_bootstrap_rust.R", out_dir="output", fig_dir="figures", keep_md=FALSE)
#' 
#' Session Info:
#+ sessionInfo
devtools::session_info()
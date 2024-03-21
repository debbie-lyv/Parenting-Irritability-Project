################################################################################
pkgs <- c("readxl", "huge", "tidyverse", "rstatix", "ggpubr", "ggstatsplot", 
          "ggplot2", "bootnet", "qgraph","dplyr", "reshape2", "ggplot2",
          "devtools", "psych", "graphicalVAR", "psychonetrics", 
          "optimr", "semPlot", "xtable", "tidyr", "ltm",
          "lavaan", "MplusAutomation", "glue", "here", "multcomp", 
          "report","ggsci", "writexl", "NetworkToolbox", "NetworkComparisonTest")
# Install CRAN packages (if not already installed)
inst <- pkgs %in% installed.packages()
if(length(pkgs[!inst]) > 0) 
  install.packages(pkgs[!inst], 
                   dependencies = TRUE, 
                   repos = 'http://cran.us.r-project.org')
# Load packages into session 
lapply(pkgs, require, character.only=TRUE)
# clear variable namespace
rm(pkgs, inst)

################################################################################

### create permutation function

permfunc <- function(iter, dat, nobs.per.person, group.per.person) {
  
  
  ### reshuffle group variable between persons
  dat$group <- rep(sample(group.per.person), times=nobs.per.person)
  
  # recreate 3 groups based on shuffled labels
  black <- subset(dat, group == "black")
  white <- subset(dat, group == "white")
  hisp <- subset(dat, group == "hisp")
  
  ### fit model to data with reshuffled data

  res1.perm <- panelgvar(black, 
                    vars = varMat, # The design matrix, with a row indicating a variable and a column a wave of measurements. Note that NA indicates missing variables # Measurement model
                    estimator = "ULS",
                    verbose = TRUE,
                    storedata = TRUE)
  res2.perm <- panelgvar(white, 
                    vars = varMat, # The design matrix, with a row indicating a variable and a column a wave of measurements. Note that NA indicates missing variables # Measurement model
                    estimator = "ULS",
                    verbose = TRUE,
                    storedata = TRUE)
  res3.perm <- panelgvar(hisp, 
                    vars = varMat, # The design matrix, with a row indicating a variable and a column a wave of measurements. Note that NA indicates missing variables # Measurement model
                    estimator = "ULS",
                    verbose = TRUE,
                    storedata = TRUE)
  
  res1.perm <- res1.perm %>% runmodel %>% getmatrix("PDC")
  print(res1.perm)
  res2.perm <- res2.perm %>% runmodel %>% getmatrix("PDC")
  print(res2.perm)
  res3.perm <- res3.perm %>% runmodel %>% getmatrix("PDC")
  print(res3.perm)
  
  # calculate difference between each pair of groups
  return(c(res1.perm - res2.perm, res1.perm - res3.perm, res2.perm - res3.perm))
  
}

### add a group variable and a subj variable to each group
wide1_detrended2 <- wide1_detrended %>% mutate(group = "black") %>% mutate(subjno = 1:1167) ############### added 1 person (total is 2408 not 2407)
wide2_detrended2 <- wide2_detrended %>% mutate(group = "white") %>% mutate(subjno = 1168:1781)
wide3_detrended2 <- wide3_detrended %>% mutate(group = "hisp") %>% mutate(subjno = 1782:2408)

# merge them together (so that we can shuffle the group label in the perm function)
dat <- wide1_detrended2 %>% full_join(wide2_detrended2)
dat <- dat %>% full_join(wide3_detrended2)

### make sure data are ordered by subjno
dat <- dat[order(dat$subjno),]



### number of permutation iterations
perms <- 10000


### fit model with actual data in each group
varMat <- matrix(
  c("cbcl_3", "agg_3", "NVdisc_3", "psychagg_3", "assault_3", "negli_3",
    "cbcl_4", "agg_4", "NVdisc_4", "psychagg_4", "assault_4", "negli_4",
    "cbcl_5", "agg_5", "NVdisc_5", "psychagg_5", "assault_5", "negli_5"),
  ncol = 3,
  byrow = F
)

res1 <- panelgvar(wide1_detrended, 
                    vars = varMat, # The design matrix, with a row indicating a variable and a column a wave of measurements. Note that NA indicates missing variables # Measurement model
                    estimator = "ULS",
                    verbose = TRUE,
                    storedata = TRUE)
res2 <- panelgvar(wide2_detrended, 
                    vars = varMat, # The design matrix, with a row indicating a variable and a column a wave of measurements. Note that NA indicates missing variables # Measurement model
                    estimator = "ULS",
                    verbose = TRUE,
                    storedata = TRUE)
res3 <- panelgvar(wide3_detrended, 
                    vars = varMat, # The design matrix, with a row indicating a variable and a column a wave of measurements. Note that NA indicates missing variables # Measurement model
                    estimator = "ULS",
                    verbose = TRUE,
                    storedata = TRUE)

res1 <- res1 %>% runmodel %>% getmatrix("PDC")
res2 <- res2 %>% runmodel %>% getmatrix("PDC")
res3 <- res3 %>% runmodel %>% getmatrix("PDC")



### number of observations per person
nobs.per.person  <- sapply(split(dat$group, dat$subjno), length)
  
### group of each person
group.per.person <- sapply(split(dat$group, dat$subjno), function(x) x[1])
  
### repeatedly apply permfunc() function
permres <- lapply(1:perms, permfunc, dat=dat, nobs.per.person=nobs.per.person, group.per.person=group.per.person)
  
### turn results into a matrix
permres <- do.call(rbind, permres)
  
  
  
### table with model-based and permutation based p-values (two definitions of the p-values)
b.diff.obs <- c(res1 - res2, res1 - res3, res2 - res3)
names(b.diff.obs) <- paste0(rep(c("grp1_vs_grp2_", "grp1_vs_grp3_", "grp2_vs_grp3_"), each=length(b.diff.obs)/3), names(b.diff.obs))
p.perm.def1 <- p.perm.def2 <- rep(NA, length(b.diff.obs))

for (j in 1:length(b.diff.obs)) {
  p.perm.def1[j] <- 2*min(mean(permres[,j] >= b.diff.obs[j], na.rm=TRUE), mean(permres[,j] <= b.diff.obs[j], na.rm=TRUE))
  p.perm.def2[j] <- min(1, 2*ifelse(b.diff.obs[j] > 0, mean(permres[,j] >= b.diff.obs[j], na.rm=TRUE), mean(permres[,j] <= b.diff.obs[j], na.rm=TRUE)))
  }

sav <- round(cbind(b.diff.obs, "p-perm.def1"=p.perm.def1, "p-perm.def2"=p.perm.def2), 4)
write.table(sav, file="sign_path.txt", sep="\t")
  
  

      
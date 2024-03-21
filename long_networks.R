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

data <- read_xlsx("avg_data.xlsx")
data <- as.data.frame(data)
set.seed(1)

######################### PANEL-LVGAR MODEL 1 #################################

summary(data)
white <- subset(data, MRaceT1 == 1)
black <- subset(data, MRaceT1 == 2)
hispanic <- subset(data, MRaceT1 == 3)
summary(white)
summary(black)
summary(hispanic)

################# BLACK PARTICIPANTS

############ prepare data for longitudinal analysis

wide1 <- black[, c(1, 5:22)]
colnames(wide1) <- c("id", "cbcl_3", "agg_3", "NVdisc_3", "psychagg_3", "assault_3", "negli_3",
                     "cbcl_4", "agg_4", "NVdisc_4", "psychagg_4", "assault_4", "negli_4",
                     "cbcl_5", "agg_5", "NVdisc_5", "psychagg_5", "assault_5", "negli_5")
str(wide1)

long1 <- reshape(wide1, varying = 2:19, idvar = "id", 
                 direction = "long", sep = "_")

colnames(long1)

# Detrending significant linear trends:
vars1 <- c("cbcl", "agg", "NVdisc", "psychagg", "assault", "negli")
for (v in seq_along(vars1)){
  ff <- as.formula(paste0(vars1[[v]]," ~ time"))
  fit <- lm(ff, data = long1)
  if (anova(fit)$P[1] < 0.05){
    message(paste("Detrending variable",v))
    long1[[vars1[v]]][!is.na(long1[[vars1[[v]]]])] <- residuals(fit)
  }
}
#variables were detrended
wide1_detrended <- reshape(long1, idvar = "id", timevar = "time",
                           direction = "wide", sep = "_")


# create a covariance matrix

varMat <- matrix(
  c("cbcl_3", "agg_3", "NVdisc_3", "psychagg_3", "assault_3", "negli_3",
    "cbcl_4", "agg_4", "NVdisc_4", "psychagg_4", "assault_4", "negli_4",
    "cbcl_5", "agg_5", "NVdisc_5", "psychagg_5", "assault_5", "negli_5"),
  ncol = 3,
  byrow = F
)

wide1_detrended <- subset(wide1_detrended, select = -c(id))



Q <- cov(wide1_detrended, use = "pairwise.complete.obs")
print(Q)
eigen(Q)$values

# Form model:
model <- panelgvar(wide1_detrended, 
                   vars = varMat, # The design matrix, with a row indicating a variable and a column a wave of measurements. Note that NA indicates missing variables # Measurement model
                   estimator = "ULS",
                   verbose = TRUE,
                   storedata = TRUE)

model_black <- model %>% runmodel


# Check fit:
model_black %>% fit #good fit: cfi=0.95, tli=0.92, rmsea=0.049
model_black %>% parameters
model_black %>% print
model_black %>% MIs

model_prune_1 <- model %>% runmodel %>% modelsearch(addalpha = 0.05, prunealpha = 0.05) #stepwise
# No more model found to improve fit. Returning optimal model.

model_prune_1 %>% fit


# extract temporal network
temporal_black <- model_black %>% getmatrix("PDC")
contemporaneous_ <- model_black %>% getmatrix("omega_zeta_within")
between <- model_black %>% getmatrix("omega_zeta_between")


## Plot graph:
latLabels <-  c("irr", "pStr", "nVio", "psyAgg", "phyAs", "neg")

qgraph(temporal_black, layout = "groups", edge.labels = TRUE, labels = latLabels, theme = "colorblind",
       directed = TRUE, cut = 0.05, title = "black participants", edge.label.cex = 1.15, label.prop = 0.9)


# depending on number of vars & settings, this may take quite long
set.seed(1)
Bootstraps <- lapply(1:1000, function(x) {
  bootstrapped_model <-
    panelgvar(
      wide1_detrended[sample(1:nrow(wide1_detrended),
                             nrow(wide1_detrended),
                             TRUE), ],
      vars = varMat,
      estimator = "ULS",
      verbose = TRUE,
      storedata = TRUE
    )  %>%
    runmodel %>%
    return(bootstrapped_model)
})


# bootstrapped results: temporal
# check if individual edges are included (y/n)
resBoots_temp <-
  lapply(Bootstraps, function(x)
    ifelse(getmatrix(x, "PDC") > 0, 1, 0))

# how often (%) is edge included, over all bootstrap iterations
bootstraps_black <- apply(simplify2array(resBoots_temp), 1:2, mean) 
print(bootstraps_black)
print(temporal_black)


## only including bootstrapped edges in model 

write.csv(temporal_black, "pdc_black.csv")
temporal_black_boot <- read.csv("pdc_black_bootstrapped.csv", head = F)

qgraph(temporal_black_boot, layout = "groups", edge.labels = TRUE, labels = latLabels, theme = "colorblind",
       directed = TRUE, cut = 0.05, title = "black participants", edge.label.cex = 1.15, label.prop = 0.9)



################# WHITE PARTICIPANTS

############ prepare data for longitudinal analysis

wide2 <- white[, c(1, 5:22)]
colnames(wide2) <- c("id", "cbcl_3", "agg_3", "NVdisc_3", "psychagg_3", "assault_3", "negli_3",
                     "cbcl_4", "agg_4", "NVdisc_4", "psychagg_4", "assault_4", "negli_4",
                     "cbcl_5", "agg_5", "NVdisc_5", "psychagg_5", "assault_5", "negli_5")
str(wide1)

long2 <- reshape(wide2, varying = 2:19, idvar = "id", 
                 direction = "long", sep = "_")

colnames(long2)

# Detrending significant linear trends:
vars1 <- c("cbcl", "agg", "NVdisc", "psychagg", "assault", "negli")
for (v in seq_along(vars1)){
  ff <- as.formula(paste0(vars1[[v]]," ~ time"))
  fit <- lm(ff, data = long2)
  if (anova(fit)$P[1] < 0.05){
    message(paste("Detrending variable",v))
    long2[[vars1[v]]][!is.na(long2[[vars1[[v]]]])] <- residuals(fit)
  }
}
#variables were detrended
wide2_detrended <- reshape(long2, idvar = "id", timevar = "time",
                           direction = "wide", sep = "_")


# create a covariance matrix

varMat <- matrix(
  c("cbcl_3", "agg_3", "NVdisc_3", "psychagg_3", "assault_3", "negli_3",
    "cbcl_4", "agg_4", "NVdisc_4", "psychagg_4", "assault_4", "negli_4",
    "cbcl_5", "agg_5", "NVdisc_5", "psychagg_5", "assault_5", "negli_5"),
  ncol = 3,
  byrow = F
)

wide2_detrended <- subset(wide2_detrended, select = -c(id))


Q2 <- cov(wide2_detrended, use = "pairwise.complete.obs")
print(Q2)
eigen(Q2)$values

# Form model:
model <- panelgvar(wide2_detrended, 
                   vars = varMat, # The design matrix, with a row indicating a variable and a column a wave of measurements. Note that NA indicates missing variables # Measurement model
                   estimator = "ULS",
                   verbose = TRUE,
                   storedata = TRUE)

model_white <- model %>% runmodel


# Check fit:
model_white %>% fit #good fit: cfi=0.98, tli=0.98, rmsea=0.024
model_white %>% parameters
model_white %>% print
model_white %>% MIs

# model_prune_1 <- model %>% runmodel %>% prune(alpha = 0.01) %>% modelsearch(criterion = "bic") #stepwise
# No more model found to improve fit. Returning optimal model.

model_prune_2 <- model %>% runmodel %>% modelsearch(addalpha = 0.05, prunealpha = 0.05) #stepwise
# No more model found to improve fit. Returning optimal model.

model_prune_2 %>% fit



# extract temporal network
temporal_white <- model_white %>% getmatrix("PDC")
contemporaneous_white <- model_white %>% getmatrix("omega_zeta_within")
between_white <- model_white %>% getmatrix("omega_zeta_between")


## Plot graph:
qgraph(temporal_white, layout = "groups", edge.labels = TRUE, labels = latLabels, theme = "colorblind",
       directed = TRUE, cut = 0.05, title = "white participants", edge.label.cex = 1.15, label.prop = 0.9)



# depending on number of vars & settings, this may take quite long
set.seed(1)
Bootstraps_w <- lapply(1:1000, function(x) {
  bootstrapped_model_w <-
    panelgvar(
      wide2_detrended[sample(1:nrow(wide2_detrended),
                             nrow(wide2_detrended),
                             TRUE), ],
      vars = varMat,
      estimator = "ULS",
      verbose = TRUE,
      storedata = TRUE
    )  %>%
    runmodel %>%
    return(bootstrapped_model_w)
})


# bootstrapped results: temporal
# check if individual edges are included (y/n)
resBoots_temp_w <-
  lapply(Bootstraps_w, function(x)
    ifelse(getmatrix(x, "PDC") > 0, 1, 0))

# how often (%) is edge included, over all bootstrap iterations
bootstraps_white <- apply(simplify2array(resBoots_temp_w), 1:2, mean) 

print(bootstraps_white)
print(temporal_white)


## only including bootstrapped edges in model 

write.csv(temporal_white, "pdc_white.csv")
temporal_white_boot <- read.csv("pdc_white_bootstrapped.csv", head = F)

qgraph(temporal_white_boot, layout = "groups", edge.labels = TRUE, labels = latLabels, theme = "colorblind",
       directed = TRUE, cut = 0.05, title = "white participants", edge.label.cex = 1.15, label.prop = 0.9)




################# HISPANIC PARTICIPANTS

############ prepare data for longitudinal analysis

wide3 <- hispanic[, c(1, 5:22)]
colnames(wide3) <- c("id", "cbcl_3", "agg_3", "NVdisc_3", "psychagg_3", "assault_3", "negli_3",
                     "cbcl_4", "agg_4", "NVdisc_4", "psychagg_4", "assault_4", "negli_4",
                     "cbcl_5", "agg_5", "NVdisc_5", "psychagg_5", "assault_5", "negli_5")

# wide3 <- subset(wide3, select = -c(NVdisc_3, NVdisc_4, NVdisc_5))
# colnames(wide3)
str(wide3)

long3 <- reshape(wide3, varying = 2:19, idvar = "id", 
                 direction = "long", sep = "_")

colnames(long3)

# Detrending significant linear trends:
vars1 <- c("cbcl", "agg","NVdisc", "psychagg", "assault", "negli")
for (v in seq_along(vars1)){
  ff <- as.formula(paste0(vars1[[v]]," ~ time"))
  fit <- lm(ff, data = long3)
  if (anova(fit)$P[1] < 0.05){
    message(paste("Detrending variable",v))
    long3[[vars1[v]]][!is.na(long3[[vars1[[v]]]])] <- residuals(fit)
  }
}
#variables were detrended
wide3_detrended <- reshape(long3, idvar = "id", timevar = "time",
                           direction = "wide", sep = "_")

# create a covariance matrix
varMat <- matrix(
  c("cbcl_3", "agg_3", "NVdisc_3", "psychagg_3", "assault_3", "negli_3",
    "cbcl_4", "agg_4", "NVdisc_4", "psychagg_4", "assault_4", "negli_4",
    "cbcl_5", "agg_5", "NVdisc_5", "psychagg_5", "assault_5", "negli_5"),
  ncol = 3,
  byrow = F
)

wide3_detrended <- subset(wide3_detrended, select = -c(id))


Q3 <- cov(wide3_detrended, use = "pairwise.complete.obs")
print(Q3)
eigen(Q3)$values

# Form model:
model <- panelgvar(wide3_detrended, 
                   vars = varMat, # The design matrix, with a row indicating a variable and a column a wave of measurements. Note that NA indicates missing variables # Measurement model
                   estimator = "ULS",
                   verbose = TRUE,
                   storedata = TRUE)

model_hispanic <- model %>% runmodel


# Check fit:
model_hispanic %>% fit #good fit: cfi=0.97, tli=0.96, rmsea=0.031
model_hispanic %>% parameters
model_hispanic %>% print
model_hispanic %>% MIs

# model_prune_3 <- model %>% runmodel %>% prune(alpha = 0.01) %>% modelsearch(criterion = "bic") #stepwise
# No more model found to improve fit. Returning optimal model.

model_prune_3 <- model %>% runmodel %>% modelsearch(addalpha = 0.05, prunealpha = 0.05) #stepwise
# No more model found to improve fit. Returning optimal model.


model_prune_3 %>% fit



# extract temporal network
temporal_hispanic <- model_hispanic %>% getmatrix("PDC")
contemporaneous_hispanic <- model_hispanic %>% getmatrix("omega_zeta_within")
between_hispanic <- model_hispanic %>% getmatrix("omega_zeta_between")


## Plot graph:
qgraph(temporal_hispanic, layout = "groups", edge.labels = TRUE, labels = latLabels, theme = "colorblind",
       directed = TRUE, cut = 0.05, title = "hispanic participants", edge.label.cex = 1.15, label.prop = 0.9)


# depending on number of vars & settings, this may take quite long
set.seed(1)
Bootstraps_h <- lapply(1:1000, function(x) {
  bootstrapped_model_h <-
    panelgvar(
      wide3_detrended[sample(1:nrow(wide3_detrended),
                             nrow(wide3_detrended),
                             TRUE), ],
      vars = varMat,
      estimator = "ULS",
      verbose = TRUE,
      storedata = TRUE
    )  %>%
    runmodel %>%
    return(bootstrapped_model_h)
})


# bootstrapped results: temporal
# check if individual edges are included (y/n)
resBoots_temp_h <-
  lapply(Bootstraps_h, function(x)
    ifelse(getmatrix(x, "PDC") > 0, 1, 0))

# how often (%) is edge included, over all bootstrap iterations
bootstraps_hispanic <- apply(simplify2array(resBoots_temp_h), 1:2, mean) 


print(bootstraps_hispanic)
print(temporal_hispanic)


## only including bootstrapped edges in model 

write.csv(temporal_hispanic, "pdc_hispanic.csv")
temporal_hispanic_boot <- read.csv("pdc_hispanic_bootstrapped.csv", head = F)

qgraph(temporal_hispanic_boot, layout = "groups", edge.labels = TRUE, labels = latLabels, theme = "colorblind",
       directed = TRUE, cut = 0.05, title = "hispanic participants", edge.label.cex = 1.15, label.prop = 0.9)


################################################################################

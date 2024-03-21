################################################################################
pkgs <- c("readxl", "huge", "tidyverse", "rstatix", "ggpubr", "ggstatsplot", 
          "ggplot2", "bootnet", "qgraph","dplyr", "reshape2", "ggplot2",
          "devtools", "psych", "graphicalVAR", "psychonetrics", 
          "optimr", "semPlot", "xtable", "tidyr", "ltm",
          "lavaan", "MplusAutomation", "glue", "here", "multcomp", 
          "report","ggsci", "writexl", "NetworkToolbox", "NetworkComparisonTest", "apaTables")
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

setwd("C:\\Users\\ldebb\\Documents\\YAYLAB\\PUBLICATION")

# Read in data
data <- read_xlsx("dataset.xlsx")

# Replacing values with NA
data[data==-9] <- NA
data[data==-8] <- NA
data[data==-7] <- NA
data[data==-6] <- NA
data[data==-5] <- NA
data[data==-4] <- NA
data[data==-3] <- NA
data[data==-2] <- NA
data[data==-1] <- NA

summary(data)


colnames(data) <- c(  "id", "cm1age", "cm1bsex", "cm1relf", "cm1edu", "cm1hhinc"
                      , "cp3pcgrel", "ch4pcgrel", "cp5pcgrel"
                      , "MRaceT1" , "MCulAttT2"  ,  "MCulPraT2"  
                      , "MAgP1T3" , "MAgP2T3" , "MAgP3T3" , "MAgP4T3"
                      , "MAgP1T4" , "MAgP2T4" , "MAgP3T4" , "MAgP4T4"      
                      , "PAgP1T5" , "PAgP2T5" , "PAgP3T5" , "PAgP4T5"   
                      , "PCBCLStubT3"  , "PCBCLMoodT3" ,"PCBCLTempT3"                      
                      , "MCBCLStubT4" , "MCBCLMoodT4" ,"MCBCLTempT4"
                      , "MCBCLStubT5"  , "MCBCLMoodT5" , "MCBCLTempT5"
                      , "PConfNVio1T3" ,"PConfAgg4T3",   "PConfPhyAs4T3", "PConfNVio4T3",  "PConfPhyAs5T3"
                      , "PConfAgg5T3" ,"PConfNeg1T3" ,  "PConfNeg2T3",   "PConfNeg3T3"  , "PConfNeg4T3"
                      , "PConfNeg5T3" ,"PConfNVio2T3" , "PConfPhyAs1T3", "PConfPhyAs2T3", "PConfNVio3T3" 
                      , "PConfAgg1T3" , "PConfPhyAs3T3", "PConfAgg2T3",   "PConfAgg3T3" 
                      , "PConfNVio1T4" , "PConfAgg4T4",   "PConfPhyAs4T4", "PConfNVio4T4" 
                      , "PConfPhyAs5T4", "PConfAgg5T4"  , "PConfNeg1T4" ,  "PConfNeg2T4",   "PConfNeg3T4"  
                      , "PConfNeg4T4" , "PConfNeg5T4" ,  "PConfNVio2T4" , "PConfPhyAs1T4", "PConfPhyAs2T4"
                      , "PConfNVio3T4",  "PConfAgg1T4" ,  "PConfPhyAs3T4", "PConfAgg2T4" ,  "PConfAgg3T4"  
                      , "PConfNVio1T5" , "PConfNVio2T5" , "PConfPhyAs1T5", "PConfPhyAs2T5", "PConfNVio3T5" 
                      , "PConfAgg1T5" ,  "PConfPhyAs3T5", "PConfAgg2T5" ,  "PConfAgg3T5",   "PConfAgg4T5"
                      , "PConfPhyAs4T5" , "PConfNVio4T5" , "PConfPhyAs5T5", "PConfAgg5T5" ,  "PConfNeg1T5"  
                      , "PConfNeg2T5" , "PConfNeg3T5"   ,"PConfNeg4T5" ,  "PConfNeg5T5" 
                      , "white", "black", "hispanic"
)


####### recode Age 9 irritability scores from 1-3 to 0-2

data$MCBCLStubT5 <- car::recode(data$MCBCLStubT5, "1=0; 2=1; 3=2")
data$MCBCLMoodT5 <- car::recode(data$MCBCLMoodT5, "1=0; 2=1; 3=2")
data$MCBCLTempT5 <- car::recode(data$MCBCLTempT5, "1=0; 2=1; 3=2")

###### only include participants if they have at least 2 of 3 timepoints

CBCLAVG_3 <- rowMeans(data[,c("PCBCLStubT3" , "PCBCLMoodT3" , "PCBCLTempT3")], na.rm = T)
data <- cbind(data, CBCLAVG_3)
CBCLAVG_4 <- rowMeans(data[,c("MCBCLStubT4" , "MCBCLMoodT4" , "MCBCLTempT4")], na.rm = T)
data <- cbind(data, CBCLAVG_4)
CBCLAVG_5 <- rowMeans(data[,c("MCBCLStubT5" , "MCBCLMoodT5" , "MCBCLTempT5")], na.rm = T)
data <- cbind(data, CBCLAVG_5)

countmissing_irr <- rowSums(is.na(data[, c("CBCLAVG_3", "CBCLAVG_4", "CBCLAVG_5")]))
data <- cbind(data, countmissing_irr)

table(countmissing_irr)

data <- subset(data, countmissing_irr < 2)
data <- subset(data, select = -c(countmissing_irr)) 


####### remove scores of 7 and 8 from conflict tactics scale

vals_to_repl <- c(7,8)
cols_to_repl <- c("PConfNVio1T3" ,"PConfAgg4T3",   "PConfPhyAs4T3", "PConfNVio4T3",  "PConfPhyAs5T3"
                  , "PConfAgg5T3" ,"PConfNeg1T3" ,  "PConfNeg2T3",   "PConfNeg3T3"  , "PConfNeg4T3"
                  , "PConfNeg5T3" ,"PConfNVio2T3" , "PConfPhyAs1T3", "PConfPhyAs2T3", "PConfNVio3T3" 
                  , "PConfAgg1T3" , "PConfPhyAs3T3", "PConfAgg2T3",   "PConfAgg3T3" 
                  , "PConfNVio1T4" , "PConfAgg4T4",   "PConfPhyAs4T4", "PConfNVio4T4" 
                  , "PConfPhyAs5T4", "PConfAgg5T4"  , "PConfNeg1T4" ,  "PConfNeg2T4",   "PConfNeg3T4"  
                  , "PConfNeg4T4" , "PConfNeg5T4" ,  "PConfNVio2T4" , "PConfPhyAs1T4", "PConfPhyAs2T4"
                  , "PConfNVio3T4",  "PConfAgg1T4" ,  "PConfPhyAs3T4", "PConfAgg2T4" ,  "PConfAgg3T4"  
                  , "PConfNVio1T5" , "PConfNVio2T5" , "PConfPhyAs1T5", "PConfPhyAs2T5", "PConfNVio3T5" 
                  , "PConfAgg1T5" ,  "PConfPhyAs3T5", "PConfAgg2T5" ,  "PConfAgg3T5",   "PConfAgg4T5"
                  , "PConfPhyAs4T5" , "PConfNVio4T5" , "PConfPhyAs5T5", "PConfAgg5T5" ,  "PConfNeg1T5"  
                  , "PConfNeg2T5" , "PConfNeg3T5"   ,"PConfNeg4T5" ,  "PConfNeg5T5")

data[cols_to_repl] <- sapply(data[cols_to_repl],
                             function(x) replace(x, x %in% vals_to_repl, 0))

summary(data)


######## inverse aggravation scores so that higher score means more aggravation

data$MAgP1T3 <- car::recode(data$MAgP1T3, "1=4; 2=3; 3=2; 4=1")
data$MAgP2T3 <- car::recode(data$MAgP2T3, "1=4; 2=3; 3=2; 4=1")
data$MAgP3T3 <- car::recode(data$MAgP3T3, "1=4; 2=3; 3=2; 4=1")
data$MAgP4T3 <- car::recode(data$MAgP4T3, "1=4; 2=3; 3=2; 4=1")

data$MAgP1T4 <- car::recode(data$MAgP1T4, "1=4; 2=3; 3=2; 4=1")
data$MAgP2T4 <- car::recode(data$MAgP2T4, "1=4; 2=3; 3=2; 4=1")
data$MAgP3T4 <- car::recode(data$MAgP3T4, "1=4; 2=3; 3=2; 4=1")
data$MAgP4T4 <- car::recode(data$MAgP4T4, "1=4; 2=3; 3=2; 4=1")

data$PAgP1T5 <- car::recode(data$PAgP1T5, "1=4; 2=3; 3=2; 4=1")
data$PAgP2T5 <- car::recode(data$PAgP2T5, "1=4; 2=3; 3=2; 4=1")
data$PAgP3T5 <- car::recode(data$PAgP3T5, "1=4; 2=3; 3=2; 4=1")
data$PAgP4T5 <- car::recode(data$PAgP4T5, "1=4; 2=3; 3=2; 4=1")


# inverse non-volent discipline scores so that higher scores = done less often/ less non-violent discipline

data$PConfNVio1T3 <- car::recode(data$PConfNVio1T3, "0=6; 1=5; 2=4; 3=3; 4=2; 5=1; 6=0")
data$PConfNVio2T3 <- car::recode(data$PConfNVio2T3, "0=6; 1=5; 2=4; 3=3; 4=2; 5=1; 6=0")
data$PConfNVio3T3 <- car::recode(data$PConfNVio3T3, "0=6; 1=5; 2=4; 3=3; 4=2; 5=1; 6=0")
data$PConfNVio4T3 <- car::recode(data$PConfNVio4T3, "0=6; 1=5; 2=4; 3=3; 4=2; 5=1; 6=0")

data$PConfNVio1T4 <- car::recode(data$PConfNVio1T4, "0=6; 1=5; 2=4; 3=3; 4=2; 5=1; 6=0")
data$PConfNVio2T4 <- car::recode(data$PConfNVio2T4, "0=6; 1=5; 2=4; 3=3; 4=2; 5=1; 6=0")
data$PConfNVio3T4 <- car::recode(data$PConfNVio3T4, "0=6; 1=5; 2=4; 3=3; 4=2; 5=1; 6=0")
data$PConfNVio4T4 <- car::recode(data$PConfNVio4T4, "0=6; 1=5; 2=4; 3=3; 4=2; 5=1; 6=0")

data$PConfNVio1T5 <- car::recode(data$PConfNVio1T5, "0=6; 1=5; 2=4; 3=3; 4=2; 5=1; 6=0")
data$PConfNVio2T5 <- car::recode(data$PConfNVio2T5, "0=6; 1=5; 2=4; 3=3; 4=2; 5=1; 6=0")
data$PConfNVio3T5 <- car::recode(data$PConfNVio3T5, "0=6; 1=5; 2=4; 3=3; 4=2; 5=1; 6=0")
data$PConfNVio4T5 <- car::recode(data$PConfNVio4T5, "0=6; 1=5; 2=4; 3=3; 4=2; 5=1; 6=0")


# CFA for non-violent discipline scale

# T3
CONFLICT1 <- '
CONF =~ PConfNVio1T3 + PConfNVio2T3 + PConfNVio3T3 + PConfNVio4T3'
CONF_model1 <- cfa(CONFLICT1, data=data)
summary(CONF_model1, fit.measures=TRUE, standardized=TRUE)
modindices(CONF_model1, sort = TRUE, maximum.number = 5)

# T4
CONFLICT2 <- '
CONF =~ PConfNVio1T4 + PConfNVio2T4 + PConfNVio3T4 + PConfNVio4T4'
CONF_model2 <- cfa(CONFLICT2, data=data)
summary(CONF_model2, fit.measures=TRUE, standardized=TRUE)
modindices(CONF_model2, sort = TRUE, maximum.number = 5)

# T5
CONFLICT3 <- '
CONF =~ PConfNVio1T5 + PConfNVio2T5 + PConfNVio3T5 + PConfNVio4T5'
CONF_model3 <- cfa(CONFLICT3, data=data)
summary(CONF_model3, fit.measures=TRUE, standardized=TRUE)
modindices(CONF_model3, sort = TRUE, maximum.number = 5)


####################### Internal consistency ################

# Age 3 variables

data %>% 
  dplyr::select(PCBCLMoodT3, PCBCLTempT3, PCBCLStubT3) %>%
  cronbach.alpha(standardized = TRUE, na.rm = TRUE)

data %>% 
  dplyr::select(PConfAgg1T3, PConfAgg2T3, PConfAgg3T3, PConfAgg4T3, PConfAgg5T3) %>%
  cronbach.alpha(standardized = TRUE, na.rm = TRUE)

data %>% 
  dplyr::select(PConfNeg1T3, PConfNeg2T3, PConfNeg3T3, PConfNeg4T3, PConfNeg5T3) %>%
  cronbach.alpha(standardized = TRUE, na.rm = TRUE)

data %>% 
  dplyr::select(PConfPhyAs1T3, PConfPhyAs2T3, PConfPhyAs3T3, PConfPhyAs4T3, PConfPhyAs5T3) %>%
  cronbach.alpha(standardized = TRUE, na.rm = TRUE)

data %>% 
  dplyr::select(PConfNVio1T3, PConfNVio2T3, PConfNVio3T3, PConfNVio4T3) %>%
  cronbach.alpha(standardized = TRUE, na.rm = TRUE)

data %>% 
  dplyr::select(MAgP1T3, MAgP2T3, MAgP3T3, MAgP4T3) %>%
  cronbach.alpha(standardized = TRUE, na.rm = TRUE)

# Age 5 variables

data %>% 
  dplyr::select(MCBCLMoodT4, MCBCLTempT4, MCBCLStubT4) %>%
  cronbach.alpha(standardized = TRUE, na.rm = TRUE)

data %>% 
  dplyr::select(PConfAgg1T4, PConfAgg2T4, PConfAgg3T4, PConfAgg4T4, PConfAgg5T4) %>%
  cronbach.alpha(standardized = TRUE, na.rm = TRUE)

data %>% 
  dplyr::select(PConfNeg1T4, PConfNeg2T4, PConfNeg3T4, PConfNeg4T4, PConfNeg5T4) %>%
  cronbach.alpha(standardized = TRUE, na.rm = TRUE)

# very low cronbach's alpha - 0.38

NEG1 <- '
NEG =~ PConfNeg1T3 + PConfNeg2T3 + PConfNeg3T3 + PConfNeg4T3 + PConfNeg5T3'
NEG_model1 <- cfa(NEG1, data=data)
summary(NEG_model1, fit.measures=TRUE, standardized=TRUE)
modindices(NEG_model1, sort = TRUE, maximum.number = 5)

NEG2 <- '
NEG =~ PConfNeg1T4 + PConfNeg2T4 + PConfNeg3T4 + PConfNeg4T4 + PConfNeg5T4'
NEG_model2 <- cfa(NEG2, data=data)
summary(NEG_model2, fit.measures=TRUE, standardized=TRUE)
modindices(NEG_model2, sort = TRUE, maximum.number = 5)

NEG3 <- '
NEG =~ PConfNeg1T5 + PConfNeg2T5 + PConfNeg3T5 + PConfNeg4T5 + PConfNeg5T5'
NEG_model3 <- cfa(NEG3, data=data)
summary(NEG_model3, fit.measures=TRUE, standardized=TRUE)
modindices(NEG_model3, sort = TRUE, maximum.number = 5)

apa.cor.table(data[,c(  "PConfNeg1T3", "PConfNeg2T3", "PConfNeg3T3", "PConfNeg4T3", "PConfNeg5T3",
                        "PConfNeg1T4", "PConfNeg2T4", "PConfNeg3T4", "PConfNeg4T4", "PConfNeg5T4",
                        "PConfNeg1T5", "PConfNeg2T5", "PConfNeg3T5", "PConfNeg4T5", "PConfNeg5T5"
                        
                        
)], 
show.conf.interval = FALSE, "cor_matrix.doc")



data %>% 
  dplyr::select(PConfPhyAs1T4, PConfPhyAs2T4, PConfPhyAs3T4, PConfPhyAs4T4, PConfPhyAs5T4) %>%
  cronbach.alpha(standardized = TRUE, na.rm = TRUE)

data %>% 
  dplyr::select(PConfNVio1T4, PConfNVio2T4, PConfNVio3T4, PConfNVio4T4) %>%
  cronbach.alpha(standardized = TRUE, na.rm = TRUE)

data %>% 
  dplyr::select(MAgP1T4, MAgP2T4, MAgP3T4, MAgP4T4) %>%
  cronbach.alpha(standardized = TRUE, na.rm = TRUE)

# Age 9 variables

data %>% 
  dplyr::select(MCBCLMoodT5, MCBCLTempT5, MCBCLStubT5) %>%
  cronbach.alpha(standardized = TRUE, na.rm = TRUE)

data %>% 
  dplyr::select(PConfAgg1T5, PConfAgg2T5, PConfAgg3T5, PConfAgg4T5, PConfAgg5T5) %>%
  cronbach.alpha(standardized = TRUE, na.rm = TRUE)

data %>% 
  dplyr::select(PConfNeg1T5, PConfNeg2T5, PConfNeg3T5, PConfNeg4T5, PConfNeg5T5) %>%
  cronbach.alpha(standardized = TRUE, na.rm = TRUE)

data %>% 
  dplyr::select(PConfPhyAs1T5, PConfPhyAs2T5, PConfPhyAs3T5, PConfPhyAs4T5, PConfPhyAs5T5) %>%
  cronbach.alpha(standardized = TRUE, na.rm = TRUE)

data %>% 
  dplyr::select(PConfNVio1T5, PConfNVio2T5, PConfNVio3T5, PConfNVio4T5) %>%
  cronbach.alpha(standardized = TRUE, na.rm = TRUE)

data %>% 
  dplyr::select(PAgP1T5, PAgP2T5, PAgP3T5, PAgP4T5) %>%
  cronbach.alpha(standardized = TRUE, na.rm = TRUE)


##########################################################

# write new dataset of clean data

# data <- sapply(data, as.numeric)
# data <- as.data.frame(data)
write_xlsx(data, "clean_data.xlsx")


######## dataset with only avg variables ############
data <- read_xlsx("clean_data.xlsx")

## Creating average variables

AGGAVGT3 <- rowMeans(data[,c("MAgP1T3" , "MAgP2T3" , "MAgP3T3", "MAgP4T3")], na.rm = T)

CONF1AVGT3 <- rowMeans(data[,c("PConfNVio1T3" , "PConfNVio2T3" , "PConfNVio3T3", "PConfNVio4T3")], na.rm = T)

CONF2AVGT3 <-  rowMeans(data[,c("PConfAgg1T3" , "PConfAgg2T3" , "PConfAgg3T3", "PConfAgg4T3", "PConfAgg5T3")], na.rm = T)

CONF3AVGT3 <- rowMeans(data[,c("PConfPhyAs1T3" , "PConfPhyAs2T3" , "PConfPhyAs3T3", "PConfPhyAs4T3", "PConfPhyAs5T3")], na.rm = T)

CONF4AVGT3 <- rowMeans(data[,c("PConfNeg1T3" , "PConfNeg2T3" , "PConfNeg3T3", "PConfNeg4T3", "PConfNeg5T3")], na.rm = T)

AGGAVGT4 <- rowMeans(data[,c("MAgP1T4" , "MAgP2T4" , "MAgP3T4", "MAgP4T4")], na.rm = T)

CONF1AVGT4 <- rowMeans(data[,c("PConfNVio1T4" , "PConfNVio2T4" , "PConfNVio3T4", "PConfNVio4T4")], na.rm = T)

CONF2AVGT4 <-  rowMeans(data[,c("PConfAgg1T4" , "PConfAgg2T4" , "PConfAgg3T4", "PConfAgg4T4", "PConfAgg5T4")], na.rm = T)

CONF3AVGT4 <- rowMeans(data[,c("PConfPhyAs1T4" , "PConfPhyAs2T4" , "PConfPhyAs3T4", "PConfPhyAs4T4", "PConfPhyAs5T4")], na.rm = T)

CONF4AVGT4 <- rowMeans(data[,c("PConfNeg1T4" , "PConfNeg2T4" , "PConfNeg3T4", "PConfNeg4T4", "PConfNeg5T4")], na.rm = T)

## Updated to primary parent instead of mother, too many missing variables
AGGAVGT5 <- rowMeans(data[,c("PAgP1T5" , "PAgP2T5" , "PAgP3T5", "PAgP4T5")], na.rm = T)

CONF1AVGT5 <- rowMeans(data[,c("PConfNVio1T5" , "PConfNVio2T5" , "PConfNVio3T5", "PConfNVio4T5")], na.rm = T)

CONF2AVGT5 <-  rowMeans(data[,c("PConfAgg1T5" , "PConfAgg2T5" , "PConfAgg3T5", "PConfAgg4T5", "PConfAgg5T5")], na.rm = T)

CONF3AVGT5 <- rowMeans(data[,c("PConfPhyAs1T5" , "PConfPhyAs2T5" , "PConfPhyAs3T5", "PConfPhyAs4T5", "PConfPhyAs5T5")], na.rm = T)

CONF4AVGT5 <- rowMeans(data[,c("PConfNeg1T5" , "PConfNeg2T5" , "PConfNeg3T5", "PConfNeg4T5", "PConfNeg5T5")], na.rm = T)


avgdata <- as.data.frame(cbind(data, AGGAVGT3, CONF1AVGT3, CONF2AVGT3, CONF3AVGT3, CONF4AVGT3,
                            AGGAVGT4, CONF1AVGT4, CONF2AVGT4, CONF3AVGT4, CONF4AVGT4,
                            AGGAVGT5, CONF1AVGT5, CONF2AVGT5, CONF3AVGT5, CONF4AVGT5))
avgdata <- subset(avgdata, select = c(id,MRaceT1, MCulAttT2, MCulPraT2,
                                      CBCLAVG_3, AGGAVGT3, CONF1AVGT3, CONF2AVGT3, CONF3AVGT3, CONF4AVGT3,
                                      CBCLAVG_4, AGGAVGT4, CONF1AVGT4, CONF2AVGT4, CONF3AVGT4, CONF4AVGT4,
                                      CBCLAVG_5, AGGAVGT5, CONF1AVGT5, CONF2AVGT5, CONF3AVGT5, CONF4AVGT5))


avgdata[sapply(avgdata, is.nan)] <- NA


# write new dataset of averaged variables
write_xlsx(avgdata, "avg_data.xlsx")


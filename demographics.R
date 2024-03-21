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

################## Demographics ##################

demographics <- read_xlsx("clean_data.xlsx")
attach(demographics)

demographics[demographics==-9] <- NA
demographics[demographics==-8] <- NA
demographics[demographics==-7] <- NA
demographics[demographics==-6] <- NA
demographics[demographics==-5] <- NA
demographics[demographics==-4] <- NA
demographics[demographics==-3] <- NA
demographics[demographics==-2] <- NA
demographics[demographics==-1] <- NA

#PCG relationship to child
demographics %>% group_by(cp3pcgrel) %>% count(cp3pcgrel) # 1=bio mother, 2 = bio father, 3 = maternal grandma, 4 = paternal grandma, 5 = other relative, 6 = other non-relative
demographics %>% group_by(ch4pcgrel) %>% count(ch4pcgrel) # 1=bio mother, 2 = bio father, 3 = maternal grandma, 4 = paternal grandma, 5 = other relative, 6 = foster care
demographics %>% group_by(cp5pcgrel) %>% count(cp5pcgrel) # 1=bio mother, 2 = bio father, 3 = grandparent, 4 = aunt/uncle, 5 = sibling, 6 = other relative, 7 = foster parent, 8 = other adult

#mother age
mean(demographics$cm1age, na.rm=TRUE)
sd(demographics$cm1age, na.rm=TRUE)

#child gender
demographics %>% group_by(cm1bsex) %>% count(cm1bsex)

#parents' relationship
demographics %>% group_by(cm1relf) %>% count(cm1relf)

#mother education
demographics %>% group_by(cm1edu) %>% count(cm1edu)

#mother income
income <- cut(demographics$cm1hhinc, 
              breaks=c(-1,25000,50000,75000,100000,200000),
              labels=c("less than 25000", "25000 - 50000", 
                       "50000 - 75000", "75000 - 100000", "more than 100000"))
table(income)

### By race/ethnicity

white1 <- subset(demographics, MRaceT1 == 1)
black1 <- subset(demographics, MRaceT1 == 2)
hispanic1 <- subset(demographics, MRaceT1 == 3)

white1 %>% group_by(cm1bsex) %>% count(cm1bsex)
black1 %>% group_by(cm1bsex) %>% count(cm1bsex)
hispanic1 %>% group_by(cm1bsex) %>% count(cm1bsex)

mean(white1$cm1age, na.rm=TRUE)
sd(white1$cm1age, na.rm=TRUE)
mean(black1$cm1age, na.rm=TRUE)
sd(black1$cm1age, na.rm=TRUE)
mean(hispanic1$cm1age, na.rm=TRUE)
sd(hispanic1$cm1age, na.rm=TRUE)

white1 %>% group_by(cm1relf) %>% count(cm1relf)
black1 %>% group_by(cm1relf) %>% count(cm1relf)
hispanic1 %>% group_by(cm1relf) %>% count(cm1relf)

white1 %>% group_by(cm1edu) %>% count(cm1edu)
black1 %>% group_by(cm1edu) %>% count(cm1edu)
hispanic1 %>% group_by(cm1edu) %>% count(cm1edu)

incomeW <- cut(white1$cm1hhinc, 
               breaks=c(-1,25000,50000,75000,100000,200000),
               labels=c("less than 25000", "25000 - 50000", 
                        "50000 - 75000", "75000 - 100000", "more than 100000"))
table(incomeW)
incomeB <- cut(black1$cm1hhinc, 
               breaks=c(-1,25000,50000,75000,100000,200000),
               labels=c("less than 25000", "25000 - 50000", 
                        "50000 - 75000", "75000 - 100000", "more than 100000"))
table(incomeB)
incomeH <- cut(hispanic1$cm1hhinc, 
               breaks=c(-1,25000,50000,75000,100000,200000),
               labels=c("less than 25000", "25000 - 50000", 
                        "50000 - 75000", "75000 - 100000", "more than 100000"))
table(incomeH)


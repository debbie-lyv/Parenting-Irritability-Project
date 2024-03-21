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

white <- subset(data, MRaceT1 == 1)
black <- subset(data, MRaceT1 == 2)
hispanic <- subset(data, MRaceT1 == 3)
summary(white)
summary(black)
summary(hispanic)

summary(data$CBCLAVG_3)

####### ANOVA T3 #########

anova1 <- data %>% anova_test(CBCLAVG_3 ~ MRaceT1, type = 3)
get_anova_table(anova1)
anova2 <- data %>% anova_test(AGGAVGT3 ~ MRaceT1, type = 3)
get_anova_table(anova2)
anova3 <- data %>% anova_test(CONF1AVGT3 ~ MRaceT1, type = 3)
get_anova_table(anova3)
anova4 <- data %>% anova_test(CONF2AVGT3 ~ MRaceT1, type = 3)
get_anova_table(anova4)
anova5 <- data %>% anova_test(CONF3AVGT3 ~ MRaceT1, type = 3)
get_anova_table(anova5)
anova6 <- data %>% anova_test(CONF4AVGT3 ~ MRaceT1, type = 3)
get_anova_table(anova6)

# CONF1AVGT3,CONF2AVGT3,CONF3AVGT3, CONF4AVGT3 significant

compare_means(CONF1AVGT3 ~ MRaceT1, data = data, method = "anova", type = 3)
compare_means(CONF2AVGT3 ~ MRaceT1, data = data, method = "anova", type = 3)
compare_means(CONF3AVGT3 ~ MRaceT1, data = data, method = "anova", type = 3)
compare_means(CONF4AVGT3 ~ MRaceT1, data = data, method = "anova", type = 3)

## all significant 

data$Race <- factor(data$MRaceT1)
comparisons <- list(c("1", "2"), c("2", "3"), c("1","3"))

ggboxplot(data, "Race", "CONF1AVGT3", ylab = "Non-vio disc Avg T3", color = "Black", fill = "Race", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 1.5)
ggboxplot(data, "Race", "CONF2AVGT3", ylab = "Psych Agg Avg T3", color = "Black", fill = "Race", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 2)
ggboxplot(data, "Race", "CONF3AVGT3", ylab = "Phys As Avg T3", color = "Black", fill = "Race", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 3.5)
ggboxplot(data, "Race", "CONF4AVGT3", ylab = "Neglect T3", color = "Black", fill = "Race", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 3.5)

group_by(data, MRaceT1) %>%
  summarise(
    mean = mean(CONF1AVGT3, na.rm = TRUE),
    sd = sd(CONF1AVGT3, na.rm = TRUE)
  )
group_by(data, MRaceT1) %>%
  summarise(
    mean = mean(CONF2AVGT3, na.rm = TRUE),
    sd = sd(CONF2AVGT3, na.rm = TRUE)
  )
group_by(data, MRaceT1) %>%
  summarise(
    mean = mean(CONF3AVGT3, na.rm = TRUE),
    sd = sd(CONF3AVGT3, na.rm = TRUE)
  )
group_by(data, MRaceT1) %>%
  summarise(
    mean = mean(CONF4AVGT3, na.rm = TRUE),
    sd = sd(CONF4AVGT3, na.rm = TRUE)
  )
##### ANOVA T4 #####

anova1.2 <- data %>% anova_test(CBCLAVG_4 ~ MRaceT1, type = 3)
get_anova_table(anova1.2)
anova2.2 <- data %>% anova_test(AGGAVGT4 ~ MRaceT1, type = 3)
get_anova_table(anova2.2)
anova3.2 <- data %>% anova_test(CONF1AVGT4 ~ MRaceT1, type = 3)
get_anova_table(anova3.2)
anova4.2 <- data %>% anova_test(CONF2AVGT4 ~ MRaceT1, type = 3)
get_anova_table(anova4.2)
anova5.2 <- data %>% anova_test(CONF3AVGT4 ~ MRaceT1, type = 3)
get_anova_table(anova5.2)
anova6.2 <- data %>% anova_test(CONF4AVGT4 ~ MRaceT1, type = 3)
get_anova_table(anova6.2)

# CBCLAVG_4, CONF1AVGT4 significant

compare_means(CBCLAVG_4 ~ MRaceT1, data = data, method = "anova", type = 3)
compare_means(CONF1AVGT4 ~ MRaceT1, data = data, method = "anova", type = 3)

# CONF1AVGT4 significant 

ggboxplot(data, "Race", "CONF1AVGT4", ylab = "Non-vio disc Avg T4", color = "Black", fill = "Race", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 1.5)

group_by(data, MRaceT1) %>%
  summarise(
    mean = mean(CONF1AVGT4, na.rm = TRUE),
    sd = sd(CONF1AVGT4, na.rm = TRUE)
  )



##### ANOVA T5 #####
anova1.3 <- data %>% anova_test(CBCLAVG_5 ~ MRaceT1, type = 3)
get_anova_table(anova1.3)
anova2.3 <- data %>% anova_test(AGGAVGT5 ~ MRaceT1, type = 3)
get_anova_table(anova2.3)
anova3.3 <- data %>% anova_test(CONF1AVGT5 ~ MRaceT1, type = 3)
get_anova_table(anova3.3)
anova4.3 <- data %>% anova_test(CONF2AVGT5 ~ MRaceT1, type = 3)
get_anova_table(anova4.3)
anova5.3 <- data %>% anova_test(CONF3AVGT5 ~ MRaceT1, type = 3)
get_anova_table(anova5.3)
anova6.3 <- data %>% anova_test(CONF4AVGT5 ~ MRaceT1, type = 3)
get_anova_table(anova6.3)

# CBCLAVG_5, AGGAVGT5, CONF1AVGT5,CONF2AVGT5 significant

compare_means(CBCLAVG_5 ~ MRaceT1, data = data, method = "anova", type = 3)
compare_means(AGGAVGT5 ~ MRaceT1, data = data, method = "anova", type = 3)
compare_means(CONF1AVGT5 ~ MRaceT1, data = data, method = "anova", type = 3)
compare_means(CONF2AVGT5 ~ MRaceT1, data = data, method = "anova", type = 3)

# all significant

ggboxplot(data, "Race", "CBCLAVG_5", ylab = "CBCL Avg T5", color = "Black", fill = "Race", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 1.5)
ggboxplot(data, "Race", "AGGAVGT5", ylab = "Agg in Parent Avg T5",  color = "Black", fill = "Race", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 2)
ggboxplot(data, "Race", "CONF1AVGT5", ylab = "Non-vio disc Avg T5", color = "Black", fill = "Race", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 1.5)
ggboxplot(data, "Race", "CONF2AVGT5", ylab = "Pscyh Agg Avg T5", color = "Black", fill = "Race", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 2)


group_by(data, MRaceT1) %>%
  summarise(
    mean = mean(CBCLAVG_5, na.rm = TRUE),
    sd = sd(CBCLAVG_5, na.rm = TRUE)
  )
group_by(data, MRaceT1) %>%
  summarise(
    mean = mean(AGGAVGT5, na.rm = TRUE),
    sd = sd(AGGAVGT5, na.rm = TRUE)
  )
group_by(data, MRaceT1) %>%
  summarise(
    mean = mean(CONF1AVGT5, na.rm = TRUE),
    sd = sd(CONF1AVGT5, na.rm = TRUE)
  )
group_by(data, MRaceT1) %>%
  summarise(
    mean = mean(CONF2AVGT5, na.rm = TRUE),
    sd = sd(CONF2AVGT5, na.rm = TRUE)
  )

## Cultural attachment variables

anova7 <- data %>% anova_test(MCulAttT2 ~ MRaceT1, type = 3)
get_anova_table(anova7)
anova8 <- data %>% anova_test(MCulPraT2 ~ MRaceT1, type = 3)
get_anova_table(anova8)

# MCulPraT2 significant

compare_means(MCulPraT2 ~ MRaceT1, data = data, method = "anova", type = 3)

# MCulPraT2 significant 

ggboxplot(data, "Race", "MCulPraT2", color = "Black", fill = "Race", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 2)


######## Non-significant variables #########

# T3

group_by(data, MRaceT1) %>%
  summarise(
    mean = mean(CBCLAVG_3, na.rm = TRUE),
    sd = sd(CBCLAVG_3, na.rm = TRUE)
  )

group_by(data, MRaceT1) %>%
  summarise(
    mean = mean(AGGAVGT3, na.rm = TRUE),
    sd = sd(AGGAVGT3, na.rm = TRUE)
  )

# T4

group_by(data, MRaceT1) %>%
  summarise(
    mean = mean(AGGAVGT4, na.rm = TRUE),
    sd = sd(AGGAVGT4, na.rm = TRUE)
  )

group_by(data, MRaceT1) %>%
  summarise(
    mean = mean(CONF2AVGT4, na.rm = TRUE),
    sd = sd(CONF2AVGT4, na.rm = TRUE)
  )

group_by(data, MRaceT1) %>%
  summarise(
    mean = mean(CONF3AVGT4, na.rm = TRUE),
    sd = sd(CONF3AVGT4, na.rm = TRUE)
  )

group_by(data, MRaceT1) %>%
  summarise(
    mean = mean(CONF4AVGT4, na.rm = TRUE),
    sd = sd(CONF4AVGT4, na.rm = TRUE)
  )

# T5

group_by(data, MRaceT1) %>%
  summarise(
    mean = mean(CONF3AVGT5, na.rm = TRUE),
    sd = sd(CONF3AVGT5, na.rm = TRUE)
  )

group_by(data, MRaceT1) %>%
  summarise(
    mean = mean(CONF4AVGT5, na.rm = TRUE),
    sd = sd(CONF4AVGT5, na.rm = TRUE)
  )

# Cultural attachment

group_by(data, MRaceT1) %>%
  summarise(
    mean = mean(MCulAttT2, na.rm = TRUE),
    sd = sd(MCulAttT2, na.rm = TRUE)
  )


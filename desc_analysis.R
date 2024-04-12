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

data$Race <- factor(data$MRaceT1)
comparisons <- list(c("White", "Black"), c("White", "Hispanic"), c("Black","Hispanic"))

####### ANOVA T3 #########

# CBCLAVG_3
CBCLAVG3_aov <- aov(CBCLAVG_3 ~ Race,
                    data = data
)
summary(CBCLAVG3_aov)

# p < .01
CBCLAVG3_phoc <- glht(CBCLAVG3_aov,
                      linfct = mcp(Race = "Tukey")
)
summary(CBCLAVG3_phoc)
plot(CBCLAVG3_phoc)
# significant differences: hispanic - black
ggboxplot(data, x = "Race", y = "CBCLAVG_3", ylab = "CBCL Avg T3", fill = "Race", color = "Black", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 1.5)



# AGGAVGT3
AGGAVGT3_aov <- aov(AGGAVGT3 ~ Race,
                    data = data
)
summary(AGGAVGT3_aov)

# p > .05, no significant differences
ggboxplot(data, x = "Race", y = "AGGAVGT3", ylab = "Agg Avg T3", fill = "Race", color = "Black", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons, label = "p.signif") + stat_compare_means(label.y = 1.5)



# CONF1AVGT3
CONF1AVGT3_aov <- aov(CONF1AVGT3 ~ Race,
                      data = data
)
summary(CONF1AVGT3_aov)

# p < .0001
CONF1AVGT3_phoc <- glht(CONF1AVGT3_aov,
                        linfct = mcp(Race = "Tukey")
)
summary(CONF1AVGT3_phoc)
plot(CONF1AVGT3_phoc)
# significant differences: white - black, white - hispanic, hispanic - black
ggboxplot(data, x = "Race", y = "CONF1AVGT3", ylab = "Non-vio Avg T3", fill = "Race", color = "Black", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 3)



# CONF2AVGT3
CONF2AVGT3_aov <- aov(CONF2AVGT3 ~ Race,
                      data = data
)
summary(CONF2AVGT3_aov)

# p < .0001
CONF2AVGT3_phoc <- glht(CONF2AVGT3_aov,
                        linfct = mcp(Race = "Tukey")
)
summary(CONF2AVGT3_phoc)
plot(CONF2AVGT3_phoc)
# significant differences: white - black, white - hispanic, hispanic - black
ggboxplot(data, "Race", "CONF2AVGT3", ylab = "Psy Agg Avg T3", color = "Black", fill = "Race", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 3)



# CONF3AVGT3
CONF3AVGT3_aov <- aov(CONF3AVGT3 ~ Race,
                      data = data
)
summary(CONF3AVGT3_aov)

# p < .0001
CONF3AVGT3_phoc <- glht(CONF3AVGT3_aov,
                        linfct = mcp(Race = "Tukey")
)
summary(CONF3AVGT3_phoc)
plot(CONF3AVGT3_phoc)
# significant differences: white - black, white - hispanic, hispanic - black
ggboxplot(data, "Race", "CONF3AVGT3", ylab = "Phy As Avg T3", color = "Black", fill = "Race", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 3)



# CONF4AVGT3
CONF4AVGT3_aov <- aov(CONF4AVGT3 ~ Race,
                      data = data
)
summary(CONF4AVGT3_aov)

# p < .05
CONF4AVGT3_phoc <- glht(CONF4AVGT3_aov,
                        linfct = mcp(Race = "Tukey")
)
summary(CONF4AVGT3_phoc)
plot(CONF4AVGT3_phoc)
# significant differences: white - black (white - hispanic p < 0.1)
ggboxplot(data, "Race", "CONF4AVGT3", ylab = "Neg Avg T3", color = "Black", fill = "Race", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 1.5)


#### T3 Means and SDs ####

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


####### ANOVA T4 #########

# CBCLAVG_4
CBCLAVG4_aov <- aov(CBCLAVG_4 ~ Race,
                    data = data
)
summary(CBCLAVG4_aov)

# p < .1, no significant differences
ggboxplot(data, x = "Race", y = "CBCLAVG_4", ylab = "CBCL Avg T4", color = "Black", fill = "Race", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 1.5)



# AGGAVGT4
AGGAVGT4_aov <- aov(AGGAVGT4 ~ Race,
                    data = data
)
summary(AGGAVGT4_aov)

# p > .05, no significant differences
ggboxplot(data, x = "Race", y = "AGGAVGT4", ylab = "Agg Avg T4", fill = "Race", color = "Black", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons, label = "p.signif") + stat_compare_means(label.y = 1.5)



# CONF1AVGT4
CONF1AVGT4_aov <- aov(CONF1AVGT4 ~ Race,
                      data = data
)
summary(CONF1AVGT4_aov)

# p < .0001
CONF1AVGT4_phoc <- glht(CONF1AVGT4_aov,
                        linfct = mcp(Race = "Tukey")
)
summary(CONF1AVGT4_phoc)
plot(CONF1AVGT4_phoc)
# significant differences: white - black, white - hispanic, hispanic - black
ggboxplot(data, x = "Race", y = "CONF1AVGT4", ylab = "Non-vio Avg T4", fill = "Race", color = "Black", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 3)



# CONF2AVGT4
CONF2AVGT4_aov <- aov(CONF2AVGT4 ~ Race,
                      data = data
)
summary(CONF2AVGT4_aov)

# p < .0001
CONF2AVGT4_phoc <- glht(CONF2AVGT4_aov,
                        linfct = mcp(Race = "Tukey")
)
summary(CONF2AVGT4_phoc)
plot(CONF2AVGT4_phoc)
# significant differences: white - black, hispanic - black
ggboxplot(data, "Race", "CONF2AVGT4", ylab = "Psy Agg Avg T4", color = "Black", fill = "Race", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 3)



# CONF3AVGT4
CONF3AVGT4_aov <- aov(CONF3AVGT4 ~ Race,
                      data = data
)
summary(CONF3AVGT4_aov)

# p < .0001
CONF3AVGT4_phoc <- glht(CONF3AVGT4_aov,
                        linfct = mcp(Race = "Tukey")
)
summary(CONF3AVGT4_phoc)
plot(CONF3AVGT4_phoc)
# significant differences: white - black, hispanic - black
ggboxplot(data, "Race", "CONF3AVGT4", ylab = "Phy As Avg T4", color = "Black", fill = "Race", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 3)



# CONF4AVGT4
CONF4AVGT4_aov <- aov(CONF4AVGT4 ~ Race,
                      data = data
)
summary(CONF4AVGT4_aov)

# p > .05, no significant differences
ggboxplot(data, "Race", "CONF4AVGT4", ylab = "Neg Avg T4", color = "Black", fill = "Race", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 1.5)


#### T4 Means and SDs ####

group_by(data, MRaceT1) %>%
  summarise(
    mean = mean(CBCLAVG_4, na.rm = TRUE),
    sd = sd(CBCLAVG_4, na.rm = TRUE)
  )
group_by(data, MRaceT1) %>%
  summarise(
    mean = mean(AGGAVGT4, na.rm = TRUE),
    sd = sd(AGGAVGT4, na.rm = TRUE)
  )
group_by(data, MRaceT1) %>%
  summarise(
    mean = mean(CONF1AVGT4, na.rm = TRUE),
    sd = sd(CONF1AVGT4, na.rm = TRUE)
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

####### ANOVA T5 #########

# CBCLAVG_5
CBCLAVG5_aov <- aov(CBCLAVG_5 ~ Race,
                    data = data
)
summary(CBCLAVG5_aov)
# p < .05
CBCLAVG5_phoc <- glht(CBCLAVG5_aov,
                      linfct = mcp(Race = "Tukey")
)
summary(CBCLAVG5_phoc)
plot(CBCLAVG5_phoc)
# significant differences: white - hispanic, hispanic - black
ggboxplot(data, x = "Race", y = "CBCLAVG_5", ylab = "CBCL Avg T5", fill = "Race", color = "Black", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 1.5)



# AGGAVGT5
AGGAVGT5_aov <- aov(AGGAVGT5 ~ Race,
                    data = data
)
summary(AGGAVGT5_aov)
#p < .001
AGGAVGT5_phoc <- glht(AGGAVGT5_aov,
                      linfct = mcp(Race = "Tukey")
)
summary(AGGAVGT5_phoc)
plot(AGGAVGT5_phoc)
# significant differences: white - hispanic, hispanic - black
ggboxplot(data, x = "Race", y = "AGGAVGT5", ylab = "Agg Avg T5", fill = "Race", color = "Black", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 3)



# CONF1AVGT5
CONF1AVGT5_aov <- aov(CONF1AVGT5 ~ Race,
                      data = data
)
summary(CONF1AVGT5_aov)

# p < .0001
CONF1AVGT5_phoc <- glht(CONF1AVGT5_aov,
                        linfct = mcp(Race = "Tukey")
)
summary(CONF1AVGT5_phoc)
plot(CONF1AVGT5_phoc)
# significant differences: white - black, white - hispanic, hispanic - black
ggboxplot(data, x = "Race", y = "CONF1AVGT5", ylab = "Non-vio Avg T5", fill = "Race", color = "Black", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 3)



# CONF2AVGT5
CONF2AVGT5_aov <- aov(CONF2AVGT5 ~ Race,
                      data = data
)
summary(CONF2AVGT5_aov)

# p < .0001
CONF2AVGT5_phoc <- glht(CONF2AVGT5_aov,
                        linfct = mcp(Race = "Tukey")
)
summary(CONF2AVGT5_phoc)
plot(CONF2AVGT5_phoc)
# significant differences: white - black, white - hispanic, hispanic - black
ggboxplot(data, "Race", "CONF2AVGT5", ylab = "Psy Agg Avg T5", color = "Black", fill = "Race", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 3)



# CONF3AVGT5
CONF3AVGT5_aov <- aov(CONF3AVGT5 ~ Race,
                      data = data
)
summary(CONF3AVGT5_aov)

# p < .0001
CONF3AVGT5_phoc <- glht(CONF3AVGT5_aov,
                        linfct = mcp(Race = "Tukey")
)
summary(CONF3AVGT5_phoc)
plot(CONF3AVGT5_phoc)
# significant differences: white - black, hispanic - black
ggboxplot(data, "Race", "CONF3AVGT5", ylab = "Phy As Avg T5", color = "Black", fill = "Race", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 3)



# CONF4AVGT4
CONF4AVGT5_aov <- aov(CONF4AVGT5 ~ Race,
                      data = data
)
summary(CONF4AVGT5_aov)

# p > .05, no significant differences
ggboxplot(data, "Race", "CONF4AVGT5", ylab = "Neg Avg T5", color = "Black", fill = "Race", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 1.5)


#### T5 Means and SDs ####

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



#### Anova Cultural attachment variables ###

# MCulAttT2
MCulAttT2_aov <- aov(MCulAttT2 ~ Race,
                      data = data
)
summary(MCulAttT2_aov)
# p < .05
MCulAttT2_phoc <- glht(MCulAttT2_aov,
                        linfct = mcp(Race = "Tukey")
)
summary(MCulAttT2_phoc)
plot(MCulAttT2_phoc)
# significant differences: white - black
ggboxplot(data, "Race", "MCulAttT2", ylab = "Cultural Attachment", color = "Black", fill = "Race", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 3)


# MCulPraT2
MCulPraT2_aov <- aov(MCulPraT2 ~ Race,
                     data = data
)
summary(MCulPraT2_aov)
# p < .05
MCulPraT2_phoc <- glht(MCulPraT2_aov,
                       linfct = mcp(Race = "Tukey")
)
summary(MCulPraT2_phoc)
plot(MCulPraT2_phoc)
# significant differences: white - black, white - hispanic, black - hispanic
ggboxplot(data, "Race", "MCulPraT2", ylab = "Cultural Practice", color = "Black", fill = "Race", palette = c("#4059AD", "#6B9AC4", "#97D8CA")) + 
  scale_x_discrete(labels = c("White", "Black", "Hispanic")) + 
  stat_compare_means(comparisons = comparisons) + stat_compare_means(label.y = 3)



group_by(data, MRaceT1) %>%
  summarise(
    mean = mean(MCulAttT2, na.rm = TRUE),
    sd = sd(MCulAttT2, na.rm = TRUE)
  )

group_by(data, MRaceT1) %>%
  summarise(
    mean = mean(MCulPraT2, na.rm = TRUE),
    sd = sd(MCulPraT2, na.rm = TRUE)
  )


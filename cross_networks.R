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
set.seed(1)

################# NETWORK ANALYSIS T3 ###############

white <- subset(data, MRaceT1 == 1)
black <- subset(data, MRaceT1 == 2)
hispanic <- subset(data, MRaceT1 == 3)
summary(white)
summary(black)
summary(hispanic)

keeps1T3 <-c("CBCLAVG_3", "AGGAVGT3", "CONF1AVGT3", "CONF2AVGT3", "CONF3AVGT3", "CONF4AVGT3", 
             "MCulAttT2", "MCulPraT2"
)
df1T3 = white[keeps1T3]

summary(df1T3)


keeps2T3 <-c("CBCLAVG_3", "AGGAVGT3", "CONF1AVGT3", "CONF2AVGT3", "CONF3AVGT3", "CONF4AVGT3", 
             "MCulAttT2", "MCulPraT2"
)
df2T3 = black[keeps2T3]

summary(df2T3)


keeps3T3 <-c("CBCLAVG_3", "AGGAVGT3", "CONF1AVGT3", "CONF2AVGT3", "CONF3AVGT3", "CONF4AVGT3", 
             "MCulAttT2", "MCulPraT2"
)
df3T3 = hispanic[keeps3T3]
summary(df3T3)




melt.df1T3 <- melt(df1T3)
melt.df2T3 <- melt(df2T3)
melt.df3T3 <- melt(df3T3)

ggplot(data = melt.df1T3, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")
ggplot(data = melt.df2T3, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")
ggplot(data = melt.df3T3, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")

ggplot(data = melt.df1T3, aes(x = value)) + 
  geom_histogram() + 
  facet_wrap(~variable, scales = "free")
ggplot(data = melt.df2T3, aes(x = value)) + 
  geom_histogram() + 
  facet_wrap(~variable, scales = "free")
ggplot(data = melt.df3T3, aes(x = value)) + 
  geom_histogram() + 
  facet_wrap(~variable, scales = "free")

xx1 <- as.data.frame(huge.npn(df1T3))
xx2 <- as.data.frame(huge.npn(df2T3))
xx3 <- as.data.frame(huge.npn(df3T3))

melt.xx1 <- melt(xx1)
head(melt.xx1)
melt.xx2 <- melt(xx2)
head(melt.xx2)
melt.xx3 <- melt(xx3)
head(melt.xx3)

ggplot(data = melt.xx1, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")
ggplot(data = melt.xx2, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")
ggplot(data = melt.xx3, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")

############# Networks #############

crossLabels <- c("irr", "pStr", "nVio", "psyAgg", "phyAs", "neg", "cAtt", "cPrac")

Net1T3 <- estimateNetwork(xx1, default = "ggmModSelect", corMethod="spearman")
Net2T3 <- estimateNetwork(xx2, default = "ggmModSelect", corMethod="spearman")
Net3T3 <- estimateNetwork(xx3, default = "ggmModSelect", corMethod="spearman")

summary(Net1T3)
summary(Net2T3)
summary(Net3T3)

NetPlot <- plot(Net1T3, layout = "circle",labels = crossLabels, label.cex = 1.8, 
                vsize = 15, label.prop = 0.5, edge.labels = TRUE, edge.label.cex = 2)
NetPlot <- plot(Net2T3, layout = "circle",labels = crossLabels, label.cex = 1.8,
                vsize = 15, label.prop = 0.5, edge.labels = TRUE, edge.label.cex = 2)
NetPlot <- plot(Net3T3, layout = "circle",labels = crossLabels, label.cex = 1.8, 
                vsize = 15, label.prop = 0.5, edge.labels = TRUE, edge.label.cex = 2)

# Centrality statistics
options(scipen = 999)
Net1T3.cenPlot <- centralityPlot(Net1T3, include = c("Betweenness","Closeness","Strength"))
Net2T3.cenPlot <- centralityPlot(Net2T3, include = c("Betweenness","Closeness","Strength"))
Net3T3.cenPlot <- centralityPlot(Net3T3, include = c("Betweenness","Closeness","Strength"))

#-------- Network edges
View(round(Net1T3$graph, digits = 2))
View(round(Net2T3$graph, digits = 2))
View(round(Net3T3$graph, digits = 2))


### Check for accuracy and stability of the network ---------

############# White participants #################

# Bootstrap using 1000 values and 8 cores
Net1T3.boot <- bootnet(Net1T3, nBoots = 1000, ncores = 8, type = "nonparametric")

print(Net1T3.boot)
summary(Net1T3.boot)

plot(Net1T3.boot, labels=TRUE, order = "sample")

# Case-dropping bootstrap to investigate stability of centrality indices
Net1T3.boot2 <- bootnet(Net1T3, nBoots = 1000, ncores = 8, type = "case", 
                        statistics = c("strength", "closeness", "betweenness"))

# Plot centrality stability ######################################################
plot(Net1T3.boot2, statistics= c("strength", "closeness", "betweenness"))

## Compute CS-coefficients
corStability(Net1T3.boot2)

### Edge-Weights & Centrality Difference Tests for all pairs of edges and centrality indices
plot(Net1T3.boot, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
plot(Net1T3.boot, "strength")

# Test for two nodes
differenceTest(Net1T3.boot, 1, 3, "strength")


############# Black participants #################

# Bootstrap using 1000 values and 8 cores
Net2T3.boot <- bootnet(Net2T3, nBoots = 1000, ncores = 8, type = "nonparametric")

print(Net2T3.boot)
summary(Net2T3.boot)

plot(Net2T3.boot, labels=TRUE, order = "sample")

# Case-dropping bootstrap to investigate stability of centrality indices
Net2T3.boot2 <- bootnet(Net2T3, nBoots = 1000, ncores = 8, type = "case", 
                        statistics = c("strength", "closeness", "betweenness"))

# Plot centrality stability ######################################################
plot(Net2T3.boot2, statistics= c("strength", "closeness", "betweenness"))

## Compute CS-coefficients
corStability(Net2T3.boot2)

### Edge-Weights & Centrality Difference Tests for all pairs of edges and centrality indices
plot(Net2T3.boot, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
plot(Net2T3.boot, "strength")

# Test for two nodes
differenceTest(Net2T3.boot, 1, 3, "strength")


############# Hispanic participants #################

# Bootstrap using 1000 values and 8 cores
Net3T3.boot <- bootnet(Net3T3, nBoots = 1000, ncores = 8, type = "nonparametric")

print(Net3T3.boot)
summary(Net3T3.boot)

plot(Net3T3.boot, labels=TRUE, order = "sample")

# Case-dropping bootstrap to investigate stability of centrality indices
Net3T3.boot2 <- bootnet(Net3T3, nBoots = 1000, ncores = 8, type = "case", 
                        statistics = c("strength", "closeness", "betweenness"))

# Plot centrality stability ######################################################
plot(Net3T3.boot2, statistics= c("strength", "closeness", "betweenness"))

## Compute CS-coefficients
corStability(Net3T3.boot2)

### Edge-Weights & Centrality Difference Tests for all pairs of edges and centrality indices
plot(Net3T3.boot, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
plot(Net3T3.boot, "strength")

# Test for two nodes
differenceTest(Net3T3.boot, 1, 3, "strength")

################# NETWORK ANALYSIS T4 ###############


keeps1T4 <-c("CBCLAVG_4", "AGGAVGT4", "CONF1AVGT4", "CONF2AVGT4", "CONF3AVGT4", "CONF4AVGT4", 
             "MCulAttT2", "MCulPraT2"
)
df1T4 = white[keeps1T4]

summary(df1T4)


keeps2T4 <-c("CBCLAVG_4", "AGGAVGT4", "CONF1AVGT4", "CONF2AVGT4", "CONF3AVGT4", "CONF4AVGT4", 
             "MCulAttT2", "MCulPraT2"
)
df2T4 = black[keeps2T4]


keeps3T4 <-c("CBCLAVG_4", "AGGAVGT4", "CONF1AVGT4", "CONF2AVGT4", "CONF3AVGT4", "CONF4AVGT4", 
             "MCulAttT2", "MCulPraT2"
)
df3T4 = hispanic[keeps3T4]


melt.df1T4 <- melt(df1T4)
melt.df2T4 <- melt(df2T4)
melt.df3T4 <- melt(df3T4)

ggplot(data = melt.df1T4, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")
ggplot(data = melt.df2T4, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")
ggplot(data = melt.df3T4, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")

ggplot(data = melt.df1T4, aes(x = value)) + 
  geom_histogram() + 
  facet_wrap(~variable, scales = "free")
ggplot(data = melt.df2T4, aes(x = value)) + 
  geom_histogram() + 
  facet_wrap(~variable, scales = "free")
ggplot(data = melt.df3T4, aes(x = value)) + 
  geom_histogram() + 
  facet_wrap(~variable, scales = "free")

yy1 <- as.data.frame(huge.npn(df1T4))
yy2 <- as.data.frame(huge.npn(df2T4))
yy3 <- as.data.frame(huge.npn(df3T4))

melt.yy1 <- melt(yy1)
head(melt.yy1)
melt.yy2 <- melt(yy2)
head(melt.yy2)
melt.yy3 <- melt(yy3)
head(melt.yy3)

ggplot(data = melt.yy1, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")
ggplot(data = melt.yy2, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")
ggplot(data = melt.yy3, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")


############# Networks #############

Net1T4 <- estimateNetwork(yy1, default = "ggmModSelect", corMethod="spearman")
Net2T4 <- estimateNetwork(yy2, default = "ggmModSelect", corMethod="spearman")
Net3T4 <- estimateNetwork(yy3, default = "ggmModSelect", corMethod="spearman")

summary(Net1T4)
summary(Net2T4)
summary(Net3T4)

NetPlot <- plot(Net1T4, layout = "circle",labels = crossLabels, label.cex = 1.8, 
                vsize = 15, label.prop = 0.5, edge.labels = TRUE, edge.label.cex = 2)
NetPlot <- plot(Net2T4, layout = "circle",labels = crossLabels, label.cex = 1.8, 
                vsize = 15, label.prop = 0.5, edge.labels = TRUE, edge.label.cex = 2)
NetPlot <- plot(Net3T4, layout = "circle",labels = crossLabels, label.cex = 1.8, 
                vsize = 15, label.prop = 0.5, edge.labels = TRUE, edge.label.cex = 2)


# Centrality statistics
options(scipen = 999)
Net1T4.cenPlot <- centralityPlot(Net1T4, include = c("Betweenness","Closeness","Strength"))
Net2T4.cenPlot <- centralityPlot(Net2T4, include = c("Betweenness","Closeness","Strength"))
Net3T4.cenPlot <- centralityPlot(Net3T4, include = c("Betweenness","Closeness","Strength"))

#-------- Network edges
View(round(Net1T4$graph, digits = 2))
View(round(Net2T4$graph, digits = 2))
View(round(Net3T4$graph, digits = 2))



### Check for accuracy and stability of the network ---------

############# White participants #################

# Bootstrap using 1000 values and 8 cores
Net1T4.boot <- bootnet(Net1T4, nBoots = 1000, ncores = 8, type = "nonparametric")

print(Net1T4.boot)
summary(Net1T4.boot)

plot(Net1T4.boot, labels=TRUE, order = "sample")

# Case-dropping bootstrap to investigate stability of centrality indices
Net1T4.boot2 <- bootnet(Net1T4, nBoots = 1000, ncores = 8, type = "case", 
                        statistics = c("strength", "closeness", "betweenness"))

# Plot centrality stability ######################################################
plot(Net1T4.boot2, statistics= c("strength", "closeness", "betweenness"))

## Compute CS-coefficients
corStability(Net1T4.boot2)

### Edge-Weights & Centrality Difference Tests for all pairs of edges and centrality indices
plot(Net1T4.boot, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
plot(Net1T4.boot, "strength")

# Test for two nodes
differenceTest(Net1T4.boot, 1, 3, "strength")


############# Black participants #################

# Bootstrap using 1000 values and 8 cores
Net2T4.boot <- bootnet(Net2T4, nBoots = 1000, ncores = 8, type = "nonparametric")

print(Net2T4.boot)
summary(Net2T4.boot)

plot(Net2T4.boot, labels=TRUE, order = "sample")

# Case-dropping bootstrap to investigate stability of centrality indices
Net2T4.boot2 <- bootnet(Net2T4, nBoots = 1000, ncores = 8, type = "case", 
                        statistics = c("strength", "closeness", "betweenness"))

# Plot centrality stability ######################################################
plot(Net2T4.boot2, statistics= c("strength", "closeness", "betweenness"))

## Compute CS-coefficients
corStability(Net2T4.boot2)

### Edge-Weights & Centrality Difference Tests for all pairs of edges and centrality indices
plot(Net2T4.boot, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
plot(Net2T4.boot, "strength")

# Test for two nodes
differenceTest(Net2T4.boot, 1, 3, "strength")


############# Hispanic participants #################

# Bootstrap using 1000 values and 8 cores
Net3T4.boot <- bootnet(Net3T4, nBoots = 1000, ncores = 8, type = "nonparametric")

print(Net3T4.boot)
summary(Net3T4.boot)

plot(Net3T4.boot, labels=TRUE, order = "sample")

# Case-dropping bootstrap to investigate stability of centrality indices
Net3T4.boot2 <- bootnet(Net3T4, nBoots = 1000, ncores = 8, type = "case", 
                        statistics = c("strength", "closeness", "betweenness"))

# Plot centrality stability ######################################################
plot(Net3T4.boot2, statistics= c("strength", "closeness", "betweenness"))

## Compute CS-coefficients
corStability(Net3T4.boot2)

### Edge-Weights & Centrality Difference Tests for all pairs of edges and centrality indices
plot(Net3T4.boot, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
plot(Net3T4.boot, "strength")

# Test for two nodes
differenceTest(Net3T4.boot, 1, 3, "strength")


################# NETWORK ANALYSIS T5 ###############


keeps1T5 <-c("CBCLAVG_5", "AGGAVGT5", "CONF1AVGT5", "CONF2AVGT5", "CONF3AVGT5", "CONF4AVGT5", 
             "MCulAttT2", "MCulPraT2"
)
df1T5 = white[keeps1T5]

summary(df1T5)


keeps2T5 <-c("CBCLAVG_5", "AGGAVGT5", "CONF1AVGT5", "CONF2AVGT5", "CONF3AVGT5", "CONF4AVGT5", 
             "MCulAttT2", "MCulPraT2"
)
df2T5 = black[keeps2T5]

summary(df2T5)


keeps3T5 <-c("CBCLAVG_5", "AGGAVGT5", "CONF1AVGT5", "CONF2AVGT5", "CONF3AVGT5", "CONF4AVGT5", 
             "MCulAttT2", "MCulPraT2"
)
df3T5 = hispanic[keeps2T5]


melt.df1T5 <- melt(df1T5)
melt.df2T5 <- melt(df2T5)
melt.df3T5 <- melt(df3T5)

ggplot(data = melt.df1T5, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")
ggplot(data = melt.df2T5, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")
ggplot(data = melt.df3T5, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")

ggplot(data = melt.df1T5, aes(x = value)) + 
  geom_histogram() + 
  facet_wrap(~variable, scales = "free")
ggplot(data = melt.df2T5, aes(x = value)) + 
  geom_histogram() + 
  facet_wrap(~variable, scales = "free")
ggplot(data = melt.df3T5, aes(x = value)) + 
  geom_histogram() + 
  facet_wrap(~variable, scales = "free")

zz1 <- as.data.frame(huge.npn(df1T5))
zz2 <- as.data.frame(huge.npn(df2T5))
zz3 <- as.data.frame(huge.npn(df3T5))

melt.zz1 <- melt(zz1)
head(melt.zz1)
melt.zz2 <- melt(zz2)
head(melt.zz2)
melt.zz3 <- melt(zz3)
head(melt.zz3)

ggplot(data = melt.zz1, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")
ggplot(data = melt.zz2, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")
ggplot(data = melt.zz3, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")

############# Networks #############

Net1T5 <- estimateNetwork(zz1, default = "ggmModSelect", corMethod="spearman")
Net2T5 <- estimateNetwork(zz2, default = "ggmModSelect", corMethod="spearman")
Net3T5 <- estimateNetwork(zz3, default = "ggmModSelect", corMethod="spearman")

summary(Net1T5)
summary(Net2T5)
summary(Net3T5)

NetPlot <- plot(Net1T5, layout = "circle",labels = crossLabels, label.cex = 1.8, 
                vsize = 15, label.prop = 0.5, edge.labels = TRUE, edge.label.cex = 2)
NetPlot <- plot(Net2T5, layout = "circle",labels = crossLabels, label.cex = 1.8, 
                vsize = 15, label.prop = 0.5, edge.labels = TRUE, edge.label.cex = 2)
NetPlot <- plot(Net3T5, layout = "circle",labels = crossLabels, label.cex = 1.8, 
                vsize = 15, label.prop = 0.5, edge.labels = TRUE, edge.label.cex = 2)

# Centrality statistics
options(scipen = 999)
Net1T5.cenPlot <- centralityPlot(Net1T5, include = c("Betweenness","Closeness","Strength"))
Net2T5.cenPlot <- centralityPlot(Net2T5, include = c("Betweenness","Closeness","Strength"))
Net3T5.cenPlot <- centralityPlot(Net3T5, include = c("Betweenness","Closeness","Strength"))

#-------- Network edges
View(round(Net1T5$graph, digits = 2))
View(round(Net2T5$graph, digits = 2))
View(round(Net3T5$graph, digits = 2))

### Check for accuracy and stability of the network ---------

############# White participants T5 #################

# Bootstrap using 1000 values and 8 cores
Net1T5.boot <- bootnet(Net1T5, nBoots = 1000, ncores = 8, type = "nonparametric")

print(Net1T5.boot)
summary(Net1T5.boot)

plot(Net1T5.boot, labels=TRUE, order = "sample")

# Case-dropping bootstrap to investigate stability of centrality indices
Net1T5.boot2 <- bootnet(Net1T5, nBoots = 1000, ncores = 8, type = "case", 
                        statistics = c("strength", "closeness", "betweenness"))

# Plot centrality stability ######################################################
plot(Net1T5.boot2, statistics= c("strength", "closeness", "betweenness"))

## Compute CS-coefficients
corStability(Net1T5.boot2)

### Edge-Weights & Centrality Difference Tests for all pairs of edges and centrality indices
plot(Net1T5.boot, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
plot(Net1T5.boot, "strength")

# Test for two nodes
differenceTest(Net1T5.boot, 1, 3, "strength")


############# Black participants T5 #################

# Bootstrap using 1000 values and 8 cores
Net2T5.boot <- bootnet(Net2T5, nBoots = 1000, ncores = 8, type = "nonparametric")

print(Net2T5.boot)
summary(Net2T5.boot)

plot(Net2T5.boot, labels=TRUE, order = "sample")

# Case-dropping bootstrap to investigate stability of centrality indices
Net2T5.boot2 <- bootnet(Net2T5, nBoots = 1000, ncores = 8, type = "case", 
                        statistics = c("strength", "closeness", "betweenness"))

# Plot centrality stability ######################################################
plot(Net2T5.boot2, statistics= c("strength", "closeness", "betweenness"))

## Compute CS-coefficients
corStability(Net2T5.boot2)

### Edge-Weights & Centrality Difference Tests for all pairs of edges and centrality indices
plot(Net2T5.boot, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
plot(Net2T5.boot, "strength")

# Test for two nodes
differenceTest(Net2T5.boot, 1, 3, "strength")


############# Hispanic participants T5 #################

# Bootstrap using 1000 values and 8 cores
Net3T5.boot <- bootnet(Net3T5, nBoots = 1000, ncores = 8, type = "nonparametric")

print(Net3T5.boot)
summary(Net3T5.boot)

plot(Net3T5.boot, labels=TRUE, order = "sample")

# Case-dropping bootstrap to investigate stability of centrality indices
Net3T5.boot2 <- bootnet(Net3T5, nBoots = 1000, ncores = 8, type = "case", 
                        statistics = c("strength", "closeness", "betweenness"))

# Plot centrality stability ######################################################
plot(Net3T5.boot2, statistics= c("strength", "closeness", "betweenness"))

## Compute CS-coefficients
corStability(Net3T5.boot2)

### Edge-Weights & Centrality Difference Tests for all pairs of edges and centrality indices
plot(Net3T5.boot, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
plot(Net3T5.boot, "strength")

# Test for two nodes
differenceTest(Net3T5.boot, 1, 3, "strength")

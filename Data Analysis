library(tidyverse)
library(ggplot2)

NBf2 <- function(data, pars){
  k <- pars[1]
  r <- pars[2]
  x <- data[,1]
  l1 <- gamma(x + k)/(gamma(x + 1)*gamma(k))
  l2 <- (r/(r+k))^x
  l3 <- (1+r/k)^{-k}
  L <- l1*l2*l3
  NLL <- -sum(log(L))
  return(NLL)
}

estimate <- function(data){
  res2 <- optim(par = c(1, 1), fn = NBf2, data = data)
  k <- 1/(res2$par[1])
  return(res2)
} 

#Non Over Dispersed Data - k = 1.12, R = 3.9 (par1 = 100, par2 = 2.0)
#Over Dispersed Data - k = 0.497, R = 3.84 (par1 = 100, par2 = 0.5)


##Pipeline Estimates


#####################################
# Non OverDispersed Different Betas #
#####################################

metaOverDispersedDataPerf <- read.csv(file = "/Users/henry/Desktop/Covasim/MetaNonOverDispersedDiffBeta/metaDataPerfNonOverDispersedDiffBeta.csv",header = TRUE)
metaOverDispersedDataPerf <- t(metaOverDispersedDataPerf)
metaEstimatesPerfR = c()
metaEstimatesPerfK = c()
for (x in 1:999) {
  tryCatch({
    estimates = estimate(as.data.frame(metaOverDispersedDataPerf[,x]))
    metaEstimatesPerfR = append(metaEstimatesPerfR,estimates$par[2])
    metaEstimatesPerfK = append(metaEstimatesPerfK,estimates$par[1])
  }, error=function(e){})
}
##Print Means and CIs
NODB_K_PERF <- mean(metaEstimatesPerfK)
NODB_R_PERF <- mean(metaEstimatesPerfR)

##CIs R_0
NODB_R_CI_PERF <- t.test(metaEstimatesPerfR)$conf.int

##CIs K
NODB_K_CI_PERF <- t.test(metaEstimatesPerfK)$conf.int

##75% Data Non OverDispersed Different Betas

metaOverDispersedData75 <- read.csv(file = "/Users/henry/Desktop/Covasim/MetaNonOverDispersedDiffBeta/metaData75NonOverDispersedDiffBeta.csv",header = TRUE)
metaOverDispersedData75 <- t(metaOverDispersedData75)
metaEstimates75R = c()
metaEstimates75K = c()
for (x in 1:999) {
  tryCatch({
    estimates = estimate(as.data.frame(metaOverDispersedData75[,x]))
    metaEstimates75R = append(metaEstimates75R,estimates$par[2])
    metaEstimates75K = append(metaEstimates75K,estimates$par[1])
  }, error=function(e){})
}
##Print Means and CIs
NODB_K_75 <- mean(metaEstimates75K)
NODB_R_75 <- mean(metaEstimates75R)

##CIs R_0
NODB_R_CI_75 <- t.test(metaEstimates75R)$conf.int

##CIs K
NODB_K_CI_75 <- t.test(metaEstimates75K)$conf.int



##50% Data Non OverDispersed Different Betas

metaOverDispersedData50 <- read.csv(file = "/Users/henry/Desktop/Covasim/MetaNonOverDispersedDiffBeta/metaData50NonOverDispersedDiffBeta.csv",header = TRUE)
metaOverDispersedData50 <- t(metaOverDispersedData50)
metaEstimates50R = c()
metaEstimates50K = c()
for (x in 1:999) {
  tryCatch({
    estimates = estimate(as.data.frame(metaOverDispersedData50[,x]))
    metaEstimates50R = append(metaEstimates50R,estimates$par[2])
    metaEstimates50K = append(metaEstimates50K,estimates$par[1])
  }, error=function(e){})
}
##Print Means and CIs
NODB_K_50 <- mean(metaEstimates50K)
NODB_R_50 <- mean(metaEstimates50R)

##CIs R_0
NODB_R_CI_50 <- t.test(metaEstimates50R)$conf.int

##CIs K
NODB_K_CI_50 <- t.test(metaEstimates50K)$conf.int

##25% Data Non OverDispersed Different Betas

metaOverDispersedData25 <- read.csv(file = "/Users/henry/Desktop/Covasim/MetaNonOverDispersedDiffBeta/metaData25NonOverDispersedDiffBeta.csv",header = TRUE)
metaOverDispersedData25 <- t(metaOverDispersedData25)
metaEstimates25R = c()
metaEstimates25K = c()
for (x in 1:999) {
  tryCatch({
    estimates = estimate(as.data.frame(metaOverDispersedData25[,x]))
    metaEstimates25R = append(metaEstimates25R,estimates$par[2])
    metaEstimates25K = append(metaEstimates25K,estimates$par[1])
  }, error=function(e){})
}
##Print Means and CIs
NODB_K_25 <- mean(metaEstimates25K)
NODB_R_25 <- mean(metaEstimates25R)

##CIs R_0
NODB_R_CI_25 <- t.test(metaEstimates25R)$conf.int

##CIs K
NODB_K_CI_25 <- t.test(metaEstimates25K)$conf.int

plot_data <- data.frame(
  x = c(0.25, 0.5, 0.75, 1),
  mean = c(NODB_K_25,NODB_K_50,NODB_K_75,NODB_K_PERF),
  ci_lower = c(NODB_K_CI_25[1],NODB_K_CI_50[1],NODB_K_CI_75[1],NODB_K_CI_PERF[1]),
  ci_upper = c(NODB_K_CI_25[2],NODB_K_CI_50[2],NODB_K_CI_75[2],NODB_K_CI_PERF[2])
) 

plot1 <- ggplot(plot_data, aes(x = x, y = mean)) +
  geom_point(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.1, position = position_dodge(0.9)) +
  labs(x = "Percentile", y = "Mean",title = "Non OverDispersed Different Betas: k Estimates") +
  scale_x_continuous(breaks = c(0.25, 0.5, 0.75, 1), labels = c("25%", "50%", "75%", "100%")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#####################################
# Non OverDispersed Same Betas #
#####################################

metaOverDispersedDataPerf <- read.csv(file = "/Users/henry/Desktop/Covasim/MetaNonOverDispersedSameBeta/metaDataPerfNonOverDispersedSameBeta.csv",header = TRUE)
metaOverDispersedDataPerf <- t(metaOverDispersedDataPerf)
metaEstimatesPerfR = c()
metaEstimatesPerfK = c()
for (x in 1:999) {
  tryCatch({
    estimates = estimate(as.data.frame(metaOverDispersedDataPerf[,x]))
    metaEstimatesPerfR = append(metaEstimatesPerfR,estimates$par[2])
    metaEstimatesPerfK = append(metaEstimatesPerfK,estimates$par[1])
  }, error=function(e){})
}
##Print Means and CIs
NOSB_K_PERF <- mean(metaEstimatesPerfK)
NOSB_R_PERF <- mean(metaEstimatesPerfR)

##CIs R_0
NOSB_R_CI_PERF <- t.test(metaEstimatesPerfR)$conf.int

##CIs K
NOSB_K_CI_PERF <- t.test(metaEstimatesPerfK)$conf.int

##75% Data Non OverDispersed Same Betas

metaOverDispersedData75 <- read.csv(file = "/Users/henry/Desktop/Covasim/MetaNonOverDispersedSameBeta/metaData75NonOverDispersedSameBeta.csv",header = TRUE)
metaOverDispersedData75 <- t(metaOverDispersedData75)
metaEstimates75R = c()
metaEstimates75K = c()
for (x in 1:999) {
  tryCatch({
    estimates = estimate(as.data.frame(metaOverDispersedData75[,x]))
    metaEstimates75R = append(metaEstimates75R,estimates$par[2])
    metaEstimates75K = append(metaEstimates75K,estimates$par[1])
  }, error=function(e){})
}
##Print Means and CIs
NOSB_K_75 <- mean(metaEstimates75K)
NOSB_R_75 <- mean(metaEstimates75R)

##CIs R_0
NOSB_R_CI_75 <- t.test(metaEstimates75R)$conf.int

##CIs K
NOSB_K_CI_75 <- t.test(metaEstimates75K)$conf.int

##50% Data Non OverDispersed Same Betas

metaOverDispersedData50 <- read.csv(file = "/Users/henry/Desktop/Covasim/MetaNonOverDispersedSameBeta/metaData50NonOverDispersedSameBeta.csv",header = TRUE)
metaOverDispersedData50 <- t(metaOverDispersedData50)
metaEstimates50R = c()
metaEstimates50K = c()
for (x in 1:999) {
  tryCatch({
    estimates = estimate(as.data.frame(metaOverDispersedData50[,x]))
    metaEstimates50R = append(metaEstimates50R,estimates$par[2])
    metaEstimates50K = append(metaEstimates50K,estimates$par[1])
  }, error=function(e){})
}
##Print Means and CIs
NOSB_K_50 <- mean(metaEstimates50K)
NOSB_R_50 <- mean(metaEstimates50R)

##CIs R_0
NOSB_R_CI_50 <- t.test(metaEstimates50R)$conf.int

##CIs K
NOSB_K_CI_50 <- t.test(metaEstimates50K)$conf.int

##25% Data Non OverDispersed Same Betas

metaOverDispersedData25 <- read.csv(file = "/Users/henry/Desktop/Covasim/MetaNonOverDispersedSameBeta/metaData25NonOverDispersedSameBeta.csv",header = TRUE)
metaOverDispersedData25 <- t(metaOverDispersedData25)
metaEstimates25R = c()
metaEstimates25K = c()
for (x in 1:999) {
  tryCatch({
    estimates = estimate(as.data.frame(metaOverDispersedData25[,x]))
    metaEstimates25R = append(metaEstimates25R,estimates$par[2])
    metaEstimates25K = append(metaEstimates25K,estimates$par[1])
  }, error=function(e){})
}
##Print Means and CIs
NOSB_K_25 <- mean(metaEstimates25K)
NOSB_R_25 <- mean(metaEstimates25R)

##CIs R_0
NOSB_R_CI_25 <- t.test(metaEstimates25R)$conf.int

##CIs K
NOSB_K_CI_25 <- t.test(metaEstimates25K)$conf.int

#####################################
#OverDispersed Same Betas #
#####################################

metaOverDispersedDataPerf <- read.csv(file = "/Users/henry/Desktop/Covasim/MetaOverDispersedSameBeta/metaDataPerfOverDispersedSameBeta.csv",header = TRUE)
metaOverDispersedDataPerf <- t(metaOverDispersedDataPerf)
metaEstimatesPerfR = c()
metaEstimatesPerfK = c()
for (x in 1:999) {
  tryCatch({
    estimates = estimate(as.data.frame(metaOverDispersedDataPerf[,x]))
    metaEstimatesPerfR = append(metaEstimatesPerfR,estimates$par[2])
    metaEstimatesPerfK = append(metaEstimatesPerfK,estimates$par[1])
  }, error=function(e){})
}
##Print Means and CIs
OSB_K_PERF <- mean(metaEstimatesPerfK)
OSB_R_PERF <- mean(metaEstimatesPerfR)

##CIs R_0
OSB_R_CI_PERF <- t.test(metaEstimatesPerfR)$conf.int

##CIs K
OSB_K_CI_PERF <- t.test(metaEstimatesPerfK)$conf.int

##75% Data OverDispersed Same Betas

metaOverDispersedData75 <- read.csv(file = "/Users/henry/Desktop/Covasim/MetaOverDispersedSameBeta/metaData75OverDispersedSameBeta.csv",header = TRUE)
metaOverDispersedData75 <- t(metaOverDispersedData75)
metaEstimates75R = c()
metaEstimates75K = c()
for (x in 1:999) {
  tryCatch({
    estimates = estimate(as.data.frame(metaOverDispersedData75[,x]))
    metaEstimates75R = append(metaEstimates75R,estimates$par[2])
    metaEstimates75K = append(metaEstimates75K,estimates$par[1])
  }, error=function(e){})
}
##Print Means and CIs
OSB_K_75 <- mean(metaEstimates75K)
OSB_R_75 <- mean(metaEstimates75R)

##CIs R_0
OSB_R_CI_75 <- t.test(metaEstimates75R)$conf.int

##CIs K
OSB_K_CI_75 <- t.test(metaEstimates75K)$conf.int

##50% Data OverDispersed Same Betas

metaOverDispersedData50 <- read.csv(file = "/Users/henry/Desktop/Covasim/MetaOverDispersedSameBeta/metaData50OverDispersedSameBeta.csv",header = TRUE)
metaOverDispersedData50 <- t(metaOverDispersedData50)
metaEstimates50R = c()
metaEstimates50K = c()
for (x in 1:999) {
  tryCatch({
    estimates = estimate(as.data.frame(metaOverDispersedData50[,x]))
    metaEstimates50R = append(metaEstimates50R,estimates$par[2])
    metaEstimates50K = append(metaEstimates50K,estimates$par[1])
  }, error=function(e){})
}
##Print Means and CIs
OSB_K_50 <- mean(metaEstimates50K)
OSB_R_50 <- mean(metaEstimates50R)

##CIs R_0
OSB_R_CI_50 <- t.test(metaEstimates50R)$conf.int

##CIs K
OSB_K_CI_50 <- t.test(metaEstimates50K)$conf.int

##25% Data OverDispersed Same Betas

metaOverDispersedData25 <- read.csv(file = "/Users/henry/Desktop/Covasim/MetaOverDispersedSameBeta/metaData25OverDispersedSameBeta.csv",header = TRUE)
metaOverDispersedData25 <- t(metaOverDispersedData25)
metaEstimates25R = c()
metaEstimates25K = c()
for (x in 1:999) {
  tryCatch({
    estimates = estimate(as.data.frame(metaOverDispersedData25[,x]))
    metaEstimates25R = append(metaEstimates25R,estimates$par[2])
    metaEstimates25K = append(metaEstimates25K,estimates$par[1])
  }, error=function(e){})
}
##Print Means and CIs
OSB_K_25 <- mean(metaEstimates25K)
OSB_R_25 <- mean(metaEstimates25R)

##CIs R_0
OSB_R_CI_25 <- t.test(metaEstimates25R)$conf.int

##CIs K
OSB_K_CI_25 <- t.test(metaEstimates25K)$conf.int


#####################################
#OverDispersed Diff Betas #
#####################################

metaOverDispersedDataPerf <- read.csv(file = "/Users/henry/Desktop/Covasim/MetaOverDispersedDiffBeta/metaDataPerfOverDispersedDiffBeta.csv",header = TRUE)
metaOverDispersedDataPerf <- t(metaOverDispersedDataPerf)
metaEstimatesPerfR = c()
metaEstimatesPerfK = c()
for (x in 1:999) {
  tryCatch({
    estimates = estimate(as.data.frame(metaOverDispersedDataPerf[,x]))
    metaEstimatesPerfR = append(metaEstimatesPerfR,estimates$par[2])
    metaEstimatesPerfK = append(metaEstimatesPerfK,estimates$par[1])
  }, error=function(e){})
}
##Print Means and CIs
ODB_K_PERF <- mean(metaEstimatesPerfK)
ODB_R_PERF <- mean(metaEstimatesPerfR)

##CIs R_0
ODB_R_CI_PERF <- t.test(metaEstimatesPerfR)$conf.int

##CIs K
ODB_K_CI_PERF <- t.test(metaEstimatesPerfK)$conf.int

##75% Data Non OverDispersed Different Betas

metaOverDispersedData75 <- read.csv(file = "/Users/henry/Desktop/Covasim/MetaOverDispersedDiffBeta/metaData75OverDispersedDiffBeta.csv",header = TRUE)
metaOverDispersedData75 <- t(metaOverDispersedData75)
metaEstimates75R = c()
metaEstimates75K = c()
for (x in 1:999) {
  tryCatch({
    estimates = estimate(as.data.frame(metaOverDispersedData75[,x]))
    metaEstimates75R = append(metaEstimates75R,estimates$par[2])
    metaEstimates75K = append(metaEstimates75K,estimates$par[1])
  }, error=function(e){})
}
##Print Means and CIs
ODB_K_75 <- mean(metaEstimates75K)
ODB_R_75 <- mean(metaEstimates75R)

##CIs R_0
ODB_R_CI_75 <- t.test(metaEstimates75R)$conf.int

##CIs K
ODB_K_CI_75 <- t.test(metaEstimates75K)$conf.int


##50% Data Non OverDispersed Different Betas

metaOverDispersedData50 <- read.csv(file = "/Users/henry/Desktop/Covasim/MetaOverDispersedDiffBeta/metaData50OverDispersedDiffBeta.csv",header = TRUE)
metaOverDispersedData50 <- t(metaOverDispersedData50)
metaEstimates50R = c()
metaEstimates50K = c()
for (x in 1:999) {
  tryCatch({
    estimates = estimate(as.data.frame(metaOverDispersedData50[,x]))
    metaEstimates50R = append(metaEstimates50R,estimates$par[2])
    metaEstimates50K = append(metaEstimates50K,estimates$par[1])
  }, error=function(e){})
}
##Print Means and CIs
ODB_K_50 <- mean(metaEstimates50K)
ODB_R_50 <- mean(metaEstimates50R)

##CIs R_0
ODB_R_CI_50 <- t.test(metaEstimates50R)$conf.int

##CIs K
ODB_K_CI_50 <- t.test(metaEstimates50K)$conf.int

##25% Data Non OverDispersed Different Betas

metaOverDispersedData25 <- read.csv(file = "/Users/henry/Desktop/Covasim/MetaOverDispersedDiffBeta/metaData25OverDispersedDiffBeta.csv",header = TRUE)
metaOverDispersedData25 <- t(metaOverDispersedData25)
metaEstimates25R = c()
metaEstimates25K = c()
for (x in 1:999) {
  tryCatch({
    estimates = estimate(as.data.frame(metaOverDispersedData25[,x]))
    metaEstimates25R = append(metaEstimates25R,estimates$par[2])
    metaEstimates25K = append(metaEstimates25K,estimates$par[1])
  }, error=function(e){})
}
##Print Means and CIs
ODB_K_25 <- mean(metaEstimates25K)
ODB_R_25 <- mean(metaEstimates25R)

##CIs R_0
ODB_R_CI_25 <- t.test(metaEstimates25R)$conf.int

##CIs K
ODB_K_CI_25 <- t.test(metaEstimates25K)$conf.int




## Plots

#Superspreading
list1 <- c(NODB_K_PERF, NODB_K_75, NODB_K_50, NODB_K_25)
list2 <- c(NOSB_K_PERF, NOSB_K_75, NOSB_K_50, 5)
list3 <- c(ODB_K_PERF, ODB_K_75, ODB_K_50, ODB_K_25)
list4 <- c(OSB_K_PERF, OSB_K_75, OSB_K_50, OSB_K_25)

x_labels <- c("100%", "75%", "50%", "25%")

legend_labels = c("No Overdispersion Different Transmission Potential", "No Overdispersion Same Transmission Potential", "Overdispersion Different Transmission Potential", "Overdispersion Same Transmission Potential")

data <- data.frame(
  x = c("100%", "75%", "50%", "25%"),
  y = c(list1, list2, list3, list4),
  group = factor(rep(1:4, each = 4))
)

custom_order <- c("100%", "75%", "50%", "25%")

# Convert the 'x' variable to a factor with the custom order
data$x <- factor(data$x, levels = custom_order)

# Set custom colors for the lines/dots
line_colors <- c("blue", "black", "green", "purple")

# Plot the data
ggplot(data, aes(x = x, y = y, group = group, color = group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  geom_hline(yintercept = list1[1], color = "blue", linetype = "solid")+
  geom_hline(yintercept = list2[1], color = "black", linetype = "solid")+
  geom_hline(yintercept = list3[1], color = "green", linetype = "solid")+
  geom_hline(yintercept = list4[1], color = "purple", linetype = "solid")+
  #geom_errorbar(aes(ymin = y - 0.05, ymax = y + 0.05), width = 0.2) +
  scale_color_manual(values = line_colors,labels = legend_labels) +
  labs(title = "Aggregated Estimates of Superspreading Potential",
       x = "Percent of Out of Home Contacts Traced",
       y = "Overdispersion Parameter, k",
       color = "Scenarios") +
  theme_minimal() +
  theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5))

#Reproductive Number
list1 <- c(NODB_R_PERF, NODB_R_75, NODB_R_50, NODB_R_25)
list2 <- c(NOSB_R_PERF, NOSB_R_75, NOSB_R_50, NOSB_R_25)
list3 <- c(ODB_R_PERF, ODB_R_75, ODB_R_50, ODB_R_25)
list4 <- c(OSB_R_PERF, OSB_R_75, OSB_R_50, OSB_R_25)

x_labels <- c("100%", "75%", "50%", "25%")

legend_labels = c("No Overdispersion Different Transmission Potential", "No Overdispersion Same Transmission Potential", "Overdispersion Different Transmission Potential", "Overdispersion Same Transmission Potential")

data <- data.frame(
  x = c("100%", "75%", "50%", "25%"),
  y = c(list1, list2, list3, list4),
  group = factor(rep(1:4, each = 4))
)

custom_order <- c("100%", "75%", "50%", "25%")

# Convert the 'x' variable to a factor with the custom order
data$x <- factor(data$x, levels = custom_order)

# Set custom colors for the lines/dots
line_colors <- c("blue", "black", "green", "purple")

# Plot the data
ggplot(data, aes(x = x, y = y, group = group, color = group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = list1[1], color = "blue", linetype = "solid")+
  geom_hline(yintercept = list2[1], color = "black", linetype = "solid")+
  geom_hline(yintercept = list3[1], color = "green", linetype = "solid")+
  geom_hline(yintercept = list4[1], color = "purple", linetype = "solid")+
  #geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  #geom_errorbar(aes(ymin = y - 0.05, ymax = y + 0.05), width = 0.2) +
  scale_color_manual(values = line_colors,labels = legend_labels) +
  labs(title = "Aggregated Estimates of Reproductive Number",
       x = "Percent of Out of Home Contacts Traced",
       y = "Reproductive Number",
       color = "Scenarios") +
  theme_minimal() +
  theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5))

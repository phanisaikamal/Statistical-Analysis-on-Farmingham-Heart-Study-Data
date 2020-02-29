knitr::opts_chunk$set(echo = TRUE)
#install.packages("units")
install.packages("ggplot2")
#library("units")
library("ggplot2")
dataSet <- read.csv(file = "framingham.csv")
head(dataSet)
summary(dataSet)
data <- dataSet[complete.cases(dataSet), ]
head(data)
summary(data)
names(data) <- c("Gender", "Age", "Education", "SmokingBehavior", "CigarettesPerDay", "BloodPressureMedication", "PrevalentStroke", "PrevalentHypertension", "DiabeticCondition", "TotalCholestrol", "SystolicBloodPressure", "DiastolicBloodPressure", "BodyMassIndex", "HeartRate", "GlucoseLevel", "TenYearCoronaryHeartDisease")
head(data)
data$Gender[data$Gender == 0] <- "Female"
data$Gender[data$Gender == 1] <- "Male"
data$Education[data$Education == 1] <- "High School"
data$Education[data$Education == 2] <- "General Education Development"
data$Education[data$Education == 3] <- "Vocational School"
data$Education[data$Education == 4] <- "College"
data$SmokingBehavior[data$SmokingBehavior == 0] <- "Non Smoker"
data$SmokingBehavior[data$SmokingBehavior == 1] <- "Smoker"
data$BloodPressureMedication[data$BloodPressureMedication == 0] <- "Not Under BP Medication"
data$BloodPressureMedication[data$BloodPressureMedication == 1] <- "Under BP Medication"
data$PrevalentStroke[data$PrevalentStroke == 0] <- "No"
data$PrevalentStroke[data$PrevalentStroke == 1] <- "Yes"
data$PrevalentHypertension[data$PrevalentHypertension == 0] <- "No"
data$PrevalentHypertension[data$PrevalentHypertension == 1] <- "Yes"
data$DiabeticCondition[data$DiabeticCondition == 0] <- "Non Diabetic"
data$DiabeticCondition[data$DiabeticCondition == 1] <- "Diabetic"
data$TenYearCoronaryHeartDisease[data$TenYearCoronaryHeartDisease == 0] <- "Immune"
data$TenYearCoronaryHeartDisease[data$TenYearCoronaryHeartDisease == 1] <- "Vulnerable"
data$Gender <- as.factor(data$Gender)
data$Education <- as.factor(data$Education)
data$SmokingBehavior <- as.factor(data$SmokingBehavior)
data$BloodPressureMedication <- as.factor(data$BloodPressureMedication)
data$PrevalentStroke <- as.factor(data$PrevalentStroke)
data$PrevalentHypertension <- as.factor(data$PrevalentHypertension)
data$DiabeticCondition <- as.factor(data$DiabeticCondition)
data$TenYearCoronaryHeartDisease <- as.factor(data$TenYearCoronaryHeartDisease)
head(data)
#units(data$Age) <- "years"
#units(data$TotalCholestrol) <- "mg/dL"
#units(data$SystolicBloodPressure) <- "mmHg"
#units(data$DiastolicBloodPressure) <- "mmHg"
#units(data$BodyMassIndex) <- "kg/m^2"
#units(data$GlucoseLevel) <- "mg/dL"
#head(data)
summary(data)
ggplot(data, aes(x = Age)) + 
  geom_histogram(bins = 30, fill = "lightblue") + 
  theme_bw() + theme_classic() +
  ggtitle("Age Distribution") + ylab("Number of People")
ggplot(data, aes(x = Education, fill = Education)) + 
  geom_bar() + ggtitle("Received Level of Education") +
  geom_text(stat = 'count', aes(label =..count..), vjust = -0.5)
ggplot(data, aes(x = Gender, fill = Gender)) + 
  geom_bar() + 
  geom_text(stat = 'count', aes(label =..count..), vjust = -0.5) + 
  theme_bw() + theme_classic() +
  ggtitle("Gender Distribution") + ylab("Number of People")
ggplot(data, aes(x = TenYearCoronaryHeartDisease)) + 
  geom_bar(aes(fill = Gender), position = 'dodge', width = 0.5, color='black') + 
  theme_bw() + theme_classic() + 
  ylab("Number of People") + ggtitle("10 Year CHD Risk Versus Gender")
ggplot(data, aes(x = CigarettesPerDay)) + 
  geom_histogram(bins = 30, fill = "gray") + 
  theme_bw() + theme_classic() +
  ggtitle("Smokers - Cigarettes Per Day Distribution") + ylab("Number of People")
ggplot(data, aes(x = SmokingBehavior)) + 
  geom_bar(fill = "lightgreen") + 
  geom_text(stat = 'count', aes(label =..count..), vjust = -0.5) + 
  theme_bw() + theme_classic() +
  ggtitle("Smoking Behaviour") + ylab("Number of People")
ggplot(data, aes(x = TenYearCoronaryHeartDisease)) + 
  geom_bar(aes(fill = SmokingBehavior), position = 'dodge', width = 0.5, color= 'black') + 
  theme_bw() + theme_classic() + 
  ylab("Number of People") + ggtitle("Smoking Habit Versus 10 Year CHD Risk")
ggplot(data, aes(x = TotalCholestrol)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  theme_minimal() + 
  ggtitle("Distribution Total Cholestrol Levels") + ylab("Number of People")
ggplot(data, aes(x = TotalCholestrol)) + 
  geom_density(aes(fill = TenYearCoronaryHeartDisease), alpha = 0.4)
ggplot(data, aes(x = SystolicBloodPressure)) + 
  geom_density(fill ="orange", alpha = 0.9) + 
  theme_minimal() + 
  ggtitle("Systolic BP Levels in People") + ylab("Number of People")
ggplot(data, aes(x = SystolicBloodPressure)) + 
  geom_density(aes(color = TenYearCoronaryHeartDisease, fill = TenYearCoronaryHeartDisease), alpha = 0.4, position = "identity") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))
# Systolic Blood Pressure vs Ten Year Coronary Heart Disease
ggplot(data, aes(x = SystolicBloodPressure))+
  geom_histogram(bins = 30, color="black", fill="white")+
  facet_grid(TenYearCoronaryHeartDisease ~ .)
ggplot(data, aes(x =BodyMassIndex)) + 
  geom_dotplot(color = "pink", fill = "pink", binwidth = 1/4)
ggplot(data, aes(x = TenYearCoronaryHeartDisease, y = BodyMassIndex)) + 
  geom_boxplot(width = 0.4, fill = "white") +
  geom_jitter(aes(color = TenYearCoronaryHeartDisease, shape = TenYearCoronaryHeartDisease), 
              width = 0.1, size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  labs(x = NULL)
ggplot(data, aes(x = TenYearCoronaryHeartDisease)) + 
  geom_bar() + 
  geom_text(stat = 'count', aes(label =..count..), vjust = -0.5) + 
  theme_bw() + theme_classic() +
  ggtitle("10 Year CHD Risk") + ylab("Number of People")
qqnorm(data$SystolicBloodPressure, main ="Normality Check for Systolic Blood Pressure Level")
qqline(data$SystolicBloodPressure)
hist(data$SystolicBloodPressure)
# the parts of the test statistic
# sample mean
x_bar <- mean(data$SystolicBloodPressure)
# null hypothesized population mean
mu_0 <- 120
# sample st. dev
s <- sd(data$SystolicBloodPressure)
# sample size
n <- length(data$SystolicBloodPressure)
# t-test test statistic
t <- (x_bar - mu_0)/(s/sqrt(n))
t
# two-sided p-value so multiply by 2
two_sided_t_pval <- pt(q = t, df = n-1, lower.tail = FALSE)*2
two_sided_t_pval
qt(0.025, n-1)
# lower bound
x_bar + (qt(0.025, n-1)*(s/sqrt(n))) # alternately you can use x_bar-(qt(0.975, n-1)*(s/sqrt(n)))
# upper bound
x_bar + (qt(0.975, n-1)*(s/sqrt(n))) # alternately you can use x_bar-(qt(0.025, n-1)*(s/sqrt(n)))
t.test(data$SystolicBloodPressure, alternative = "two.sided", mu = 120)
set.seed(0)
# This data is pretty skewed so even though n is large, I'm going to do a lot of simulations
num_sims <- 10000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  results[i] <- mean(sample(x = data$SystolicBloodPressure,
                            size = n,
                            replace = TRUE))
}
# Finally plot the results
hist(results, freq = FALSE, main='Sampling Distribution of the Sample Mean', xlab = 'Average Systolic Blood Pressure', ylab = 'Density')
# estimate a normal curve over it - this looks pretty good!
lines(x = seq(130, 134, .01), dnorm(seq(130, 134, .01), mean = x_bar, sd = s/sqrt(n)))
set.seed(0)
# Shift the sample so that the null hypothesis is true
bp_given_H0_true <- data$SystolicBloodPressure - mean(data$SystolicBloodPressure) + mu_0
# This data is pretty skewed so even though n is large, I'm going to do a lot of simulations
num_sims <- 10000
# A vector to store my results
results_given_H0_true <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  results_given_H0_true[i] <- mean(sample(x = bp_given_H0_true,
                                          size = n,
                                          replace = TRUE))
}
# Finally plot the results
hist(results_given_H0_true, freq = FALSE, main='Sampling Distribution of the Sample Mean, Given Null Hypothesis is True', xlab = 'Average Systolic Blood Pressure', ylab = 'Density')
# add line to show values more extreme on upper end
abline(v=x_bar, col = "red")
# add line to show values more extreme on lower end
low_end_extreme <- mean(results_given_H0_true)+(mean(results_given_H0_true)-x_bar)
lines(x = seq(117, 122, .01), dnorm(seq(117, 122, .01), mean = mean(results_given_H0_true), sd = sd(results_given_H0_true)))
abline(v=low_end_extreme, col="red")
# counts of values more extreme than the test statistic in our original sample, given H0 is true
# two sided given the alternate hypothesis
count_of_more_extreme_lower_tail <- sum(results_given_H0_true <= low_end_extreme)
count_of_more_extreme_upper_tail <- sum(results_given_H0_true >= x_bar)
bootstrap_pvalue <- (count_of_more_extreme_lower_tail + count_of_more_extreme_upper_tail)/num_sims
bootstrap_pvalue
# two sided t p-value
two_sided_t_pval
# need the standard error which is the standard deviation of the results
bootstrap_SE_X_bar <- sd(results)
# an estimate is to use the formula statistic +/- 2*SE
c(x_bar - 2*bootstrap_SE_X_bar, x_bar + 2*bootstrap_SE_X_bar)
# you can also use the 5th and 95th quantiles to determine the bounds:
c(quantile(results, c(.025, .975)))
# compare to our t-methods
c(x_bar+(qt(0.025, n-1)*(s/sqrt(n))), x_bar+(qt(0.975, n-1)*(s/sqrt(n))))
p_0 <- 0.48
p_0
p <- length(data$Gender[data$Gender == "Female"])
p
n <- length(data$Gender)
n
p_hat <- p/n
p_hat
z <- (p_hat - p_0) / sqrt((p_0*(1-p_0)) / n)
z
binom.test(x = p, n = n, p = p_0, alternative = "greater")
pnorm(z, lower.tail = FALSE)
cat("Exact Binomial Test")
binom.test(x = p, n = n, p = p_0, alternative = "greater")$conf.int
cat("Normal Approx")
c(p_hat - (1.64)*sqrt(((p_hat)*(1-p_hat))/n), 1)
female <- data$Gender
female
female <- relevel(female, "Male")
levels(female) <- c(0, 1)
female
table(female)
set.seed(0)
# This data is pretty skewed so even though n is large, I'm going to do a lot of simulations
num_sims <- 10000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  results[i] <- mean(as.numeric(sample(x = female, size = n, replace = TRUE))-1)
}
# Finally plot the results
hist(results, freq = FALSE, main='Sampling Distribution of the Sample Proportion', xlab = 'Proportion of Female', ylab = 'Density')
# estimate a normal curve over it - this looks pretty good!
lines(x = seq(.52, .60, .001), dnorm(seq(.52, .60, .001), mean = mean(results), sd = sd(results)))
cat("Bootstrap Confidence Interval")
c(quantile(results, c(0.05,1)))
cat("exact binomial test")
binom.test(x = p, n = n, p = p_0, alternative = "greater")$conf.int
cat("normal approx")
c(p_hat - (1.64)*sqrt(((p_hat)*(1-p_hat))/n),1)
# Under the assumption that the null hypothesis is true, we have 48% female
female_sim <- rep(c(1, 0), c(.48*n, (1-.48)*n))
num_sims <- 10000
# A vector to store my results
results_H0_true <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  results_H0_true[i] <- mean(sample(x = female_sim,
                                    size = n,
                                    replace = TRUE))
}
# Finally plot the results
hist(results_H0_true, freq = FALSE, main='Sampling Distribution of the Sample Proportion under H_0:p = 0.48', xlab = 'Proportion of Female', ylab = 'Density')
# estimate a normal curve over it - this looks pretty good!
lines(x = seq(.30, .65, .001), dnorm(seq(.30, .65, .001), mean = mean(results_H0_true), sd = sd
                                     (results_H0_true)))
abline(v=p_hat, col="red")
count_of_more_extreme_upper_tail <- sum(results_H0_true >= p_hat)
bootstrap_pvalue <- count_of_more_extreme_upper_tail/num_sims
cat("Bootstrap p-value")
bootstrap_pvalue
cat("Exact Binomial p-value")
binom.test(x = p, n = n, p = p_0, alternative = "greater")$p.value
cat("Normal Approximation p-value")
pnorm(z, lower.tail = FALSE)
qqnorm(data$SystolicBloodPressure, main ="Normality Check for Systolic Blood Pressure Level")
qqline(data$SystolicBloodPressure)
qqnorm(data$SystolicBloodPressure[data$TenYearCoronaryHeartDisease == "Vulnerable"], main ="Normality Check for Systolic Blood Pressure of patients Vulnerable to 10 Year CHD")
qqline(data$SystolicBloodPressure[data$TenYearCoronaryHeartDisease == "Vulnerable"])
qqnorm(data$SystolicBloodPressure[data$TenYearCoronaryHeartDisease == "Immune"], main ="Normality Check for Systolic Blood Pressure of patients Immune to 10 Year CHD")
qqline(data$SystolicBloodPressure[data$TenYearCoronaryHeartDisease == "Immune"])
set.seed(0)
immunePatientsData <- subset(data, data$TenYearCoronaryHeartDisease == "Immune")
immuneDataSample <- immunePatientsData[sample(nrow(immunePatientsData), 300), ]
head(immuneDataSample)
set.seed(0)
vulnerablePatientsData <- subset(data, data$TenYearCoronaryHeartDisease == "Vulnerable")
vulnerableDataSample <- vulnerablePatientsData[sample(nrow(vulnerablePatientsData), 300), ]
head(vulnerableDataSample)
sampleData <- rbind(immuneDataSample, vulnerableDataSample)
head(sampleData)
summary(sampleData)
qqnorm(sampleData$SystolicBloodPressure, main ="Sample Data - Normality Check for Systolic Blood Pressure Level")
qqline(sampleData$SystolicBloodPressure)
qqnorm(sampleData$SystolicBloodPressure[sampleData$TenYearCoronaryHeartDisease == "Vulnerable"], main ="Sample - Data Normality Check for Systolic Blood Pressure of patients Vulnerable to 10 Year CHD")
qqline(sampleData$SystolicBloodPressure[sampleData$TenYearCoronaryHeartDisease == "Vulnerable"])
qqnorm(sampleData$SystolicBloodPressure[sampleData$TenYearCoronaryHeartDisease == "Immune"], main ="Sample Data - Normality Check for Systolic Blood Pressure of patients Immune to 10 Year CHD")
qqline(sampleData$SystolicBloodPressure[sampleData$TenYearCoronaryHeartDisease == "Immune"])
t.test(sampleData$SystolicBloodPressure[sampleData$TenYearCoronaryHeartDisease == "Vulnerable"], sampleData$SystolicBloodPressure[sampleData$TenYearCoronaryHeartDisease == "Immune"])
# Mean Systolic Blood Pressure of Vulnerable Patients
mu_v <- mean(sampleData$SystolicBloodPressure[sampleData$TenYearCoronaryHeartDisease == 'Vulnerable'])
mu_v
# Mean Systolic Blood Pressure of Immune Patients
mu_i <- mean(sampleData$SystolicBloodPressure[sampleData$TenYearCoronaryHeartDisease == 'Immune'])
mu_i
# Null Hypothesis
mu_0 <- 0
# Variance of Systolic Blood Pressure of Vulnerable Patients
var_v <- var(sampleData$SystolicBloodPressure[sampleData$TenYearCoronaryHeartDisease == 'Vulnerable'])
var_v
# Variance of Systolic Blood Pressure of Vulnerable Patients
var_i <- var(sampleData$SystolicBloodPressure[sampleData$TenYearCoronaryHeartDisease == 'Immune'])
var_i
# Sample Size of Systolic Blood Pressure of Vulnerable Patients
n_v <- length(sampleData$SystolicBloodPressure[sampleData$TenYearCoronaryHeartDisease == 'Vulnerable'])
n_v
# Sample Size of Systolic Blood Pressure of Vulnerable Patients
n_i <- length(sampleData$SystolicBloodPressure[sampleData$TenYearCoronaryHeartDisease == 'Immune'])
n_i
# t-value (test statistic)
t <- (mu_v - mu_i - mu_0)/sqrt(var_v/n_v + var_i/n_i)
t
# p-value for 2 sided t-test
p_value <- pt(q = t, df = min(n_v, n_i) - 1, lower.tail = FALSE)*2
p_value
# Lower Boundary of Confidence Interval
lowerBound <- mu_v - mu_i + qt(0.05, min(n_v, n_i) - 1)*sqrt(var_v/n_v + var_i/n_i)
lowerBound
# Upper Boundary of Confidence Interval
upperBound <- mu_v - mu_i + qt(0.95, min(n_v, n_i) - 1)*sqrt(var_v/n_v + var_i/n_i)
upperBound
set.seed(0)
num_sims <- 10000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  mean_immune <- mean(sample(x = sampleData$SystolicBloodPressure[sampleData$TenYearCoronaryHeartDisease == 'Immune'],
                             size = 300,
                             replace = TRUE))
  mean_vulnerable <- mean(sample(x = sampleData$SystolicBloodPressure[sampleData$TenYearCoronaryHeartDisease == 'Vulnerable'],
                                 size = 300,
                                 replace = TRUE))
  results[i] <- mean_vulnerable - mean_immune
}
# Finally plot the results
hist(results, freq = FALSE, main='Sampling Distribution of the Sample Mean', xlab = 'Average Difference Systolic Blood Pressure', ylab = 'Density')
lines(x = seq(9, 21, .01), dnorm(seq(9, 21, .01), mean = mean(results), sd = sd(results)))
# Bootstrap one-sided CI
c(quantile(results, c(.025, .975)))
t.test(sampleData$SystolicBloodPressure[sampleData$TenYearCoronaryHeartDisease == "Vulnerable"], sampleData$SystolicBloodPressure[sampleData$TenYearCoronaryHeartDisease == "Immune"])$conf.int
set.seed(0)
transform(sampleData,Group=sample(TenYearCoronaryHeartDisease))
set.seed(0)
num_sims <- 10000
# A vector to store my results
results_given_H0_true <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  # idea here is if there is no relationshipm we should be able to shuffle the groups
  shuffled_groups <- transform(sampleData,Group=sample(TenYearCoronaryHeartDisease))
  mean_immune <- mean(shuffled_groups$SystolicBloodPressure[shuffled_groups$Group=="Immune"])
  mean_vulnerable <- mean(shuffled_groups$SystolicBloodPressure[shuffled_groups$Group=="Vulnerable"])
  results_given_H0_true[i] <- mean_vulnerable - mean_immune
}
# Finally plot the results
hist(results_given_H0_true, freq = FALSE, main='Dist. of the Diff in Sample Means Under Null', xlab = 'Average Difference Systolic Blood Pressure under Null', ylab = 'Density')
diff_in_sample_means <- mean(sampleData$SystolicBloodPressure[sampleData$TenYearCoronaryHeartDisease=="Vulnerable"]) - mean(sampleData$SystolicBloodPressure[sampleData$TenYearCoronaryHeartDisease=="Immune"])
low_end_extreme <- mean(results_given_H0_true) + (mean(results_given_H0_true) - diff_in_sample_means)
lines(x = seq(-6, 6, .01), dnorm(seq(-6, 6, .01), mean = mean(results_given_H0_true), sd = sd(results_given_H0_true)))
abline(v=diff_in_sample_means, col = "blue")
abline(v=abs(diff_in_sample_means), col = "red")
# counts of values more extreme than the test statistic in our original sample, given H0 is true
# two sided given the alternate hypothesis
count_of_more_extreme_lower_tail <- sum(results_given_H0_true <= low_end_extreme)
count_of_more_extreme_upper_tail <- sum(results_given_H0_true >= diff_in_sample_means)
bootstrap_pvalue <- (count_of_more_extreme_lower_tail + count_of_more_extreme_upper_tail)/num_sims
cat("Bootstrap p-value")
bootstrap_pvalue
cat("t-test p-value")
t.test(sampleData$SystolicBloodPressure[sampleData$TenYearCoronaryHeartDisease == "Vulnerable"], sampleData$SystolicBloodPressure[sampleData$TenYearCoronaryHeartDisease == "Immune"])$p.value
# the parts of the test statistic
# sample props
p_hat_M <- length(data$Gender[data$Gender == "Male" & data$TenYearCoronaryHeartDisease == "Vulnerable"])/length(data$Gender[data$Gender == "Male"])
p_hat_F <- length(data$Gender[data$Gender == "Female" & data$TenYearCoronaryHeartDisease == "Vulnerable"])/length(data$Gender[data$Gender == "Female"])
# null hypothesized population prop difference between the two groups
p_0 <- 0
# sample size
n_M <- length(data$Gender[data$Gender == "Male"])
n_F <- length(data$Gender[data$Gender == "Female"])
# sample variances
den_p_M <- (p_hat_M*(1-p_hat_M))/n_M
den_p_F <- (p_hat_F*(1-p_hat_F))/n_F
# z-test test statistic
z <- (p_hat_M - p_hat_F - p_0)/sqrt(den_p_M + den_p_F)
z
# two sided p-value
two_sided_diff_prop_pval <- pnorm(q = z, lower.tail = FALSE)*2
two_sided_diff_prop_pval
# lower bound
(p_hat_M - p_hat_F)+(qnorm(0.025)*sqrt(den_p_M + den_p_F))
# upper bound
(p_hat_M - p_hat_F)+(qnorm(0.975)*sqrt(den_p_M + den_p_F))
# Make the data
male <- rep(c(1, 0), c(length(data$Gender[data$Gender == "Male" & data$TenYearCoronaryHeartDisease == "Vulnerable"]), n_M - length(data$Gender[data$Gender == "Male" & data$TenYearCoronaryHeartDisease == "Vulnerable"])))
female <- rep(c(1,0), c(length(data$Gender[data$Gender == "Female" & data$TenYearCoronaryHeartDisease == "Vulnerable"]), n_F - length(data$Gender[data$Gender == "Female" & data$TenYearCoronaryHeartDisease == "Vulnerable"])))
num_sims <- 10000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  prop_M <- mean(sample(male,
                        size = n_M,
                        replace = TRUE))
  prop_F <- mean(sample(x = female,
                        size = n_F,
                        replace = TRUE))
  results[i] <- prop_M - prop_F
}
# Finally plot the results
hist(results, freq = FALSE, main='Dist. of the Diff in Prop', xlab = 'Difference in Prop. of Patients Vulnerable to Heart Disease', ylab = 'Density')
lines(x = seq(0.01, 0.13, .001), dnorm(seq(0.01, 0.13, .001), mean = mean(results), sd = sd(results)))
cat("Bootstrap")
c(quantile(results, c(.025, .975)))
cat("Normal Approximation")
c((p_hat_M - p_hat_F)+(qnorm(0.025)*sqrt(den_p_M + den_p_F)), (p_hat_M - p_hat_F)+(qnorm(0.975)*sqrt(den_p_M + den_p_F)))
# Make the data
df_combined <- data.frame("vulnerable_patients" = c(male, female), "gender" = rep(c("male", "female"), c(n_M, n_F)))
# Sanity checks
summary(df_combined$gender)
mean(df_combined$vulnerable_patients[df_combined$gender=="male"]) == p_hat_M
mean(df_combined$vulnerable_patients[df_combined$gender=="female"]) == p_hat_F
num_sims <- 1000
# A vector to store my results
results_given_H0_true <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  # idea here is if there is no relationshipm we should be able to shuffle the groups
  shuffled_groups <- transform(df_combined, gender=sample(gender))
  prop_M <- mean(shuffled_groups$vulnerable_patients[shuffled_groups$gender=="male"
                                                     ])
  prop_F <- mean(shuffled_groups$vulnerable_patients[shuffled_groups$gender=="female"
                                                     ])
  results_given_H0_true[i] <- prop_M - prop_F
}
# Finally plot the results
hist(results_given_H0_true, freq = FALSE,
     main='Dist. of the Diff in Sample Sample Props Under Null',
     xlab = 'Average Difference in Prop. Vulnerable Patients under Null',
     ylab = 'Density')
diff_in_sample_props <- p_hat_M - p_hat_F
lines(x = seq(-0.05, 0.05, .001), dnorm(seq(-0.05, 0.05, .001), mean = mean(results_given_H0_true), sd = sd(results_given_H0_true)))
abline(v=diff_in_sample_props, col = "blue")
abline(v=-diff_in_sample_props, col = "red")
# counts of values more extreme than the test statistic in our original sample, given H0 is true
# two sided given the alternate hypothesis
count_of_more_extreme_lower_tail <- sum(results_given_H0_true <= -diff_in_sample_props)
count_of_more_extreme_upper_tail <- sum(results_given_H0_true >= diff_in_sample_props)
bootstrap_pvalue <- (count_of_more_extreme_lower_tail + count_of_more_extreme_upper_tail)/num_sims
cat("Bootstrap p-value")
bootstrap_pvalue
cat("Normal Approx p-value")
two_sided_diff_prop_pval
chiData <- data$Education
head(chiData)
table(chiData)
prop.table(table(chiData))
n <- 3658
r <- 4
npi <- 914.5
tchi <- sum(((table(chiData) - npi)^2)/npi)
tchi
p_value <- pchisq(tchi, df = r-1, lower.tail = FALSE)
p_value
# Create our data under the assumption that H_0 is true
solutions_under_H_0 <- rep(c("C", "GED", "HS", "VS"), npi)
# Sanity Check
table(solutions_under_H_0)
num_sims <- 10000
# A vector to store my results
chisq_stats_under_H0 <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  new_samp <- sample(solutions_under_H_0, n, replace = T)
  chisq_stats_under_H0[i] <- sum(((table(new_samp) - npi)^2)/npi)
}
hist(chisq_stats_under_H0, freq = FALSE,
     main='Dist. of the Chi-Square Statistic Under Null',
     xlab = 'Chi-Square Stat under Null',
     ylab = 'Density')
abline(v=sum(((table(chiData) - npi)^2)/npi), col="red")
#The randomization p-value
sum(chisq_stats_under_H0 >= sum(((table(chiData) - npi)^2)/npi))/num_sims
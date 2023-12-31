## PY6009 Project

# Set libraries
library(tidyverse)
library(psych)
library(readxl)
library(ggplot2)
library(dplyr)
library(gt)
library(car)

# Dataset: McBride et al. (2020), C19PRC_Extracted_Data_Project(Juna Park)_W1only.xlsb
file_path2 <- "C:/Users/kinu1/Documents/PSY6009_Project_Juna_Park/Dataset/C19PRC_Extracted_Data_Project_W1only.xlsx"
data2 <- read_excel(file_path2)

# Check alpha coefficients of items
# religiosity
religiosity_items2 <- data2[, grep("^REL", colnames(data2))]
alpha_religiosity2 <- alpha(religiosity_items2)
print(alpha_religiosity2)

# atheism
atheism_items2 <- data2[, grep("^ATH", colnames(data2))]
alpha_atheism2 <- alpha(atheism_items2)
print(alpha_atheism2)

# cognitive reflection
crt_items2 <- data2[, grep("^CRT", colnames(data2))]
alpha_crt2 <- alpha(crt_items2)
print(alpha_crt2)

# anxiety
anx_items2 <- data2[, grep("^GAD", colnames(data2))]
alpha_anx2 <- alpha(anx_items2)
print(alpha_anx2)

# depression
dep_items2 <- data2[, grep("^PHQ", colnames(data2))]
alpha_dep2 <- alpha(dep_items2)
print(alpha_dep2)

# death anxiety
da_items2 <- data2[, grep("^DA", colnames(data2))]
alpha_da2 <- alpha(da_items2)
print(alpha_da2)

# Before analysis, run median split on the participants based on their religiosity scores
data_split2 <- data2 %>%
  mutate(
    Religiosity_Group = if_else(
      (REL1 + REL2 + REL3 + REL4) > median(REL1 + REL2 + REL3 + REL4, na.rm = TRUE), "High", "Low"),
    Atheism_Group = if_else(
      (ATH1 + ATH2 + ATH3 + ATH4) > median(ATH1 + ATH2 + ATH3 + ATH4, na.rm = TRUE), "High", "Low"))

# Create 4 groups based on the obtained scores
data_split2 <- data_split2 %>%
  mutate(Rel_Athe_Group = case_when(
    Religiosity_Group == "High" & Atheism_Group == "Low" ~ "Religious",
    Religiosity_Group == "Low" & Atheism_Group == "High" ~ "Atheist",
    Religiosity_Group == "High" & Atheism_Group == "High" ~ "Agnostic",
    Religiosity_Group == "Low" & Atheism_Group == "Low" ~ "Apatheist",
    TRUE ~ NA_character_))

# Number and percentage of individuals in each group
group_counts2 <- data_split2 %>%
  count(Rel_Athe_Group) %>%
  mutate(Percentage = n / sum(n) * 100)

print(group_counts2)

# Group the data by the new categorical group for another descriptive stats
grouped_stats2 <- data_split2 %>%
  group_by(Rel_Athe_Group) %>%
  summarize(
    Mean_REL = mean(REL1 + REL2 + REL3 + REL4, na.rm = TRUE),    # Calculate mean for religiosity scores
    SD_REL = sd(REL1 + REL2 + REL3 + REL4, na.rm = TRUE),        # Calculate standard deviation for religiosity scores
    Median_REL = median(REL1 + REL2 + REL3 + REL4, na.rm = TRUE),  # Calculate median for religiosity scores
    Mean_ATH = mean(ATH1 + ATH2 + ATH3 + ATH4, na.rm = TRUE), # same process for atheism scores
    SD_ATH = sd(ATH1 + ATH2 + ATH3 + ATH4, na.rm = TRUE),
    Median_ATH = median(ATH1 + ATH2 + ATH3 + ATH4, na.rm = TRUE))

# Print the grouped statistics
print(grouped_stats2)

# Calculate the ratio of male and female within each group
sex_ratio2 <- data_split2 %>%
  group_by(Rel_Athe_Group) %>%
  summarize(
    Male_Count = sum(Gender == 1, na.rm = TRUE),     
    Female_Count = sum(Gender == 2, na.rm = TRUE)    
  ) %>%
  mutate(
    Total_Count = Male_Count + Female_Count,      # Total count of males and females
    Male_Ratio = Male_Count / Total_Count,         # Ratio of males
    Female_Ratio = Female_Count / Total_Count      # Ratio of females
  )

# Print the sex ratio statistics
print(sex_ratio2)


### ------------------------------------------- ANALYSIS -------------------------------------------------
## Analysis of Dataset
# Hypothesis 1: Agnostics will have higher educational achievement compared to the other groups. 
#               The apathists will have lower educational achievement than the other groups.

# Dichotomize the educational level of participants into 'Graduate' and 'Non-graduate'.
# Graduate will include 'undergraduate' or higher (4 to 8 in scale). 
# Non-graduate will include 'A-level or similar' or lower (1 to 3 in scale).
data_split2 <- data_split2 %>%
  mutate(Educational_Group = if_else(Edu >= 14, "Graduate", "Non-graduate"))

# Number and percentage of individuals in each group
group_counts_Edu2 <- data_split2 %>%
  count(Educational_Group) %>%
  mutate(Percentage = n / sum(n) * 100)

print(group_counts_Edu2)


# Create a contingency table of 'Educational_Group' and 'Rel_Athe_Group'
contingency_table2 <- table(data_split2$Educational_Group, data_split2$Rel_Athe_Group)

# Run the chi-square test
chi_square2 <- chisq.test(contingency_table2)

# Print the results
print(chi_square2)


# Hypothesis 2: Agnostics will score higher than the other groups on analytic reasoning, as measured by 
#               the Cognitive Reflection Task which is assumed to be a measure of thoughtful reflection. 


# is there age differnce between belief groups? based on assumption of religious = conservative = older
# ANOVA for check

# Create a boxplot to visualize the distribution of age across belief groups
boxplot(Age ~ Rel_Athe_Group, data = data_split2)

# Perform one-way ANOVA
AGE_Diff2 <- aov(Age ~ Rel_Athe_Group, data = data_split2)

# Print the ANOVA table
print(summary(AGE_Diff2))

# Post hoc (TukeyHSD) for the result
posthoc_results2 <- TukeyHSD(AGE_Diff2)

# View the results
print(posthoc_results2)

# Using aggregate() function
belief_group_stats2 <- aggregate(Age ~ Rel_Athe_Group, data = data_split2, FUN = function(x) c(Mean = mean(x), SD = sd(x)))

# View the results
print(belief_group_stats2)


# Thus, there is a significant difference in age between the different belief groups.


# Belief iv, CRT dv, age covariate - ancova
# The performance of participants on CRT
data_split2 <- data_split2 %>%
  mutate(CRTtot = CRT1 + CRT2 + CRT3 + CRT4 + CRT5)

# ANCOVA: IV (Belief type), DV (CRT),  Covariate (AGE)
hyp2_ANCOVA2 <- lm(CRTtot ~ Rel_Athe_Group + Age, data = data_split2)

# Print the ANCOVA summary
summary(hyp2_ANCOVA2)



#Two-way ANOVA with SEX
hyp2_ANOVA2 <- aov(CRTtot ~ Rel_Athe_Group + Gender, data = data_split2)

# Print the ANOVA table
summary(hyp2_ANOVA2)

# Post hoc test for Belief Group (Rel_Athe_Group)
tukey_test2_hyp2 <- TukeyHSD(aov(CRTtot ~ Rel_Athe_Group, data = data_split2))
print(tukey_test2_hyp2)

# Post hoc test for Sex (SEX)
t_test2_hyp2 <- t.test(CRTtot ~ Gender, data = data_split2)
print(t_test2_hyp2)


# Interaction between Gender and Rel_Athe_Group

# Perform the two-way ANOVA with interaction
model2 <- lm(CRTtot ~ Rel_Athe_Group * Gender, data = data_split2)

# Obtain the significance of the interaction
interaction_anova2 <- Anova(model2, type = "III")
print(interaction_anova2)

# Get the model summary to find R-squared
model_summary2 <- summary(model2)
r_squared2 <- model_summary2$r.squared

# Print the R-squared value
print(paste("R-squared (Multiple R-squared):", round(r_squared2, 4)))

# Calculate mean, standard deviation, and 95% CI for CRT by belief type only
summary_table_crt2 <- data_split2 %>%
  group_by(Rel_Athe_Group) %>%
  summarize(mean_value = mean(CRTtot),
            sd_value = sd(CRTtot),
            lower_ci = mean_value - qt(0.975, df = n() - 1) * (sd_value / sqrt(n())),
            upper_ci = mean_value + qt(0.975, df = n() - 1) * (sd_value / sqrt(n())))

# Print the summarized table
print(summary_table_crt2)


# Hypothesis 3: Agnostics will score higher than the other groups on death anxiety.

#Total death anxiety
data_split2 <- data_split2 %>%
  mutate(DAtot = DA1 + DA2 + DA3 + DA4 + DA5 + DA6 + DA7 + DA8 + 
           DA9 + DA10 + DA11 + DA12 + DA13 + DA14 + DA15 + DA16 + DA17)

#Two-way ANOVA with SEX
hyp3_ANOVA2 <- aov(DAtot ~ Rel_Athe_Group + Gender, data = data_split2)

# Print the ANOVA table
summary(hyp3_ANOVA2)

# Post hoc test for Belief Group (Rel_Athe_Group)
tukey_test2_hyp3 <- TukeyHSD(aov(DAtot ~ Rel_Athe_Group, data = data_split2))
print(tukey_test2_hyp3)

# Post hoc test for Sex (SEX)
t_test2_hyp3 <- t.test(DAtot ~ Gender, data = data_split2)
print(t_test2_hyp3)


# Perform the two-way ANOVA with interaction
model2_2 <- lm(DAtot ~ Rel_Athe_Group * Gender, data = data_split2)

# Obtain the significance of the interaction
interaction_anova2_2 <- Anova(model2_2, type = "III")
print(interaction_anova2_2)

# Get the model summary to find R-squared
model_summary2_2 <- summary(model2_2)
r_squared2_2 <- model_summary2_2$r.squared

# Print the R-squared value
print(paste("R-squared (Multiple R-squared):", round(r_squared2_2, 4)))


# Calculate mean, standard deviation, and 95% CI for death anxiety by belief type only
summary_table_da2 <- data_split2 %>%
  group_by(Rel_Athe_Group) %>%
  summarize(mean_value = mean(DAtot),
            sd_value = sd(DAtot),
            lower_ci = mean_value - qt(0.975, df = n() - 1) * (sd_value / sqrt(n())),
            upper_ci = mean_value + qt(0.975, df = n() - 1) * (sd_value / sqrt(n())))

# Print the summarized table
print(summary_table_da2)


#ANCOVA with age
hyp3_ANCOVA3 <- lm(DAtot ~ Rel_Athe_Group + Age, data = data_split2)

# Print the ANCOVA summary
summary(hyp3_ANCOVA3)


# Hypothesis 4: Agnostics will show higher levels of depression as assessed by the PhQ9. Also, they will show
#               higher levels of anxiety as assessed by the GAD7

#Total depression
data_split2 <- data_split2 %>%
  mutate(PHQ9tot = PHQ1 + PHQ2 + PHQ3 + PHQ4 + PHQ5 + PHQ6 + PHQ7 + PHQ8 + PHQ9)

#Two-way ANOVA with SEX
hyp4_ANOVA2 <- aov(PHQ9tot ~ Rel_Athe_Group + Gender, data = data_split2)

# Print the ANOVA table
summary(hyp4_ANOVA2)


# Post hoc test for Belief Group (Rel_Athe_Group)
tukey_test2_hyp4 <- TukeyHSD(aov(PHQ9tot ~ Rel_Athe_Group, data = data_split2))
print(tukey_test2_hyp4)

# Post hoc test for Sex (SEX)
t_test2_hyp4 <- t.test(PHQ9tot ~ Gender, data = data_split2)
print(t_test2_hyp4)


# Perform the two-way ANOVA with interaction
model2_3 <- lm(PHQ9tot ~ Rel_Athe_Group * Gender, data = data_split2)

# Obtain the significance of the interaction
interaction_anova2_3 <- Anova(model2_3, type = "III")
print(interaction_anova2_3)

# Get the model summary to find R-squared
model_summary2_3 <- summary(model2_3)
r_squared2_3 <- model_summary2_3$r.squared

# Print the R-squared value
print(paste("R-squared (Multiple R-squared):", round(r_squared2_3, 4)))


# Calculate mean, standard deviation, and 95% CI for depression by belief type only
summary_table_dep2 <- data_split2 %>%
  group_by(Rel_Athe_Group) %>%
  summarize(mean_value = mean(PHQ9tot),
            sd_value = sd(PHQ9tot),
            lower_ci = mean_value - qt(0.975, df = n() - 1) * (sd_value / sqrt(n())),
            upper_ci = mean_value + qt(0.975, df = n() - 1) * (sd_value / sqrt(n())))

# Print the summarized table
print(summary_table_dep2)


#ANCOVA with age
hyp4_ANCOVA2 <- lm(PHQ9tot ~ Rel_Athe_Group + Age, data = data_split2)

# Print the ANCOVA summary
summary(hyp4_ANCOVA2)




##Total anxiety
data_split2 <- data_split2 %>%
  mutate(GAD7tot = GAD1 + GAD2 + GAD3 + GAD4 + GAD5 + GAD6 + GAD7)

#Two-way ANOVA with SEX
hyp4_ANOVA2_2 <- aov(GAD7tot ~ Rel_Athe_Group + Gender, data = data_split2)

# Print the ANOVA table
summary(hyp4_ANOVA2_2)

# Post hoc test for Belief Group (Rel_Athe_Group)
tukey_test2_hyp4_2 <- TukeyHSD(aov(GAD7tot ~ Rel_Athe_Group, data = data_split2))
print(tukey_test2_hyp4_2)

# Post hoc test for Sex (SEX)
t_test2_hyp4_2 <- t.test(GAD7tot ~ Gender, data = data_split2)
print(t_test2_hyp4_2)


# Perform the two-way ANOVA with interaction
model2_4 <- lm(GAD7tot ~ Rel_Athe_Group * Gender, data = data_split2)

# Obtain the significance of the interaction
interaction_anova2_4 <- Anova(model2_4, type = "III")
print(interaction_anova2_4)

# Get the model summary to find R-squared
model_summary2_4 <- summary(model2_4)
r_squared2_4 <- model_summary2_4$r.squared

# Print the R-squared value
print(paste("R-squared (Multiple R-squared):", round(r_squared2_4, 4)))


# Calculate mean, standard deviation, and 95% CI for anxiety by belief type only
summary_table_anx2 <- data_split2 %>%
  group_by(Rel_Athe_Group) %>%
  summarize(mean_value = mean(GAD7tot),
            sd_value = sd(GAD7tot),
            lower_ci = mean_value - qt(0.975, df = n() - 1) * (sd_value / sqrt(n())),
            upper_ci = mean_value + qt(0.975, df = n() - 1) * (sd_value / sqrt(n())))

# Print the summarized table
print(summary_table_anx2)



#ANCOVA with age
hyp4_ANCOVA2_2 <- lm(GAD7tot ~ Rel_Athe_Group + Age, data = data_split2)

# Print the ANCOVA summary
summary(hyp4_ANCOVA2_2)




# Hypothesis 5: The C19PRC dataset has measures of the big-5 personality traits. 
#               We do not have firm predictions about performance on these measures
#               but tentatively predict that agnostics will be high in neuroticism, conscientiousness 
#               and openness. We do not expect to see group differences in extroversion and agreeableness.

# Openness

# is there age differnce between belief groups? based on assumption of religious = conservative = older
# ANOVA for check

# Create a boxplot to visualize the distribution of Openness trait across belief groups
boxplot(Open ~ Rel_Athe_Group, data = data_split2)

# Perform one-way ANOVA
Open_Diff <- aov(Open ~ Rel_Athe_Group, data = data_split2)

# Print the ANOVA table
print(summary(Open_Diff))


#Two-way ANOVA with SEX
hyp5_ANOVA1 <- aov(Open ~ Rel_Athe_Group + Gender, data = data_split2)

# Print the ANOVA table
summary(hyp5_ANOVA1)


#ANCOVA with age
hyp5_ANCOVA1 <- lm(Open ~ Rel_Athe_Group + Age, data = data_split2)

# Print the ANCOVA summary
summary(hyp5_ANCOVA1)


# Conscientiousness
# Create a boxplot to visualize the distribution of Conscientiousness trait across belief groups
boxplot(Consci ~ Rel_Athe_Group, data = data_split2)

# Perform one-way ANOVA
Consci_Diff <- aov(Consci ~ Rel_Athe_Group, data = data_split2)

# Print the ANOVA table
print(summary(Consci_Diff))


#Two-way ANOVA with SEX
hyp5_ANOVA2 <- aov(Consci ~ Rel_Athe_Group + Gender, data = data_split2)

# Print the ANOVA table
summary(hyp5_ANOVA2)



#ANCOVA with age
hyp5_ANCOVA2 <- lm(Consci ~ Rel_Athe_Group + Age, data = data_split2)

# Print the ANCOVA summary
summary(hyp5_ANCOVA2)



# Extrovert
# Create a boxplot to visualize the distribution of Extrovert trait across belief groups
boxplot(Extro ~ Rel_Athe_Group, data = data_split2)

# Perform one-way ANOVA
Extro_Diff <- aov(Extro ~ Rel_Athe_Group, data = data_split2)

# Print the ANOVA table
print(summary(Extro_Diff))


#Two-way ANOVA with SEX
hyp5_ANOVA3 <- aov(Extro ~ Rel_Athe_Group + Gender, data = data_split2)

# Print the ANOVA table
summary(hyp5_ANOVA3)


#ANCOVA with age
hyp5_ANCOVA3 <- lm(Extro ~ Rel_Athe_Group + Age, data = data_split2)

# Print the ANCOVA summary
summary(hyp5_ANCOVA3)



# Agreeableness
# Create a boxplot to visualize the distribution of Agreeableness trait across belief groups
boxplot(Agree ~ Rel_Athe_Group, data = data_split2)

# Perform one-way ANOVA
Agree_Diff <- aov(Agree ~ Rel_Athe_Group, data = data_split2)

# Print the ANOVA table
print(summary(Agree_Diff))


#Two-way ANOVA with SEX
hyp5_ANOVA4 <- aov(Agree ~ Rel_Athe_Group + Gender, data = data_split2)

# Print the ANOVA table
summary(hyp5_ANOVA4)


#ANCOVA with age
hyp5_ANCOVA4 <- lm(Agree ~ Rel_Athe_Group + Age, data = data_split2)

# Print the ANCOVA summary
summary(hyp5_ANCOVA4)



# Neuroticism
# Create a boxplot to visualize the distribution of Neuroticism trait across belief groups
boxplot(Neuro ~ Rel_Athe_Group, data = data_split2)

# Perform one-way ANOVA
Neuro_Diff <- aov(Neuro ~ Rel_Athe_Group, data = data_split2)

# Print the ANOVA table
print(summary(Neuro_Diff))


#Two-way ANOVA with SEX
hyp5_ANOVA5 <- aov(Neuro ~ Rel_Athe_Group + Gender, data = data_split2)

# Print the ANOVA table
summary(hyp5_ANOVA5)


#ANCOVA with age
hyp5_ANCOVA5 <- lm(Neuro ~ Rel_Athe_Group + Age, data = data_split2)

# Print the ANCOVA summary
summary(hyp5_ANCOVA5)


# --------------------------- Further Analysis--------------------------------
# Moderated mediation using PROCESS macro model 7

# get IV: religiosity and DV: atheism
data_split2 <- data_split2 %>%
  mutate(RELtot = REL1 + REL2 + REL3 + REL4)

data_split2 <- data_split2 %>%
  mutate(ATHtot = ATH1 + ATH2 + ATH3 + ATH4)

# Depression (PHQ9) with a covariate AGE, and death anxiety as the first stage moderator
process(data = data_split2, y = "PHQ9tot", x = "RELtot", m = "DAtot", w = "ATHtot", model=7, conf = 95,
        boot = 1000, seed = 12345, cov = c("Age"), center=2, plot = 1, intprobe = 1)

# Anxiety (GAD7) with a covariate AGE, and death anxiety as the first stage moderator
process(data = data_split2, y = "GAD7tot", x = "RELtot", m = "DAtot", w = "ATHtot", model=7, conf = 95,
        boot = 1000, seed = 12345, cov = c("Age"), center=2, plot = 1, intprobe = 1)


# --------------------------- Descriptive ------------------------------------

# Generate descriptive statistics for the selected variables
descriptive_stats2 <- data_split2 %>%
  select(Age, Edu, RELtot, ATHtot, CRTtot, PHQ9tot, GAD7tot, DAtot, Open, Consci, Extro, Agree, Neuro) %>%
  summary(descriptive_stats2)

# Print the descriptive statistics
print(descriptive_stats2)

## PY6009 Project

# Set libraries
library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
library(gt)
library(car)

# Dataset: Alsuhibani et al., 2021_Extracted_Data_Project(Juna Park).xlsb
file_path1 <- "C:/Users/kinu1/Documents/PSY6009_Project_Juna_Park/dataset/Alsuhibani et al., 2021_Extracted_Data_Project(Juna Park).xlsx"
data1 <- read_excel(file_path1)

# Before analysis, run median split on the participants based on their religiosity scores
data_split <- data1 %>%
  mutate(
    Religiosity_Group = if_else(
      (REL1 + REL2 + REL5 + REL6 + REL7 + REL11 + REL13 + REL14 + REL16 + REL17 + REL18) > 
        median(REL1 + REL2 + REL5 + REL6 + REL7 + REL11 + REL13 + REL14 + REL16 + REL17 + REL18, na.rm = TRUE),
      "High", "Low"),
    Atheism_Group = if_else(
      (ATH3 + ATH4 + ATH8 + ATH9 + ATH10 + ATH12 + ATH15) > 
        median(ATH3 + ATH4 + ATH8 + ATH9 + ATH10 + ATH12 + ATH15, na.rm = TRUE),
      "High", "Low"))

# Create 4 groups based on the obtained scores
data_split <- data_split %>%
    mutate(Rel_Athe_Group = case_when(
      Religiosity_Group == "High" & Atheism_Group == "Low" ~ "Religious",
      Religiosity_Group == "Low" & Atheism_Group == "High" ~ "Atheist",
      Religiosity_Group == "High" & Atheism_Group == "High" ~ "Agnostic",
      Religiosity_Group == "Low" & Atheism_Group == "Low" ~ "Apatheist",
      TRUE ~ NA_character_))

# Number and percentage of individuals in each group
group_counts <- data_split %>%
  count(Rel_Athe_Group) %>%
  mutate(Percentage = n / sum(n) * 100)

print(group_counts)


# Group the data by the new four groups
grouped_stats <- data_split %>%
  group_by(Rel_Athe_Group) %>%
  summarize(
    Mean_REL = mean(REL1 + REL2 + REL5 + REL6 + REL7 + REL11 + REL13 + REL14 + REL16 + REL17 + REL18, na.rm = TRUE),    # Calculate mean for religiosity scores
    SD_REL = sd(REL1 + REL2 + REL5 + REL6 + REL7 + REL11 + REL13 + REL14 + REL16 + REL17 + REL18, na.rm = TRUE),        # Calculate standard deviation for religiosity scores
    Median_REL = median(REL1 + REL2 + REL5 + REL6 + REL7 + REL11 + REL13 + REL14 + REL16 + REL17 + REL18, na.rm = TRUE),  # Calculate median for religiosity scores
    Mean_ATH = mean(ATH3 + ATH4 + ATH8 + ATH9 + ATH10 + ATH12 + ATH15, na.rm = TRUE),    # Calculate mean for atheism scores
    SD_ATH = sd(ATH3 + ATH4 + ATH8 + ATH9 + ATH10 + ATH12 + ATH15, na.rm = TRUE),        # Calculate standard deviation for atheism scores
    Median_ATH = median(ATH3 + ATH4 + ATH8 + ATH9 + ATH10 + ATH12 + ATH15, na.rm = TRUE)  # Calculate median for atheism scores
  )

# Print the grouped statistics
print(grouped_stats)


# Calculate the ratio of male and female within each group
sex_ratio <- data_split %>%
  group_by(Rel_Athe_Group) %>%
  summarize(
    Male_Count = sum(SEX == 0, na.rm = TRUE),     # Count of males (Sex == 0)
    Female_Count = sum(SEX == 1, na.rm = TRUE)    # Count of females (Sex == 1)
  ) %>%
  mutate(
    Total_Count = Male_Count + Female_Count,      # Total count of males and females
    Male_Ratio = Male_Count / Total_Count,         # Ratio of males
    Female_Ratio = Female_Count / Total_Count      # Ratio of females
  )

# Print the sex ratio statistics
print(sex_ratio)



### ------------------------------------------- ANALYSIS -------------------------------------------------
## Analysis of Dataset
# Hypothesis 1: Agnostics will have higher educational achievement (available in both datasets) 
#               compared to the other groups. The apathists will have lower educational achievement 
#               than the other groups. This hypothesis is tentative and based on the assumption 
#               that people who hold agnoistic (combined religious and atheist) beliefs will be 
#               more intellectually engaged with issues surrounding religiosity.


# Dichotomize the educational level of participants into 'Graduate' and 'Non-graduate'.
# Graduate will include 'undergraduate' or higher (4 to 8 in scale). 
# Non-graduate will include 'A-level or similar' or lower (1 to 3 in scale).
data_split <- data_split %>%
  mutate(Educational_Group = if_else(QUAL >= 4, "Graduate", "Non-graduate"))

# Number and percentage of individuals in each group
group_counts_Edu <- data_split %>%
  count(Educational_Group) %>%
  mutate(Percentage = n / sum(n) * 100)

print(group_counts_Edu)


# Create a contingency table of 'Educational_Group' and 'Rel_Athe_Group'
contingency_table <- table(data_split$Educational_Group, data_split$Rel_Athe_Group)

# Run the chi-square test
chi_square <- chisq.test(contingency_table)

# Print the results
print(chi_square)



# Hypothesis 2: Agnostics will score higher than the other groups on analytic reasoning, as measured by 
#               the Cognitive Reflection Task (available in both datasets) which is assumed to be a measure 
#               of thoughtful reflection. Previous research has shown that both atheists and agnostics tend to 
#               score high on this measure, but the relevant studies did not distinguish between these 
#               two kinds of disbelief (Pennycook et al. 2016).


# is there age differnce between belief groups? based on assumption of religious = conservative = older
# ANOVA for check

# Create a boxplot to visualize the distribution of age across belief groups
boxplot(AGE ~ Rel_Athe_Group, data = data_split)

# Perform one-way ANOVA
AGE_Diff <- aov(AGE ~ Rel_Athe_Group, data = data_split)

# Print the ANOVA table
print(summary(AGE_Diff))

# Post hoc (TukeyHSD) for the result
posthoc_results <- TukeyHSD(AGE_Diff)

# View the results
print(posthoc_results)

# Using aggregate() function
belief_group_stats <- aggregate(AGE ~ Rel_Athe_Group, data = data_split, FUN = function(x) c(Mean = mean(x), SD = sd(x)))

# View the results
print(belief_group_stats)



# View the results
print(belief_group_stats)



# Belief iv, CRT dv, age covariate - ancova
# The performance of participants on CRT
data_split <- data_split %>%
  mutate(CRTtot = CRT01 + CRT02 + CRT03 + CRT04 + CRT05 +
           CRT06 + CRT07 + CRT08 + CRT09 + CRT10)

# ANCOVA: IV (Belief type), DV (CRT),  Covariate (AGE)
hyp2_ANCOVA <- lm(CRTtot ~ Rel_Athe_Group + AGE, data = data_split)

# Print the ANCOVA summary
summary(hyp2_ANCOVA)



#Two-way ANOVA with SEX
hyp2_ANOVA <- aov(CRTtot ~ Rel_Athe_Group + SEX, data = data_split)

# Print the ANOVA table
summary(hyp2_ANOVA)

# Post hoc test for Belief Group (Rel_Athe_Group)
tukey_test1_hyp2 <- TukeyHSD(aov(CRTtot ~ Rel_Athe_Group, data = data_split))
print(tukey_test1_hyp2)

# Post hoc test for Sex (SEX)
t_test1_hyp2 <- t.test(CRTtot ~ SEX, data = data_split)
print(t_test1_hyp2)


# Interaction between sex and Rel_Athe_Group

# Perform the two-way ANOVA with interaction
model <- lm(CRTtot ~ Rel_Athe_Group * SEX, data = data_split)

# Obtain the significance of the interaction
interaction_anova <- Anova(model, type = "III")
print(interaction_anova)


# Perform the two-way ANOVA with interaction
model <- lm(CRTtot ~ Rel_Athe_Group * SEX, data = data_split)

# Get the model summary to find R-squared
model_summary1_1 <- summary(model)
r_squared1_1 <- model_summary1_1$r.squared

# Print the R-squared value
print(paste("R-squared (Multiple R-squared):", round(r_squared1_1, 4)))


# Hypothesis 3: Agnostics will score higher than the other groups on death anxiety (available in both datasets).
#               This hypothesis is based on previous research showing that people who score low or high on 
#               conventional measures of religiosity tend to have low death anxiety (Jong et al. 2018).

#Total death anxiety
data_split <- data_split %>%
  mutate(DAtot = DTHANX01 + DTHANX02 + DTHANX03 + DTHANX04 + DTHANX05 + DTHANX06 + DTHANX07 + DTHANX08 + 
           DTHANX09 + DTHANX10 + DTHANX11 + DTHANX12 + DTHANX13 + DTHANX14 + DTHANX15 + DTHANX16 + DTHANX17)

#Two-way ANOVA with SEX
hyp3_ANOVA <- aov(DAtot ~ Rel_Athe_Group + SEX, data = data_split)

# Print the ANOVA table
summary(hyp3_ANOVA)

# Post hoc test for Belief Group (Rel_Athe_Group)
tukey_test1_hyp3 <- TukeyHSD(aov(DAtot ~ Rel_Athe_Group, data = data_split))
print(tukey_test1_hyp3)

# Post hoc test for Sex (SEX)
t_test1_hyp3 <- t.test(DAtot ~ SEX, data = data_split)
print(t_test1_hyp3)


# Interaction between Sex and Rel_Athe_Group

# Perform the two-way ANOVA with interaction
model_2 <- lm(DAtot ~ Rel_Athe_Group * SEX, data = data_split)

# Obtain the significance of the interaction
interaction_anova_2 <- Anova(model_2, type = "III")
print(interaction_anova_2)

# Get the model summary to find R-squared
model_summary_2 <- summary(model_2)
r_squared_2 <- model_summary_2$r.squared

# Print the R-squared value
print(paste("R-squared (Multiple R-squared):", round(r_squared_2, 4)))


#ANCOVA with age
hyp3_ANCOVA <- lm(DAtot ~ Rel_Athe_Group + AGE, data = data_split)

# Print the ANCOVA summary
summary(hyp3_ANCOVA)



# Hypothesis 4: Agnostics will show higher levels of depression as assessed by the PhQ9.
#               This hypothesis is based on the assumption that agnostics experience a conflict between 
#               two belief systems (religious and atheist).

#Total depression
data_split <- data_split %>%
  mutate(PHQ9tot = DEPPHQ1 + DEPPHQ2 + DEPPHQ3 + DEPPHQ4 + DEPPHQ5 + DEPPHQ6 + DEPPHQ7 + DEPPHQ8 + DEPPHQ9)

#Two-way ANOVA with SEX
hyp4_ANOVA <- aov(PHQ9tot ~ Rel_Athe_Group + SEX, data = data_split)

# Print the ANOVA table
summary(hyp4_ANOVA)

# Post hoc test for Belief Group (Rel_Athe_Group)
tukey_test1_hyp4 <- TukeyHSD(aov(PHQ9tot ~ Rel_Athe_Group, data = data_split))
print(tukey_test1_hyp4)

# Post hoc test for Sex (SEX)
t_test1_hyp4 <- t.test(PHQ9tot ~ SEX, data = data_split)
print(t_test1_hyp4)


# Interaction between Sex and Rel_Athe_Group

# Perform the two-way ANOVA with interaction
model_3 <- lm(PHQ9tot ~ Rel_Athe_Group * SEX, data = data_split)

# Obtain the significance of the interaction
interaction_anova_3 <- Anova(model_3, type = "III")
print(interaction_anova_3)

# Get the model summary to find R-squared
model_summary_3 <- summary(model_3)
r_squared_3 <- model_summary_3$r.squared

# Print the R-squared value
print(paste("R-squared (Multiple R-squared):", round(r_squared_3, 4)))



#ANCOVA with age
hyp4_ANCOVA <- lm(PHQ9tot ~ Rel_Athe_Group + AGE, data = data_split)

# Print the ANCOVA summary
summary(hyp4_ANCOVA)



# --------------------------- Further Analysis--------------------------------
# Moderated mediation using PROCESS macro model 7

# get IV: religiosity and DV: atheism
data_split <- data_split %>%
  mutate(RELtot = REL1 + REL2 + REL5 + REL6 + REL7 + REL11 + REL13 + REL14 + REL16 + REL17 + REL18)

data_split <- data_split %>%
  mutate(ATHtot = ATH3 + ATH4 + ATH8 + ATH9 + ATH10 + ATH12 + ATH15)

# Depression (PHQ9) with a covariate AGE, and death anxiety as the first stage moderator
process(data = data_split, y = "PHQ9tot", x = "RELtot", m = "DAtot", w = "ATHtot", model=7, conf = 95,
        boot = 1000, seed = 12345, cov = c("AGE"), center=2, plot = 1, intprobe = 1)


# --------------------------- Descriptive ------------------------------------

# Generate descriptive statistics for the selected variables
descriptive_stats <- data_split %>%
  select(AGE, QUAL, RELtot, ATHtot, CRTtot, PHQ9tot, DAtot) %>%
  summary(descriptive_stats)

# Print the descriptive statistics
print(descriptive_stats)



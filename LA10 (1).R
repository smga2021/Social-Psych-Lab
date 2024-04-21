library(dplyr)

#Are people who express more anti-Fat bias more likely to also exhibit 
#more sexism?

#add each ppt score for AFA
AFA_columns_to_sum <- c("AFA1", "AFA2", "AFA3", "AFA4", "AFA5", "AFA6", "AFA7",
                    "AFA8", "AFA9", "AFA10", "AFA11", "AFA12", "AFA13")

# Add composite score and count of non-missing values for AFA
SPD_S24_1_ <- SPD_S24_1_ %>%
  mutate(
    AFA_Composite_Score = rowSums(select(., all_of(AFA_columns_to_sum)), na.rm = TRUE),
    AFA_Items_Answered = rowSums(!is.na(select(., all_of(AFA_columns_to_sum))))
  )

# Divide composite score by number of items actually answered for AFA
SPD_S24_1_$AFA_Composite_Score <- SPD_S24_1_$AFA_Composite_Score / SPD_S24_1_$AFA_Items_Answered

# Repeat for Sexism

Sexism_columns_to_sum <- c("Sexism1", "Sexism2", "Sexism3", "Sexism4", "Sexism5",
                           "Sexism6", "Sexism7", "Sexism8", "Sexism9", "Sexism10",
                           "Sexism11", "Sexism12", "Sexism13", "Sexism14", "Sexism15",
                           "Sexism16", "Sexism17", "Sexism18", "Sexism19", "Sexism20",
                           "Sexism21", "Sexism22")

# Add composite score and count of non-missing values for Sexism
SPD_S24_1_ <- SPD_S24_1_ %>%
  mutate(
    Sexism_Composite_Score = rowSums(select(., all_of( Sexism_columns_to_sum)), na.rm = TRUE),
    Sexism_Items_Answered = rowSums(!is.na(select(., all_of( Sexism_columns_to_sum))))
  )

# Divide composite score by number of items actually answered for Sexism
SPD_S24_1_$Sexism_Composite_Score <- SPD_S24_1_$Sexism_Composite_Score / SPD_S24_1_$Sexism_Items_Answered

# Pearson correlation test between AFA and Sexism
cor_test <- cor.test(SPD_S24_1_$AFA_Composite_Score, SPD_S24_1_$Sexism_Composite_Score)
print(cor_test)

#means and standard deviations for both
group_summary <- SPD_S24_1_ %>%
  summarize(
    Mean_AFA = mean(AFA_Composite_Score, na.rm = TRUE),
    SD_AFA = sd(AFA_Composite_Score, na.rm = TRUE),
    Mean_Sexism = mean(Sexism_Composite_Score, na.rm = TRUE),
    SD_Sexism = sd(Sexism_Composite_Score, na.rm = TRUE)
  )

print(group_summary)


#Is there a relationship between people’s Anti-Muslim bias and their transphobia?

# Repeat for Anti-Muslim bias

Anti_Muslim_bias_columns_to_sum <- c("AntiMuslim1", "AntiMuslim2", "AntiMuslim3", "AntiMuslim4",
                                     "AntiMuslim5", "AntiMuslim6", "AntiMuslim7", "AntiMuslim8",
                                     "AntiMuslim9", "AntiMuslim10", "AntiMuslim11", "AntiMuslim12",
                                     "AntiMuslim13", "AntiMuslim14", "AntiMuslim15", "AntiMuslim16")

# Add composite score and count of non-missing values for Anti-Muslim bias
SPD_S24_1_ <- SPD_S24_1_ %>%
  mutate(
    Anti_Muslim_bias_Composite_Score = rowSums(select(., all_of( Anti_Muslim_bias_columns_to_sum)), na.rm = TRUE),
    Anti_Muslim_bias_Items_Answered = rowSums(!is.na(select(., all_of( Anti_Muslim_bias_columns_to_sum))))
  )

# Divide composite score by number of items actually answered for Anti-Muslim bias
SPD_S24_1_$Anti_Muslim_bias_Composite_Score <- SPD_S24_1_$Anti_Muslim_bias_Composite_Score / SPD_S24_1_$Anti_Muslim_bias_Items_Answered


# Repeat for Transphobia

Transphobia_columns_to_sum <- c("Transphobia1", "Transphobia2", "Transphobia3", "Transphobia4",
                           "Transphobia5", "Transphobia6", "Transphobia7", "Transphobia8",
                           "Transphobia9")

# Add composite score and count of non-missing values for Transphobia
SPD_S24_1_ <- SPD_S24_1_ %>%
  mutate(
    Transphobia_Composite_Score = rowSums(select(., all_of( Transphobia_columns_to_sum)), na.rm = TRUE),
    Transphobia_Items_Answered = rowSums(!is.na(select(., all_of( Transphobia_columns_to_sum))))
  )

# Divide composite score by number of items actually answered for Transphobia
SPD_S24_1_$Transphobia_Composite_Score <- SPD_S24_1_$Transphobia_Composite_Score / SPD_S24_1_$Transphobia_Items_Answered


# Pearson correlation test between Anti-Muslim bias and Transphobia
cor_test2 <- cor.test(SPD_S24_1_$Anti_Muslim_bias_Composite_Score, SPD_S24_1_$Transphobia_Composite_Score)
print(cor_test2)

#means and standard deviations for both
group_summary2 <- SPD_S24_1_ %>%
  summarize(
    Mean_Anti_Muslim_bias = mean(Anti_Muslim_bias_Composite_Score, na.rm = TRUE),
    SD_Anti_Muslim_bias = sd(Anti_Muslim_bias_Composite_Score, na.rm = TRUE),
    Mean_Transphobia = mean(Transphobia_Composite_Score, na.rm = TRUE),
    SD_Transphobia = sd(Transphobia_Composite_Score, na.rm = TRUE)
  )

print(group_summary2)


#Is there a relationship between people’s racism and their classism?

# Repeat for Racism

Racism_columns_to_sum <- c("Racism1", "Racism2", "Racism3", "Racism4", "Racism5", 
                           "Racism6", "Racism7", "Racism8")

# Add composite score and count of non-missing values for Racism
SPD_S24_1_ <- SPD_S24_1_ %>%
  mutate(
    Racism_Composite_Score = rowSums(select(., all_of( Racism_columns_to_sum)), na.rm = TRUE),
    Racism_Items_Answered = rowSums(!is.na(select(., all_of( Racism_columns_to_sum))))
  )

# Divide composite score by number of items actually answered for Racism
SPD_S24_1_$Racism_Composite_Score <- SPD_S24_1_$Racism_Composite_Score / SPD_S24_1_$Racism_Items_Answered


# Repeat for classism

Classism_columns_to_sum <- c("Classism1", "Classism2", "Classism3", "Classism4",
                             "Classism5", "Classism6", "Classism7", "Classism8",
                             "Classism9", "Classism10", "Classism11", "Classism12")

# Add composite score and count of non-missing values for Classism
SPD_S24_1_ <- SPD_S24_1_ %>%
  mutate(
    Classism_Composite_Score = rowSums(select(., all_of( Classism_columns_to_sum)), na.rm = TRUE),
    Classism_Items_Answered = rowSums(!is.na(select(., all_of( Classism_columns_to_sum))))
  )

# Divide composite score by number of items actually answered for Classism
SPD_S24_1_$Classism_Composite_Score <- SPD_S24_1_$Classism_Composite_Score / SPD_S24_1_$Classism_Items_Answered


# Pearson correlation test between Racism and Classism
cor_test3 <- cor.test(SPD_S24_1_$Racism_Composite_Score, SPD_S24_1_$Classism_Composite_Score)
print(cor_test3)

#means and standard deviations for both
group_summary3 <- SPD_S24_1_ %>%
  summarize(
    Mean_Racism = mean(Racism_Composite_Score, na.rm = TRUE),
    SD_Racism = sd(Racism_Composite_Score, na.rm = TRUE),
    Mean_Classism = mean(Classism_Composite_Score, na.rm = TRUE),
    SD_Classism = sd(Classism_Composite_Score, na.rm = TRUE)
  )

print(group_summary3)



#Is there a relationship between people’s homophobia and their transphobia?

# Repeat for Homophobia

Homophobia_columns_to_sum <- c("Homophobia1", "Homophobia2", "Homophobia3", "Homophobia4",
                               "Homophobia5", "Homophobia6", "Homophobia7", "Homophobia8",
                               "Homophobia9", "Homophobia10", "Homophobia11", "Homophobia12",
                               "Homophobia13", "Homophobia14", "Homophobia15", "Homophobia16",
                               "Homophobia17", "Homophobia18", "Homophobia19")

# Add composite score and count of non-missing values for Racism
SPD_S24_1_ <- SPD_S24_1_ %>%
  mutate(
    Homophobia_Composite_Score = rowSums(select(., all_of( Homophobia_columns_to_sum)), na.rm = TRUE),
    Homophobia_Items_Answered = rowSums(!is.na(select(., all_of( Homophobia_columns_to_sum))))
  )

# Divide composite score by number of items actually answered for Racism
SPD_S24_1_$Homophobia_Composite_Score <- SPD_S24_1_$Homophobia_Composite_Score / SPD_S24_1_$Homophobia_Items_Answered


# Pearson correlation test between Homophobia and Transphobia
cor_test4 <- cor.test(SPD_S24_1_$Homophobia_Composite_Score, SPD_S24_1_$Transphobia_Composite_Score)
print(cor_test4)

#means and standard deviations for both
group_summary4 <- SPD_S24_1_ %>%
  summarize(
    Mean_Homophobia = mean(Homophobia_Composite_Score, na.rm = TRUE),
    SD_Homophobia = sd(Homophobia_Composite_Score, na.rm = TRUE),
    Mean_Transphobia = mean(Transphobia_Composite_Score, na.rm = TRUE),
    SD_Transphobia = sd(Transphobia_Composite_Score, na.rm = TRUE)
  )

print(group_summary4)




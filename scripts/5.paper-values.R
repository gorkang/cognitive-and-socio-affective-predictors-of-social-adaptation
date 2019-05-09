
# ADD LIBRARIES AND DATA FRAME (source(1.data...) OR read_csv)


# Participants ------------------------------------------------------------

# Number of participants
str(DF_HR_FINAL)

# Ages
summary(DF_HR_TOTALS$dem_edad)
# falta agrear cómo se calculó que el 95% se encontraba entre los 18 y 65 años de edad
age_mean = DF_HR_FINAL %>% summarise(age_mean = mean(dem_edad)) %>% pull(age_mean)
cat("The mean age of the sample is", age_mean)
age_sd = DF_HR_FINAL %>% summarise(age_sd = SD(dem_edad)) %>% pull(age_sd)
cat("The standard deviation of age of the sample is", age_sd)
quantile(DF_HR_TOTALS$dem_edad, prob = seq(0, 1, length = 20), type = 5) 
cat("94.7% of the sample was between 18 and 65 years old")

# Education Level
summary(DF_HR_TOTALS$dem_nivedu)
table2 # subjects with each level of education
percentage = table2 %>% select(Freq)*100/232 #percentage of the sample with each level of education

# Materials ---------------------------------------------------------------

# Line 228, Social Adaptation
a.sass = alpha(temp_SASS_alpha)
a.sass

# Line 244, Anxious Attachment
a.att = alpha(temp_ECRRS_ANS_alpha)
a.att

# Line 250, Self-esteem
a.ear  = alpha(temp_SE_alpha, check.keys = TRUE)
a.ear 

# Lines 262 and 263, Locus of control
  #Internal
a.intlocus = alpha(temp_control_interno_alpha)
a.intlocus
  # Other powerful
a.op = alpha(temp_otros_poderosos_alpha)
a.op
  # Chance
a.chance = alpha(temp_azar_alpha)
a.chance

# Line 272, Stress
a.stress = alpha(temp_GHQ_alpha)
a.stress

# Line 281, Fluid Intelligence
a.fluid = alpha(temp_WMAT_alpha1)
a.fluid

# Lines 288 and 289, Crystallized Intelligence
a.crystallized = alpha(temp_WVOC_alpha)
a.crystallized

# Line 298, Numeracy
a.numeracy = alpha(temp_lkns_alpha)
a.numeracy

# Line 316, Logical Reasoning
a.logic = alpha(temp_bfbs_alpha)
a.logic
a.logic2 = alpha(temp_bfbs_alpha2)
a.logic2

# Results -----------------------------------------------------------------

# Bivariate Correlations

# Line 338, Self-Esteem
cor.se

# Line 338, Stress
cor.stress

# Line 338, Locus of Control
cor.locus

# Line 339, Crystallized Intelligence
cor.crystallized

# Line 340, Fluid IntelligenceR.13
cor.fluid

# Line 341, Working Memory
cor.wm

# Line 341, Numeracy
cor.numeracy

# Line 343, Anxious Attachment
cor.anxious

# Line 343, Logical Reasoning
cor.logic

# Line 344, Probabilistic Reasoning 
cor.probabilistic


# Hierarchical Regression

# Line 356, Socioaffective model 
summary(Socioaffective)

# Line 358, Cognitive model
summary(Cognitive)

# Line 359, Variance explained by the cognitive model over the socio-affective model
delta

# Line 361, Complete model
summary(Complete)

# Line 361, Normality check
wilks.complete

# Line 364, VIF 
sqrt(car::vif(Complete)) > 1.6 

# Limitations

# Line 480

temp = DF_HR_FINAL %>% select(`Social Adaptation`, `Self-Esteem`)
summary(temp)
temp_highsa = filter (temp, `Social Adaptation` >= 49) # select only those who fit in the "perfectly adapted" score, according to the literature
summary(temp_highsa) # observe new Self-esteem's new mean


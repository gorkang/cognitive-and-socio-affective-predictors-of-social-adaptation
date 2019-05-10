# should be run after source("1.data_preparation.R")

# 1. Hierarchical analysis is conducted
# 2. Assumption checks are evaluated (normality and multi-collinearity)
# 3. Latex tabel of hierarchical analysis is generated
# 4. Latex table of hierarchical analysis with non-standardized betas is generated

# Libraries and functions -------------------------------------------------

  # To avoid conflict with select between MASS and dplyr, we install QuantPsyc (calls MASS) and unload them.
  if (!require('pacman')) install.packages('pacman'); library('pacman')
  p_load(QuantPsyc, AutoModel, lmSupport)
  
    detach("package:QuantPsyc", unload = TRUE)
    detach("package:MASS", unload = TRUE, force = TRUE)

  if (!require('pacman')) install.packages('pacman'); library('pacman')
  p_load(car, corrplot, apaTables, Rmisc, stargazer, sjPlot)



# Select only variables that will be used in the analysis-------------------------------------------------
  
  DF_HR_TOTALS =
    DF_HR_FINAL %>%
    dplyr::select(
      `Fluid Intelligence`,
      `Crystallized Intelligence`,
      `Self-Esteem`,
      `Internal Locus`,
      `Logical Reasoning`,
      `Anxious Attachment`,
      `Social Adaptation`,
      `Stress`,
      `Numeracy`,
      `Probabilistic Reasoning`,
      `Working Memory`,
      Sex,
      Age,
      `Education Level`
    ) %>%
    as_tibble()
  
  # Hierarchical Models ANOVAS-----------------------------------------------------------------
  
  #Eliminate rows containing missing data
  DF_HR_TOTALS_na_dropped = DF_HR_TOTALS %>% drop_na(
    `Social Adaptation`,
    `Internal Locus`,
    `Self-Esteem`,
    `Anxious Attachment`,
    `Stress`,
    `Working Memory`,
    `Fluid Intelligence`,
    `Crystallized Intelligence`,
    `Logical Reasoning`,
    `Probabilistic Reasoning`,
    `Numeracy`
  )
  
  # Regression for the socio-affective variables
  Socioaffective = lm(
    `Social Adaptation` ~ `Internal Locus` + `Self-Esteem` + `Anxious Attachment` + `Stress`,
    data = DF_HR_TOTALS_na_dropped
  )

  # Regression for the cognitive variables
  Cognitive =
    lm(
      `Social Adaptation` ~ `Working Memory` + `Fluid Intelligence` + `Crystallized Intelligence` + `Logical Reasoning` + `Probabilistic Reasoning` + `Numeracy`,
      data = DF_HR_TOTALS_na_dropped
    )
  
  # Hierarchical model
  Complete = lm(
    `Social Adaptation` ~ `Internal Locus` + `Self-Esteem` + `Anxious Attachment` + `Stress` + `Working Memory` + `Fluid Intelligence` + `Crystallized Intelligence` + `Logical Reasoning` + `Probabilistic Reasoning` + `Numeracy`,
    data = DF_HR_TOTALS_na_dropped
  )
  
  # plot(SA)
  summary(Socioaffective)
  summary(Cognitive)
  summary(Complete)

  # Analyzing if the hierarchical model explains more variance of social adaptation than the socio-affective regression
  Anova_HR = anova(Socioaffective, Complete); Anova_HR
  
  # Deltas
  delta = modelCompare(Socioaffective, Complete)
  delta2 = modelCompare(Cognitive, Complete)
 
# Assumptions -------------------------------------------------------------
  
  # Residuals normality
  hist(Socioaffective$residuals)
  wilks.socioaffective = shapiro.test(Socioaffective$residuals)

  hist(Complete$residuals)
  wilks.complete = shapiro.test(Complete$residuals)

  
  # http://www.statmethods.net/stats/rdiagnostics.html
  # Multi-collinearity
  car::vif(Socioaffective) # variance inflation factors 
  sqrt(car::vif(Socioaffective)) > 1.6 
  
  car::vif(Complete) # variance inflation factors 
  sqrt(car::vif(Complete)) > 1.6 
  
# Table Hierarchical Regression ------------------------------------------------------------------

  # Hieriarchical Regression Table Latex
  Complete_std <- QuantPsyc::lm.beta(Complete)

  stargazer(
    Socioaffective,
    Cognitive,
    Complete,
    ci = TRUE,
    title = "Regression Results",
    align = TRUE,
    dep.var.labels = c("Social Adaptation"),
    column.labels = c("Cognitive Model", "Socio-affective Model", "Complete Model"),
    covariate.labels = c(
      "Internal Locus of Control",
      "Self-Esteem",
      "Anxious Attachment",
      "Stress",
      "Working Memory",
      "Fluid Intelligence",
      "Cryst. Intelligence",
      "Logical Reasoning",
      "Prob. Reasoning",
      "Numeracy"
    ),
    omit.stat = c("LL", "ser"),
    no.space = TRUE,
    style = "default",
    out = "outputs/results/Table 3 - hierarchical regression_non-std.tex"
  )

  # For a table with standardized values for the hierarchical regression see script "4.results-plots.R"
  
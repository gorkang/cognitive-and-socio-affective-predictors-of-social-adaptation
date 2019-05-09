# should be run after source("1.data_preparation.R")

# 1. Hierarchical analysis is conducted
# 2. Assumption checks are evaluated (normality and multi-collinearity)
# 3. Latex tabel of hierarchical analysis is generated
# 4. Latex table of hierarchical analysis with standardized betas is generated

# Libraries and functions -------------------------------------------------

  # To avoid conflict with select between MASS and dplyr, we install QuantPsyc (calls MASS) and unload them.
  if (!require('pacman')) install.packages('pacman'); library('pacman')
  p_load(QuantPsyc, AutoModel, lmSupport)
  
    detach("package:QuantPsyc", unload = TRUE)
    detach("package:MASS", unload = TRUE)

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
      `Logic Reasoning`,
      `Anxious Attachment`,
      `Social Adaptation`,
      `Stress`,
      `Numeracy`,
      `Probabilistic Reasoning`,
      `Working Memory`,
      dem_genero,
      dem_edad,
      dem_nivedu
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
    `Logic Reasoning`,
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
      `Social Adaptation` ~ `Working Memory` + `Fluid Intelligence` + `Crystallized Intelligence` + `Logic Reasoning` + `Probabilistic Reasoning` + `Numeracy`,
      data = DF_HR_TOTALS_na_dropped
    )
  
  # Hierarchical model
  Complete = lm(
    `Social Adaptation` ~ `Internal Locus` + `Self-Esteem` + `Anxious Attachment` + `Stress` + `Working Memory` + `Fluid Intelligence` + `Crystallized Intelligence` + `Logic Reasoning` + `Probabilistic Reasoning` + `Numeracy`,
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
      "Logic Reasoning",
      "Prob. Reasoning",
      "Numeracy"
    ),
    omit.stat = c("LL", "ser"),
    no.space = TRUE,
    style = "default",
    out = "outputs/results/Table 3 - hierarchical regression_non-std.tex"
  )

  
  # The following replaces the beta coefficient in the latex table with beta standadized values
  complete_names <- Complete$coefficients %>% names
  compa_df <- 
  Complete$coefficients %>% 
    as.tibble() %>% 
    bind_cols(as.tibble(complete_names))
  
  Complete_std_names <- Complete_std %>% names
  compa_std_df <- 
    Complete_std %>% 
    as.tibble() %>% 
    bind_cols(as.tibble(Complete_std_names))
  
  all_beta <- 
    compa_df %>% full_join(compa_std_df, "value1") %>% 
    set_names(., c("no_std", "test", "std")) %>% 
    filter(!test == "(Intercept)") %>% as.data.frame()
  
  all_beta <- 
    all_beta %>% 
    select(-test) %>% 
    mutate(no_std = round(no_std, 3), 
           std = round(std, 3))
  
  text_table <- readChar("outputs/results/Table 3 - hierarchical regression_non-std.tex", file.size("outputs/results/Table 3 - hierarchical regression_non-std.tex"))
  
  text_table_x <- text_table

  for (i in 1:nrow(all_beta)) {
    # i <- 1
    a <- format(round(all_beta$no_std[i], 3), nsmall = 3)
    b <- format(round(all_beta$std[i], 3), nsmall = 3)
    
    text_table_x <- gsub(a, b, text_table_x)
  }
  
  write_lines(text_table_x, "outputs/results/Table 3 - hierarchical regression_std.tex")
  
  

  
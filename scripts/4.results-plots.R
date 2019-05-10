# should be run after source("2.results-hierarchical-regression.R")

# This script generates the plot of the hierarchical regression analysis and a table with the standardized cofficients of the hierarchical regression


# Libraries and functions -------------------------------------------------

  if (!require('pacman')) install.packages('pacman'); library('pacman') 
  p_load(devtools, ggplot2, gridExtra, cowplot, lsr) # QuantPsyc


# Data preparation --------------------------------------------------------

  Complete_std <- QuantPsyc::lm.beta(Complete)

  # Extract standardized beta values and create variable names  
  data = names(Complete_std) %>% as_tibble() %>%
    mutate(betas_std = Complete_std) %>%
    mutate(variables = gsub(" ", "\n", value)) %>%
    mutate(variables = gsub("`", "", variables)) %>%
    mutate(
      dimension =
        case_when(
          value %in% names(coefficients(Socioaffective)) ~ "Socio-affective",
          value %in% names(coefficients(Cognitive)) ~ "Cognitive",
          TRUE ~ "NA"
        )
    )
  
  # Create dimension names and extract r squared of each model
  dimension_x <-
    c('Cognitive', 'Socio\nAffective\n\n', 'Complete', 'Delta')
  r_dimension = c(
    summary(Cognitive)$r.squared,
    summary(Socioaffective)$r.squared,
    summary(Complete)$r.squared,
    summary(Complete)$r.squared - summary(Socioaffective)$r.squared
  ) # calculate delta
  
  data.2 <- data.frame(dimension_x, r_dimension) %>%
    mutate(dimension_x = fct_relevel(dimension_x, dimension_x %>% as.vector))
  
  
  

# Hierarchical Plot -------------------------------------------------------
  # Barplot: standardized beta per variable 
  b_dimensions_std <-
    ggplot(data = data, aes(y = betas_std, x = variables, fill = dimension)) +
    geom_col() +
    scale_fill_brewer(type = 'div',
                      palette = "Set1",
                      direction = 1) +
    labs(title = "std Beta per dimension",
         x = "Variables",
         y = "std Beta",
         fill = "dimension_Y") +
    theme(axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      size = 10
    )) +
    theme(legend.position = "none") +
    theme(text=element_text(family="Arial", size=10))
  
  # Barplot: r squared per model  
  r_models <- ggplot(data.2, aes(dimension_x, r_dimension, fill = dimension_x)) + 
    geom_col() +
    scale_fill_brewer(type = 'div',
                      palette = "Set1",
                      direction = 1) +
    labs(title = "R² per model",
         x = "Models",
         y = "R²",
         fill = NULL) +
    theme(axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      size = 10)) + 
    theme(legend.position = "none") +
    theme(text=element_text(family="Arial", size=10))
  
  # Paste both barplots
  final_plot = cowplot::plot_grid(b_dimensions_std, r_models, labels = c("A)", "B)"))
  ggsave("outputs/results/Figure 1 - hierarchical_model.tiff", final_plot, 
         height = 6, 
         width = 7.5, 
         units="in",
         dpi = 300, 
         compression = "lzw")
  
  
  # Hierarchical Regression table with standardized values ------------------
  
  
  install.packages("snakecase")
  library(sjPlot)
  library(sjmisc)
  library(sjlabelled)
  tab_model(Cognitive, Socioaffective, Complete, 
            dv.labels = c("Cognitive", "Socioaffective", "Complete"),
            p.style = "numeric", # both
            show.std = TRUE, show.est = TRUE, show.r2 = TRUE)
  
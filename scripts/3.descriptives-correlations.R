# should be run after source("2.results-hierarchical-regression.R")

# 1. We selected only the dependent and cognitive and socio-affective variables 
# 2. Effect sizes between each independent variable and the dependent variable are calculated
# 3. Variables are renamed and a latex table with descriptives is generated
# 4. Correlation analysis with its respective plots and latex table that can be found in paper section "results"

# Libraries and functions -------------------------------------------------
  
  if (!require('pacman')) install.packages('pacman'); library('pacman')
  p_load(corrplot, dplyr, stargazer, sjPlot, apaTables, Rmisc, Hmisc, xtable) #sjstats, apaStyle



# Data preparation --------------------------------------------------------

  DF_HR_TOTALES =
    DF_HR_FINAL %>% 
    select(`Social Adaptation`,
           `Internal Locus`,
           `Self-Esteem`,
           `Anxious Attachment`,
           `Stress`,
           `Working Memory`,
           `Fluid Intelligence`,
           `Crystallized Intelligence`,
           `Logic Reasoning`,
           `Probabilistic Reasoning`,
           `Numeracy`) %>% filter(complete.cases(.))
  
  summary(DF_HR_TOTALES)

  # Effect Size -------------------------------------------------------------
cor.locus =  cor.test(DF_HR_FINAL$`Social Adaptation`,  DF_HR_FINAL$`Internal Locus`, method=c("pearson"))
cor.anxious = cor.test(DF_HR_FINAL$`Social Adaptation`,  DF_HR_FINAL$`Anxious Attachment`, method=c("pearson"))
cor.se =  cor.test(DF_HR_FINAL$`Social Adaptation`,  DF_HR_FINAL$`Self-Esteem`, method=c("pearson"))
cor.stress =  cor.test(DF_HR_FINAL$`Social Adaptation`,  DF_HR_FINAL$Stress, method=c("pearson"))
  
cor.wm =  cor.test(DF_HR_FINAL$`Social Adaptation`,  DF_HR_FINAL$`Working Memory`, method=c("pearson"))
cor.probabilistic =  cor.test(DF_HR_FINAL$`Social Adaptation`,  DF_HR_FINAL$`Probabilistic Reasoning`, method=c("pearson"))
cor.fluid =  cor.test(DF_HR_FINAL$`Social Adaptation`,  DF_HR_FINAL$`Fluid Intelligence`, method=c("pearson"))
cor.numeracy =  cor.test(DF_HR_FINAL$`Social Adaptation`,  DF_HR_FINAL$Numeracy, method=c("pearson"))
cor.logic =  cor.test(DF_HR_FINAL$`Social Adaptation`,  DF_HR_FINAL$`Logic Reasoning`, method=c("pearson"))
cor.crystallized =  cor.test(DF_HR_FINAL$`Social Adaptation`,  DF_HR_FINAL$`Crystallized Intelligence`, method=c("pearson"))


# Rename variables for descriptives-------------------------------------------------
  
  DF_HR_TOTALS_DESCRIPTIVES = 
    DF_HR_FINAL %>% 
    select(`Social Adaptation`,
           `Internal Locus`,
           `Self-Esteem`,
           `Anxious Attachment`,
           `Stress`,
           `Working Memory`,
           `Fluid Intelligence`,
           `Crystallized Intelligence`,
           `Logic Reasoning`,
           `Probabilistic Reasoning`,
           `Numeracy`,
           dem_genero,
           dem_edad,
           dem_nivedu) %>% 
     as_tibble()  %>% 
    rename_("Age" = "dem_edad")
  
  
  summary(DF_HR_TOTALS)


  
# Descriptives Table Latex ------------------------------------------------------------
  
  stargazer(DF_HR_TOTALS_DESCRIPTIVES %>% as.data.frame(),
            omit.summary.stat = c("p25", "p75"),
            title = "Descriptive information of variables",
            out = "outputs/results/Table 1 - descriptives.tex")
  
  # Confidence intervals for descriptive statistics
  names(DF_HR_TOTALS_DESCRIPTIVES) %>% 
    map_df(~ MeanCI(DF_HR_TOTALS_DESCRIPTIVES[, .x] %>% unlist(), conf.level=0.95, na.rm = TRUE) %>% 
             t() %>%  as_tibble() %>% mutate(name_variable = names(DF_HR_TOTALS_DESCRIPTIVES[.x])))
  
  

# Frequencies for education and sex ---------------------------------------

  educ <- as.factor(DF_HR_FINAL$dem_nivedu) %>% chisq.test(DF_HR_FINAL$dem_nivedu, simulate.p.value = TRUE)
  demos <- DF_HR_FINAL %>% select(dem_nivedu, dem_genero)
  
  #Age
  quantile(DF_HR_TOTALS$dem_edad, prob = seq(0, 1, length = 20), type = 5) # Calculate decils of age
  
  # Sex
  demos$dem_genero <- factor(demos$dem_genero)
  table1 <- table(demos$dem_genero)
  table1 <- as.data.frame(table1)
  
  # Education
  demos$dem_nivedu <- factor(demos$dem_nivedu)
  table2 <- table(demos$dem_nivedu)
  table2 <- as.data.frame(table2)
  
# Correlation Table Latex ------------------------------------------------------------

  corstars <- function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                       result=c("none", "html", "latex")){
    #Compute correlation matrix
    require(Hmisc)
    x <- as.matrix(x)
    correlation_matrix<-rcorr(x, type=method[1])
    R <- correlation_matrix$r # Matrix of correlation coeficients
    p <- correlation_matrix$P # Matrix of p-value 
    
    ## Define notions for significance levels; spacing is important.
    mystars <- ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    ")))
    ## trunctuate the correlation matrix to two decimal
    R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
    
    ## build a new matrix that includes the correlations with their apropriate stars
    Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
    diag(Rnew) <- paste(diag(R), " ", sep="")
    rownames(Rnew) <- colnames(x)
    colnames(Rnew) <- paste(colnames(x), "", sep="")
    
    ## remove upper triangle of correlation matrix
    if(removeTriangle[1]=="upper"){
      Rnew <- as.matrix(Rnew)
      Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
      Rnew <- as.data.frame(Rnew)
    }
    
    ## remove lower triangle of correlation matrix
    else if(removeTriangle[1]=="lower"){
      Rnew <- as.matrix(Rnew)
      Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
      Rnew <- as.data.frame(Rnew)
    }
    
    ## remove last column and return the correlation matrix
    Rnew <- cbind(Rnew[1:length(Rnew)-1])
    if (result[1]=="none") return(Rnew)
    else{
      if(result[1]=="html") print(xtable(Rnew), type="html")
      else print(xtable(Rnew), type="latex") 
    }
  } 
  
write_lines(corstars(DF_HR_TOTALS_DESCRIPTIVES, result = "latex"), "outputs/results/Table 2 - Correlation matrix.tex")


# Cognitive-and-socio-affective-predictors-of-social-adaptation

Repository uploaded to OSF for the project "Cognitive and socio-affective predictors of social adaptation in vulnerable contexts". This project is a product of three FONDECYT projects; Nº 1140114 and 1171200, of PI David Huepe and N° 1171035 of PI Gorka Navarrete.

# Abstract
People who live in vulnerable environments have more difficulties adapting to their context. However, despite this condition, an important number of them achieve to adapt adequately to their surroundings. The aim of this study is to describe which cognitive and socio-affective variables are related to social adaptation in vulnerable contexts. The sample included 232 adults living in vulnerable contexts (mean age=42.3 years, equal amount of men and women). Our main hypotheses were that socio-affective variables (anxious attachment style, external locus of control, low self-esteem and high levels of stress) would explain variations in social adaptation and that cognitive variables (fluid intelligence, crystallized intelligence, working memory, numeracy, probabilistic reasoning and logical reasoning) would add a significant amount of explanation to the dependent variables variance. These hypotheses were confirmed, and it was found that socio-affective and cognitive variables explained an important amount of variance in social adaptation.

# How to run R project

1. Open "cognitive-and-socio-affective-predictors-of-social-adaptation.Rproj" with Rstudio  

2. The script "run-all.R" allows you to run all the necessary scripts  

3. After running "run-all.R", the folder "outputs" should contain:  
    1. data/sa-prepared.csv: clean DB used for the analysis  
    2. results/Figure 1 - hierarchical_model.png: Figure 1 in paper  
    3. results/Table 1 - descriptives.tex: Table 1 in paper
    4. results/Table 2 - Correlation matrix.tex: Table 2 in paper
    5. results/Table 3 - hierarchical regression_non-std.tex: Table not in paper containing non-standardized betas  
    6. results/Table 3 - hierarchical regression_std.tex: Table 3 in paper containing standardized betas  
    
---  

run-all.R includes the following:  

- **1.data-preparation.R:** Here we prepare our data performing reliability tests on each variable. After this we create a new data frame that contains variables already processed and the other variables that will be used in posterior analysis.

- **2.results-hierarchical-regression.R:** Here you can find the hierarchical analysis with its respective latex table that was used in the paper. You can also find the assumption checks that are required for this analyisis (normality and multi-collinearity).

- **3.results-correlation.R:** In this script you can find effect sizes between each independent variable and the dependen variable, and descriptive information of the data. Finally, correlations between variables were calculated, generating the respective latex table used in the paper. 

- **4.results-plots.R:** Here you can find the elaboration of the bar plot that is available in the paper.

- **5.paper-values.R:** Here you can find easily the line that gives the value of each peace of data described by text in the paper.




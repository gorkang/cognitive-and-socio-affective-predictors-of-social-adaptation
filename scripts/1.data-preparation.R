#This script contains reliability analysis

# For each test or questionnaire the following steps are taken
# 1. Select from the raw data frame the items of each test
# 2. Calculate alpha's using the alpha function which calculates raw alpha
# 3. If std alpha < 0.6 then items suggested by alphadrops are removed and the totals are recalculated
# 4. We add the totals of each measured variable to the final data frame
# 5. We add the rest of data necessary for posterior analysis (like demographic information) to the final data frame that is saved as "sa_prepared.csv"


# Libraries and functions -------------------------------------------------

if (!require('pacman')) install.packages('pacman'); library('pacman')
p_load(tidyverse, psych, readr)
options(scipen = 999)

# Functions
source("functions/alphadrops.R")


# Read data ---------------------------------------------------------------

DF = read_csv("data-raw/sa-raw-anonymised.csv")

# Process data for each scale -------------------------------------------
      
    # Crystallized Intelligence Test--------------------------------------------
        
temp_WVOC_alpha = DF %>%
          select(matches("WVOC"),-matches("TOTAL")) %>%
          select(matches("cod"))
        

        #Reliability - Calculate, filter and add to data frame
        
        #Alpha
        a = alpha(temp_WVOC_alpha)
        a #raw_alpha
        #ALPHA: 0.84
        #Filtering: alphadrop(a) indicated that no filtering is necessary
        
        #Add to local dataframe
        temp_WVOC =  DF %>%
        transmute(`Crystallized Intelligence` = WVOC_TOTAL_STD) #, CI_DIR = WVOC_TOTAL)
        # transmute(`Crystallized Intelligence` = WVOC_TOTAL)
        
        # Add to final dataframe
        DF_HR_FINAL = temp_WVOC
        
    # Self-Esteem Scale---------------------------------------------------------------------
        
        temp_SE_alpha =
          DF %>%
          select(matches("EAR"),-matches("TOTAL"))
        
        #Reliability - Calculate, filter and add to data frame
        
        #Alpha
        a = alpha(temp_SE_alpha, check.keys = TRUE)
        a #raw_alpha
        #ALPHA: 0.81
        #Filtering: alphadrop(a) indicated that no filtering is necessary
        
        
        # Calculate totals and dd to local dataframe
        max_score_ear <- 4
        temp =
          temp_SE_alpha %>%
          rowwise() %>%
          mutate(
            `Self-Esteem` = sum(
              EAR_01_raw,
              EAR_02_raw,
              (max_score_ear - EAR_03_raw),
              EAR_04_raw,
              (max_score_ear - EAR_05_raw),
              EAR_06_raw,
              EAR_07_raw,
              (max_score_ear - EAR_08_raw),
              (max_score_ear - EAR_09_raw),
              (max_score_ear - EAR_10_raw)
            )
          ) %>% ungroup()
        
        
        # Add to final dataframe
        DF_HR_FINAL = DF_HR_FINAL %>% cbind(temp)
        


    # Locus of Control Scale----------------------------------------------------------------------
        
        temp = DF %>%
          select(matches("EA_"),-matches("TOTAL"))
        
        #Reliability - Calculate, filter and add to data frame
        
        #Items of internal locus of control (1,4,5,9,18,19,21,23)
        temp_control_interno_alpha = temp %>% select(
          EA_01_raw,
          EA_04_raw,
          EA_05_raw,
          EA_09_raw,
          EA_18_raw,
          EA_19_raw,
          EA_21_raw,
          EA_23_raw
        )
        
        #Alpha
        a = alpha(temp_control_interno_alpha)
        a #raw_alpha
        #Alpha: 0.72
        #Filtering: alphadrop(a) indicated that no filtering is necessary
        
        #Calculate totals and add to local dataframe EA_INTERNO
        
        temp_control_interno = temp_control_interno_alpha %>%
          mutate(`Internal Locus` = rowSums(.[grep("0[1459]|1[89]|2[13]", names(.))]) +
                   24)
        
        
        #Items of other powerful dimension in locus of control (3,8,11,15,17,20,22)
        temp_otros_poderosos_alpha = temp %>% select(EA_03_raw,
                                               EA_08_raw,
                                               EA_11_raw,
                                               EA_15_raw,
                                               EA_17_raw,
                                               EA_20_raw,
                                               EA_22_raw)
        
        #Alpha
        a = alpha(temp_otros_poderosos_alpha)
        a #raw_alpha
        #Alpha: 0.75 (0.71) # Este alpha cambió, habíamos reportado 0.75 y ahora está 0.71
        #Filtering: alphadrop(a) indicated that no filtering is necessary
        
        # Add to local dataframe
        temp_otros_poderosos = temp_otros_poderosos_alpha %>%
          mutate(`Other Powerful Locus of Control` = rowSums(.[grep("0[38]|1[1357]|2[02]", names(.))]) +
                   24)
        
        
        
        # Items of randomness dimension in locus of control (2,6,10,12,14,16,24)
        temp_azar_alpha = temp %>% select(EA_02_raw,
                                    EA_06_raw,
                                    EA_10_raw,
                                    EA_12_raw,
                                    EA_14_raw,
                                    EA_16_raw,
                                    EA_24_raw)
        
        #Alpha
        a = alpha(temp_azar_alpha)
        a #raw_alpha
        #Alpha: 0.67
        #Filtering: alphadrop(a) indicated that no filtering is necessary
        
        # Add to local dataframe
        temp_azar = temp_azar_alpha %>%
          mutate(`Randomness Locus of Control` = rowSums(.[grep("0[267]|1[0246]|24", names(.))]) +
                   24)
        
        
        
        # Add to final dataframe
        DF_HR_FINAL = DF_HR_FINAL %>%
          cbind(temp_control_interno) %>%
          cbind(temp_otros_poderosos) %>%
          cbind(temp_azar)
        
            
        
    # Belief Bias Test-------------------------------------------------------------

        
        temp = DF %>%
          select(matches("bfbs"), -matches("TOTAL")) %>% select(matches("cod"))
        
   
    #Reliability - Calculate, filter and add to data frame 
        
        # belief bias
        temp_bfbs_alpha = temp %>% mutate(bfbs_TOTAL = rowSums(.))
        
        #Alpha
        a = alpha(temp_bfbs_alpha); a #raw_alpha
        alphadrop(a)
        
        
        #Filter
        temp_bfbs_alpha2 = temp_bfbs_alpha %>% dplyr::select(-c(bfbs_03_cod))
        a = alpha(temp_bfbs_alpha2); a #raw_alpha eliminating item 3
        
        
        #Inicial Alpha: 0.64  #Final Alpha: 0.65
        
        # Add to local dataframe
        temp2 = temp_bfbs_alpha2 %>% 
                 mutate(`Logical Reasoning` = rowSums(.))

        # Add to final dataframe
        DF_HR_FINAL = DF_HR_FINAL %>% cbind(temp2)
        
        
    # Attachment Scale -----------------------------------------------------------------

        temp = DF %>%
          select(matches("ECR"), -matches("TOTAL")) #%>% select(matches("cod"))

        #Reliability - Calculate, filter and add to data frame 
        
        # Evitative Attachment Dimension
        temp_ECRRS_EV_alpha = temp %>% select(ECRRS_madre_01_raw, ECRRS_madre_02_raw, ECRRS_madre_03_raw, ECRRS_madre_04_raw, ECRRS_madre_05_raw,
                                              ECRRS_madre_06_raw, ECRRS_padre_01_raw, ECRRS_padre_02_raw, ECRRS_padre_03_raw, ECRRS_padre_04_raw, 
                                              ECRRS_padre_05_raw, ECRRS_padre_06_raw, ECRRS_pareja_01_raw, ECRRS_pareja_02_raw, ECRRS_pareja_03_raw, 
                                              ECRRS_pareja_04_raw, ECRRS_pareja_05_raw, ECRRS_pareja_06_raw, ECRRS_mejoramig_01_raw, ECRRS_mejoramig_02_raw, 
                                              ECRRS_mejoramig_03_raw, ECRRS_mejoramig_04_raw, ECRRS_mejoramig_05_raw, ECRRS_mejoramig_06_raw)
        
        #Alpha
        a = alpha(temp_ECRRS_EV_alpha); a #raw_alpha
        alphadrop(a)
        
        #Filter
        temp_ECRRS_EV_alpha2 = temp_ECRRS_EV_alpha %>% dplyr::select(-c(ECRRS_madre_05_raw, ECRRS_padre_05_raw, ECRRS_pareja_05_raw, ECRRS_pareja_06_raw, ECRRS_madre_06_raw, ECRRS_padre_06_raw, 
                                                                        ECRRS_mejoramig_05_raw, ECRRS_mejoramig_06_raw))
        a = alpha(temp_ECRRS_EV_alpha2); a #raw_alpha eliminating corresponding items
        
        
        #Initial Alpha: 0.57  #Final Alpha: 0.78
        
        # Add to local dataframe
        max_score_ecrrs_ev <- 1
        temp2 = temp_ECRRS_EV_alpha2 %>%
          mutate(`Avoidant Attachment` = ((max_score_ecrrs_ev - ECRRS_madre_01_raw) + (max_score_ecrrs_ev - ECRRS_madre_02_raw) ++(max_score_ecrrs_ev - ECRRS_madre_03_raw) +
                                       (max_score_ecrrs_ev - ECRRS_madre_04_raw) + (max_score_ecrrs_ev - ECRRS_padre_01_raw) + (max_score_ecrrs_ev - ECRRS_padre_02_raw) +
                                       (max_score_ecrrs_ev - ECRRS_padre_03_raw) + (max_score_ecrrs_ev - ECRRS_padre_04_raw) + (max_score_ecrrs_ev - ECRRS_pareja_01_raw) +
                                       (max_score_ecrrs_ev - ECRRS_pareja_02_raw) + (max_score_ecrrs_ev - ECRRS_pareja_03_raw) + (max_score_ecrrs_ev - ECRRS_pareja_04_raw) +
                                       (max_score_ecrrs_ev - ECRRS_mejoramig_01_raw) + (max_score_ecrrs_ev - ECRRS_mejoramig_02_raw) + (max_score_ecrrs_ev - ECRRS_mejoramig_03_raw) +
                                       (max_score_ecrrs_ev - ECRRS_mejoramig_04_raw) /
                                       16
          ))
        
        
        
        # Anxious Attachment Dimension
        temp_ECRRS_ANS_alpha = temp %>% select(
          ECRRS_madre_07_raw,
          ECRRS_madre_08_raw,
          ECRRS_madre_09_raw,
          ECRRS_padre_07_raw,
          ECRRS_padre_08_raw,
          ECRRS_padre_09_raw,
          ECRRS_pareja_07_raw,
          ECRRS_pareja_08_raw,
          ECRRS_pareja_09_raw,
          ECRRS_mejoramig_07_raw,
          ECRRS_mejoramig_08_raw,
          ECRRS_mejoramig_09_raw
        )
        
        
        #Alpha
        a = alpha(temp_ECRRS_ANS_alpha); 
        a #raw_alpha
        #Alpha: 0.87 
        #Filtering: alphadrop(a) indicated that no filtering is necessary 
        
        # Add to local dataframe
        temp_ECRRS_ANS_total = temp_ECRRS_ANS_alpha %>%
          mutate(
            `Anxious Attachment` = (
              ECRRS_madre_07_raw + ECRRS_madre_08_raw + ECRRS_madre_09_raw + ECRRS_padre_07_raw + ECRRS_padre_08_raw + ECRRS_padre_09_raw + ECRRS_pareja_07_raw + ECRRS_pareja_08_raw +
                ECRRS_pareja_09_raw + ECRRS_mejoramig_07_raw + ECRRS_mejoramig_08_raw + ECRRS_mejoramig_09_raw
            ) / 12
          )
        
        
        
        # Add to final dataframe
        DF_HR_FINAL = DF_HR_FINAL %>% cbind(temp2) %>% cbind(temp_ECRRS_ANS_total)
   
        
    # SOCIAL ADAPTATION SCALE --------------------------------------------------------------------
        
        temp = DF %>%
          select(matches("SASS"),-matches("TOTAL"))
        
        
        #Reliability - Calculate, filter and add to data frame
        
        # Select all items
        temp_SASS_alpha = temp %>% select(
          SASS_01_raw,
          SASS_02_raw,
          SASS_03_raw,
          SASS_04_raw,
          SASS_05_raw,
          SASS_06_raw,
          SASS_07_raw,
          SASS_08_raw,
          SASS_09_raw,
          SASS_10_raw,
          SASS_11_raw,
          SASS_12_raw,
          SASS_13_raw,
          SASS_14_raw,
          SASS_15_raw,
          SASS_16_raw,
          SASS_17_raw,
          SASS_18_raw,
          SASS_19_raw,
          SASS_20_raw,
          SASS_21_raw,
          SASS_trabajo
        )
        
        # Alpha
        a = alpha(temp_SASS_alpha)
        a #raw_alpha
        # Alpha: 0.8
        
        
        # Add to local dataframe
        temp_SASS_total = temp_SASS_alpha %>%
          mutate(`Social Adaptation` = case_when(
            .$SASS_trabajo == 0 ~ rowSums(.[grep("0[2-9]|1[0-9]|2[01]", names(.))]),
            .$SASS_trabajo == 1 ~ rowSums(.[grep("0[13-9]|1[0-9]|2[01]", names(.))])
          ))
        # Add to final dataframe
        DF_HR_FINAL = DF_HR_FINAL %>% cbind(temp_SASS_total)
        
        
        
    # GENERAL HEALTH QUESTIONNAIRE (STRESS) ---------------------------------------------------------------------
        
        temp = DF %>%
          select(matches("GHQ"),-matches("TOTAL"))
        
        
        #Reliability - Calculate, filter and add to data frame
        
        # Stress Dimension
        temp_GHQ_alpha = temp %>% select(GHQ_02, GHQ_05, GHQ_09)
        
        # Alpha
        a = alpha(temp_GHQ_alpha)
        a #raw_alpha
        #Alpha: 0.74 
        
        
        # Add to local dataframe
        temp_GHQ_ES =  temp_GHQ_alpha %>%
          mutate(`Stress` = rowSums(.[grep("0[259]", names(.))]))
        
        # Add to final dataframe
        DF_HR_FINAL = DF_HR_FINAL %>% cbind(temp_GHQ_ES)
        
        
        
        
    # Numeracy Test -------------------------------------------------------------------
        temp = DF %>%
          select(matches("lkns"),-matches("TOTAL")) %>% select(matches("cod"))
        
        #Reliability - Calculate, filter and add to data frame
        
        # Select all items
        temp_lkns_alpha = temp %>% select(
          lkns_01_cod,
          lkns_02_cod,
          lkns_03_cod,
          lkns_04_cod,
          lkns_05_cod,
          lkns_06_cod,
          lkns_07_cod,
          lkns_08_cod,
          lkns_09_cod,
          lkns_10_cod,
          lkns_11_cod
        )
        
        #Alpha
        a = alpha(temp_lkns_alpha)
        a #raw_alpha
        
        
        #Initial Alpha: 0.71 
        
        
        # Add to local dataframe
        temp_lkns_TOTAL =  temp_lkns_alpha %>%
          mutate(`Numeracy` = rowSums(.[grep("[01][0-6|9]", names(.))], na.rm = TRUE))
        
        
        # Add to final dataframe
        DF_HR_FINAL = DF_HR_FINAL %>% cbind(temp_lkns_TOTAL)
        
        
        
    # Fluid Intelligence test --------------------------------------------------------------------

        temp = DF %>%
          select(matches("WMAT"), -matches("TOTAL")) %>% select(matches("cod"))
        
        #Reliability - Calculate, filter and add to data frame 
        
        # Select items
        temp_WMAT_alpha1 = temp %>% select(WMAT_01_cod, WMAT_02_cod, WMAT_03_cod, WMAT_04_cod, WMAT_05_cod, WMAT_06_cod, WMAT_07_cod, WMAT_08_cod, WMAT_09_cod, WMAT_10_cod, WMAT_11_cod, WMAT_12_cod,
                                          WMAT_13_cod, WMAT_14_cod, WMAT_15_cod, WMAT_16_cod, WMAT_17_cod, WMAT_18_cod, WMAT_19_cod, WMAT_20_cod, WMAT_21_cod, WMAT_22_cod, WMAT_23_cod, WMAT_24_cod,
                                          WMAT_25_cod, WMAT_26_cod)
        
        # Alpha
        a = alpha(temp_WMAT_alpha1); a #raw_alpha
        a
        #Alpha: 0.92 
        #Filtering: alphadrop(a) indicated that no filtering is necessary 
        
        # Add to local dataframe
        
        temp_WMAT_alpha2 =  DF %>% 
          transmute(`Fluid Intelligence` = wmat_total_std)
        
        # Add to final dataframe
        DF_HR_FINAL = DF_HR_FINAL %>% cbind(temp_WMAT_alpha2)
        
        
    # Probabilistic Reasoning Test --------------------------------------------------------------------
        
        # Each item measures a different type of probabilistic reasoning
        # This measure is part of a separate experiment with a 2x2 manipulation: 2 presentation format (text and pictorial) x 2 response type (quantitative and qualitative).
        # We don't calculate reliability here.
        
        temp = DF %>%
         transmute(`Probabilistic Reasoning` = bayes_all_accuracy)
        
        DF_HR_FINAL = DF_HR_FINAL %>% cbind(temp)
        
        
    # Working Memory Test --------------------------------------------------------------------
        
        # This is a performance scale with increasing difficulty. 
        # We don't calculate reliability here. 
        
        temp = DF %>%
          transmute(`Working Memory` = wdig_total_std)
        
        # Add to final dataframe (since this scale has only one total value, reliability is not calculated)
        DF_HR_FINAL = DF_HR_FINAL %>% cbind(temp)
        
        
    # Demographic Questions ---------------------------------------------------
        
        dem = DF %>% select(matches("dem"))
        ID = DF  %>% select("ID")
        
        # Add to final dataframe
        DF_HR_FINAL = DF_HR_FINAL %>% cbind(dem) %>% cbind(ID)
        
        
        
        
# FINAL --------------------------------------------------------------
        
        # Create final dataframe
        
        names(DF_HR_FINAL)
        
        DF_HR_FINAL[DF_HR_FINAL %>% names() %>% duplicated()]
        
        
        DF_HR_FINAL =
        DF_HR_FINAL %>%
          as_tibble() %>%
          select(
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
            matches("dem"),
            "ID") %>% 
          rename(Sex = "dem_genero",
                 Age = "dem_edad",
                 `Education Level` = "dem_nivedu")
        
        tail(DF_HR_FINAL)
        
        # Create new csv with prepared data
        write_csv(DF_HR_FINAL, "outputs/data/sa-prepared.csv")

        
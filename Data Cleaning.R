library(here)
library(foreign)
library(tidyverse)
library(missForest)

raw_data <- read.spss("H:\\My Drive\\Research\\Projects\\Depression Beliefs\\BADS\\BADS Data\\BADS_UM_2022_2023_FullDataSet_Scored_&MSU_Take2_wAmherst_Take2_wFollowUP.sav",
                      to.data.frame = T)

clean_data <- raw_data %>%
  
  # Create new/clean variables
  transmute(
    
    # Attention checks passed (out of five)
    attention_checks = Attn_FinalSum,
    
    # Age (numeric)
    age = Demo_Age %>%
      trimws() %>% # Remove leading and trailing spaces
      recode(
        "' 18" = "18",
        "2o" = "20"
      ) %>% # Replace mis-typed responses with numbers
      as.numeric() %>% # Switch to numeric variable
      if_else(. == 10, NA_real_, .), # Set likely invalid outlier to NA
    
    # Gender (factor, female as reference group)
    gender = case_when(
      Demo_GenID == "Male" & (is.na(Dem_Trans) | Dem_Trans  == "No") ~ "Cis Male",
      Demo_GenID == "Female" & (is.na(Dem_Trans) | Dem_Trans == "No") ~ "Cis Female",
      Demo_GenID != "Prefer not to answer" ~ "TGD",
      T ~ NA_character_
    ) %>%
      factor(levels = c("Cis Female", "Cis Male", "TGD")),
    
    race_ethnicity = case_when(
      Demo_Hisp == "Yes" ~ "Hispanic",
      Demo_Race == "A. American Indian or Alaskan Native" ~ "AI/AN non-Hispanic",
      Demo_Race == "\tB. Asian or Asian American" ~ "Asian non-Hispanic",
      Demo_Race == "C. Black or African American" ~ "Black non-Hispanic",
      Demo_Race == "\tD. Native Hawaiian or Other Pacific Islander" ~ "NH/PI non-Hispanic",
      Demo_Race == "\tE. Middle Eastern or North African" ~ "ME/NA non-Hispanic",
      Demo_Race == "\tF. White or European American" ~ "White non-Hispanic",
      Demo_Race == "G. Other (fill in the blank)" ~ "Other or Multiracial non-Hispanic",
      T ~ NA_character_
    ) %>%
      factor(levels = c("White non-Hispanic", "AI/AN non-Hispanic", "Asian non-Hispanic", "Black non-Hispanic", "NH/PI non-Hispanic", "ME/NA non-Hispanic", "Other or Multiracial non-Hispanic", "Hispanic")),
    
    family_income = case_when(
      Demo_Income %in% c(" $0-$20,000", "$20,000 - $50,000", "$50,000-$70,000") ~ "Below U.S. Median",
      Demo_Income %in% c("$70,000 - $100,000", "$100,000 - $200,000", "Over $200,000") ~ "Above U.S. Median",
      T ~ NA_character_
    ) %>%
      factor(levels = c("Below U.S. Median", "Above U.S. Median")),
    
    # Depression self-identification (self-labeling)
    depression_self_id = PPBS_depYN == "Yes",
    
    # Depression diagnosis (professional labeling)
    depression_dx = Treatment_Exper_1_9 == "Yes",
    
    # Depression (PHQ8)
    PHQ8 = PHQ8_Tot,
    PHQ8_cutoff = PHQ8_Tot >= 10,
    
    # Perceived control (PPBS + ITQ)
    perceived_control = scale(
      rowMeans(
        cbind(
          scale(as.numeric(PPBS_5)),
          scale(as.numeric(PPBS_6)),
          scale(as.numeric(ITQ_16)) * -1,
          scale(as.numeric(ITQ_17)) * -1,
          scale(as.numeric(ITQ_18)) * -1,
          scale(as.numeric(ITQ_19)) * -1
        )
      )
    ),
    
    # Cognitive emotion regulation (CERQ)
    CERQ_self_blame = scale(CERQ_SelfBlame),
    CERQ_other_blame = scale(CERQ_Blame),
    CERQ_rumination = scale(CERQ_Rumin),
    CERQ_catastrophizing = scale(CERQ_Catastroph),
    CERQ_perspective = scale(CERQ_Perspective),
    CERQ_refocusing = scale(CERQ_PosRefocus),
    CERQ_reappraising = scale(CERQ_Reappraisal),
    CERQ_acceptance = scale(CERQ_Accept),
    CERQ_planning = scale(CERQ_Planning),
    
    # Attitudes towards treatment (MHSAS)
    MHSAS_therapy = scale(Therapy_Seeking_tot),
    MHSAS_medication = scale(Med_Seeking_tot),
    
  ) %>%
  
  # Filter to respondents who passed attention checks, none older than 25 y/o
  filter(
    attention_checks >= 4,
    age <= 25
  ) %>%
  
  # Reclass as numeric and factor to play nice with missForest
  mutate(across(where(is.character), as.factor),
         across(where(is.logical), as.factor))

set.seed(5269846)
imputed_data <- missForest(clean_data, verbose = T) # Could be doing this with the full dataset for more accuracy, but there are so few missing values it shouldn't ultimately matter

# Change factor variables back to logical, where applicable
imputed_data$ximp <- imputed_data$ximp %>%
  mutate(across(all_of(c("depression_self_id", "depression_dx", "PHQ8_cutoff")), as.logical))

saveRDS(imputed_data$ximp, file = here("Clean Data.rds"))
saveRDS(imputed_data$OOBerror, file = here("Imputation Results.rds"))

library(foreign)
library(tidyverse)
library(psych)
`%+%` <- paste0

raw_data <- read.spss("H:\\My Drive\\Research\\Projects\\Depression Beliefs\\BADS\\BADS Data\\BADS_UM_2022_2023_FullDataSet_Scored_&MSU_Take2_wAmherst_Take2_wFollowUP.sav",
                      to.data.frame = T)

filtered_data <- raw_data %>%
  filter(Attn_FinalSum >= 4)


## PHQ-8
filtered_data %>%
  select(matches("^PHQ_8.*rc$")) %>%
  alpha()


## Perceived Control
filtered_data %>%
  transmute(
    PPBQ_5 = scale(as.numeric(PPBS_5)),
    PPBS_6 = scale(as.numeric(PPBS_6)),
    ITQ_16 = scale(as.numeric(ITQ_16)) * -1,
    ITQ_17 = scale(as.numeric(ITQ_17)) * -1,
    ITQ_18 = scale(as.numeric(ITQ_18)) * -1,
    ITQ_19 = scale(as.numeric(ITQ_19)) * -1
  ) %>%
  alpha()


## CERQ: Subscales figured out from attr(raw_data, "variable.labels")["CERQ_[X]"]
# CERQ - Self-Blame
filtered_data %>%
  select("CERQ_" %+% c(1, 10, 19, 28)) %>%
  mutate(across(everything(), as.numeric)) %>%
  alpha()

# CERQ - Acceptance
filtered_data %>%
  select("CERQ_" %+% c(2, 11, 20, 29)) %>%
  mutate(across(everything(), as.numeric)) %>%
  alpha()

# CERQ - Rumination
filtered_data %>%
  select("CERQ_" %+% c(3, 12, 21, 30)) %>%
  mutate(across(everything(), as.numeric)) %>%
  alpha()

# CERQ - Refocusing
filtered_data %>%
  select("CERQ_" %+% c(4, 13, 22, 31)) %>%
  mutate(across(everything(), as.numeric)) %>%
  alpha()

# CERQ - Planning
filtered_data %>%
  select("CERQ_" %+% c(5, 14, 23, 32)) %>%
  mutate(across(everything(), as.numeric)) %>%
  alpha()

# CERQ - Reappraisal
filtered_data %>%
  select("CERQ_" %+% c(6, 15, 24, 33)) %>%
  mutate(across(everything(), as.numeric)) %>%
  alpha()

# CERQ - Perspective
filtered_data %>%
  select("CERQ_" %+% c(7, 16, 25, 34)) %>%
  mutate(across(everything(), as.numeric)) %>%
  alpha()

# CERQ - Catastrophizing
filtered_data %>%
  select("CERQ_" %+% c(8, 17, 26, 35)) %>%
  mutate(across(everything(), as.numeric)) %>%
  alpha()

# CERQ - Other-Blame
filtered_data %>%
  select("CERQ_" %+% c(9, 18, 27, 36)) %>%
  mutate(across(everything(), as.numeric)) %>%
  alpha()


## MHSAS
filtered_data %>%
  select(all_of("Therapy_Seeking_" %+% c("1", "2_r", "3", "4", "5_r", "6_r", "7", "8_r", "9_r"))) %>%
  mutate(across(everything(), as.numeric)) %>%
  alpha()

filtered_data %>%
  select(all_of("Med_Seeking_" %+% c("1", "2_r", "3", "4", "5_r", "6_r", "7", "8_r", "9_r"))) %>%
  mutate(across(everything(), as.numeric)) %>%
  alpha()
 
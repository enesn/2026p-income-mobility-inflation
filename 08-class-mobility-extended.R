## ==================================================================================================#
# Inflation and redistribution in Turkey
# May 2025
# EI
## ==================================================================================================#

# ==================================================================================================#
# Class mobility panel with more explanatory variables 
# ==================================================================================================#

class_mobility_ext <-
  class_mobility %>%
    left_join(
            class_mobility %>%
            filter(FK095==1) %>%
            select(HKIMLIK, income_year, FK070, FK090, FK210, FE030, FI010, FI120, FI130, FI190, HB050, HH020) %>%
            mutate(head_age = FK070, head_gender = FK090, head_status = FK210, head_edu = FE030, head_status2 = FI010,
                   head_status3 = FI120, head_occupation = FI130, head_formality = FI190) %>%
            mutate(head_manager_professional = ifelse(head_occupation == 1 | head_occupation == 2, 1,0),
                   head_manager_professional = ifelse(is.na(head_manager_professional), 0, head_manager_professional),
                   head_occupation = ifelse(is.na(head_occupation), 0, head_occupation),

                   head_male = ifelse(head_gender == 1, 1, 0),
                   head_informal = ifelse(head_formality==2 ,1,0),
                   head_informal = ifelse(is.na(head_informal), 0, head_informal),

                    single_person_hh = ifelse(HB050 == "1", 1, 0),
                    couple_hh = ifelse(HB050 == "21", 1, 0),
                    parents_children_hh = ifelse(HB050 == "22", 1, 0),
                    single_parent_children_hh = ifelse(HB050 == "23",1, 0),
                   home_owner = ifelse(HH020 == "1",1,0)
                   )  %>%
              select(-FK070:-HH020), by = c("HKIMLIK","income_year")

  ) %>%
  left_join(
    read_xlsx("other-input-data/minimum_wage.xlsx")
  ) %>%
  left_join(
    read_xlsx("other-input-data/national_income_currentlcu.xlsx") %>%
      select(income_year, per_adult_national_income)
  ) %>%
  left_join(
    class_mobility %>%
      group_by(income_year) %>%
      summarise(median_equal_split_income = weighted_median(x=equal_split_income, w = as.numeric(FK060_4)))
  ) %>%
  mutate(
    poverty_gap = equal_split_income /median_equal_split_income,
    mw_gap = equal_split_income / (minimum_wage / hh_size),
    gdp_gap = equal_split_income / per_adult_national_income
  ) %>%


  arrange(FKIMLIK, income_year) %>%
  group_by(FKIMLIK) %>%
  mutate(poverty_gap_prev3 = lag(poverty_gap, 3)) %>%
  mutate(poverty_gap_prev1 = lag(poverty_gap, 1)) %>%
  mutate(poverty_gap_diff3 = poverty_gap - poverty_gap_prev3) %>%
  mutate(poverty_gap_diff1 = poverty_gap - poverty_gap_prev1) %>%

  mutate(mw_gap_prev3 = lag(mw_gap, 3)) %>%
  mutate(mw_gap_prev1 = lag(mw_gap, 1)) %>%
  mutate(mw_gap_diff3 = mw_gap - mw_gap_prev3) %>%
  mutate(mw_gap_diff1 = mw_gap - mw_gap_prev1) %>%

  mutate(gdp_gap_prev3 = lag(gdp_gap, 3)) %>%
  mutate(gdp_gap_prev1 = lag(gdp_gap, 1)) %>%
  mutate(gdp_gap_diff3 = gdp_gap - gdp_gap_prev3) %>%
  mutate(gdp_gap_diff1 = gdp_gap - gdp_gap_prev1) %>%
  ungroup()

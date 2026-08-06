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
  # Household head's education and occupation. A missing occupation is read as "not a manager or
  # professional" rather than as unknown, and is coded 0 alongside the other occupation codes.
  left_join(
    class_mobility %>%
      filter(FK095 == 1) %>%
      select(HKIMLIK, income_year, head_edu = FE030, head_occupation = FI130) %>%
      mutate(
        head_manager_professional = ifelse(head_occupation %in% c(1, 2), 1, 0),
        head_occupation           = ifelse(is.na(head_occupation), 0, head_occupation)
      ),
    by = c("HKIMLIK", "income_year")
  ) %>%

  left_join(
    read_xlsx("other-input-data/minimum_wage.xlsx"),
    by = "income_year"
  ) %>%

  arrange(FKIMLIK, income_year)

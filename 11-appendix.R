## ==================================================================================================#
# Relative Income Mobility around an Inflationary Shock
# May 2025
# EI
## ==================================================================================================#

# ==================================================================================================#
# Table A1, Mobility in table
# ==================================================================================================#


step_header("11 | Appendix: Tables A1-A4")

step_note("Table A1: the Fig 2 mobility shares as a table")

class_mobility %>%
  filter(!is.na(p_labor_entry2_contribution)) %>%
  filter(!type %in% c("Financier", "Rentier")) %>% 
  mutate(
    mobility = case_when(
      upward_2yr == 1 ~ "Upward",
      downward_2yr == 1 ~ "Downward",
      TRUE ~ "Immobile"
    )
  ) %>% 
  filter(income_year > 2011) %>%
  group_by(type, income_year, mobility) %>% 
  summarise(
    n = sum(as.numeric(FK060_4)),
    .groups = "drop"
  ) %>%
  group_by(type, income_year) %>% 
  mutate(p = 100 * n / sum(n)) %>%
  ungroup() %>%
  select(type, income_year, mobility, p) %>%
  pivot_wider(
    names_from = mobility,
    values_from = p
  ) %>%
  arrange(type, income_year) %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    digits = 1,
    caption = "Income mobility shares by class and year"
  ) %>%
  kableExtra::kable_styling(latex_options = c("hold_position")) %>%
  kableExtra::collapse_rows(columns = 1, latex_hline = "major") %>%
  kableExtra::add_header_above(
    c(" " = 1, "Year" = 1, "Mobility states" = 3)
  )


# ==================================================================================================#
# Table A2, Summary statistics
# ==================================================================================================#
# --------------  
# Panel A: All
# --------------
  step_note("Table A2, panel A: rank changes and class shares over the whole sample")

  all_rank_changes <- class_mobility_ext %>%
    filter(income_year > 2011) %>%
    mutate(delta_rank = deciles2 - decile_prev1) %>% 
    filter(!is.na(delta_rank)) %>% 
    left_join(
      read_xlsx("other-input-data/evds_inflation_fxrate.xlsx") %>% 
        mutate(
          Date = as.Date(paste0(Date, "-01")),
          inflation = as.numeric(inflation),
          fxchange = as.numeric(fxchange)
        ) %>%   
        filter(year(Date) < 2024) %>% mutate(income_year = year(Date)) %>% group_by(income_year) %>% summarise(annual_inflation = mean(inflation))
    ) %>%
    distinct(HKIMLIK, income_year,  .keep_all = T) %>%
    mutate(
      pensioner = as.numeric(type == "Pensioner"),
      laborer = as.numeric(type == "Laborer"),
      employer = as.numeric(type == "Employer"),
      self_employer = as.numeric(type == "Self-employer"),
      transfer_dependent = as.numeric(type == "Transfer-Dependent"),
      financier = as.numeric(type == "Financier"),
      rentier = as.numeric(type == "Rentier")
    ) %>%
    select(
      delta_rank,
      pensioner,
      laborer,
      employer,
      self_employer,
      transfer_dependent,
      financier,
      rentier
    ) %>%
    mutate(across(everything(), as.numeric)) %>%
    as.data.frame()
  
  #Latex table panel A:
  stargazer::stargazer(
   as.data.frame(all_rank_changes),
    summary = TRUE,
    type = "latex"
  )
# --------------
# Panel B: Labor
# --------------
  step_note("Table A2, panel B: the labor market variables behind Table 3")

   lmarket_descriptive <- lmarket_panel %>%
    filter(income_year > 2011) %>%
    mutate(delta_rank = deciles2 - decile_prev1) %>%
    mutate(delta_aroundmw_mean = around_minimumwage_wmean - around_minimumwage_wmean_prev1) %>% 
    mutate(delta_jtjt_wmean = jtjt_wmean - jtjt_wmean_prev1) %>% 
    mutate(delta_informality_wmean = informality_wmean - informality_wmean_prev1) %>%
    mutate(delta_worked_months_wmean = worked_months_wmean - worked_months_wmean_prev1) %>% 
    mutate(delta_working_hour_wmean = working_hour_wmean - working_hour_wmean_prev1) %>%
    filter(!is.na(delta_jtjt_wmean)) %>%
    select(HKIMLIK, 
      delta_rank,
      delta_aroundmw_mean,
      delta_jtjt_wmean, 
      delta_informality_wmean, 
      delta_worked_months_wmean,
      delta_working_hour_wmean,
      informality_wmean_prev1,
      around_minimumwage_wmean_prev1
    ) 
  stargazer::stargazer(
    as.data.frame(lmarket_descriptive),
    summary = TRUE
  )

# ==================================================================================================#
# Tables A3-A4, Full coefficients for the model presented in Fig 4
# ==================================================================================================#

step_note("Tables A3-A4: the full coefficient sets behind Fig 4")

stargazer::stargazer(m2, m2upper, m2bottom)

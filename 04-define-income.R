## ==================================================================================================#
# Inflation and redistribution in Turkey
# May 2025
# EI
## ==================================================================================================#

## ==================================================================================================#
# Personal income
## ==================================================================================================#

income_defined <-
  sample_silc %>%
  mutate(
    FG030 = ifelse(as.numeric(FG030) < 0, 0, FG030),
    FG040 = ifelse(as.numeric(FG040) < 0, 0, FG040),

    wage_income  = as.numeric(FG010) %+% as.numeric(FG020),
    entrp_income = as.numeric(FG030) %+% as.numeric(FG040),

    # Individualistic adult income: wage, entrepreneurial, pension and transfers.
    # FG070 stays out because it includes severance payments.
    disp_personal_income =
      wage_income %+%
      as.numeric(FG030) %+% as.numeric(FG040) %+%
      as.numeric(FG080) %+%
      as.numeric(FG120) %+% as.numeric(FG100) %+% as.numeric(FG090) %+% as.numeric(FG110),

    personal_labor_income    = wage_income,
    personal_transfer_income = as.numeric(FG120) %+% as.numeric(FG100) %+%
                               as.numeric(FG090) %+% as.numeric(FG110),
    personal_pension_income  = as.numeric(FG080)
  ) %>%

  # Which kind of income a person's earnings count as, given their employment status (FI120),
  # sector (FI145), formality (FI190) and occupation (FI130).
  mutate(
    personal_employer_income     = ifelse(FI120 == 3 & entrp_income > 0, entrp_income, 0),
    personal_selfemployer_income = ifelse((!FI120 == 3 | is.na(FI120)) & entrp_income > 0, entrp_income, 0),

    # The sector split alone keeps NA where FI145 is missing, so an unknown sector stays
    # distinguishable from a genuine zero.
    personal_privateseclabor_income = ifelse(FI120 == 1 & FI145 == 1 & wage_income > 0, wage_income,
                                             ifelse(is.na(FI145), NA, 0)),
    personal_publicseclabor_income  = ifelse(FI120 == 1 & FI145 == 2 & wage_income > 0, wage_income,
                                             ifelse(is.na(FI145), NA, 0)),

    personal_hourlylabor_income = ifelse(FI120 == 2 & wage_income > 0, wage_income, 0),  # yevmiyeli çalışan

    personal_formallabor_income   = ifelse(FI190 == 1 & wage_income > 0, wage_income, 0),
    personal_informallabor_income = ifelse(FI190 == 2 & wage_income > 0, wage_income, 0),

    personal_nonmanagernonprofessional_labor_income =
      ifelse(!(FI130 == 1 | FI130 == 2) & wage_income > 0, wage_income, 0)
  ) %>%

  # For every type except the sector split, a missing classifier means the person is not of that
  # type rather than that their income is unknown.
  mutate(
    across(
      c(personal_employer_income, personal_selfemployer_income, personal_hourlylabor_income,
        personal_formallabor_income, personal_informallabor_income,
        personal_nonmanagernonprofessional_labor_income),
      ~ ifelse(is.na(.x), 0, .x)
    ),
    FB010 = as.character(FB010)
  ) %>%

  select(-wage_income, -entrp_income)


## ==================================================================================================#
# Household income
## ==================================================================================================#

income_defined <-
  income_defined %>%

  # Personal incomes summed over the household-year, alongside the household size they are split by.
  left_join(
    income_defined %>%
      summarise(
        hh_income = sum(disp_personal_income, na.rm = TRUE),
        across(
          c(pension_income                         = personal_pension_income,
            employer_income                        = personal_employer_income,
            selfemployer_income                    = personal_selfemployer_income,
            transfer_income                        = personal_transfer_income,
            labor_income                           = personal_labor_income,
            privateseclabor_income                 = personal_privateseclabor_income,
            publicseclabor_income                  = personal_publicseclabor_income,
            hourlylabor_income                     = personal_hourlylabor_income,
            formallabor_income                     = personal_formallabor_income,
            informallabor_income                   = personal_informallabor_income,
            nonmanagernonprofessional_labor_income = personal_nonmanagernonprofessional_labor_income),
          ~ sum(.x, na.rm = TRUE),
          .names = "hh_{.col}"
        ),
        hh_size = n(),
        .by = c(HKIMLIK, FB010)
      ),
    by = join_by(HKIMLIK, FB010)
  ) %>%

  left_join(
    read_xlsx("other-input-data/imputed_rent_correction.xlsx") %>% mutate(FB010 = as.character(FB010)),
    by = join_by(FB010)
  ) %>%

  mutate(
    imputed_rent = as.numeric(HG010),
    imputed_rent = ifelse(imputed_rent < 0, imputed_rent * (-1), imputed_rent),
    imputed_rent = ifelse(is.na(imputed_rent), 0, imputed_rent),
    # imputed_rent = imputed_rent * imprent_upgrade_factor,

    # Total disposable household income. HG030-HG060 are the household-level transfers,
    # HG070 rent, HG080 interest.
    hh_disp_income = hh_income %+%
      as.numeric(HG020) %+%                                 # child labor income
      as.numeric(HG030N) %+% as.numeric(HG030A) %+%
      as.numeric(HG040) %+%
      as.numeric(HG050N) %+% as.numeric(HG050A) %+%
      as.numeric(HG060N) %+% as.numeric(HG060A) %+%
      as.numeric(HG070) %+%
      as.numeric(HG080) %+%
      imputed_rent %+%
      as.numeric(HG105),

    hh_rental_income    = as.numeric(HG070),
    hh_financial_income = as.numeric(HG080),

    hh_full_transfers = hh_transfer_income %+%
      as.numeric(HG030N) %+% as.numeric(HG030A) %+%
      as.numeric(HG040) %+%
      as.numeric(HG050N) %+% as.numeric(HG050A) %+%
      as.numeric(HG060N) %+% as.numeric(HG060A),

    oecd_equivalence_scale = ifelse(hh_size == 1, 1, 1 + (hh_size - 1) * 0.5),

    # Adjusted personal income
    equivalent_income  = hh_disp_income / oecd_equivalence_scale,
    equal_split_income = hh_disp_income / hh_size
  ) %>%

  mutate(
    across(c(hh_rental_income, hh_financial_income, personal_pension_income),
           ~ ifelse(is.na(.x), 0, .x))
  ) %>%

## ==================================================================================================#
# Equal-split income
## ==================================================================================================#

  # Every household aggregate split equally over household members.
  mutate(
    across(
      c(financial                       = hh_financial_income,
        rental                          = hh_rental_income,
        imprent                         = imputed_rent,
        transfers                       = hh_full_transfers,
        pension                         = hh_pension_income,
        employer                        = hh_employer_income,
        selfemployer                    = hh_selfemployer_income,
        labor                           = hh_labor_income,
        privateseclabor                 = hh_privateseclabor_income,
        publicseclabor                  = hh_publicseclabor_income,
        hourlylabor                     = hh_hourlylabor_income,
        formallabor                     = hh_formallabor_income,
        informallabor                   = hh_informallabor_income,
        nonmanagernonprofessional_labor = hh_nonmanagernonprofessional_labor_income),
      ~ .x / hh_size,
      .names = "equal_split_{.col}"
    ),
    equal_split_rent = (as.numeric(HH040) * 12) / hh_size
  ) %>%

  # Deflated to December 2003 prices. The CPI year is shifted by one because FB010 is the survey
  # year while the incomes it records are the previous year's.
  left_join(
    read_xlsx("other-input-data/tuik_cpi.xlsx") %>% mutate(year = as.character(year + 1)),
    by = join_by(FB010 == year)
  ) %>%

  mutate(
    across(
      c(equal_split = equal_split_income,
        equal_split_financial, equal_split_rental, equal_split_imprent, equal_split_transfers,
        equal_split_pension, equal_split_employer, equal_split_selfemployer, equal_split_labor,
        equal_split_rent),
      ~ .x / december_cpi_100_2003 * 100,
      .names = "real_{.col}"
    )
  )

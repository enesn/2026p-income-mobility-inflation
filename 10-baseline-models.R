## ==================================================================================================#
# Relative Income Mobility around an Inflationary Shock
# May 2025
# EI
## ==================================================================================================#

# ==================================================================================================#
# Estimation samples
# ==================================================================================================#

step_header("10 | Baseline models: does inflation move classes around the distribution? (Fig 4, Tables 1-3)")

step_note("Estimation samples: household-years after 2011, dropping the two classes too small to estimate")

# Every model below is fitted on the same underlying sample: household-years after 2011, outside the
# two classes that are too small to estimate, one row per household and year.

model_base <-
  class_mobility_ext %>%
  select(HKIMLIK, income_year, type, FK060_4, deciles2, decile_prev1,
         p_labor_delta_entry2, esplit_p_labor_delta_entry2, esplit_p_selfemployer_delta_entry2) %>%
  filter(income_year > 2011) %>%
  filter(!type == "Rentier") %>%
  filter(!type == "Financier") %>%
  mutate(
    delta_rank = deciles2 - decile_prev1,
    post_2021  = ifelse(income_year > 2021, 1, 0)
  )

# The decile restrictions are applied before the household-year deduplication, as before.
rank_sample  <- model_base %>% filter(!is.na(delta_rank)) %>%
  distinct(HKIMLIK, income_year, .keep_all = TRUE)
upper_sample <- model_base %>% filter(decile_prev1 > 7, !is.na(delta_rank)) %>%
  distinct(HKIMLIK, income_year, .keep_all = TRUE)
lower_sample <- model_base %>% filter(decile_prev1 < 8, !is.na(delta_rank)) %>%
  distinct(HKIMLIK, income_year, .keep_all = TRUE)

# The household fixed-effects model keeps household-years with a missing rank change.
fe_sample <- model_base %>% distinct(HKIMLIK, income_year, .keep_all = TRUE)

# The extensive-margin models condition on an observed labor income change instead.
margin_sample <-
  model_base %>%
  filter(!is.na(p_labor_delta_entry2)) %>%
  distinct(HKIMLIK, income_year, .keep_all = TRUE) %>%
  mutate(
    entry_into_laborincome   = ifelse(esplit_p_labor_delta_entry2 > 0, 1, 0),
    entry_into_selfempincome = ifelse(esplit_p_selfemployer_delta_entry2 > 0, 1, 0)
  )

# Annual inflation, read once rather than once per model.
annual_inflation <-
  read_xlsx("other-input-data/evds_inflation_fxrate.xlsx") %>%
  mutate(
    income_year = year(as.Date(paste0(Date, "-01"))),
    inflation   = as.numeric(inflation)
  ) %>%
  filter(income_year < 2024) %>%
  summarise(annual_inflation = mean(inflation), .by = income_year)

inflation_sample <- rank_sample %>% left_join(annual_inflation, by = "income_year")


# ==================================================================================================#
# Fig 4, 2-year relative positional changes of social classes
# ==================================================================================================#

step_note("Fig 4: class-by-year decile change, for all deciles and for the top 3 and bottom 7 apart")

# All deciles
m2 <- lm(
  delta_rank ~ as.factor(type) * as.factor(income_year) + as.factor(decile_prev1),
  data = rank_sample, weights = as.numeric(FK060_4)
)
summary(m2)

# Upper deciles
m2upper <- lm(
  delta_rank ~ as.factor(type) * as.factor(income_year) + as.factor(decile_prev1),
  data = upper_sample, weights = as.numeric(FK060_4)
)
summary(m2upper)

# Lower deciles
m2bottom <- lm(
  delta_rank ~ as.factor(type) * as.factor(income_year) + as.factor(decile_prev1),
  data = lower_sample, weights = as.numeric(FK060_4)
)
summary(m2bottom)

# The class-by-year coefficients and their standard errors, long, for plotting. Coefficients that
# lm() drops as aliased keep the NA standard error they had before.
class_year_coefs <- function(model, decile) {
  estimates <- model$coefficients
  errors    <- summary(model)$coefficients[, "Std. Error"]

  tibble(rowname = names(estimates), coeff = unname(estimates)) %>%
    left_join(tibble(rowname = names(errors), se = unname(errors)), by = "rowname") %>%
    mutate(
      year   = str_extract(rowname, "(?<=income_year\\))\\d+"),
      type   = str_extract(rowname, "(?<=type\\))\\w+"),
      decile = decile
    ) %>%
    filter(!is.na(year), !is.na(type))
}

m2s <- bind_rows(
  class_year_coefs(m2,       "All"),
  class_year_coefs(m2bottom, "Bottom 7"),
  class_year_coefs(m2upper,  "Upper 3")
)

# Plot coefficients
(m2plot <- m2s %>%
   mutate(type = ifelse(type == "Transfer", "Transfer-Dependent", ifelse(type == "Self", "Self-Employed", type))) %>%

   ggplot(aes(y = coeff, x = year, color = decile, shape = decile)) +
   geom_point(position = position_dodge(width = 0.45), size = 3) +
   geom_errorbar(
     aes(ymin = coeff - 1.96 * se, ymax = coeff + 1.96 * se),
     width = 0.2,
     position = position_dodge(width = 0.45)
   ) +
   facet_wrap(~type, nrow = 3) +
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(x = "Destination year (t)", y = expression(beta[gt])) +
   theme_bw() +
   scale_color_viridis_d() +
   labs(color = "Sample restriction", shape = "Sample restriction") +
   theme(legend.position = "bottom") +
   theme(
     axis.title.x = element_text(size = 16),
     axis.title.y = element_text(size = 22),
     legend.title = element_text(size = 16),
     legend.text = element_text(size = 16),
     axis.text.x = element_text(angle = 90)
   )
)
ggsave(filename = "outputs-included/fig4-m2plot-theta50.pdf", plot = m2plot, width = 10, height = 8, dpi = 300)

# ==================================================================================================#
# Table 1, New Models in Response to Reviewer Comments in the First Round
# ==================================================================================================#

step_note("Table 1: class times inflation, class times post-2021, and the same with household fixed effects")

# Equation 3
pi_x_class <- lm(
  delta_rank ~ as.factor(type) * log(annual_inflation) + as.factor(decile_prev1),
  data = inflation_sample, weights = as.numeric(FK060_4)
)

# Equation 4
did_post2021 <- lm(
  delta_rank ~ as.factor(type) * as.factor(post_2021) + as.factor(decile_prev1),
  data = inflation_sample, weights = as.numeric(FK060_4)
)

# Equation 5
# Observe the same household over time, but make sure no household shows up twice in a given year.
hhlevel_fe <- fixest::feols(
  deciles2 ~ as.factor(type) * post_2021 | HKIMLIK + income_year,
  data = fe_sample, weights = ~ as.numeric(FK060_4)
)

# Latex table
modelsummary::modelsummary(
  list(
    "(3)" = pi_x_class,
    "(4)" = did_post2021,
    "(5)" = hhlevel_fe
  ),
  stars = TRUE,
  output = "latex"
)

# =====================================================================================================#
# Table 2, Class income reallocation in response to inflation, in response Reviewer 2's first round comment
# =====================================================================================================#

step_note("Table 2: whether classes take up labor or self-employment income after 2021")

# Ext.labor
labor_extensive_margin_response <- lm(
  entry_into_laborincome ~ as.factor(type) * as.factor(post_2021),
  data = margin_sample, weights = as.numeric(FK060_4)
)

# Ext.self-employed
selfemployer_extensive_margin_response <- lm(
  entry_into_selfempincome ~ as.factor(type) * as.factor(post_2021),
  data = margin_sample, weights = as.numeric(FK060_4)
)

# Latex table
modelsummary::modelsummary(
  list(
    "Ext.Labor" = labor_extensive_margin_response,
    "Ext.Self-employer" = selfemployer_extensive_margin_response
  ),
  stars = TRUE,
  output = "latex"
)

# ==================================================================================================#
# Table 3, Labor market adjusment
# ==================================================================================================#

step_note("Table 3: how laborer households adjust — hours, months, job changes, informality, minimum wage")

# Labor market sample restriction
lmarket_panel <-
  class_mobility_ext %>%
  filter(type == "Laborer") %>%

  # FI-coded labor market variables refer to the income reference year + 1, except worked_months.
  mutate(
    jtjt              = ifelse(FI255 == 1, 1, 0),
    jtjt_betterjob    = ifelse(FI256 == 1, 1, 0),
    informality       = ifelse(FI190 == 2, 1, 0),
    working_hour      = as.numeric(ifelse(!is.na(FI150), FI150, 0)),
    worked_months     = as.numeric(ifelse(!is.na(FI240), FI240, 0)),
    permenant_job     = ifelse(FI210 == 1, 1, 0),
    lmarket_year      = income_year + 1,
    age               = FK070,
    female            = ifelse(FK090 == 2, 1, 0),
    hh_position       = FK095,
    employment_status = FI010,
    occupation        = FI130,
    sector            = FI140,
    education         = FE030,
    FK060_4           = as.numeric(FK060_4),
    minimum_wage      = minimum_wage * 12,
    around_minimumwage = ifelse(personal_labor_income < (minimum_wage * 1.2) &
                                  personal_labor_income > (minimum_wage * 0.8), 1, 0)
  ) %>%

  # NB: the key is HKIMLIK *and* hh_size, because both frames carry a column of that name. The
  # implicit by= was already doing this; it is spelled out so it cannot change silently. The two
  # hh_size definitions never agree (survey household size vs. person-years per household), so
  # hh_weight comes out NA throughout - it is not read anywhere, but that is why.
  left_join(
    class_mobility %>%
      summarise(hh_weight = sum(as.numeric(FK060_4)), hh_size = n(), .by = HKIMLIK),
    by = join_by(HKIMLIK, hh_size)
  ) %>%

  select(FK060_4, FKIMLIK, HKIMLIK, type, hh_weight, hh_size, income_year, age, female,
         head_manager_professional, head_occupation, head_edu, employment_status, occupation,
         sector, education, hh_position, lmarket_year, deciles2, decile_prev1, decile_prev2,
         jtjt, jtjt_betterjob, around_minimumwage, informality, working_hour, worked_months,
         permenant_job, equal_split_income, personal_labor_income, personal_pension_income,
         personal_selfemployer_income, personal_employer_income, personal_transfer_income,
         hh_financial_income, hh_rental_income, imputed_rent) %>%

  # A missing code means the person is not of that kind, not that the value is unknown.
  mutate(
    across(c(jtjt_betterjob, informality, permenant_job, occupation, sector),
           ~ ifelse(is.na(.x), 0, .x))
  ) %>%

  # One row per household-year, every variable as a weighted mean over its members.
  summarise(
    across(where(is.numeric), ~ weighted.mean(.x, w = FK060_4, na.rm = TRUE), .names = "{.col}_wmean"),
    .by = c(HKIMLIK, income_year)
  ) %>%

  arrange(HKIMLIK, income_year) %>%

  # The labor market variables are reported for the income year + 1
  mutate(
    jtjt_wmean_prev1               = lag(jtjt_wmean, 2),
    around_minimumwage_wmean_prev1 = lag(around_minimumwage_wmean, 1),
    informality_wmean_prev1        = lag(informality_wmean, 2),
    working_hour_wmean_prev1       = lag(working_hour_wmean, 2),
    worked_months_wmean_prev1      = lag(worked_months_wmean, 1),

    jtjt_wmean_prev2               = lag(jtjt_wmean, 3),
    around_minimumwage_wmean_prev2 = lag(around_minimumwage_wmean, 2),
    informality_wmean_prev2        = lag(informality_wmean, 3),
    working_hour_wmean_prev2       = lag(working_hour_wmean, 3),
    worked_months_wmean_prev2      = lag(worked_months_wmean, 2),
    .by = HKIMLIK
  ) %>%

  rename(hh_weight = hh_weight_wmean) %>%

  left_join(
    class_mobility_ext %>%
      select(HKIMLIK, income_year, deciles2, decile_prev1, decile_prev2) %>%
      distinct(HKIMLIK, income_year, .keep_all = TRUE),
    by = c("HKIMLIK", "income_year")
  )

# Model
lm_model_3wayinteraction <- lm(
  delta_rank ~
    as.factor(decile_prev1) +
    delta_worked_months_wmean * post_2021 * lower_decile +
    delta_jtjt_wmean * post_2021 * lower_decile +
    delta_informality_wmean * post_2021 * lower_decile +
    delta_aroundmw_mean * post_2021 * lower_decile +
    hh_size_wmean * post_2021 * lower_decile +
    around_minimumwage_wmean_prev1 * post_2021 * lower_decile +
    informality_wmean_prev1 * post_2021 * lower_decile +
    delta_working_hour_wmean * post_2021 * lower_decile,

  data = lmarket_panel %>%
    filter(income_year > 2011) %>%
    mutate(
      delta_rank                = deciles2 - decile_prev1,
      delta_aroundmw_mean       = around_minimumwage_wmean - around_minimumwage_wmean_prev1,
      delta_jtjt_wmean          = jtjt_wmean - jtjt_wmean_prev1,
      delta_informality_wmean   = informality_wmean - informality_wmean_prev1,
      delta_worked_months_wmean = worked_months_wmean - worked_months_wmean_prev1,
      delta_working_hour_wmean  = working_hour_wmean - working_hour_wmean_prev1,
      post_2021                 = ifelse(income_year > 2021, 1, 0),
      lower_decile              = ifelse(decile_prev1 < 8, 1, 0)
    ) %>%
    filter(!is.na(delta_jtjt_wmean)) %>%
    distinct(HKIMLIK, income_year, .keep_all = TRUE),

  weights = as.numeric(FK060_4_wmean)
)

stargazer::stargazer(lm_model_3wayinteraction)

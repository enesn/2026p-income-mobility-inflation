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
saved_note("outputs-included/fig4-m2plot-theta50.pdf")

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
#
# Class is fixed over the four years a household is observed, so its main effect is collinear with the
# household fixed effect and is not identified here: written as as.factor(type) * post_2021, fixest
# dropped some of those main effects and returned the rest with standard errors in the millions, which
# is what the table used to print. i() asks for the class-by-post-2021 interactions alone, which are
# the identified terms and come out numerically identical to what the interaction rows held before.
# The post-2021 main effect is absorbed by the year fixed effect either way.
hhlevel_fe <- fixest::feols(
  deciles2 ~ i(type, post_2021, ref = "Employer") | HKIMLIK + income_year,
  data = fe_sample, weights = ~ as.numeric(FK060_4)
)

# --------------------------------------------------
# In response to Reviewer 2's "Inference" comment in round three
# --------------------------------------------------

# Naive collapse: "aggregate the data to the group-year level
# (by taking the mean of the dependent variable within each cell)
# and re-estimate the data on cell- means."

step_note("Table 1 robustness: equation 4 re-estimated on class-year cell means")

class_year_cells <-
  inflation_sample %>%
  summarise(
    delta_rank   = weighted.mean(delta_rank, w = as.numeric(FK060_4)),
    cell_weight  = sum(as.numeric(FK060_4)),
    n_households = n(),
    .by = c(type, income_year)
  ) %>%
  mutate(post_2021 = ifelse(income_year > 2021, 1, 0))

step_note(sprintf("%d class-year cells over %d classes and %d years; smallest cell holds %d households",
                  nrow(class_year_cells), n_distinct(class_year_cells$type),
                  n_distinct(class_year_cells$income_year), min(class_year_cells$n_households)))

# Each cell mean is estimated off a different number of households, so the cell regression is
# weighted by the population its cell stands for
did_post2021_cellmeans <- lm(
  delta_rank ~ as.factor(type) * as.factor(post_2021),
  data = class_year_cells, weights = cell_weight
)

# The collapse above has to drop the origin-decile control, since decile_prev1 varies inside a cell
# and has no cell-level counterpart. 
decile_adjustment <- lm(
  delta_rank ~ as.factor(decile_prev1),
  data = inflation_sample, weights = as.numeric(FK060_4), na.action = na.exclude
)

class_year_cells_adj <-
  inflation_sample %>%
  mutate(delta_rank_adj = residuals(decile_adjustment)) %>%
  filter(!is.na(delta_rank_adj)) %>%
  summarise(
    delta_rank_adj = weighted.mean(delta_rank_adj, w = as.numeric(FK060_4)),
    cell_weight    = sum(as.numeric(FK060_4)),
    .by = c(type, income_year)
  ) %>%
  mutate(post_2021 = ifelse(income_year > 2021, 1, 0))


did_post2021_cellmeans_adj <- lm(
  delta_rank_adj ~ as.factor(type) * as.factor(post_2021),
  data = class_year_cells_adj, weights = cell_weight
)

# Latex table
# Same treatment as Table 2 below: readable row labels, the origin-decile dummies folded into a single
# line at the foot instead of forty rows of controls, and the table shrunk to the text width if the row
# labels push it over.
tidy_table1_name <- function(x) {
  x %>%
    str_replace_all(fixed("as.factor(type)"), "") %>%
    str_replace_all(fixed("type::"), "") %>%
    str_replace_all(fixed("as.factor(post_2021)1"), "Post-2021") %>%
    str_replace_all("\\bpost_2021\\b", "Post-2021") %>%
    str_replace_all(fixed("log(annual_inflation)"), "Log inflation") %>%
    str_replace_all(fixed("×"), "$\\times$") %>%
    str_replace_all(fixed(":"), " $\\times$ ")
}

# The fixed-effects rows come out of fixest's own glance(), so they are named by the variable they
# absorb; the rest of modelsummary's default fit statistics (AIC, BIC, log-likelihood, RMSE, the within
# R2) say nothing the paper uses and are dropped.
table1_gof <- tribble(
  ~raw,              ~clean,                    ~fmt,
  "nobs",            "Observations",            0,
  "r.squared",       "R$^2$",                   3,
  "FE: HKIMLIK",     "Household fixed effects", 0,
  "FE: income_year", "Year fixed effects",      0
)

stars_note_option <- getOption("modelsummary_stars_note")
options(modelsummary_stars_note = FALSE)

table1_body <-
  modelsummary::modelsummary(
    list(
      "(3)" = pi_x_class,
      "(4)" = did_post2021,
      "(5)" = hhlevel_fe,
      "(6)" = did_post2021_cellmeans_adj #newly added
    ),
    stars       = TRUE,
    coef_rename = tidy_table1_name,
    coef_omit   = "decile_prev1",
    gof_map     = table1_gof,
    # Equation 5 holds the origin decile fixed through the household effect; equation 6 cannot carry a
    # decile dummy at all, and nets the deciles out of the dependent variable before collapsing.
    add_rows    = tibble(
      term  = "Origin decile dummies",
      `(3)` = "X", `(4)` = "X", `(5)` = "", `(6)` = "residualised"
    ),
    output      = "tinytable"
  ) %>%
  tinytable::theme_latex(
    environment_table = FALSE,
    resize_width      = 1,
    resize_direction  = "down"
  ) %>%
  tinytable::save_tt("latex")

options(modelsummary_stars_note = stars_note_option)

table1_note <- paste(
  c(
    "\\textit{Notes:}",
    "+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001."
  ),
  collapse = "\n"
)

paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Relative income mobility by class, against inflation and against the\n",
  "         post-2021 break}\n",
  "\\label{tab:class-inflation-interactions}\n",
  table1_body,
  "\n\n\\begin{minipage}{\\linewidth}\n",
  "\\footnotesize\n",
  "\\setlength{\\parindent}{0pt}\\setlength{\\parskip}{0.5em}\n",
  table1_note,
  "\n\\end{minipage}\n",
  "\\end{table}\n"
) %>%
  save_tex("outputs-included/table1-class-inflation-interactions.tex")

# =====================================================================================================#
# Table 2, Class income reallocation in response to inflation, in response to Reviewer 2's first round comment
# =====================================================================================================#

# --------------------------------------------------------------------------------------------------
# In response to Reviewer 2's "Extensive Margin Table" comment in round three
# --------------------------------------------------------------------------------------------------
# Ingore persistence-based definition of class, just focus on annual dominant income to define a class.

step_note("Annual classes: how the household income mix responds to inflation, class fixed at t-1")

# The annual class. 06 asks which source was dominant in more than `persistence_threshold` of the
# four years; here the same dominance test is applied to year t alone:

class_annual_panel <-
  class_mobility %>%
  arrange(FKIMLIK, income_year) %>%

  mutate(
    across(
      c(financial    = equal_split_financial,
        rental       = equal_split_rental,
        labor        = equal_split_labor,
        pension      = equal_split_pension,
        employer     = equal_split_employer,
        selfemployer = equal_split_selfemployer,
        transfer     = equal_split_transfers),
      ~ .x / equal_split_income > dominance_threshold_theta,
      .names = "{.col}_dominant"
    ),

    class_annual = case_when(
      if_any(ends_with("_dominant"), is.na) ~ NA_character_,
      financial_dominant                    ~ "Financier",
      rental_dominant                       ~ "Rentier",
      labor_dominant                        ~ "Laborer",
      pension_dominant                      ~ "Pensioner",
      employer_dominant                     ~ "Employer",
      selfemployer_dominant                 ~ "Self-employer",
      transfer_dominant                     ~ "Transfer-Dependent",
      .default = "Mixed"
    )
  ) %>%

  # Each person contributes four consecutive years, so a within-person lag is a shift; `adjacent`
  # only guards the first year of each spell, where there is no t-1 to read.
  mutate(
    adjacent           = income_year - lag(income_year) == 1,
    class_annual_prev1 = if_else(adjacent, lag(class_annual), NA_character_),
    .by = FKIMLIK
  ) %>%

  select(-ends_with("_dominant"), -adjacent)

# One row per household-year, as everywhere else in this script, carrying the class the household
# held the year before and the inflation it then met.
annual_class_hh <-
  class_annual_panel %>%
  distinct(HKIMLIK, income_year, .keep_all = TRUE) %>%
  select(HKIMLIK, income_year, type, FK060_4, class_annual, class_annual_prev1)

too_small_to_estimate <- c("Financier", "Rentier")

mix_change_sample <-
  annual_class_hh %>%
  filter(income_year > 2011) %>%
  filter(!type == "Rentier") %>%
  filter(!type == "Financier") %>%
  filter(!is.na(class_annual), !is.na(class_annual_prev1)) %>%
  filter(!class_annual_prev1 %in% too_small_to_estimate) %>%
  mutate(
    post_2021    = ifelse(income_year > 2021, 1, 0),
    changed_mix  = ifelse(class_annual != class_annual_prev1, 1, 0)
  )

# Each outcome against the post-2021 break, as in Table 1's equation 4.
mix_change_post <- lm(
  changed_mix ~ as.factor(class_annual_prev1) * as.factor(post_2021),
  data = mix_change_sample, weights = as.numeric(FK060_4)
)

# ==================================================================================================#
# What "any change of mix" is made of
# ==================================================================================================#

# A household leaves its class in one of two quite different ways, and the binary above cannot tell
# them apart. Either no source clears theta any more, so the household lands in the residual "Mixed"
# state and nothing has replaced what it lived on - the household's own source was diluted. Or a
# different source clears theta instead, and something has genuinely taken over.
#
# The three destinations below are mutually exclusive and between them exhaust a change, so on a
# linear probability model fitted to the same sample with the same regressors their coefficients sum
# to the coefficient on `changed_mix` exactly. The stopifnot() holds the arithmetic to that.
earned_classes <- c("Laborer", "Self-employer", "Employer")

mix_change_sample <-
  mix_change_sample %>%
  mutate(
    to_no_dominant = ifelse(changed_mix == 1 & class_annual == "Mixed", 1, 0),
    to_earned      = ifelse(changed_mix == 1 & class_annual %in% earned_classes, 1, 0),
    to_unearned    = ifelse(changed_mix == 1 &
                              !class_annual %in% c("Mixed", earned_classes), 1, 0)
  )

stopifnot(with(mix_change_sample,
               all(to_no_dominant + to_earned + to_unearned == changed_mix)))

destination_model <- function(outcome) {
  lm(as.formula(paste(outcome, "~ as.factor(class_annual_prev1) * as.factor(post_2021)")),
     data = mix_change_sample, weights = as.numeric(FK060_4))
}

mix_change_to_no_dominant <- destination_model("to_no_dominant")
mix_change_to_earned      <- destination_model("to_earned")
mix_change_to_unearned    <- destination_model("to_unearned")

# Latex table
# noise down the side of the table. 
tidy_coef_name <- function(x) {
  x %>%
    str_replace_all(fixed("as.factor(class_annual_prev1)"), "") %>%
    str_replace_all(fixed("as.factor(post_2021)1"), "Post-2021") %>%
    str_replace_all(fixed(":"), " $\\times$ ")
}

# modelsummary writes the caption 
stars_note_option <- getOption("modelsummary_stars_note")
options(modelsummary_stars_note = FALSE)

# resize_direction = "down" shrinks the table to the text width when the row labels push it over and
# leaves it alone when they do not, so the type never comes out larger than the body of the paper.
income_mix_body <-
  modelsummary::modelsummary(
    list(
      "Any change"        = mix_change_post,
      "To no dominant"    = mix_change_to_no_dominant,
      "To earned income"  = mix_change_to_earned,
      "To other unearned" = mix_change_to_unearned
    ),
    stars       = TRUE,
    coef_rename = tidy_coef_name,
    gof_map     = c("nobs", "r.squared"),
    output      = "tinytable"
  ) %>%
  tinytable::theme_latex(
    environment_table = FALSE,
    resize_width      = 1,
    resize_direction  = "down"
  ) %>%
  tinytable::save_tt("latex")

options(modelsummary_stars_note = stars_note_option)

income_mix_note <- paste(
  c(
    "\\textit{Notes:} ",
    "In column (1), the dependent variable is a dummy variable that reports whether the household's dominant",
    "income source in year $t$ differs from the one it held in year $t-1$. Columns (2) to (4) decompose that change by where the",
    "household's income mix ended up. Columns (2) to (4) are mutually exclusive, so they sum to column (1)",
    "exactly. \\emph{To no dominant} means no income source reaches the benchmark dominance threshold any",
    "longer; \\emph{to",
    "earned income} means labor, self-employment or employer income became the dominant income sources; \\emph{to other",
    "unearned} means pension, transfer, financial or rental income took over.",
    "",
    "The key explanatory variable in these models is the class the household did belong to in the \\emph{previous} year.",
    "Contrast this with the four-year persistent class definition employed by the models discussed elsewhere in the paper.",
    "Employer is the reference class. Homoskedastic standard errors in parentheses.",
    "+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001."
  ),
  collapse = "\n"
)

paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Change in the Household's Dominant Income Source after 2021,\n",
  "         by the Class It Held a Year Earlier}\n",
  "\\label{tab:income-mix-response}\n",
  income_mix_body,
  "\n\n\\begin{minipage}{\\linewidth}\n",
  "\\footnotesize\n",
  # Table notes read as blocks, not as prose: no first-line indent, a little air between them.
  "\\setlength{\\parindent}{0pt}\\setlength{\\parskip}{0.5em}\n",
  income_mix_note,
  "\n\\end{minipage}\n",
  "\\end{table}\n"
) %>%
  save_tex("outputs-included/table2-income-mix-response-to-inflation.tex")

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

# Latex table
# The caption and the label were empty here, which printed a bare "Table 3:" in the paper and left the
# table impossible to \ref{}; the appendix table that prints this model in full refers to it by label.
stargazer::stargazer(
  lm_model_3wayinteraction,
  title = "Labor market adjustment and relative income mobility of laborer households",
  label = "tab:labor-market-adjustment",
  out = "outputs-included/table3-labor-market-adjustment.tex"
)

## ==================================================================================================#
# Inflation and redistribution in Turkey
# May 2025
# EI
## ==================================================================================================#

# ==================================================================================================#
# Fig 4, 2-year relative positional changes of social classes
# ==================================================================================================#

# All deciles
summary(
 m2 <- lm( 
    delta_rank ~ as.factor(type)*as.factor(income_year) + as.factor(decile_prev1),
    
    data = class_mobility_ext %>%
      filter(income_year > 2011) %>%
      filter(!type == "Rentier") %>% 
      filter(!type == "Financier") %>% 
      mutate(delta_rank = deciles2 - decile_prev1) %>% 
      filter(!is.na(delta_rank)) %>% 
      distinct(HKIMLIK, income_year,  .keep_all = T),
    weights = as.numeric(FK060_4)
    
  )
)

# Upper deciles
summary(
  m2upper <- lm( 
    delta_rank ~ as.factor(type)*as.factor(income_year)+ as.factor(decile_prev1),

    data = class_mobility_ext %>%
      filter(decile_prev1 > 7) %>%
      filter(income_year > 2011) %>%
      filter(!type == "Rentier") %>% 
      filter(!type == "Financier") %>% 
      mutate(delta_rank = deciles2 - decile_prev1) %>% 
      filter(!is.na(delta_rank)) %>% 
      distinct(HKIMLIK, income_year,  .keep_all = T),

    weights = as.numeric(FK060_4)
    
  )
)
# Lower deciles
summary(
  m2bottom <- lm( 
    delta_rank ~ as.factor(type)*as.factor(income_year)+ as.factor(decile_prev1),
    
    data = class_mobility_ext %>%
      filter(decile_prev1 < 8) %>%
      filter(income_year > 2011) %>%
      filter(!type == "Rentier") %>% 
      filter(!type == "Financier") %>% 
      mutate(delta_rank = deciles2 - decile_prev1) %>% 
      filter(!is.na(delta_rank)) %>% 
      distinct(HKIMLIK, income_year,  .keep_all = T),

    weights = as.numeric(FK060_4)
    
  )
)

# Extract coefficients from the model All
m2coeff <- as.data.frame(m2$coefficients)
m2se <- as.data.frame(summary(m2)$coefficients[, "Std. Error"])

# Extract coefficients from the model Upper
m2uppercoeff <- as.data.frame(m2upper$coefficients)
m2upperse <- as.data.frame(summary(m2upper)$coefficients[, "Std. Error"])

# Extract coefficients from the model Lower
m2bottomcoeff <- as.data.frame(m2bottom$coefficients)
m2bottomse <- as.data.frame(summary(m2bottom)$coefficients[, "Std. Error"])

# Toward rbinding
m2_df <- m2coeff %>% rownames_to_column() %>%
  rename("coeff"="m2$coefficients") %>%
  left_join(
    m2se %>% rownames_to_column() %>%
      rename("se"='summary(m2)$coefficients[, "Std. Error"]')
    
  )  %>% 
  mutate(
    year = str_extract(rowname, "(?<=income_year\\))\\d+"),
    type = str_extract(rowname, "(?<=type\\))\\w+")
  ) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(type)) %>%
  mutate(decile = "All")

m2upper_df <- m2uppercoeff %>% rownames_to_column() %>%
  rename("coeff"="m2upper$coefficients") %>%
  left_join(
    m2upperse %>% rownames_to_column() %>%
      rename("se"='summary(m2upper)$coefficients[, "Std. Error"]')
    
  )  %>% 
  mutate(
    year = str_extract(rowname, "(?<=income_year\\))\\d+"),
    type = str_extract(rowname, "(?<=type\\))\\w+")
  ) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(type)) %>%
  mutate(decile = "Upper 3")


m2bottom_df <- m2bottomcoeff %>% rownames_to_column() %>%
  rename("coeff"="m2bottom$coefficients") %>%
  left_join(
    m2bottomse %>% rownames_to_column() %>%
      rename("se"='summary(m2bottom)$coefficients[, "Std. Error"]')
    
  )  %>% 
  mutate(
    year = str_extract(rowname, "(?<=income_year\\))\\d+"),
    type = str_extract(rowname, "(?<=type\\))\\w+")
  ) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(type))  %>%
  mutate(decile = "Bottom 7")

# Rbind coefficients
m2s <- rbind(m2_df, m2bottom_df, m2upper_df)

# Plot coefficients
 (m2plot <- m2s %>%
    mutate(type = ifelse(type == "Transfer", "Transfer-Dependent", ifelse(type == "Self", "Self-Employed",type))) %>%
    
    ggplot(aes(y=coeff, x= year, color=decile, shape = decile)) +
     geom_point(position = position_dodge(width = 0.45), size = 3) +
     geom_errorbar(
       aes(ymin = coeff - 1.96 * se, ymax = coeff + 1.96 * se),
       width = 0.2,
       position = position_dodge(width = 0.45)
                 ) +
    facet_wrap(~type, nrow = 3) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x= "Destination year (t)", y = expression(beta[gt]))+
    theme_bw() +
    scale_color_viridis_d()+
    labs(color = "Sample restriction", shape = "Sample restriction") +
    theme(legend.position = "bottom")+
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

# Equation 3 
pi_x_class <- lm(
  delta_rank ~ as.factor(type)*log(annual_inflation) + as.factor(decile_prev1),
  data = class_mobility_ext %>%
    filter(income_year > 2011) %>%
    
    filter(!type == "Rentier") %>% 
    filter(!type == "Financier") %>% 
    mutate(delta_rank = deciles2 - decile_prev1) %>% 
    filter(!is.na(delta_rank)) %>% 
    mutate(post_2021 = ifelse(income_year > 2021, 1,0) ) %>%
    left_join(
      read_xlsx("other-input-data/evds_inflation_fxrate.xlsx") %>% 
        mutate(
          
          Date = as.Date(paste0(Date, "-01")),
          inflation = as.numeric(inflation),
          fxchange = as.numeric(fxchange)
        ) %>%   
        filter(year(Date) < 2024) %>% mutate(income_year = year(Date)) %>% group_by(income_year) %>% summarise(annual_inflation = mean(inflation))
    ) %>%
    distinct(HKIMLIK, income_year,  .keep_all = T)  ,
  weights = as.numeric(FK060_4)
  
)

# Equation 4
did_post2021 <- lm(
  delta_rank ~ as.factor(type)*as.factor(post_2021) + as.factor(decile_prev1),
  data = class_mobility_ext %>%
    filter(income_year > 2011) %>%
    
    filter(!type == "Rentier") %>% 
    filter(!type == "Financier") %>% 
    mutate(delta_rank = deciles2 - decile_prev1) %>% 
    filter(!is.na(delta_rank)) %>% 
    mutate(post_2021 = ifelse(income_year > 2021, 1,0) ) %>%
    left_join(
      read_xlsx("other-input-data/evds_inflation_fxrate.xlsx") %>% 
        mutate(
          
          Date = as.Date(paste0(Date, "-01")),
          inflation = as.numeric(inflation),
          fxchange = as.numeric(fxchange)
        ) %>%   
        filter(year(Date) < 2024) %>% mutate(income_year = year(Date)) %>% group_by(income_year) %>% summarise(annual_inflation = mean(inflation))
    ) %>%
    distinct(HKIMLIK, income_year,  .keep_all = T)  ,
  weights = as.numeric(FK060_4)
  
)

# Equation 5
hhlevel_fe <- fixest::feols(deciles2 ~ as.factor(type) * post_2021 | HKIMLIK + income_year, 
      data = class_mobility_ext %>%
        filter(income_year > 2011) %>%
        filter(!type == "Rentier") %>% 
        filter(!type == "Financier") %>% 
        mutate(post_2021 = ifelse(income_year > 2021, 1,0) ) %>%
        distinct(HKIMLIK, income_year,  .keep_all = T), #observe the same household over time, but make sure no household show up twice in a given year
      weights =~as.numeric(FK060_4))

fe_sum <- summary(hhlevel_fe)
fe_model_for_stargazer <- hhlevel_fe
class(fe_model_for_stargazer) <- "lm"

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

# Ext.labor 
labor_extensive_margin_response <- lm(
  entry_into_laborincome ~ as.factor(type)*as.factor(post_2021),
  data = class_mobility_ext %>%
    filter(income_year > 2011) %>%
    mutate(entry_into_laborincome = ifelse(esplit_p_labor_delta_entry2>0, 1, 0)) %>%
    filter(!type == "Rentier") %>% 
    filter(!type == "Financier") %>% 
    filter(!is.na(p_labor_delta_entry2)) %>%
    mutate(post_2021 = ifelse(income_year > 2021, 1,0) ) %>%
    distinct(HKIMLIK, income_year,  .keep_all = T) ,
  weights = as.numeric(FK060_4)
  
)

# Ext.self-employed
selfemployer_extensive_margin_response <-lm(
  entry_into_selfempincome ~ as.factor(type)*as.factor(post_2021),
  data = class_mobility_ext %>%
    filter(income_year > 2011) %>%
    mutate(entry_into_selfempincome = ifelse(esplit_p_selfemployer_delta_entry2>0, 1, 0)) %>%
    filter(!type == "Rentier") %>% 
    filter(!type == "Financier") %>% 
    filter(!is.na(p_labor_delta_entry2)) %>%
    mutate(post_2021 = ifelse(income_year > 2021, 1,0) ) %>%
    distinct(HKIMLIK, income_year,  .keep_all = T) ,
  weights = as.numeric(FK060_4)
  
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

  # Labor market sample restriction
 lmarket_panel <- class_mobility_ext %>% 
  filter(type == "Laborer") %>%
  mutate(jtjt = ifelse(FI255 == 1, 1, 0)) %>% # income reference year + 1
  mutate(jtjt_betterjob = ifelse(FI256 == 1, 1, 0)) %>% # income reference year + 1
  mutate(informality = ifelse(FI190==2, 1, 0)) %>%  # income reference year + 1
  mutate(working_hour = ifelse(!is.na(FI150), FI150, 0)) %>% # income reference year + 1
  mutate(worked_months = ifelse(!is.na(FI240), FI240, 0)) %>% # income reference year 
  mutate(permenant_job = ifelse(FI210 == 1, 1, 0)) %>%
  mutate(lmarket_year = income_year + 1) %>% 
  mutate(age = FK070) %>%
  mutate(female = ifelse(FK090 == 2, 1, 0)) %>%
  mutate(hh_position = FK095) %>%
  mutate(employment_status = FI010) %>%
  mutate(occupation = FI130) %>%
  mutate(sector = FI140) %>%
  mutate(education = FE030) %>%
  mutate(minimum_wage  = minimum_wage*12) %>% 
  mutate(around_minimumwage= ifelse(personal_labor_income < (minimum_wage*1.2) & personal_labor_income > (minimum_wage*0.8), 1, 0)) %>% 
    left_join(
      class_mobility %>%
        group_by(HKIMLIK) %>%
        summarise(hh_weight = sum(as.numeric(FK060_4)), hh_size =n())
    ) %>%
    select(FK060_4,FKIMLIK, HKIMLIK, type, hh_weight, hh_size, income_year, age, female, head_manager_professional, head_occupation, head_edu, employment_status, occupation, sector, education, hh_position, lmarket_year, deciles2, decile_prev1, decile_prev2, jtjt, jtjt_betterjob, around_minimumwage, informality, working_hour, worked_months, permenant_job, equal_split_income, personal_labor_income, personal_pension_income, personal_selfemployer_income, personal_employer_income, personal_transfer_income, hh_financial_income, hh_rental_income, imputed_rent) %>%
    mutate(jtjt_betterjob = ifelse(is.na(jtjt_betterjob), 0,  jtjt_betterjob)) %>%
    mutate(informality = ifelse(is.na(informality), 0,  informality)) %>%
    mutate(permenant_job = ifelse(is.na(permenant_job), 0,  permenant_job)) %>%
    mutate(occupation = ifelse(is.na(occupation), 0 , occupation)) %>%
    mutate(sector = ifelse(is.na(sector), 0, sector)) %>% 
    mutate(working_hour = as.numeric(working_hour)) %>%
    mutate(worked_months = as.numeric(worked_months)) %>%
    mutate(FK060_4 = as.numeric(FK060_4)) %>% 
    group_by(HKIMLIK, income_year) %>%
    summarise(
      # Weighted mean
      across(
        where(is.numeric),
        ~ weighted.mean(.x, w = FK060_4, na.rm = TRUE),
        .names = "{.col}_wmean"
      ),
      # Weighted sum
      across(
        where(is.numeric),
        ~ sum(.x * FK060_4, na.rm = TRUE),
        .names = "{.col}_wsum"
      ),
      .groups = "drop"
    ) %>%
    arrange(HKIMLIK, income_year) %>%
    group_by(HKIMLIK) %>%
    mutate(
      jtjt_wmean_prev1 = lag(lag(jtjt_wmean,1), 1), #reference period of labor market variables = income reference year + 1
      around_minimumwage_wmean_prev1 = lag(around_minimumwage_wmean, 1), 
      informality_wmean_prev1 = lag(lag(informality_wmean,1), 1),
      working_hour_wmean_prev1 = lag(lag(working_hour_wmean,1), 1),
      worked_months_wmean_prev1 = lag(worked_months_wmean, 1),
      
      jtjt_wmean_prev2 = lag(lag(jtjt_wmean,1), 2), #reference period of labor market variables = income reference year + 1
      around_minimumwage_wmean_prev2 = lag(around_minimumwage_wmean, 2), 
      informality_wmean_prev2 = lag(lag(informality_wmean,1), 2),
      working_hour_wmean_prev2 = lag(lag(working_hour_wmean,1), 2),
      worked_months_wmean_prev2 = lag(worked_months_wmean, 2),  
    ) %>%
    ungroup() %>% 
    rename(hh_weight = hh_weight_wmean) %>%
  left_join(
    class_mobility_ext %>% distinct(HKIMLIK, income_year, .keep_all = T) %>%
      select(HKIMLIK, income_year, deciles2, decile_prev1, decile_prev2)
  )

 #Model 
  summary(
    lm_model_3wayinteraction <-  lm(
      delta_rank ~ 
        as.factor(decile_prev1) +
        delta_worked_months_wmean * post_2021*lower_decile +
        delta_jtjt_wmean * post_2021*lower_decile + 
        delta_informality_wmean * post_2021*lower_decile  + 
        delta_aroundmw_mean * post_2021*lower_decile +
        hh_size_wmean * post_2021*lower_decile+
        around_minimumwage_wmean_prev1 * post_2021*lower_decile +
        informality_wmean_prev1 * post_2021*lower_decile +
        delta_working_hour_wmean * post_2021*lower_decile,
      
      data = lmarket_panel %>%
        filter(income_year > 2011) %>%
        mutate(delta_rank = deciles2 - decile_prev1) %>%
        mutate(delta_aroundmw_mean = around_minimumwage_wmean - around_minimumwage_wmean_prev1) %>% 
        mutate(delta_jtjt_wmean = jtjt_wmean - jtjt_wmean_prev1) %>% 
        mutate(delta_informality_wmean = informality_wmean - informality_wmean_prev1) %>%
        mutate(delta_worked_months_wmean = worked_months_wmean - worked_months_wmean_prev1) %>% 
        mutate(delta_working_hour_wmean = working_hour_wmean - working_hour_wmean_prev1)  %>%
        mutate(post_2021 = ifelse(income_year > 2021, 1,0) ) %>%
        mutate(lower_decile = ifelse(decile_prev1 < 8,1,0)) %>% 
        filter(!is.na(delta_jtjt_wmean))  %>%
        distinct(HKIMLIK, income_year,  .keep_all = T),
      
      weights = as.numeric(FK060_4_wmean)
      
    )
  )
  stargazer::stargazer(lm_model_3wayinteraction)

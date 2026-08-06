## ==================================================================================================#
# Inflation and redistribution in Turkey
# May 2025
# EI
## ==================================================================================================#

## ==================================================================================================#
# Social classes
## ==================================================================================================#

# An income source is dominant in a person-year when its equal-split amount exceeds theta of that
# year's equal-split income. A person belongs to the class of the first source, in the priority
# order below, that is dominant in more of their four years than the persistence threshold.
# The seven sources share the same recipe, so across() applies it once instead of spelling out
# seven near-identical dominance and persistence columns.

class_mobility <-
  income_decomposed %>%

  # In how many of the person's years was each source dominant?
  mutate(
    across(
      c(financial    = equal_split_financial,
        rental       = equal_split_rental,
        labor        = equal_split_labor,
        pension      = equal_split_pension,
        employer     = equal_split_employer,
        selfemployer = equal_split_selfemployer,
        transfer     = equal_split_transfers),
      ~ sum(.x / equal_split_income > dominance_threshold_theta),
      .names = "{.col}_years"
    ),
    .by = FKIMLIK
  ) %>%

  mutate(
    type = case_when(
      # A household with no disposable income has no defined shares, and the counts above come out
      # NA. Those people were left unclassified before and still are.
      if_any(ends_with("_years"), is.na)         ~ NA_character_,
      financial_years    > persistence_threshold ~ "Financier",
      rental_years       > persistence_threshold ~ "Rentier",
      labor_years        > persistence_threshold ~ "Laborer",
      pension_years      > persistence_threshold ~ "Pensioner",
      employer_years     > persistence_threshold ~ "Employer",
      selfemployer_years > persistence_threshold ~ "Self-employer",
      transfer_years     > persistence_threshold ~ "Transfer-Dependent",
      .default = "Mixed"
    )
  ) %>%

  select(-ends_with("_years"))


## ==================================================================================================#

(mobility_of_class <- 
    class_mobility %>% 
    filter(!is.na(p_labor_entry2_contribution)) %>%
    filter(!type == "Financier") %>% 
    filter(!type == "Rentier") %>% 
    mutate(mobility = ifelse( !(upward_2yr == 1 | downward_2yr == 1), "immobile", ifelse(upward_2yr == 1, "upward","downward"))) %>% 
    filter(income_year > 2011) %>%
    
    group_by(income_year, type, mobility) %>% 
    # summarise(n=n()) %>%
    summarise(n=sum(as.numeric(FK060_4)), obs = n() ) %>%
    # filter(obs > 20) %>%
    
    group_by(income_year, type) %>% mutate(sum=sum(n)) %>% mutate(p = n/sum) %>% 
    ggplot(aes(x=income_year, y = p*100)) +
    geom_col(aes(fill=mobility)) +
    facet_wrap(~type) +
    scale_x_continuous(breaks= c(seq(2012, 2023, 1)))+
    labs( x = "Destination Year", y = "%")+
    scale_fill_viridis_d() +
    theme_bw()+
    theme(legend.position = "bottom", legend.title = element_blank())+
    theme(axis.title.x = element_text(size = 18), axis.title.y = element_text(size=18))+
    theme(legend.text = element_text(size = 18))+
    theme(axis.text.x = element_text(angle = 60, hjust = 1)))

ggsave(filename = "outputs-included/fig2-class_2yrmobility_theta50.pdf", plot = mobility_of_class, width = 10, height = 8, dpi = 300)

library(knitr)
library(kableExtra)

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

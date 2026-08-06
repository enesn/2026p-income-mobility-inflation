## ==================================================================================================#
# Relative Income Mobility around an Inflationary Shock
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

step_header("06 | Social classes: the income source each person persistently lives on (Fig 2)")

step_note(sprintf("A source is dominant above %g%% of equal-split income, in more than %d of the four years",
                  dominance_threshold_theta * 100, persistence_threshold))

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

step_note("Fig 2: shares moving up, down or nowhere between t-1 and t, by class")

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


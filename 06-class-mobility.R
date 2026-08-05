## ==================================================================================================#
# Inflation and redistribution in Turkey
# May 2025
# EI
## ==================================================================================================#

## ==================================================================================================#
  
class_mobility <- 
  income_decomposed %>%
  mutate(
    informallabor_income_dominance = ifelse(equal_split_informallabor/equal_split_income > dominance_threshold_theta, 1, 0  ),
    hourlywage_income_dominance = ifelse(equal_split_hourlylabor/equal_split_income >dominance_threshold_theta, 1, 0  ),
    nonprofessionallabor_income_dominance = ifelse(equal_split_nonmanagernonprofessional_labor/equal_split_income > dominance_threshold_theta, 1, 0  ), 
    publiclabor_income_dominance =ifelse(equal_split_publicseclabor/equal_split_income > dominance_threshold_theta, 1, 0  ),
    
    owner = ifelse(HH020 == "1", 1, 0),
    tenant = ifelse(HH020 == "2", 1, 0),
    lodging = ifelse(HH020 == "3", 1, 0),

    single = ifelse(HB050 == "1", 1, 0),
    couple = ifelse(HB050 == "21", 1, 0),
    couple_children = ifelse(HB050 == "22", 1, 0),
    single_parent = ifelse(HB050 == "23", 1, 0),
    extended = ifelse(HB050 == "3", 1, 0),
    sharing = ifelse(HB050 == "4", 1, 0)
     
  ) %>%
  group_by(FKIMLIK) %>%
  mutate(
    informallabor_income_persistence = sum(informallabor_income_dominance),
    hourlywage_income_persistence = sum(hourlywage_income_dominance),
    nonprofessionallabor_income_persistence = sum(nonprofessionallabor_income_dominance),
    publiclabor_income_persistence = sum(publiclabor_income_dominance),
    
    owner_persistence = sum(owner),
    tenant_persistence = sum(tenant),
    lodging_persistence = sum(lodging),

    single_persistence = sum(single),
    couple_persistence = sum(couple),
    couplechildren_persistence = sum(couple_children),
    singleparent_persistence = sum(single_parent),
    extended_persistence = sum(extended),
    sharing_persistence = sum(sharing)
    
  ) %>% 
  
ungroup() %>% 
  
  
  mutate(
    financial_income_dominance = ifelse(equal_split_financial/equal_split_income > dominance_threshold_theta, 1, 0  ),
    rental_income_dominance = ifelse(equal_split_rental/equal_split_income > dominance_threshold_theta, 1, 0  ),
    labor_income_dominance = ifelse(equal_split_labor/equal_split_income > dominance_threshold_theta, 1, 0  ),
    pension_income_dominance = ifelse(equal_split_pension/equal_split_income > dominance_threshold_theta, 1, 0  ),
    transfer_income_dominance = ifelse(equal_split_transfers/equal_split_income > dominance_threshold_theta, 1, 0  ),
    employer_income_dominance = ifelse(equal_split_employer/equal_split_income > dominance_threshold_theta, 1,0 ),
    selfemployer_income_dominance = ifelse(equal_split_selfemployer/equal_split_income > dominance_threshold_theta, 1,0 ),
    
    
  ) %>%
  group_by(FKIMLIK) %>%
  mutate(
    financial_income_persistence = sum(financial_income_dominance),
    rental_income_persistence = sum(rental_income_dominance),
    labor_income_persistence = sum(labor_income_dominance),
    pension_income_persistence = sum(pension_income_dominance),
    transfer_income_persistence = sum(transfer_income_dominance),
    employer_income_persistence = sum(employer_income_dominance),
    selfemployer_income_persistence = sum(selfemployer_income_dominance)
  ) %>% 
  # select(FKIMLIK, income_year, equal_split_financial:equal_split_labor, equal_split_entrp,equal_split_pension,financial_income_dominance:employer_income_persistence) %>% View()
  ungroup() %>% 
  mutate(type = ifelse(financial_income_persistence > persistence_threshold , "Financier", 
                       ifelse(rental_income_persistence  > persistence_threshold, "Rentier", 
                              ifelse(labor_income_persistence   > persistence_threshold, "Laborer", 
                                     ifelse(pension_income_persistence   > persistence_threshold, "Pensioner", 
                                            ifelse(employer_income_persistence   > persistence_threshold, "Employer", 
                                                   ifelse(selfemployer_income_persistence   > persistence_threshold, "Self-employer",       
                                                          ifelse(transfer_income_persistence  > persistence_threshold, "Transfer-Dependent",
                                                                 # ifelse(already_homeowner == 1,"Already Homeowner",
                                                                 # ifelse(new_homeowner ==1, "New Homeowner",  
                                                                 "Mixed")))))))) %>%
  
  left_join(
    sample_silc %>%
      group_by(HKIMLIK, FKIMLIK, FB010) %>%
      summarise(
        is_head = any(FK095 == 1),
        ever_employed = any(FI010 < 5), # a small inconsistency in earlier years, resulting in inclusion of those in education
        .groups = "drop"
      ) %>%
      group_by(HKIMLIK,FB010) %>%
      summarise(
        head_ever_employed = any(is_head & ever_employed),
        num_nonhead_ever_employed = sum(!is_head & ever_employed),
        .groups = "drop"
      ) %>%
      mutate(
        household_emp_status = case_when(
          head_ever_employed & num_nonhead_ever_employed == 0 ~ "Only head works",
          head_ever_employed & num_nonhead_ever_employed == 1 ~ "Head + 1 works",
          head_ever_employed & num_nonhead_ever_employed >= 2 ~ "Head + 2 or more workers",
          TRUE ~ "other"  # e.g., head not employed
        )
        
      ) %>% mutate(income_year = as.numeric(FB010)-1) %>% select(-FB010), by = c("income_year","HKIMLIK")
    
) %>% 
  
  mutate(
    head_only = ifelse(household_emp_status == "Only head works", 1, 0),
    head_plus_1 = ifelse(household_emp_status == "Head + 1 works", 1, 0),
    head_plus_2_or_more = ifelse(household_emp_status == "Head + 2 or more workers", 1, 0)
  
  ) %>%
  group_by(FKIMLIK) %>%
  mutate(
    head_only_persistence = sum(head_only), 
    head_plus_1_persistence = sum(head_plus_1), 
    head_plus_2_or_more_persistence = sum(head_plus_2_or_more)
    
  )%>% ungroup()%>% 
  
  
   mutate(labor_type = ifelse(informallabor_income_persistence > 2 , "Informal Laborer", 
                                    ifelse(nonprofessionallabor_income_persistence   > 2, "Non-Professional/Non-Manager Laborer",
                                           "Others/Mixed")),
          
          property_type = ifelse(owner_persistence == 4, "Owner",
                                 ifelse(tenant_persistence == 4, "Tenant", 
                                        ifelse(lodging_persistence == 4, "Lodging", "Others/Mixed"))), 
          
          family_type  = ifelse(single_persistence == 4, "Single", 
                                ifelse(couple_persistence == 4, "Couples",
                                       ifelse(couplechildren_persistence == 4, "Couples with children", 
                                              ifelse(singleparent_persistence == 4, "Single parents", 
                                                     ifelse(couple_persistence == 4, "Couples without children", 
                                                            ifelse(extended_persistence == 4, "Extended", "Others/Mixed")))))),
          
          household_emp_type = ifelse(head_only_persistence > 2, "Only head works typically",
                                      ifelse(head_plus_1_persistence > 2, "Head + 1 works typically",
                                             ifelse(head_plus_2_or_more_persistence > 2, "Head + 2 or more works typically", "Others")))
          
          )


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

library(kableExtra)

library(dplyr)
library(tidyr)
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

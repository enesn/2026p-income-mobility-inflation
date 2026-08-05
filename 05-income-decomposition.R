## ==================================================================================================#
# Inflation and redistribution in Turkey
# May 2025
# EI
## ==================================================================================================#

## ==================================================================================================#

income_decomposed <- income_defined %>%
  group_by(FB010) %>% 
  mutate(
    deciles2 = weighted_ntile(equal_split_income, as.numeric(FK060_4), 10),
    deciles1 = weighted_ntile(equal_split_income, as.numeric(FK060_2), 10),
  ) %>%
  ungroup() %>%
  arrange(FKIMLIK, FB010) %>%
  group_by(FKIMLIK) %>%
  mutate(
    decile_prev3 = lag(deciles2, 3),
    decile_prev1 = lag(deciles2, 1),
    decile_prev2 = lag(deciles2, 2),
    
    
    
    decile1_prev3 = lag(deciles1, 3),
    decile1_prev1 = lag(deciles1, 1),
    
    equal_split_prev1 = lag(equal_split_income, 1),
    financial_split_prev1 = lag(equal_split_financial, 1),
    rental_split_prev1 = lag(equal_split_rental, 1),
    employer_split_prev1 = lag(equal_split_employer, 1),
    selfemployer_split_prev1 = lag(equal_split_selfemployer, 1),
    imprent_split_prev1 = lag(equal_split_imprent, 1),
    labor_split_prev1 = lag(equal_split_labor, 1),
    pension_split_prev1 = lag(equal_split_pension, 1),
    transfer_split_prev1 = lag(equal_split_transfers, 1),
    
    equal_split_prev3 = lag(equal_split_income, 3),
    financial_split_prev3 = lag(equal_split_financial, 3),
    rental_split_prev3 = lag(equal_split_rental, 3),
    # entrp_split_prev3 = lag(equal_split_entrp, 3),
    employer_split_prev3 = lag(equal_split_employer, 3),
    selfemployer_split_prev3 = lag(equal_split_selfemployer, 3),
    imprent_split_prev3 = lag(equal_split_imprent, 3),
    labor_split_prev3 = lag(equal_split_labor, 3),
    pension_split_prev3 = lag(equal_split_pension, 3),
    transfer_split_prev3 = lag(equal_split_transfers, 3)
  ) %>%
  ungroup() %>%
  
  mutate(
    
    upward_2yr = ifelse(deciles2 > decile_prev1, 1, 0), 
    downward_2yr = ifelse(deciles2 < decile_prev1, 1, 0), 
    notransition_2yr = ifelse(deciles2 == decile_prev1, 1, 0),
    
    upward_4yr = ifelse(deciles2 > decile_prev3, 1, 0), 
    downward_4yr = ifelse(deciles2 < decile_prev3, 1, 0), 
    notransition_4yr = ifelse(deciles2 == decile_prev3, 1, 0)
    
  ) %>%
  
  mutate(equal_split_imprent = ifelse(is.na(equal_split_imprent), 0, equal_split_imprent))%>% 
  mutate(
    income_year = as.numeric(FB010) - 1
  ) %>% 
  
  arrange(FKIMLIK, FB010) %>%
  group_by(FKIMLIK) %>%
  
  
  ungroup() %>%
  group_by(HKIMLIK) %>%
  mutate(FI010_HH_same = n_distinct(FI010) == 1) %>%
  ungroup() %>%
  
  
  select(ALTORN, FKIMLIK, HKIMLIK, income_year, FK060_4,
         equal_split_income, equivalent_income, deciles2,decile_prev3, upward_4yr,
         hh_size, FI010, FI130, FI140,downward_4yr,
         
         
          personal_pension_income, personal_transfer_income, personal_labor_income,
         personal_employer_income, personal_selfemployer_income, 
         hh_rental_income, hh_financial_income,
         
         
         hh_full_transfers, hh_transfer_income,imputed_rent,
         
         
         
         equal_split_income, equal_split_employer, equal_split_selfemployer,  equal_split_financial, equal_split_imprent, 
         equal_split_rental, equal_split_labor, equal_split_pension, equal_split_transfers, everything()
         
  )  %>% 
  mutate(hh_direct_transfers = hh_full_transfers - hh_transfer_income) %>%
  
  group_by(FKIMLIK) %>% 

mutate(
  personal_labor_margin4 = ifelse(personal_labor_income> 0 & lag(personal_labor_income, 3) > 0, "Intensive", 
                                  ifelse(personal_labor_income> 0 & lag(personal_labor_income, 3) == 0, "Entry", 
                                         ifelse(personal_labor_income == 0 & lag(personal_labor_income, 3)> 0, "Exit", 
                                                ifelse(personal_labor_income == 0 & lag(personal_labor_income,3) == 0, "No change", NA)))),
  
  
  personal_labor_margin2 = ifelse(personal_labor_income> 0 & lag(personal_labor_income, 1) > 0, "Intensive", 
                                  ifelse(personal_labor_income> 0 & lag(personal_labor_income, 1) == 0, "Entry", 
                                         ifelse(personal_labor_income == 0 & lag(personal_labor_income, 1)> 0, "Exit", 
                                                ifelse(personal_labor_income == 0 & lag(personal_labor_income, 1) == 0, "No change", NA)))),
  
  
  personal_pension_margin4 = ifelse(personal_pension_income> 0 & lag(personal_pension_income, 3) > 0, "Intensive", 
                                    ifelse(personal_pension_income> 0 & lag(personal_pension_income, 3) == 0, "Entry", 
                                           ifelse(personal_pension_income == 0 & lag(personal_pension_income, 3)> 0, "Exit", 
                                                  ifelse(personal_pension_income == 0 & lag(personal_pension_income,3) == 0, "No change", NA)))),
  
  personal_pension_margin2 = ifelse(personal_pension_income> 0 & lag(personal_pension_income, 1) > 0, "Intensive", 
                                    ifelse(personal_pension_income> 0 & lag(personal_pension_income, 1) == 0, "Entry", 
                                           ifelse(personal_pension_income == 0 & lag(personal_pension_income, 1)> 0, "Exit", 
                                                  ifelse(personal_pension_income == 0 & lag(personal_pension_income,1) == 0, "No change", NA)))),
  
  
  personal_employer_margin4 = ifelse(personal_employer_income> 0 & lag(personal_employer_income, 3) > 0, "Intensive", 
                                  ifelse(personal_employer_income> 0 & lag(personal_employer_income, 3) == 0, "Entry", 
                                         ifelse(personal_employer_income == 0 & lag(personal_employer_income, 3)> 0, "Exit", 
                                                ifelse(personal_employer_income == 0 & lag(personal_employer_income,3) == 0, "No change", NA)))),
  
  
  personal_employer_margin2 = ifelse(personal_employer_income> 0 & lag(personal_employer_income, 1) > 0, "Intensive", 
                                     ifelse(personal_employer_income> 0 & lag(personal_employer_income, 1) == 0, "Entry", 
                                            ifelse(personal_employer_income == 0 & lag(personal_employer_income, 1)> 0, "Exit", 
                                                   ifelse(personal_employer_income == 0 & lag(personal_employer_income, 1) == 0, "No change", NA)))),
  
  
  personal_selfemployer_margin4 = ifelse(personal_selfemployer_income> 0 & lag(personal_selfemployer_income, 3) > 0, "Intensive", 
                                     ifelse(personal_selfemployer_income> 0 & lag(personal_selfemployer_income, 3) == 0, "Entry", 
                                            ifelse(personal_selfemployer_income == 0 & lag(personal_selfemployer_income, 3)> 0, "Exit", 
                                                   ifelse(personal_selfemployer_income == 0 & lag(personal_selfemployer_income,3) == 0, "No change", NA)))),
  
  personal_selfemployer_margin2 = ifelse(personal_selfemployer_income> 0 & lag(personal_selfemployer_income, 1) > 0, "Intensive", 
                                         ifelse(personal_selfemployer_income> 0 & lag(personal_selfemployer_income, 1) == 0, "Entry", 
                                                ifelse(personal_selfemployer_income == 0 & lag(personal_selfemployer_income, 1)> 0, "Exit", 
                                                       ifelse(personal_selfemployer_income == 0 & lag(personal_selfemployer_income,3) == 0, "No change", NA)))),
  
  personal_transfer_margin4 = ifelse(personal_transfer_income> 0 & lag(personal_transfer_income, 3) > 0, "Intensive", 
                                     ifelse(personal_transfer_income> 0 & lag(personal_transfer_income, 3) == 0, "Entry", 
                                            ifelse(personal_transfer_income == 0 & lag(personal_transfer_income, 3)> 0, "Exit", 
                                                   ifelse(personal_transfer_income == 0 & lag(personal_transfer_income,3) == 0, "No change", NA)))),
  
  personal_transfer_margin2 = ifelse(personal_transfer_income> 0 & lag(personal_transfer_income, 3) > 0, "Intensive", 
                                     ifelse(personal_transfer_income> 0 & lag(personal_transfer_income, 3) == 0, "Entry", 
                                            ifelse(personal_transfer_income == 0 & lag(personal_transfer_income, 3)> 0, "Exit", 
                                                   ifelse(personal_transfer_income == 0 & lag(personal_transfer_income,3) == 0, "No change", NA)))),
  
  hh_rental_margin4 = ifelse(hh_rental_income> 0 & lag(hh_rental_income, 3) > 0, "Intensive", 
                             ifelse(hh_rental_income> 0 & lag(hh_rental_income, 3) == 0, "Entry", 
                                    ifelse(hh_rental_income == 0 & lag(hh_rental_income, 3)> 0, "Exit", 
                                           ifelse(hh_rental_income == 0 & lag(hh_rental_income,3) == 0, "No change", NA)))),
  
  hh_rental_margin2 = ifelse(hh_rental_income> 0 & lag(hh_rental_income, 1) > 0, "Intensive", 
                             ifelse(hh_rental_income> 0 & lag(hh_rental_income, 1) == 0, "Entry", 
                                    ifelse(hh_rental_income == 0 & lag(hh_rental_income, 1)> 0, "Exit", 
                                           ifelse(hh_rental_income == 0 & lag(hh_rental_income, 1) == 0, "No change", NA)))),
  
  hh_financial_margin4 = ifelse(hh_financial_income> 0 & lag(hh_financial_income, 3) > 0, "Intensive", 
                                ifelse(hh_financial_income> 0 & lag(hh_financial_income, 3) == 0, "Entry", 
                                       ifelse(hh_financial_income == 0 & lag(hh_financial_income, 3)> 0, "Exit", 
                                              ifelse(hh_financial_income == 0 & lag(hh_financial_income,3) == 0, "No change", NA)))),
  
  hh_financial_margin2 = ifelse(hh_financial_income> 0 & lag(hh_financial_income, 1) > 0, "Intensive", 
                                ifelse(hh_financial_income> 0 & lag(hh_financial_income, 1) == 0, "Entry", 
                                       ifelse(hh_financial_income == 0 & lag(hh_financial_income, 1)> 0, "Exit", 
                                              ifelse(hh_financial_income == 0 & lag(hh_financial_income,1) == 0, "No change", NA)))),
  
  hh_transfers_margin4 = ifelse(hh_direct_transfers> 0 & lag(hh_direct_transfers, 3) > 0, "Intensive", 
                                ifelse(hh_direct_transfers> 0 & lag(hh_direct_transfers, 3) == 0, "Entry", 
                                       ifelse(hh_direct_transfers == 0 & lag(hh_direct_transfers, 3)> 0, "Exit", 
                                              ifelse(hh_direct_transfers == 0 & lag(hh_direct_transfers,3) == 0, "No change", NA)))),
  
  hh_transfers_margin2 = ifelse(hh_direct_transfers> 0 & lag(hh_direct_transfers, 1) > 0, "Intensive", 
                                ifelse(hh_direct_transfers> 0 & lag(hh_direct_transfers, 1) == 0, "Entry", 
                                       ifelse(hh_direct_transfers == 0 & lag(hh_direct_transfers, 1)> 0, "Exit", 
                                              ifelse(hh_direct_transfers == 0 & lag(hh_direct_transfers, 1) == 0, "No change", NA)))),
  
  imputed_rent_margin4 = ifelse(imputed_rent> 0 & lag(imputed_rent, 3) > 0, "Intensive", 
                                ifelse(imputed_rent> 0 & lag(imputed_rent, 3) == 0, "Entry", 
                                       ifelse(imputed_rent == 0 & lag(imputed_rent, 3)> 0, "Exit", 
                                              ifelse(imputed_rent == 0 & lag(imputed_rent,3) == 0, "No change", NA)))),
  
  imputed_rent_margin2 = ifelse(imputed_rent> 0 & lag(imputed_rent, 1) > 0, "Intensive", 
                                ifelse(imputed_rent> 0 & lag(imputed_rent, 1) == 0, "Entry", 
                                       ifelse(imputed_rent == 0 & lag(imputed_rent, 1)> 0, "Exit", 
                                              ifelse(imputed_rent == 0 & lag(imputed_rent,1) == 0, "No change", NA))))
  
) %>%
  
  mutate(
    p_pension_delta_intensive4 = ifelse(personal_pension_margin4 == "Intensive", personal_pension_income - lag(personal_pension_income, 3), 0),
    p_labor_delta_intensive4 = ifelse(personal_labor_margin4 == "Intensive", personal_labor_income - lag(personal_labor_income, 3), 0),
    p_employer_delta_intensive4 = ifelse(personal_employer_margin4 == "Intensive", personal_employer_income - lag(personal_employer_income, 3), 0),
    p_selfemployer_delta_intensive4 = ifelse(personal_selfemployer_margin4 == "Intensive", personal_selfemployer_income - lag(personal_selfemployer_income, 3), 0),
    p_transfer_delta_intensive4 = ifelse(personal_transfer_margin4 == "Intensive", personal_transfer_income - lag(personal_transfer_income, 3), 0),
    
    
    
    p_pension_delta_intensive2 = ifelse(personal_pension_margin2 == "Intensive", personal_pension_income - lag(personal_pension_income, 1), 0),
    p_labor_delta_intensive2 = ifelse(personal_labor_margin2 == "Intensive", personal_labor_income - lag(personal_labor_income, 1), 0),
    p_employer_delta_intensive2 = ifelse(personal_employer_margin2 == "Intensive", personal_employer_income - lag(personal_employer_income, 1), 0),
    p_selfemployer_delta_intensive2 = ifelse(personal_selfemployer_margin2 == "Intensive", personal_selfemployer_income - lag(personal_selfemployer_income, 1), 0),
    p_transfer_delta_intensive2 = ifelse(personal_transfer_margin2 == "Intensive", personal_transfer_income - lag(personal_transfer_income, 1), 0),
    
    
    
    hh_rental_delta_intensive4 = ifelse(hh_rental_margin4 == "Intensive", hh_rental_income - lag(hh_rental_income, 3), 0),
    hh_financial_delta_intensive4 = ifelse(hh_financial_margin4 == "Intensive", hh_financial_income - lag(hh_financial_income, 3), 0),
    hh_transfer_delta_intensive4 = ifelse(hh_transfers_margin4 == "Intensive", hh_direct_transfers - lag(hh_direct_transfers, 3), 0),
    hh_imprent_delta_intensive4 = ifelse(imputed_rent_margin4 == "Intensive", imputed_rent - lag(imputed_rent, 3), 0),
    
    
    hh_rental_delta_intensive2 = ifelse(hh_rental_margin2 == "Intensive", hh_rental_income - lag(hh_rental_income, 1), 0),
    hh_financial_delta_intensive2 = ifelse(hh_financial_margin2 == "Intensive", hh_financial_income - lag(hh_financial_income, 1), 0),
    hh_transfer_delta_intensive2 = ifelse(hh_transfers_margin2 == "Intensive", hh_direct_transfers - lag(hh_direct_transfers, 1), 0),
    hh_imprent_delta_intensive2 = ifelse(imputed_rent_margin2 == "Intensive", imputed_rent - lag(imputed_rent, 1), 0),
    
    
    p_pension_delta_entry4 = ifelse(personal_pension_margin4 == "Entry", personal_pension_income - lag(personal_pension_income, 3), 0),
    p_labor_delta_entry4 = ifelse(personal_labor_margin4 == "Entry", personal_labor_income - lag(personal_labor_income, 3), 0),
    p_employer_delta_entry4 = ifelse(personal_employer_margin4 == "Entry", personal_employer_income - lag(personal_employer_income, 3), 0),
    p_selfemployer_delta_entry4 = ifelse(personal_selfemployer_margin4 == "Entry", personal_selfemployer_income - lag(personal_selfemployer_income, 3), 0),
    p_transfer_delta_entry4 = ifelse(personal_transfer_margin4 == "Entry", personal_transfer_income - lag(personal_transfer_income, 3), 0),
    
    
    p_pension_delta_entry2 = ifelse(personal_pension_margin2 == "Entry", personal_pension_income - lag(personal_pension_income, 1), 0),
    p_labor_delta_entry2 = ifelse(personal_labor_margin2 == "Entry", personal_labor_income - lag(personal_labor_income, 1), 0),
    p_employer_delta_entry2 = ifelse(personal_employer_margin2 == "Entry", personal_employer_income - lag(personal_employer_income, 1), 0),
    p_selfemployer_delta_entry2 = ifelse(personal_selfemployer_margin2 == "Entry", personal_selfemployer_income - lag(personal_selfemployer_income, 1), 0),
    p_transfer_delta_entry2 = ifelse(personal_transfer_margin2 == "Entry", personal_transfer_income - lag(personal_transfer_income, 1), 0),
    
    hh_rental_delta_entry4 = ifelse(hh_rental_margin4 == "Entry", hh_rental_income - lag(hh_rental_income, 3), 0),
    hh_financial_delta_entry4 = ifelse(hh_financial_margin4 == "Entry", hh_financial_income - lag(hh_financial_income, 3), 0),
    hh_transfer_delta_entry4 = ifelse(hh_transfers_margin4 == "Entry", hh_direct_transfers - lag(hh_direct_transfers, 3), 0),
    hh_imprent_delta_entry4 = ifelse(imputed_rent_margin4 == "Entry", imputed_rent - lag(imputed_rent, 3), 0),
    
    hh_rental_delta_entry2 = ifelse(hh_rental_margin2 == "Entry", hh_rental_income - lag(hh_rental_income, 1), 0),
    hh_financial_delta_entry2 = ifelse(hh_financial_margin2 == "Entry", hh_financial_income - lag(hh_financial_income, 1), 0),
    hh_transfer_delta_entry2 = ifelse(hh_transfers_margin2 == "Entry", hh_direct_transfers - lag(hh_direct_transfers, 1), 0),
    hh_imprent_delta_entry2 = ifelse(imputed_rent_margin2 == "Entry", imputed_rent - lag(imputed_rent, 1), 0),
    
    
    p_pension_delta_exit4 = ifelse(personal_pension_margin4 == "Exit", personal_pension_income - lag(personal_pension_income, 3), 0),
    p_labor_delta_exit4 = ifelse(personal_labor_margin4 == "Exit", personal_labor_income - lag(personal_labor_income, 3), 0),
    p_employer_delta_exit4 = ifelse(personal_employer_margin4 == "Exit", personal_employer_income - lag(personal_employer_income, 3), 0),
    p_selfemployer_delta_exit4 = ifelse(personal_selfemployer_margin4 == "Exit", personal_selfemployer_income - lag(personal_selfemployer_income, 3), 0),
    p_transfer_delta_exit4 = ifelse(personal_transfer_margin4 == "Exit", personal_transfer_income - lag(personal_transfer_income, 3), 0),
    
    p_pension_delta_exit2 = ifelse(personal_pension_margin2 == "Exit", personal_pension_income - lag(personal_pension_income, 1), 0),
    p_labor_delta_exit2 = ifelse(personal_labor_margin2 == "Exit", personal_labor_income - lag(personal_labor_income, 1), 0),
    p_employer_delta_exit2 = ifelse(personal_employer_margin2 == "Exit", personal_employer_income - lag(personal_employer_income, 1), 0),
    p_selfemployer_delta_exit2 = ifelse(personal_selfemployer_margin2 == "Exit", personal_selfemployer_income - lag(personal_selfemployer_income, 1), 0),
    p_transfer_delta_exit2 = ifelse(personal_transfer_margin2 == "Exit", personal_transfer_income - lag(personal_transfer_income, 1), 0),
    
    hh_rental_delta_exit4 = ifelse(hh_rental_margin4 == "Exit", hh_rental_income - lag(hh_rental_income, 3), 0),
    hh_financial_delta_exit4 = ifelse(hh_financial_margin4 == "Exit", hh_financial_income - lag(hh_financial_income, 3), 0),
    hh_transfer_delta_exit4 = ifelse(hh_transfers_margin4 == "Exit", hh_direct_transfers - lag(hh_direct_transfers, 3), 0),
    hh_imprent_delta_exit4 = ifelse(imputed_rent_margin4 == "Exit", imputed_rent - lag(imputed_rent, 3), 0),
    
    
    hh_rental_delta_exit2 = ifelse(hh_rental_margin2 == "Exit", hh_rental_income - lag(hh_rental_income, 1), 0),
    hh_financial_delta_exit2 = ifelse(hh_financial_margin2 == "Exit", hh_financial_income - lag(hh_financial_income, 1), 0),
    hh_transfer_delta_exit2 = ifelse(hh_transfers_margin2 == "Exit", hh_direct_transfers - lag(hh_direct_transfers, 1), 0),
    hh_imprent_delta_exit2 = ifelse(imputed_rent_margin2 == "Exit", imputed_rent - lag(imputed_rent, 1), 0)
    
    
  ) %>% 
  group_by(HKIMLIK, income_year) %>%
  mutate(
    esplit_p_pension_delta_intensive4 = sum(p_pension_delta_intensive4, na.rm = T)/hh_size, 
    esplit_p_labor_delta_intensive4 = sum(p_labor_delta_intensive4, na.rm = T)/hh_size, 
    esplit_p_employer_delta_intensive4 = sum(p_employer_delta_intensive4, na.rm = T)/hh_size, 
    esplit_p_selfemployer_delta_intensive4 = sum(p_selfemployer_delta_intensive4, na.rm = T)/hh_size, 
    esplit_p_transfer_delta_intensive4 = sum(p_transfer_delta_intensive4, na.rm = T)/hh_size,
    
    
    
    
    esplit_p_pension_delta_intensive2 = sum(p_pension_delta_intensive2, na.rm = T)/hh_size, 
    esplit_p_labor_delta_intensive2 = sum(p_labor_delta_intensive2, na.rm = T)/hh_size, 
    esplit_p_employer_delta_intensive2 = sum(p_employer_delta_intensive2, na.rm = T)/hh_size, 
    esplit_p_selfemployer_delta_intensive2 = sum(p_selfemployer_delta_intensive2, na.rm = T)/hh_size, 
    esplit_p_transfer_delta_intensive2 = sum(p_transfer_delta_intensive2, na.rm = T)/hh_size,
    
    
    
    esplit_hh_rental_delta_intensive4  = hh_rental_delta_intensive4 /hh_size, 
    esplit_hh_financial_delta_intensive4 = hh_financial_delta_intensive4/hh_size, 
    esplit_hh_transfer_delta_intensive4 = hh_transfer_delta_intensive4/hh_size, 
    esplit_hh_imprent_delta_intensive4 = hh_imprent_delta_intensive4/hh_size, 
    
    
    esplit_hh_rental_delta_intensive2  = hh_rental_delta_intensive2 /hh_size, 
    esplit_hh_financial_delta_intensive2 = hh_financial_delta_intensive2/hh_size, 
    esplit_hh_transfer_delta_intensive2 = hh_transfer_delta_intensive2/hh_size, 
    esplit_hh_imprent_delta_intensive2 = hh_imprent_delta_intensive2/hh_size, 
    
    
    esplit_p_pension_delta_entry4  = sum(p_pension_delta_entry4, na.rm = T)/hh_size, 
    esplit_p_labor_delta_entry4 = sum(p_labor_delta_entry4, na.rm = T)/hh_size, 
    esplit_p_employer_delta_entry4 = sum(p_employer_delta_entry4, na.rm = T)/hh_size, 
    esplit_p_selfemployer_delta_entry4 = sum(p_selfemployer_delta_entry4, na.rm = T)/hh_size, 
    esplit_p_transfer_delta_entry4 = sum(p_transfer_delta_entry4, na.rm = T)/hh_size, 
    
    
    
    esplit_p_pension_delta_entry2 = sum(p_pension_delta_entry2, na.rm = T)/hh_size, 
    esplit_p_labor_delta_entry2 = sum(p_labor_delta_entry2, na.rm = T)/hh_size, 
    esplit_p_employer_delta_entry2 = sum(p_employer_delta_entry2, na.rm = T)/hh_size, 
    esplit_p_selfemployer_delta_entry2 = sum(p_selfemployer_delta_entry2, na.rm = T)/hh_size, 
    esplit_p_transfer_delta_entry2 = sum(p_transfer_delta_entry2, na.rm = T)/hh_size, 
    
    
    esplit_hh_rental_delta_entry4  = hh_rental_delta_entry4/hh_size, 
    esplit_hh_financial_delta_entry4 =hh_financial_delta_entry4/hh_size, 
    esplit_hh_transfer_delta_entry4 =hh_transfer_delta_entry4/hh_size, 
    esplit_hh_imprent_delta_entry4 = hh_imprent_delta_entry4/hh_size, 
    
    
    esplit_hh_rental_delta_entry2  = hh_rental_delta_entry2/hh_size, 
    esplit_hh_financial_delta_entry2 =hh_financial_delta_entry2/hh_size, 
    esplit_hh_transfer_delta_entry2 =hh_transfer_delta_entry2/hh_size, 
    esplit_hh_imprent_delta_entry2 = hh_imprent_delta_entry2/hh_size, 
    
    
    
    esplit_p_pension_delta_exit4  = sum(p_pension_delta_exit4, na.rm = T)/hh_size, 
    esplit_p_labor_delta_exit4 = sum(p_labor_delta_exit4, na.rm = T)/hh_size, 
    esplit_p_employer_delta_exit4 = sum(p_employer_delta_exit4, na.rm = T)/hh_size, 
    esplit_p_selfemployer_delta_exit4 = sum(p_selfemployer_delta_exit4, na.rm = T)/hh_size, 
    esplit_p_transfer_delta_exit4 = sum(p_transfer_delta_exit4, na.rm = T)/hh_size, 
    
    
    
    esplit_p_pension_delta_exit2  = sum(p_pension_delta_exit2, na.rm = T)/hh_size, 
    esplit_p_labor_delta_exit2 = sum(p_labor_delta_exit2, na.rm = T)/hh_size, 
    esplit_p_employer_delta_exit2 = sum(p_employer_delta_exit2, na.rm = T)/hh_size, 
    esplit_p_selfemployer_delta_exit2 = sum(p_selfemployer_delta_exit2, na.rm = T)/hh_size, 
    esplit_p_transfer_delta_exit2 = sum(p_transfer_delta_exit2, na.rm = T)/hh_size, 
    
    
    esplit_hh_rental_delta_exit4  = hh_rental_delta_exit4/hh_size, 
    esplit_hh_financial_delta_exit4 = hh_financial_delta_exit4/hh_size, 
    esplit_hh_transfer_delta_exit4 = hh_transfer_delta_exit4/hh_size, 
    esplit_hh_imprent_delta_exit4 = hh_imprent_delta_exit4/hh_size,
    
    esplit_hh_rental_delta_exit2  = hh_rental_delta_exit2/hh_size, 
    esplit_hh_financial_delta_exit2 = hh_financial_delta_exit2/hh_size, 
    esplit_hh_transfer_delta_exit2 = hh_transfer_delta_exit2/hh_size, 
    esplit_hh_imprent_delta_exit2 = hh_imprent_delta_exit2/hh_size
    
  ) %>% 
  ungroup() %>%
  mutate(
    total_delta4 =
      abs(esplit_p_pension_delta_intensive4) +
      abs(esplit_p_labor_delta_intensive4) +
      abs(esplit_p_employer_delta_intensive4) +
      abs(esplit_p_selfemployer_delta_intensive4) +
      abs(esplit_p_transfer_delta_intensive4) +
      
      
      abs(esplit_hh_rental_delta_intensive4) +
      abs(esplit_hh_financial_delta_intensive4) +
      abs(esplit_hh_transfer_delta_intensive4) +
      abs(esplit_hh_imprent_delta_intensive4) +
      
      abs(esplit_p_pension_delta_entry4) +
      abs(esplit_p_labor_delta_entry4) +
      abs(esplit_p_employer_delta_entry4) +
      abs(esplit_p_selfemployer_delta_entry4) +
      abs(esplit_p_transfer_delta_entry4) +
      
      abs(esplit_hh_rental_delta_entry4) +
      abs(esplit_hh_financial_delta_entry4) +
      abs(esplit_hh_transfer_delta_entry4) +
      abs(esplit_hh_imprent_delta_entry4) +
      
      abs(esplit_p_pension_delta_exit4) +
      abs(esplit_p_labor_delta_exit4) +
      abs(esplit_p_employer_delta_exit4) +
      abs(esplit_p_selfemployer_delta_exit4) +
      abs(esplit_p_transfer_delta_exit4) +
      
      
      abs(esplit_hh_rental_delta_exit4) +
      abs(esplit_hh_financial_delta_exit4) +
      abs(esplit_hh_transfer_delta_exit4) +
      abs(esplit_hh_imprent_delta_exit4),
    
    
    
    
    total_delta2 =
      abs(esplit_p_pension_delta_intensive2) +
      abs(esplit_p_labor_delta_intensive2) +
      abs(esplit_p_employer_delta_intensive2) +
      abs(esplit_p_selfemployer_delta_intensive2) +
      abs(esplit_p_transfer_delta_intensive2) +
      
      
      abs(esplit_hh_rental_delta_intensive2) +
      abs(esplit_hh_financial_delta_intensive2) +
      abs(esplit_hh_transfer_delta_intensive2) +
      abs(esplit_hh_imprent_delta_intensive2) +
      
      abs(esplit_p_pension_delta_entry2) +
      abs(esplit_p_labor_delta_entry2) +
      abs(esplit_p_employer_delta_entry2) +
      abs(esplit_p_selfemployer_delta_entry2) +
      abs(esplit_p_transfer_delta_entry2) +
      
      abs(esplit_hh_rental_delta_entry2) +
      abs(esplit_hh_financial_delta_entry2) +
      abs(esplit_hh_transfer_delta_entry2) +
      abs(esplit_hh_imprent_delta_entry2) +
      
      abs(esplit_p_pension_delta_exit2) +
      abs(esplit_p_labor_delta_exit2) +
      abs(esplit_p_employer_delta_exit2) +
      abs(esplit_p_selfemployer_delta_exit2) +
      abs(esplit_p_transfer_delta_exit2) +
      
      
      abs(esplit_hh_rental_delta_exit2) +
      abs(esplit_hh_financial_delta_exit2) +
      abs(esplit_hh_transfer_delta_exit2) +
      abs(esplit_hh_imprent_delta_exit2),
    
    
    p_pension_intensive4_contribution = esplit_p_pension_delta_intensive4/total_delta4,
    p_labor_intensive4_contribution = esplit_p_labor_delta_intensive4/total_delta4,
    p_employer_intensive4_contribution = esplit_p_employer_delta_intensive4/total_delta4,
    p_selfemployer_intensive4_contribution = esplit_p_selfemployer_delta_intensive4/total_delta4,
    p_transfer_intensive4_contribution = esplit_p_transfer_delta_intensive4/total_delta4,
    
    
    p_pension_intensive2_contribution = esplit_p_pension_delta_intensive2/total_delta2,
    p_labor_intensive2_contribution = esplit_p_labor_delta_intensive2/total_delta2,
    p_employer_intensive2_contribution = esplit_p_employer_delta_intensive2/total_delta2,
    p_selfemployer_intensive2_contribution = esplit_p_selfemployer_delta_intensive2/total_delta2,
    p_transfer_intensive2_contribution = esplit_p_transfer_delta_intensive2/total_delta2,
    
    
    hh_rental_intensive4_contribution = esplit_hh_rental_delta_intensive4/total_delta4,
    hh_financial_intensive4_contribution = esplit_hh_financial_delta_intensive4/total_delta4,
    hh_transfer_intensive4_contribution = esplit_hh_transfer_delta_intensive4/total_delta4,
    hh_imprent_intensive4_contribution = esplit_hh_imprent_delta_intensive4/total_delta4,
    
    
    hh_rental_intensive2_contribution = esplit_hh_rental_delta_intensive2/total_delta2,
    hh_financial_intensive2_contribution = esplit_hh_financial_delta_intensive2/total_delta2,
    hh_transfer_intensive2_contribution = esplit_hh_transfer_delta_intensive2/total_delta2,
    hh_imprent_intensive2_contribution = esplit_hh_imprent_delta_intensive2/total_delta2,
  
    
    p_pension_entry4_contribution = esplit_p_pension_delta_entry4/total_delta4,
    p_labor_entry4_contribution = esplit_p_labor_delta_entry4/total_delta4,
    p_employer_entry4_contribution = esplit_p_employer_delta_entry4/total_delta4,
    p_selfemployer_entry4_contribution = esplit_p_selfemployer_delta_entry4/total_delta4,
    p_transfer_entry4_contribution = esplit_p_transfer_delta_entry4/total_delta4,
    
    
    p_pension_entry2_contribution = esplit_p_pension_delta_entry2/total_delta2,
    p_labor_entry2_contribution = esplit_p_labor_delta_entry2/total_delta2,
    p_employer_entry2_contribution = esplit_p_employer_delta_entry2/total_delta2,
    p_selfemployer_entry2_contribution = esplit_p_selfemployer_delta_entry2/total_delta2,
    p_transfer_entry2_contribution = esplit_p_transfer_delta_entry2/total_delta2,
    
    
    hh_rental_entry4_contribution = esplit_hh_rental_delta_entry4/total_delta4,
    hh_financial_entry4_contribution = esplit_hh_financial_delta_entry4/total_delta4,
    hh_transfer_entry4_contribution = esplit_hh_transfer_delta_entry4/total_delta4,
    hh_imprent_entry4_contribution = esplit_hh_imprent_delta_entry4/total_delta4,
    
    hh_rental_entry2_contribution = esplit_hh_rental_delta_entry2/total_delta2,
    hh_financial_entry2_contribution = esplit_hh_financial_delta_entry2/total_delta2,
    hh_transfer_entry2_contribution = esplit_hh_transfer_delta_entry2/total_delta2,
    hh_imprent_entry2_contribution = esplit_hh_imprent_delta_entry2/total_delta2,
    
    
    
    p_pension_exit4_contribution = esplit_p_pension_delta_exit4/total_delta4,
    p_labor_exit4_contribution = esplit_p_labor_delta_exit4/total_delta4,
    p_employer_exit4_contribution = esplit_p_employer_delta_exit4/total_delta4,
    p_selfemployer_exit4_contribution = esplit_p_selfemployer_delta_exit4/total_delta4,
    p_transfer_exit4_contribution = esplit_p_transfer_delta_exit4/total_delta4,
    
    p_pension_exit2_contribution = esplit_p_pension_delta_exit2/total_delta2,
    p_labor_exit2_contribution = esplit_p_labor_delta_exit2/total_delta2,
    p_employer_exit2_contribution = esplit_p_employer_delta_exit2/total_delta2,
    p_selfemployer_exit2_contribution = esplit_p_selfemployer_delta_exit2/total_delta2,
    p_transfer_exit2_contribution = esplit_p_transfer_delta_exit2/total_delta2,
    
    hh_rental_exit4_contribution = esplit_hh_rental_delta_exit4/total_delta4,
    hh_financial_exit4_contribution = esplit_hh_financial_delta_exit4/total_delta4,
    hh_transfer_exit4_contribution = esplit_hh_transfer_delta_exit4/total_delta4,
    hh_imprent_exit4_contribution = esplit_hh_imprent_delta_exit4/total_delta4,
    
    
    hh_rental_exit2_contribution = esplit_hh_rental_delta_exit2/total_delta2,
    hh_financial_exit2_contribution = esplit_hh_financial_delta_exit2/total_delta2,
    hh_transfer_exit2_contribution = esplit_hh_transfer_delta_exit2/total_delta2,
    hh_imprent_exit2_contribution = esplit_hh_imprent_delta_exit2/total_delta2
  ) %>%

  
  select(FKIMLIK, HKIMLIK, income_year, deciles2, FI010, FI130, FI140, FK060_4, 
         personal_employer_income, personal_selfemployer_income, personal_pension_income, personal_transfer_income, personal_labor_income,
         hh_rental_income, hh_financial_income, upward_4yr, downward_4yr, deciles2, decile_prev3, 
         
         equal_split_income, equivalent_income, imputed_rent, hh_imprent_delta_entry4, p_pension_intensive4_contribution:hh_imprent_exit4_contribution, 
         esplit_p_pension_delta_intensive4:esplit_hh_imprent_delta_exit4, everything() ) %>%
  arrange(FKIMLIK, FB010) %>%
  group_by(FKIMLIK) %>%
  
  mutate(
    real_equal_split_prev3 = lag(real_equal_split, 3)) %>% 
  ungroup()



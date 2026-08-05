## ==================================================================================================#
# Inflation and redistribution in Turkey
# May 2025
# EI
## ==================================================================================================#

## ==================================================================================================#

#Personal income
income_defined <- sample_silc %>% 
  mutate( 
    FG030 = ifelse(as.numeric(FG030)<0, 0, FG030),
    FG040 = ifelse(as.numeric(FG040)<0, 0, FG040),
    ############################
    disp_personal_income =  #individualistic adult income
      ############################
    #Net annual wage income from primary job and from secondary job 
    as.numeric(FG010) %+%
      #in-kind income from employer
      as.numeric(FG020)%+% 
      #entrepreneurial 
      as.numeric(FG030) %+% as.numeric(FG040)%+%
      #transfers
      as.numeric(FG080) %+%  as.numeric(FG120) %+% 
      as.numeric(FG100) %+% 
      # as.numeric(FG070) %+% #includes severance payments
      as.numeric(FG090) %+% as.numeric(FG110),
    ############################
    personal_labor_income = 
    ############################
    #Net annual wage income from primary job and from secondary job 
      as.numeric(FG010) %+%
      #in-kind income from employer
      as.numeric(FG020),
    ############################
    personal_transfer_income =
    ############################
      as.numeric(FG120) %+% 
      as.numeric(FG100) %+% 
      # as.numeric(FG070) %+% #includes severance payments
      as.numeric(FG090) %+% as.numeric(FG110),
    ############################
    personal_pension_income = 
      ############################
    as.numeric(FG080)
    ############################
    # personal_entrepreneurial_income =
    #   ############################
    # #entrepreneurial 
    # as.numeric(FG030) %+% as.numeric(FG040)
  ) %>%
  mutate(
    personal_employer_income = ifelse(FI120 == 3 & as.numeric(FG030) %+% as.numeric(FG040) > 0, as.numeric(FG030) %+% as.numeric(FG040), 0),
    personal_selfemployer_income = ifelse( (!FI120 == 3 | is.na(FI120)) & as.numeric(FG030) %+% as.numeric(FG040) > 0,as.numeric(FG030) %+% as.numeric(FG040),  0),
    
    
    personal_privateseclabor_income = ifelse(FI120 == 1 & FI145 == 1 & as.numeric(FG010) %+% as.numeric(FG020) > 0, as.numeric(FG010) %+% as.numeric(FG020),
                                             ifelse(is.na(FI145), NA, 0
                                             )), 
    personal_publicseclabor_income = ifelse(FI120 == 1 & FI145 == 2 & as.numeric(FG010) %+% as.numeric(FG020) > 0, as.numeric(FG010) %+% as.numeric(FG020),
                                            ifelse(is.na(FI145), NA, 0
                                            )),
    #yevmiyeli çalışan
    personal_hourlylabor_income = ifelse(FI120 == 2 & as.numeric(FG010) %+% as.numeric(FG020) > 0, as.numeric(FG010) %+% as.numeric(FG020), 0),
    
    
    personal_formallabor_income = ifelse(FI190 == 1 & as.numeric(FG010) %+% as.numeric(FG020) > 0, as.numeric(FG010) %+% as.numeric(FG020), 0),
    personal_informallabor_income = ifelse(FI190 == 2 & as.numeric(FG010) %+% as.numeric(FG020) > 0, as.numeric(FG010) %+% as.numeric(FG020), 0),
    
    
    personal_nonmanagernonprofessional_labor_income = ifelse( !(FI130 == 1 | FI130 == 2) & 
                                                                as.numeric(FG010) %+% as.numeric(FG020) > 0, 
                                                              as.numeric(FG010) %+% as.numeric(FG020), 0)
    
  ) %>%
  mutate(personal_employer_income = ifelse(is.na(personal_employer_income), 0, personal_employer_income), 
         personal_selfemployer_income = ifelse(is.na(personal_selfemployer_income), 0, personal_selfemployer_income ),
         
         personal_hourlylabor_income = ifelse(is.na(personal_hourlylabor_income), 0, personal_hourlylabor_income ),
         personal_formallabor_income = ifelse(is.na(personal_formallabor_income), 0, personal_formallabor_income ),
         
         personal_informallabor_income = ifelse(is.na(personal_informallabor_income), 0, personal_informallabor_income ),
         personal_nonmanagernonprofessional_labor_income = ifelse(is.na(personal_nonmanagernonprofessional_labor_income), 0, personal_nonmanagernonprofessional_labor_income ),
         
         
         
         ) %>%
    mutate(FB010 = as.character(FB010))

#Household income
income_defined <- income_defined %>% 
  left_join(
    income_defined %>% group_by(HKIMLIK, FB010) %>%  summarise(hh_income = sum(disp_personal_income, na.rm = T),
                                                            hh_pension_income = sum(personal_pension_income, na.rm = T),
                                                            # hh_entrepreneurial_income = sum(personal_entrepreneurial_income, na.rm = T),
                                                            hh_employer_income = sum(personal_employer_income, na.rm = T), 
                                                            hh_selfemployer_income = sum(personal_selfemployer_income, na.rm = T), 
                                                            hh_transfer_income = sum(personal_transfer_income, na.rm = T),
                                                            hh_labor_income = sum(personal_labor_income, na.rm = T),
                                                            hh_privateseclabor_income = sum(personal_privateseclabor_income, na.rm = T),
                                                            hh_publicseclabor_income = sum(personal_publicseclabor_income, na.rm = T),
                                                            hh_hourlylabor_income = sum(personal_hourlylabor_income, na.rm = T),
                                                            hh_formallabor_income = sum(personal_formallabor_income, na.rm = T),
                                                            hh_informallabor_income = sum(personal_informallabor_income, na.rm = T),
                                                            hh_nonmanagernonprofessional_labor_income = sum(personal_nonmanagernonprofessional_labor_income, na.rm = T),
    ) #sum of personal incomes
    
  ) %>% 
  left_join(
    income_defined %>% group_by(HKIMLIK, FB010) %>% summarise(hh_size=n())
  ) %>% 
  left_join(
    read_xlsx("other-input-data/imputed_rent_correction.xlsx") %>% mutate(FB010 = as.character(FB010))
  ) %>%
  
  mutate(
    imputed_rent =as.numeric(HG010), #imputed rent
    imputed_rent = ifelse(imputed_rent < 0, imputed_rent*(-1), imputed_rent),
    imputed_rent = ifelse(is.na(imputed_rent), 0, imputed_rent),
    # imputed_rent = imputed_rent * imprent_upgrade_factor,
    hh_disp_income = hh_income %+%  #Total disposable household income
      as.numeric(HG020) %+% #child labor income
      as.numeric(HG030N) %+% #transfer1
      as.numeric(HG030A) %+% #transfer1
      as.numeric(HG040) %+% #transfer1
      as.numeric(HG050N) %+% #transfer1
      as.numeric(HG050A) %+% #transfer1
      as.numeric(HG060N) %+% #transfer1
      as.numeric(HG060A) %+% #transfer1
      as.numeric(HG070) %+% #rent
      as.numeric(HG080) %+% #interest (financial income)
      imputed_rent %+%
      as.numeric(HG105),
    hh_rental_income = as.numeric(HG070),
    hh_financial_income = as.numeric(HG080),
    hh_full_transfers = hh_transfer_income %+%
      as.numeric(HG030N) %+% #transfer1
      as.numeric(HG030A) %+% #transfer1
      as.numeric(HG040) %+% #transfer1
      as.numeric(HG050N) %+% #transfer1
      as.numeric(HG050A) %+% #transfer1
      as.numeric(HG060N) %+% #transfer1
      as.numeric(HG060A) #transfer1
    
    
  ) %>% 
  mutate(
    oecd_equivalence_scale = ifelse(hh_size == 1, 1, 1+(hh_size-1)*0.5)
  ) %>% 
  
  #Adjusted personal income
  mutate(
    equivalent_income = hh_disp_income / oecd_equivalence_scale, 
    equal_split_income = hh_disp_income / hh_size
    
  ) %>%
  mutate(
    hh_rental_income = ifelse(is.na(hh_rental_income), 0, hh_rental_income),
    hh_financial_income = ifelse(is.na(hh_financial_income), 0, hh_financial_income),
    personal_pension_income = ifelse(is.na(personal_pension_income), 0, personal_pension_income)
  ) %>%
  mutate(
    equal_split_financial = hh_financial_income / hh_size, 
    equal_split_rental = hh_rental_income / hh_size, 
    equal_split_imprent = imputed_rent / hh_size, 
    equal_split_transfers = hh_full_transfers / hh_size,
    equal_split_pension = hh_pension_income / hh_size, 
    # equal_split_entrp = hh_entrepreneurial_income / hh_size,
    equal_split_employer = hh_employer_income / hh_size, 
    equal_split_selfemployer = hh_selfemployer_income / hh_size, 
    equal_split_labor = hh_labor_income / hh_size,
    equal_split_privateseclabor = hh_privateseclabor_income / hh_size,
    equal_split_publicseclabor = hh_publicseclabor_income / hh_size,
    equal_split_hourlylabor = hh_hourlylabor_income / hh_size, 
    equal_split_formallabor = hh_formallabor_income / hh_size,
    equal_split_informallabor = hh_informallabor_income / hh_size,
    equal_split_nonmanagernonprofessional_labor = hh_nonmanagernonprofessional_labor_income / hh_size,
    equal_split_rent = (as.numeric(HH040)*12) / hh_size
  ) %>%
  left_join(
    read_xlsx("other-input-data/tuik_cpi.xlsx") %>% mutate(year = year + 1, year= as.character(year)), by = c("FB010"="year")
  ) %>%
  mutate(
   real_equal_split = equal_split_income/december_cpi_100_2003*100,
     real_equal_split_financial = equal_split_financial/december_cpi_100_2003*100,
    real_equal_split_rental = equal_split_rental/december_cpi_100_2003*100,
    real_equal_split_imprent = equal_split_imprent/december_cpi_100_2003*100,
    real_equal_split_transfers = equal_split_transfers/december_cpi_100_2003*100,
    real_equal_split_pension = equal_split_pension/december_cpi_100_2003*100,
    real_equal_split_employer = equal_split_employer/december_cpi_100_2003*100,
    real_equal_split_selfemployer = equal_split_selfemployer/december_cpi_100_2003*100,
    real_equal_split_labor = equal_split_labor/december_cpi_100_2003*100,
   real_equal_split_rent = equal_split_rent/december_cpi_100_2003*100
    
  )


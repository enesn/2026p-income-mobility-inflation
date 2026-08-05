## ==================================================================================================#
# Inflation and redistribution in Turkey
# May 2025
# EI
## ==================================================================================================#

## ==================================================================================================#

  #Restriction
  sample_silc <- silc0824 %>% 
  filter(times_seen == 4) %>% #keep individuals who were followed for four years
  filter(as.numeric(FK070) > 19) %>% #keep DINA adults
  filter(FK100 == "1") %>% #keep "Sample person" and exclude "co-residents"
  filter(!FK110 == "8") %>% #drop individuals "Recorded as household member by mistake"
  filter(!FK110 == "6") 
  
  
  #Keep individuals who satisfy the restriction for four years
  sample_silc <-  sample_silc %>% 
    left_join(
      sample_silc %>% group_by(FKIMLIK) %>% summarise(n=n())
    ) %>%
    filter(n==4) %>%
    select(-n) %>%
    filter(!ALTORN == "22") #ALTORN 22 refers to 2008-2024
    
  
  #Keep households with the constant hh_size
  sample_silc <- sample_silc %>% left_join(
  sample_silc %>% 
    group_by(HKIMLIK, FB010) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    group_by(HKIMLIK) %>%
    mutate(max = max(n), min =  min(n)) %>%
    mutate(size_change = ifelse(!max==n | !min == n, 1, 0)) %>%
    distinct(HKIMLIK, .keep_all = T) %>%
    select(HKIMLIK, size_change)
  ) %>%
    filter(size_change == 0) %>%
    left_join(
                sample_silc %>% 
                group_by(HKIMLIK, FB010) %>% 
                summarise(n=n()) %>%
                group_by(HKIMLIK) %>% 
                summarise(n=n()) 
    ) %>% filter(n==4)

  
  #Adjust weights
  #TUIK instruction: 
  
  #If an analysis is to be conducted using a 2-year panel dataset, 
  #one must first select the records from the year 2023 in the dataset named “GYK20212223_FK”
  #where the 2-year panel weight coefficient (FK060_2 variable) is non-missing. 
  #After selecting these records, the corresponding records from 2022 should be
  #matched using the individual identifier (FKIMLIK – the Fert key variable), and a 2-year panel dataset should be constructed.
  #If a 3-year panel dataset is to be used, 
  #the records from 2023 where the 3-year panel weight coefficient (FK060_3 variable) 
  #is non-missing should be selected.
  #Then, using the individual identifier (FKIMLIK), the corresponding records from 2022 and 2021, respectively, 
  #should be matched to construct a 3-year panel dataset.
  #The same procedure should be followed for constructing a 4-year panel dataset, 
  #using the 4-year panel weight coefficient variable (FK060_4).
  
  
  sample_silc <- sample_silc %>% 
    arrange(FKIMLIK, desc(FB010)) %>% fill(FK060_2, .direction = "down") %>%
    fill(FK060_3, .direction = "down") %>% 
    fill(FK060_4, .direction = "down")
  
  

  rm(silc0824)

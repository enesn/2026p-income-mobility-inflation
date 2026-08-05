## ==================================================================================================#
# Inflation and redistribution in Turkey
# May 2025
# EI
## ==================================================================================================#

# ==================================================================================================#
# Figure 3, Mean change in income decile by social class
# ==================================================================================================#

(mean_delta_rank <- class_mobility_ext %>% 
  filter(income_year > 2011) %>%
  filter(!type == "Rentier") %>% 
  filter(!type == "Financier") %>% 
  mutate(delta_rank = deciles2 - decile_prev1) %>% 
  filter(!is.na(delta_rank)) %>% 
  distinct(HKIMLIK, income_year,  .keep_all = T) %>%
  group_by(income_year, type) %>%
  summarise(mean_delta_rank = mean(delta_rank)) %>% 
  
  ggplot(aes(x=income_year, mean_delta_rank)) +
  geom_col() +
  scale_x_continuous(breaks=seq(2012,2023,1)) +
  labs(y= "Mean Change in Decile, from t-1 to t",  x= "Destination Year (t)")+
  geom_hline(yintercept = 0, linetype = "dashed", size= 1)+
  facet_wrap(~type)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90), axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)))
  
ggsave(filename = "outputs-included/fig3-mean-delta-rank.pdf", plot = mean_delta_rank, width = 10, height = 8, dpi = 300)


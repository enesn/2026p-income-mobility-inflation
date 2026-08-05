## ==================================================================================================#
# Inflation and redistribution in Turkey
# May 2025
# EI
## ==================================================================================================#

(contributions_2yr <- class_mobility %>%
    filter(!is.na(p_pension_entry2_contribution)) %>%
    filter(income_year > 2011) %>% 
    mutate(mobility = ifelse( !(upward_2yr == 1 | downward_2yr == 1), "Immobile", ifelse(upward_2yr == 1, "Upward","Downward"))) %>%
    select(FKIMLIK, income_year, mobility, FK060_4,
           p_labor_intensive2_contribution, p_labor_entry2_contribution,p_labor_exit2_contribution,
           p_pension_intensive2_contribution, p_pension_entry2_contribution, p_pension_exit2_contribution,
           p_employer_intensive2_contribution, p_employer_entry2_contribution, p_employer_exit2_contribution,
           p_selfemployer_intensive2_contribution, p_selfemployer_entry2_contribution, p_selfemployer_exit2_contribution,
           hh_imprent_intensive2_contribution, hh_imprent_entry2_contribution, hh_imprent_exit2_contribution,
           hh_rental_intensive2_contribution, hh_rental_entry2_contribution, hh_rental_exit2_contribution,
           hh_financial_intensive2_contribution, hh_financial_entry2_contribution, hh_financial_exit2_contribution

    ) %>%
    pivot_longer(cols = p_labor_intensive2_contribution:hh_financial_exit2_contribution, names_to = "Source", values_to = "Contribution") %>%
    group_by(income_year,  mobility, Source) %>%
    summarise(mean.Contribution = weighted.mean(Contribution, as.numeric(FK060_4), na.rm = T), se = weighted_se(Contribution, as.numeric(FK060_4))) %>%
    mutate(income = if_else(str_detect(Source, "labor"), "Labor income",
                            if_else(str_detect(Source, "pension"), "Pension income",
                                    if_else(str_detect(Source, "imprent"), "Imputed rent income",
                                            ifelse(str_detect(Source, "self"), "Self-employer income",
                                                   ifelse(str_detect(Source, "rental" ), "Rental income",
                                                          ifelse(str_detect(Source, "financial"), "Financial income", "Employer income"))))))) %>%

    mutate(margin = if_else(str_detect(Source,"intensive"), "m = Intensive margin",
                            ifelse(str_detect(Source, "exit"), "m = Extensive (exit) margin", "m = Extensive (entry) margin" ))) %>% 
    


    ggplot(aes(x = income_year, y = mean.Contribution, fill = as.factor(income))) +
    # geom_smooth(aes(color = as.factor(income) ), se = F, span=0.4)+
    geom_col(aes(fill = as.factor(income))) +
    # geom_errorbar(aes(ymin = mean.Contribution - 2*se, ymax = mean.Contribution + 2*se, color = as.factor(income)), width = 0.2) +
    facet_grid(vars(margin),vars(mobility), scales = "free_y") +
    theme_bw() +
    theme(axis.title.y = element_text(size = 18))+
    theme(axis.title.x = element_text(size = 18))+
    theme(legend.title  = element_text(size = 21))+
    theme(legend.text  = element_text(size = 13))+
    scale_x_continuous(breaks= c(seq(2012, 2023, 1)))+
    scale_fill_viridis_d() +
    labs(fill="k", x = "Destination year (t)", color = "") +
    ylab(expression(bar(C)^{group("", paste(k, ",", m), "")}))+
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 60, hjust = 1)))


ggsave(filename = "outputs-included/fig5-contributions_by_mobility_2yr.pdf", plot = contributions_2yr, width = 10, height = 8, dpi = 300)


## ==================================================================================================#
# Relative Income Mobility around an Inflationary Shock
# May 2025
# EI
## ==================================================================================================#

# ==================================================================================================#
# Figure 1 - Macro inflation trend
# ==================================================================================================#

step_header("01 | Macro trend: annual inflation 2013-2023, the three inflation regimes (Fig 1)")

(inflation_fx <-read_xlsx("other-input-data/evds_inflation_fxrate.xlsx") %>% 
    mutate(
      Date = as.Date(paste0(Date, "-01")),
      inflation = as.numeric(inflation),
      fxchange = as.numeric(fxchange)
    ) %>%   
    filter(year(Date) < 2024) %>%
  ggplot(aes(x=(Date)))+
  geom_line(aes(y=as.numeric(inflation), color  = "Inflation"), size = 1 )+
  geom_point(aes(y=as.numeric(inflation), color  = "Inflation"), size = 3) +
  # geom_line(aes(y=as.numeric(fxchange), color = "Year to Year Percentage Change in USD against TL"), size = 1) +
  # geom_point(aes(y=as.numeric(fxchange), color = "Year to Year Percentage Change in USD against TL"), size = 3) +
  scale_x_date(
      limits = as.Date(c("2013-01-01", "2023-12-31")),
      date_breaks = "1 year",
      date_labels = "%Y"
    ) +
  scale_color_viridis_d() +
  # geom_smooth(aes(y=as.numeric(inflation), color  = "Inflation"), se=F, span = 0.4) +
  scale_y_continuous(breaks = seq(-10,120,5))+
  theme_bw()+
  geom_vline(xintercept = as.Date("2018-01-01"), linetype = "dashed", color = "black", size = 1) +
    annotate(
      "text",
      x = as.Date("2015-01-01"),
      y = 45,                     # adjust based on your data range
      label = "Stable inflation",# your label text
      color = "black",
      angle = 90,
      vjust = -0.5,
      hjust = 0,
      size = 5
    ) +
    annotate(
      "text",
      x = as.Date("2020-01-01"),
      y = 45,                     # adjust based on your data range
      label = "Accelarating inflation",# your label text
      color = "black",
      angle = 90,
      vjust = -0.5,
      hjust = 0,
      size = 5
    ) +
    
    annotate(
      "text",
      x = as.Date("2022-09-01"),
      y = 45,                     # adjust based on your data range
      label = "Runaway inflation",# your label text
      color = "black",
      angle = 90,
      vjust = -0.5,
      hjust = 0,
      size = 5
    ) + 
    geom_vline(xintercept = as.Date("2022-01-01"), linetype = "dashed", color = "black", size = 1) +
    geom_vline(xintercept = as.Date("2023-01-01"), linetype = "dashed", color = "black", size = 1) +
    theme(legend.position = "bottom")+
    theme(axis.title.x = element_text(size = 16))+
    theme(axis.title.y = element_text(size = 16))+
    theme(legend.text = element_text(size = 16))+
    labs(y = "%", x= "Years", color = ""))

ggsave(filename = "outputs-included/fig1-inflation.pdf", plot = inflation_fx, width = 10, height = 8, dpi = 300)


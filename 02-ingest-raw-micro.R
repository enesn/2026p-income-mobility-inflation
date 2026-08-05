## ==================================================================================================#
# Inflation and redistribution in Turkey
# May 2025
# EI
## ==================================================================================================#

## ==================================================================================================#

silc08091011f <- read_csv("raw-micro-data/GYKA_Panel_2008-2011/turkce/downloads/GYK08091011_F.csv")
silc08091011fk <- read_csv("raw-micro-data/GYKA_Panel_2008-2011/turkce/downloads/GYK08091011_fk.csv")
silc08091011hk <- read_csv("raw-micro-data/GYKA_Panel_2008-2011/turkce/downloads/gyk08091011_hk.csv")
silc08091011h <- read_csv("raw-micro-data/GYKA_Panel_2008-2011/turkce/downloads/gyk08091011_h.csv")

silc08091011 <- right_join(silc08091011f, silc08091011fk, by = c("FKIMLIK","HKIMLIK", c("FB010"="FK010"))) %>%
  left_join(silc08091011h, by = c("HKIMLIK", c("FB010"="HB010"))) %>%
  right_join(silc08091011hk, by = c("HKIMLIK", c("FB010"="HK010")))

rm(silc08091011f, silc08091011fk, silc08091011hk, silc08091011h)

## ==================================================================================================#

silc10111213f <- read.csv("raw-micro-data/GYKA_Panel_2010-2013/downloads/gyk10111213_f.csv",sep = ";")
silc10111213fk <- read.csv("raw-micro-data/GYKA_Panel_2010-2013/downloads/gyk10111213_fk.csv", sep = ";")
silc10111213h <- read.csv("raw-micro-data/GYKA_Panel_2010-2013/downloads/gyk10111213_h.csv", sep =";")
silc10111213hk <- read.csv("raw-micro-data/GYKA_Panel_2010-2013/downloads/gyk10111213_hk.csv", sep = ";")

silc10111213 <- right_join(silc10111213f, silc10111213fk, by = c("FKIMLIK","HKIMLIK", c("FB010"="FK010"))) %>%
  left_join(silc10111213h, by = c("HKIMLIK", c("FB010"="HB010"))) %>%
  right_join(silc10111213hk, by = c("HKIMLIK", c("FB010"="HK010")))

rm(silc10111213f, silc10111213fk, silc10111213hk, silc10111213h)


## ==================================================================================================#

silc12131415f <- read.csv("raw-micro-data/GYKA_Panel_2012-2015/downloads/gyk12131415_f.csv",sep = ";")
silc12131415fk <- read.csv("raw-micro-data/GYKA_Panel_2012-2015/downloads/gyk12131415_fk.csv",sep = ";")
silc12131415h <- read.csv("raw-micro-data/GYKA_Panel_2012-2015/downloads/gyk12131415_h.csv",sep = ";")
silc12131415hk <- read.csv("raw-micro-data/GYKA_Panel_2012-2015/downloads/gyk12131415_hk.csv",sep = ";")

silc12131415 <- right_join(silc12131415f, silc12131415fk, by = c("FKIMLIK","HKIMLIK", c("FB010"="FK010"))) %>%
  left_join(silc12131415h, by = c("HKIMLIK", c("FB010"="HB010"))) %>%
  right_join(silc12131415hk, by = c("HKIMLIK", c("FB010"="HK010")))

rm(silc12131415f, silc12131415fk, silc12131415hk, silc12131415h)

## ==================================================================================================#

silc14151617f <- read.csv("raw-micro-data/GYKA_Panel_2014-2017/turkce/gyk14151617_f.csv",sep = ";")
silc14151617fk <- read.csv("raw-micro-data/GYKA_Panel_2014-2017/turkce/gyk14151617_fk.csv",sep = ";")
silc14151617h <- read.csv("raw-micro-data/GYKA_Panel_2014-2017/turkce/gyk14151617_h.csv",sep = ";")
silc14151617hk <- read.csv("raw-micro-data/GYKA_Panel_2014-2017/turkce/gyk14151617_hk.csv",sep = ";")

silc14151617 <- right_join(silc14151617f, silc14151617fk, by = c("FKIMLIK","HKIMLIK", c("FB010"="FK010"))) %>%
  left_join(silc14151617h, by = c("HKIMLIK", c("FB010"="HB010"))) %>%
  right_join(silc14151617hk, by = c("HKIMLIK", c("FB010"="HK010")))

rm(silc14151617f, silc14151617fk, silc14151617hk, silc14151617h)


## ==================================================================================================#

silc15161718f <- read.csv("raw-micro-data/GYKA_Panel_2015-2018/turkce/gyk15161718_f.csv",sep = ";")
silc15161718fk <- read.csv("raw-micro-data/GYKA_Panel_2015-2018/turkce/gyk15161718_fk.csv",sep = ";")
silc15161718h <- read.csv("raw-micro-data/GYKA_Panel_2015-2018/turkce/gyk15161718_h.csv",sep = ";")
silc15161718hk <- read.csv("raw-micro-data/GYKA_Panel_2015-2018/turkce/gyk15161718_hk.csv",sep = ";")

silc15161718 <- right_join(silc15161718f, silc15161718fk, by = c("FKIMLIK","HKIMLIK", c("FB010"="FK010"))) %>%
  left_join(silc15161718h, by = c("HKIMLIK", c("FB010"="HB010"))) %>%
  right_join(silc15161718hk, by = c("HKIMLIK", c("FB010"="HK010")))

rm(silc15161718f, silc15161718fk, silc15161718hk, silc15161718h)

## ==================================================================================================#

silc16171819f <- read.csv("raw-micro-data/GYKA_Panel_2016-2019/Turkce/gyk16171819_f.csv",sep = ",")
silc16171819fk <- read.csv("raw-micro-data/GYKA_Panel_2016-2019/Turkce/gyk16171819_fk.csv",sep = ",")
silc16171819h <- read.csv("raw-micro-data/GYKA_Panel_2016-2019/Turkce/gyk16171819_h.csv",sep = ",")
silc16171819hk <- read.csv("raw-micro-data/GYKA_Panel_2016-2019/Turkce/gyk16171819_hk.csv",sep = ",")

silc16171819 <- right_join(silc16171819f, silc16171819fk, by = c("FKIMLIK","HKIMLIK", c("FB010"="FK010"))) %>%
  left_join(silc16171819h, by = c("HKIMLIK", c("FB010"="HB010"))) %>%
  right_join(silc16171819hk, by = c("HKIMLIK", c("FB010"="HK010")))

rm(silc16171819f, silc16171819fk, silc16171819hk, silc16171819h)

## ==================================================================================================#

silc17181920f <- read.csv("raw-micro-data/GYKA_Panel_2017-2020/veri_seti/csv/gyk17181920_f.csv",sep = ",")
silc17181920fk <- read.csv("raw-micro-data/GYKA_Panel_2017-2020/veri_seti/csv/gyk17181920_fk.csv",sep = ",")
silc17181920h <- read.csv("raw-micro-data/GYKA_Panel_2017-2020/veri_seti/csv/gyk17181920_h.csv",sep = ",")
silc17181920hk <- read.csv("raw-micro-data/GYKA_Panel_2017-2020/veri_seti/csv/gyk17181920_hk.csv",sep = ",")

silc17181920 <- right_join(silc17181920f, silc17181920fk, by = c("FKIMLIK","HKIMLIK", c("FB010"="FK010"))) %>%
  left_join(silc17181920h, by = c("HKIMLIK", c("FB010"="HB010"))) %>%
  right_join(silc17181920hk, by = c("HKIMLIK", c("FB010"="HK010")))

rm(silc17181920f, silc17181920fk, silc17181920hk, silc17181920h)

## ==================================================================================================#

silc18192021f <- read.csv("raw-micro-data/GYKA_Panel_2018-2021/gyk18192021_f.csv",sep = ",")
silc18192021fk <- read.csv("raw-micro-data/GYKA_Panel_2018-2021/gyk18192021_fk.csv",sep = ",")
silc18192021h <- read.csv("raw-micro-data/GYKA_Panel_2018-2021/gyk18192021_h.csv",sep = ",")
silc18192021hk <- read.csv("raw-micro-data/GYKA_Panel_2018-2021/gyk18192021_hk.csv",sep = ",")

silc18192021 <- right_join(silc18192021f, silc18192021fk, by = c("FKIMLIK","HKIMLIK", c("FB010"="FK010"))) %>%
  left_join(silc18192021h, by = c("HKIMLIK", c("FB010"="HB010"))) %>%
  right_join(silc18192021hk, by = c("HKIMLIK", c("FB010"="HK010")))

rm(silc18192021f, silc18192021fk, silc18192021hk, silc18192021h)

## ==================================================================================================#

silc19202122f <- read.csv("raw-micro-data/GYKA_Panel_2019-2022/csv/gyk19202122_f.csv",sep = ",")
silc19202122fk <- read.csv("raw-micro-data/GYKA_Panel_2019-2022/csv/gyk19202122_fk.csv",sep = ",")
silc19202122h <- read.csv("raw-micro-data/GYKA_Panel_2019-2022/csv/gyk19202122_h.csv",sep = ",")
silc19202122hk <- read.csv("raw-micro-data/GYKA_Panel_2019-2022/csv/gyk19202122_hk.csv",sep = ",")

silc19202122 <- right_join(silc19202122f, silc19202122fk, by = c("FKIMLIK","HKIMLIK", c("FB010"="FK010"))) %>%
  left_join(silc19202122h, by = c("HKIMLIK", c("FB010"="HB010"))) %>%
  right_join(silc19202122hk, by = c("HKIMLIK", c("FB010"="HK010")))

rm(silc19202122f, silc19202122fk, silc19202122hk, silc19202122h)

## ==================================================================================================#

silc20212223f <- read.csv("raw-micro-data/GYKA_Panel_2020-2023/TURKÇE/csv/gyk20212223_f.csv",sep = ",")
silc20212223fk <- read.csv("raw-micro-data/GYKA_Panel_2020-2023/TURKÇE/csv/gyk20212223_fk.csv",sep = ",")
silc20212223h <- read.csv("raw-micro-data/GYKA_Panel_2020-2023/TURKÇE/csv/gyk20212223_h.csv",sep = ",")
silc20212223hk <- read.csv("raw-micro-data/GYKA_Panel_2020-2023/TURKÇE/csv/gyk20212223_hk.csv",sep = ",")

silc20212223 <- right_join(silc20212223f, silc20212223fk, by = c("FKIMLIK","HKIMLIK", c("FB010"="FK010"))) %>%
  left_join(silc20212223h, by = c("HKIMLIK", c("FB010"="HB010"))) %>%
  right_join(silc20212223hk, by = c("HKIMLIK", c("FB010"="HK010")))

rm(silc20212223f, silc20212223fk, silc20212223hk, silc20212223h)

## ==================================================================================================#
silc21222324f <- read.csv("raw-micro-data/GYKA_Panel_2021-2024/csv_TURKÇE/gyk21222324_f.csv",sep = ",")
silc21222324fk <- read.csv("raw-micro-data/GYKA_Panel_2021-2024/csv_TURKÇE/gyk21222324_fk.csv",sep = ",")
silc21222324h <- read.csv("raw-micro-data/GYKA_Panel_2021-2024/csv_TURKÇE/gyk21222324_h.csv",sep = ",")
silc21222324hk <- read.csv("raw-micro-data/GYKA_Panel_2021-2024/csv_TURKÇE/gyk21222324_hk.csv",sep = ",")

silc21222324 <- right_join(silc21222324f, silc21222324fk, by = c("FKIMLIK","HKIMLIK", c("FB010"="FK010"))) %>%
  left_join(silc21222324h, by = c("HKIMLIK", c("FB010"="HB010"))) %>%
  right_join(silc21222324hk, by = c("HKIMLIK", c("FB010"="HK010")))

rm(silc21222324f, silc21222324fk, silc21222324hk, silc21222324h)

## ==================================================================================================#

dfs <- list(
  silc08091011,
  silc10111213,
  silc12131415,
  silc14151617, 
  silc15161718, 
  silc16171819, 
  silc17181920, 
  silc18192021, 
  silc19202122,
  silc20212223,
  silc21222324
)

dfs <- lapply(dfs, function(df) {
  df[] <- lapply(df, as.character)
  return(df)
})


silc0824 <- bind_rows(dfs)


silc0824 <- silc0824 %>% 
  distinct(FKIMLIK, FB010, .keep_all = TRUE) 

silc0824 <-  left_join(silc0824, 
    silc0824 %>% group_by(FKIMLIK) %>% summarise(times_seen=n())
  )

rm(
  silc08091011,
  silc10111213,
  silc12131415,
  silc14151617, 
  silc15161718, 
  silc16171819, 
  silc17181920, 
  silc18192021, 
  silc19202122,
  silc20212223,
  silc21222324,
  dfs
)

# Loaded ELSA waves 1-8 from Stata files
# This file is not to be used; Stata files not included

library(haven)
library(dplyr)

elsa_w1 <- read_dta("./inst/data/raw_data/wave_1_core_data_v3.dta")
elsa_w2 <- read_dta("./inst/data/raw_data/wave_2_core_data_v4.dta")
elsa_w3 <- read_dta("./inst/data/raw_data/wave_3_elsa_data_v4.dta")
elsa_w4 <- read_dta("./inst/data/raw_data/wave_4_elsa_data_v3.dta")
elsa_w5 <- read_dta("./inst/data/raw_data/wave_5_elsa_data_v4.dta")
elsa_w6 <- read_dta("./inst/data/raw_data/wave_6_elsa_data_v2.dta")
elsa_w7 <- read_dta("./inst/data/raw_data/wave_7_elsa_data.dta")
elsa_w8 <- read_dta("./inst/data/raw_data/wave_8_elsa_data_eul_v1.dta")

save(elsa_w1, elsa_w2, elsa_w3, elsa_w4, elsa_w5, elsa_w6, elsa_w7, elsa_w8, file="./inst/data/raw_data/elsa_1_8.RData")

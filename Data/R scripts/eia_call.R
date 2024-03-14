library(eia)
library(dplyr)

setwd("C:/Users/sweisberg/OneDrive - Research Triangle Institute/Documents/EMA/EMA/Data/R Scripts")

key = "f0998895fce6b12ff52dbd7267cce0fd"
eia_set_key(key)

source("./eia_api_funs.R")
source("./get_eia_data.R")
AEO_crosswalk_csv <- read.csv("./sources/api_request_seds.csv", header = T, stringsAsFactors = F)
#AEO_crosswalk_csv_hist_gas <- read_csv("DataGen/EIA/api_request_cases_hist_gas.csv")
#AEO_crosswalk_csv_hist_coal <- read_csv("DataGen/EIA/api_request_cases_hist_coal.csv")


AEO_consumption_data_total <- AEO_crosswalk_csv %>%
  interp_eia_key_data(EIA_pub = "SEDS", EIA_scenario = "TETCB") %>% # change this to whatever AEO projection base year you want and whatever EIA scenario (probably REF for now)
  get_write_eia_api_data(file = "./sources/seds_consumption_total.csv")

AEO_consumption_data_residential <- AEO_crosswalk_csv %>%
  interp_eia_key_data(EIA_pub = "SEDS", EIA_scenario = "TERCB") %>% # change this to whatever AEO projection base year you want and whatever EIA scenario (probably REF for now)
  get_write_eia_api_data(file = "./sources/seds_consumption_residential.csv")

AEO_consumption_data_industrial <- AEO_crosswalk_csv %>%
  interp_eia_key_data(EIA_pub = "SEDS", EIA_scenario = "TEICB") %>% # change this to whatever AEO projection base year you want and whatever EIA scenario (probably REF for now)
  get_write_eia_api_data(file = "./sources/seds_consumption_industrial.csv")

AEO_consumption_data_commercial <- AEO_crosswalk_csv %>%
  interp_eia_key_data(EIA_pub = "SEDS", EIA_scenario = "TECCB") %>% # change this to whatever AEO projection base year you want and whatever EIA scenario (probably REF for now)
  get_write_eia_api_data(file = "./sources/seds_consumption_commercial.csv")

AEO_consumption_data_transportation <- AEO_crosswalk_csv %>%
  interp_eia_key_data(EIA_pub = "SEDS", EIA_scenario = "TEACB") %>% # change this to whatever AEO projection base year you want and whatever EIA scenario (probably REF for now)
  get_write_eia_api_data(file = "./sources/seds_consumption_transportation.csv")

AEO_net_imports <- AEO_crosswalk_csv %>%
  interp_eia_key_data(EIA_pub = "SEDS", EIA_scenario = "ELNIB") %>% # change this to whatever AEO projection base year you want and whatever EIA scenario (probably REF for now)
  get_write_eia_api_data(file = "./sources/seds_net_imports.csv")



#STEO.CLEUDUS.A-coal
#STEO.NGEUDUS.A-gas


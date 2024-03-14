# Data pre-processing for generating empirical supply curves

library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)
options(dplyr.summarise.inform = FALSE)

wd = getwd()
setwd('C:/Users/sweisberg/OneDrive - Research Triangle Institute/Documents/EMA/EMA/Model/Results')

GEO = "region"
GEO = "state"

NATIONAL_GAS_PRICE = 3.43

N = 100
nrow = 65
nrow_price = nrow / 5
ncol = 4
ncol_price = 3

N_HOURS = 8760

#############################################################
# Price vs Quantity

# # Read in csv with mapping towards generation types
# fuel_map = read.csv("fuel_map.csv", header = T, stringsAsFactors = F)
# 
# # Read in csv with details on the shock price for each iteration
# shock_ratios = read.csv("shock_ratio.csv", header = T, stringsAsFactors = F) %>%
#   mutate(iteration = seq(1,N, by = 1))
# 
# # Read in baseline data
# price_baseline = read.csv("../../Data/updated_data/PE_pf_BASELINE.csv", header = T, stringsAsFactors = F) %>%
#   filter(fuel_type %in% c("COL","GAS")) %>%
#   mutate(fuel_type = ifelse(fuel_type == "COL","COAL",fuel_type))
# 
# generation_baseline = read.csv("./sim_generation/PE_output_generation_baseline.csv", header = T, stringsAsFactors = F) %>%
#   left_join(fuel_map, by = "unit") %>%
#   group_by(region, gen_type) %>% summarize(value = sum(value)) %>%
#   left_join(price_baseline, by = c("region" = "region", "gen_type" = "fuel_type"))
# 
# colnames(generation_baseline) = c("region", "gen_type", "generation", "year", "fuel_price")
# 
# # Build a dataframe with all of the data from the simulations
# # Also read in all of the price data
# data_full = data.frame(matrix(rep(0,nrow * ncol * N), ncol = ncol))
# prices_full = data.frame(matrix(rep(0,nrow_price * ncol_price * N), ncol = ncol_price))
# colnames(data_full) = c("region", "gen_type", "generation","iteration")
# colnames(prices_full) = c("region", "gas_price", "iteration")
# for (i in 1:N) {
#   filename = paste("./sim_generation/PE_output_generation_",i,".csv", sep = "")
#   temp = read.csv(filename, header = T, stringsAsFactors = F) %>% select(-c(vintage, time)) %>%
#     left_join(fuel_map, by = "unit") %>%
#     group_by(region, gen_type) %>% summarize(value = sum(value)) %>%
#     mutate(iteration = i)
#   
#   data_full[(nrow * (i - 1) + 1) : (nrow * i),] = temp[1:nrow,]
#   
#   filename = paste("../../Data/updated_data/fuel_shock/PE_pf_SHOCK_", i, ".csv", sep = "")
#   temp = read.csv(filename, header = T, stringsAsFactors = F) %>% filter(fuel_type == "GAS") %>%
#     select(-c(year,fuel_type)) %>%
#     mutate(iteration = i)
#   
#   
#   prices_full[(nrow_price * (i - 1) + 1) : (nrow_price * i),] = temp[1:nrow_price,]
# }
# 
# # merge with the ratios by iteration--might be useful later
# data_full = data_full %>% left_join(shock_ratios, by = "iteration") %>%
#   left_join(prices_full, by = c("iteration", "region")) %>%
#   filter(gen_type %in% c("COAL", "GAS")) %>%
#   mutate(generation = 10^6 * generation, # convert from TWh -> MWh
#          gas_price = gas_price / 1) # convert from $/mmbtu to $/mwh ## MIGHT HAVE TO CHANGE THIS TO ACCOMODATE HEATRATES--WE'LL SEE... ##
#       
# data_full_usa = data_full %>% group_by(gen_type, iteration, ratio) %>% 
#   summarize(generation = sum(generation)) %>%
#   mutate(gas_price = NATIONAL_GAS_PRICE * ratio,
#          region = "USA")
# 
# supply_curve_data = rbind(data_full, data_full_usa)
# 
# rm(data_full)
# rm(data_full_usa)
# rm(fuel_map)
# rm(generation_baseline)
# rm(price_baseline)
# rm(prices_full)
# rm(shock_ratios)
# rm(temp)

##################################################
# Heatrate vs quantity

region_map = read.csv('../../Data/R scripts/sources/region_map.csv', header = T, stringsAsFactors = F)

gentype_map = read.csv('../../Data/R scripts/sources/gentype_map.csv', header = T, stringsAsFactors = F)

planttype_map = read.csv('../../Data/R scripts/sources/planttype_map.csv', header = T, stringsAsFactors = F)

# Read in MEEDE
meede_data = read.csv('../../Data/R scripts/sources/MEEDE_Econ_Annual_WIDE.csv', header = T, stringsAsFactors = F) %>%
  filter(year == 2019) %>%
  # choose columns
  select(PID, BID, plant_nm, state, PT, FC, 
         netgen, blr_netgen_tot, # MWh
         NP, NP_plant, # MW
         heat_rt, # MMBtu / MW 
         fuel_pxbtu, # $/MMBtu
         VOM_gen # ($)
  ) %>% 
  left_join(region_map, by = "state") %>% # map to regions %>%
  #left_join(gentype_map, by = "FC") %>% # map to fueltype groupings
  left_join(planttype_map, by = c("PT","FC")) %>%
  mutate(gen_type = ifelse(plant_abbr == "col", "COAL",
                           ifelse(plant_abbr == "wnd", "WIND",
                                  ifelse(plant_abbr == "slr", "SOLAR",
                                         ifelse(plant_abbr == "nuc", "NUC",
                                                ifelse(plant_abbr %in% c("hyd","stog"),"HYDRO",
                                                       ifelse(plant_abbr %in% c("ngcc", "ngtb"),"GAS","OTHER"))))))) %>%
  # remove NAs and other irrelevant data
  filter(region != 'AKHI', !is.na(NP), 
         !is.na(heat_rt)) %>%
  filter(gen_type %in% c("HYDRO","WIND","SOLAR","NUC") | (!is.na(VOM_gen) & !is.na(fuel_pxbtu))) %>%
  mutate(VOM_gen = replace_na(VOM_gen,0), fuel_pxbtu = replace_na(fuel_pxbtu,0))

  # roll up blr level data at either region or state level
if (GEO == "region") {
  meede_data = meede_data %>% group_by(PID, plant_nm, region, gen_type, NP, heat_rt, fuel_pxbtu) %>% summarize(VOM_gen = sum(VOM_gen), netgen = sum(blr_netgen_tot)) %>%
    group_by(PID, plant_nm, region, gen_type, heat_rt, fuel_pxbtu) %>% summarize(NP = sum(NP), VOM_gen = sum(VOM_gen), netgen = sum(netgen))
} else if (GEO == "state") {
  meede_data = meede_data %>% group_by(PID, plant_nm, state, gen_type, NP, heat_rt, fuel_pxbtu) %>% summarize(VOM_gen = sum(VOM_gen), netgen = sum(blr_netgen_tot)) %>%
    group_by(PID, plant_nm, state, gen_type, heat_rt, fuel_pxbtu) %>% summarize(NP = sum(NP), VOM_gen = sum(VOM_gen), netgen = sum(netgen))
}

if (GEO == "state") {
  meede_data = meede_data %>% rename("region" = "state")
}

# continue calculations
meede_data = meede_data %>% mutate(VOM_rate = VOM_gen / (netgen), # $/MWh
         fuel_rate = fuel_pxbtu * heat_rt) %>% # $/MMBtu * MMBtu/MWh
  mutate(marginal_cost = VOM_rate + fuel_rate) %>% # $/MWh + $/MWh = $/MWh
  arrange(region, marginal_cost)

axis_lengths = meede_data %>% filter(gen_type %in% c("GAS","COAL")) %>% 
  group_by(region, gen_type) %>% summarize(netgen = sum(netgen)) %>% 
  mutate(x_axis_max = netgen / N_HOURS / 1000) # convert from MWh -> GW (or technically MWh per hr)

capacities = meede_data %>% filter(gen_type %in% c("GAS", "COAL")) %>%
  group_by(region, gen_type) %>% summarize(NP = sum(NP)) %>%
  mutate(capacity_max = NP / 1000) # convert from MW -> GW


adjust_fuel_px = function(original_meede_data, ratio, geo = "region") {
  new_meede_data = original_meede_data %>%
    mutate(fuel_pxbtu = ifelse(gen_type == "GAS", fuel_pxbtu * ratio, fuel_pxbtu)) %>%
    mutate(fuel_rate = fuel_pxbtu * heat_rt) %>%
    mutate(marginal_cost = VOM_rate + fuel_rate) %>% arrange(region, marginal_cost)
  
  return(new_meede_data)
}

# Gas/Coal/Nuclear (VOM but no fuel)/Hydro/Wind/Solar/Other -- single supply curve by color

rm(region_map)
rm(gentype_map)

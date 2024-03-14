# Pre-process MEEDE data for use by PE Dispatch model

library(dplyr)
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

wd = getwd()
setwd('C:/Users/sweisberg/OneDrive - Research Triangle Institute/Documents/EMA/EMA/Data/R Scripts')

YEAR = 2019
PE_YEAR = 2020
ONLY_2020 = TRUE
STATE_LEVEL = TRUE
COAL = c("col")
STEAMOIL_GAS = c("stog")
COMBUSTION_TURBINES = c("ngcc","ngtb")

# Need to have a nonzero capacity for every region-plant_type combination to avoid divide-by-zero errors in the optimization model
CAPACITY_CORRECTION = TRUE
PLANTS_NEEDED = c('stog_X3','lfg_X')
EPSILON = 0.000001

# Read data
dta = read.csv("sources/MEEDE_Econ_Annual_WIDE.csv", header = T, stringsAsFactors = F)
regions = read.csv("sources/region_map.csv", header = T, stringsAsFactors = F)
#plant_types = read.csv("sources/planttype_map.csv", header = T, stringsAsFactors = F)
plant_types = read.csv("sources/planttype_map_alt.csv", header = T, stringsAsFactors = F)
heatrates_future = read.csv("sources/heatrates_future.csv", header = T, stringsAsFactors = F)
fomcost_future = read.csv("sources/fomcost_future.csv", header = T, stringsAsFactors = F)
vomcost_future = read.csv("sources/vomcost_future.csv", header = T, stringsAsFactors = F)

# supplemental data
plant_map = read.csv("sources/new_existing.csv", header = T, stringsAsFactors = F)
dele_ratios = read.csv("sources/dele_ratios.csv", header = T, stringsAsFactors = F)
fuel_type_map = read.csv("sources/fuel_types.csv", header = T, stringsAsFactors = F)
fuel_ratios = read.csv("sources/fuel_ratios.csv", header = T, stringsAsFactors = F)
fuel_old = read.csv("sources/pf_old.csv", header = T, stringsAsFactors = F)

# SEDS data for electricity demand--testing
seds_total = read.csv("sources/seds_consumption_total.csv", header = T, stringsAsFactors = F)
seds_imports = read.csv("sources/seds_net_imports.csv", header = T, stringsAsFactors = F)
# seds_residential = read.csv("sources/seds_consumption_residential.csv", header = T, stringsAsFactors = F) %>%
#   mutate(series = "residential")
# seds_commercial = read.csv("sources/seds_consumption_commercial.csv", header = T, stringsAsFactors = F) %>%
#   mutate(series = "commercial")
# seds_industrial = read.csv("sources/seds_consumption_industrial.csv", header = T, stringsAsFactors = F) %>%
#   mutate(series = "industrial")
# seds_transportation = read.csv("sources/seds_consumption_transportation.csv", header = T, stringsAsFactors = F) %>%
#   mutate(series = "transportation")
# seds_full = rbind(seds_residential, seds_commercial, seds_industrial, seds_transportation)
# 

## Pre-processing

# filter to one year
dta = dta %>% filter(year == YEAR)
dta$year = PE_YEAR

# merge data to get region/plant type mappings
dta_merged = dta %>% left_join(y = regions, by = "state") %>%
  left_join(y = plant_types, by = c("PT","FC")) %>%
  left_join(y = fuel_type_map, by = "FC")

# heat rate unit conversion MMbtu/mWh -> btu/kWh
dta_merged = dta_merged %>% mutate(heat_rt = heat_rt * 1000)

# create heat class column based on heat rates and plant type
dta_merged = dta_merged %>% mutate(heat_class = ifelse((is.na(heat_rt) & (plant_abbr %in% COAL | plant_abbr %in% STEAMOIL_GAS | plant_abbr %in% COMBUSTION_TURBINES)),2,
                          ifelse(is.na(heat_rt),"",
                              ifelse(plant_abbr %in% COAL & heat_rt < 10000,1,
                                   ifelse(plant_abbr %in% COAL & heat_rt <= 12000, 2, 
                                          ifelse(plant_abbr %in% COAL & heat_rt > 12000, 3,
                                                 ifelse(plant_abbr %in% STEAMOIL_GAS & heat_rt < 10000, 1,
                                                        ifelse(plant_abbr %in% STEAMOIL_GAS & heat_rt <= 12000, 2,
                                                               ifelse(plant_abbr %in% STEAMOIL_GAS & heat_rt > 12000, 3,
                                                                      ifelse(plant_abbr %in% COMBUSTION_TURBINES & heat_rt < 11000, 1,
                                                                             ifelse(plant_abbr %in% COMBUSTION_TURBINES & heat_rt <= 13000, 2,
                                                                                    ifelse(plant_abbr %in% COMBUSTION_TURBINES & heat_rt > 13000, 3,
                                                                                           ifelse(is.na(heat_rt),2, # for now put the nas as class 2
                                                                                                  "")))))))))))))

# create carbon capture/storage column based on emissions reduction
#dta_merged = dta_merged %>% mutate(ccs = ifelse(plant_abbr %in% COAL & (!is.na(ctrl_typ_SOX)) | !(is.na(ctrl_typ_NOX)) | !(is.na(ctrl_typ_HG)) | !(is.na(ctrl_typ_PM)),"ccs",""))

# to-do: add biofuel cofiring column maybe

# Full plant_type column
dta_merged = dta_merged %>% mutate(plant_type_full = paste(plant_abbr,"_X",heat_class,sep = ""))

# Filter out AK and HI because they're not in the PE model :(
dta_merged = dta_merged %>% filter(region != "AKHI")


## Create individual input  files

# Capacity
# dimensions: region, plant_type, year
# units: GW
if (STATE_LEVEL) {
  dta_capacity_mid = dta_merged %>% group_by(PID, state, plant_type_full, year, NP) %>% summarise(count = n()) %>%
    filter(!is.na(NP))
  
  dta_capacity = dta_capacity_mid %>% group_by(state, plant_type_full, year) %>% summarize(Value = sum(NP)) %>%
    mutate(Value = Value / 1000)
} else {
  dta_capacity_mid = dta_merged %>% group_by(PID, region, plant_type_full, year, NP) %>% summarise(count = n()) %>%
    filter(!is.na(NP))
  
  dta_capacity = dta_capacity_mid %>% group_by(region, plant_type_full, year) %>% summarize(Value = sum(NP)) %>%
    mutate(Value = Value / 1000)
}

# Need to have a nonzero capacity for every region-plant_type combination to avoid divide-by-zero errors in the optimization model
if (CAPACITY_CORRECTION) {
  if (STATE_LEVEL) {
    states = unique(dta_merged$state)
    plants = rep(PLANTS_NEEDED,length(states))
    states = sort(rep(states, length(PLANTS_NEEDED)))
    years = rep(PE_YEAR,length(states))
    eps = rep(EPSILON,length(states))
    correction_df = as.data.frame(cbind(states, plants, years, eps))
    colnames(correction_df) = colnames(dta_capacity)
    correction_df$year = as.numeric(correction_df$year)
    correction_df$Value = as.numeric(correction_df$Value)
    
    dta_capacity = rbind(dta_capacity, correction_df)
  } else {
    regions = unique(dta_merged$region)
    plants = rep(PLANTS_NEEDED,length(regions))
    regions = sort(rep(regions, length(PLANTS_NEEDED)))
    years = rep(PE_YEAR,length(regions))
    eps = rep(EPSILON,length(regions))
    correction_df = as.data.frame(cbind(regions, plants, years, eps))
    colnames(correction_df) = colnames(dta_capacity)
    correction_df$year = as.numeric(correction_df$year)
    correction_df$Value = as.numeric(correction_df$Value)
    
    dta_capacity = rbind(dta_capacity, correction_df)
  }
}

if (STATE_LEVEL) {
  write.csv(dta_capacity, "../updated_data/PE_capacity_STATE.csv", row.names = F)
} else {
  write.csv(dta_capacity, "../updated_data/PE_capacity.csv", row.names = F)
}

# Heatrate
# dimensions: region, plant_type, year
# units: unclear (MEEDE is in MMBtu / MWh) (EMA in Btu/kWh)

if (STATE_LEVEL) {
  dta_heatrate_mid = dta_merged %>% group_by(PID, state, plant_type_full, year, heat_rt) %>% summarise(count = n()) %>%
    filter(!is.na(heat_rt))
  
  dta_heatrate = dta_heatrate_mid %>% group_by(state, plant_type_full, year) %>% summarize(Value = mean(heat_rt)) %>%
    mutate(Value = Value / 1) # not sure about this
  
  dta_heatrate_new = heatrates_future %>% filter(year == PE_YEAR) %>%
    left_join(regions, by = "region") %>%
    select(state, plant_type_full, year, Value)
  
  dta_heatrate_full = rbind(dta_heatrate, dta_heatrate_new)
  
  write.csv(dta_heatrate_full, "../updated_data/PE_heatrate_STATE.csv", row.names = F)
} else {
  dta_heatrate_mid = dta_merged %>% group_by(PID, region, plant_type_full, year, heat_rt) %>% summarise(count = n()) %>%
    filter(!is.na(heat_rt))
  
  dta_heatrate = dta_heatrate_mid %>% group_by(region, plant_type_full, year) %>% summarize(Value = mean(heat_rt)) %>%
    mutate(Value = Value / 1) # not sure about this
  
  #merge with future values
  dta_heatrate_full = rbind(dta_heatrate, heatrates_future)
  
  if (ONLY_2020) {
    dta_heatrate_full = dta_heatrate_full %>% filter(year == 2020)
  }
  write.csv(dta_heatrate_full, "../updated_data/PE_heatrate.csv", row.names = F)
}

# Size
# TBD

# Count
# dimensions: region, plant_type
# units: NA
# dta_count_mid = dta_merged %>% group_by(PID, region, plant_type_full, NP) %>% summarise(count = n()) %>%
#   filter(!is.na(NP))
# 
# dta_count = dta_count_mid %>% group_by(region, plant_type_full) %>% summarise(Value = sum(count))
# 
# write.csv(dta_count, "../updated_data/PE_count.csv", row.names = F)

# fomcost
# dimensions: region, plant_type
# units: $ per kW (MEEDE gives fomcost in $ so we need some calculations)
if (STATE_LEVEL) {
  dta_fomcost_mid = dta_merged %>% group_by(state, plant_type_full) %>% summarise(total_fom = sum(FOM_tot)) %>%
    inner_join(select(dta_capacity,c(state, plant_type_full, Value)), on = c("state","plant_type_full")) %>%
    mutate(fom_calc = (total_fom / (1000000 * Value)))
  
  dta_fomcost = dta_fomcost_mid %>% select(c(state, plant_type_full, fom_calc))
  colnames(dta_fomcost) = c("state","plant_type_full","Value")
  
  dta_fomcost_new = fomcost_future %>%
    left_join(regions, by = "region") %>%
    select(state, plant_type_full, Value)
  
  dta_fomcost_full = rbind(dta_fomcost, dta_fomcost_new)
  
  write.csv(dta_fomcost_full, "../updated_data/PE_fomcost_STATE.csv", row.names = F)
} else {
  dta_fomcost_mid = dta_merged %>% group_by(region, plant_type_full) %>% summarise(total_fom = sum(FOM_tot)) %>%
    inner_join(select(dta_capacity,c(region, plant_type_full, Value)), on = c("region","plant_type_full")) %>%
    mutate(fom_calc = (total_fom / (1000000 * Value)))
  
  dta_fomcost = dta_fomcost_mid %>% select(c(region, plant_type_full, fom_calc))
  colnames(dta_fomcost) = c("region","plant_type_full","Value")
  
  dta_fomcost_full = rbind(dta_fomcost, fomcost_future)  

  write.csv(dta_fomcost_full, "../updated_data/PE_fomcost.csv", row.names = F)
}

# vomcost
# dimensions: region, plant_type
# units: $ per mwh (VOM_tot in $, netgen in mwh)
if (STATE_LEVEL) {
  dta_vomcost = dta_merged %>% group_by(state, plant_type_full) %>% 
    # total variable cost divided by total boiler generation
    summarise(total_vom = sum(VOM_tot), total_gen = sum(blr_netgen_tot)) %>%
    mutate(Value = total_vom / total_gen) %>%
    select(c(state, plant_type_full, Value))
  
  # replace NAs (mostly bio) with 0s
  dta_vomcost$Value[is.na(dta_vomcost$Value)] = 0
  
  dta_vomcost_new = vomcost_future %>%
    left_join(regions, by = "region") %>%
    select(state, plant_type_full, Value)
  
  dta_vomcost_full = rbind(dta_vomcost, dta_vomcost_new)
  
  write.csv(dta_vomcost_full, "../updated_data/PE_vomcost_STATE.csv", row.names = F)
} else {
  dta_vomcost = dta_merged %>% group_by(region, plant_type_full) %>% 
    # total variable cost divided by total boiler generation
    summarise(total_vom = sum(VOM_tot), total_gen = sum(blr_netgen_tot)) %>%
    mutate(Value = total_vom / total_gen) %>%
    select(c(region, plant_type_full, Value))
  
  # replace NAs (mostly bio) with 0s
  dta_vomcost$Value[is.na(dta_vomcost$Value)] = 0
  
  dta_vomcost_full = rbind(dta_vomcost, vomcost_future)
  
  write.csv(dta_vomcost_full, "../updated_data/PE_vomcost.csv", row.names = F)
}

# net generation/dele
# for state-level, we're now incorporating net imports from SEDS
# dimensions: region, plant_type
# units: MWh -> TWh
if (STATE_LEVEL) {
  dta_netgen = dta_merged %>% group_by(state, plant_type_full) %>%
    summarize(netgen = sum(blr_netgen_tot)) %>%
    mutate(netgen = netgen / 1000000)
  
  dta_dele = dta_netgen %>% group_by(state) %>%
    summarize(value = sum(netgen)) %>%
    mutate(year = 2020) %>%
    select(state, year, value)

  colnames(dta_dele) = c("state", "year", "netgen")
  
  dta_imports = seds_imports %>% filter(year == YEAR) %>%
    mutate(year = PE_YEAR) %>%
    select(region, year, value) %>%
    mutate(value = value * 2.93e-4) # convert from BBtu -> TWh
  
  colnames(dta_imports) = c("state", "year", "net_imports")
  
  dta_dele_full = dta_dele %>% left_join(dta_imports, by = c("state", "year")) %>%
    mutate(Value = netgen + net_imports) %>%
    select(state, year, Value)
  
  write.csv(dta_dele_full, "../updated_data/PE_dele_STATE.csv", row.names = F)
} else {
  dta_netgen = dta_merged %>% group_by(region, plant_type_full) %>%
    summarize(netgen = sum(blr_netgen_tot)) %>%
    mutate(netgen = netgen / 1000000)
  
  dta_dele = dta_netgen %>% group_by(region) %>%
    summarize(value = sum(netgen)) %>%
    mutate(year = 2020)

  dta_dele_full = dele_ratios %>% left_join(dta_dele, by = "region") %>%
    mutate(value_final = value.y * ratio) %>%
    select(region, year.x, value_final)

  colnames(dta_dele_full) = c("region", "year", "Value")

  if (ONLY_2020) {
    dta_dele_full = dta_dele_full %>% filter(year == 2020)
  }

  write.csv(dta_dele_full, "../updated_data/PE_dele.csv", row.names = F)
}

# fuel price
# dimensions: region, fuel type, year
# units: $/MMBtu
if(STATE_LEVEL) {
  dta_fuel = dta_merged %>% filter(fuel_type %in% c("COL","GAS","OIL","BIO","NUC")) %>%
    group_by(state, fuel_type) %>% summarize(Value = mean(fuel_pxbtu, na.rm = TRUE)) %>%
    mutate(year = 2020) %>%
    select(state, fuel_type, year, Value)
  
  colnames(dta_fuel) = c("state", "fuel_type", "year", "Value_mid")
  
  fuel_old = fuel_old %>% filter(year == PE_YEAR) %>%
    left_join(regions, by = "region")
  
  # coal, gas, oil costs from MEEDE
  dta_fuel_full = dta_fuel %>% left_join(fuel_old, by = c("state", "fuel_type", "year")) %>%
    mutate(Value = ifelse(is.na(Value_mid),Value_old,Value_mid)) %>%
    filter(!(fuel_type %in% c("BIO","NUC"))) %>%
    select(state, region, fuel_type, year, Value)
  
  # nuc, bio costs from previous PE data--will be replaced with updated data eventually
  dta_fuel_mid = fuel_ratios %>%
    left_join(regions, by = "region") %>%
    filter(fuel_type %in% c("bio","nuc"), year == PE_YEAR) %>%
    #select(-ratio) %>%
    select(state, region, fuel_type, year, Value, -ratio)
  
  dta_fuel_full = rbind(dta_fuel_mid, dta_fuel_full)
  
  dta_fuel_full = dta_fuel_full %>% 
    select(state, fuel_type, year, Value)
  
  write.csv(dta_fuel_full, "../updated_data/PE_pf_STATE.csv", row.names = F)
  
  # to-do: get nuc costs by state from EIA
} else {
  dta_fuel = dta_merged %>% filter(fuel_type %in% c("COL","GAS","OIL","BIO","NUC")) %>%
    group_by(region, fuel_type) %>% summarize(Value = mean(fuel_pxbtu, na.rm = TRUE)) %>%
    mutate(year = 2020) %>%
    select(region, fuel_type, year, Value)
  
  # project to 2075 using ratios from old data
  dta_fuel_mid = fuel_ratios %>% left_join(dta_fuel, by = c("region", "fuel_type")) %>%
    mutate(value_final = Value.y * ratio) %>%
    select(region, fuel_type, year.x, value_final)
  
  colnames(dta_fuel_mid) = c("region", "fuel_type", "year", "Value_mid")
  
  # fill in NAs (nuc, bio) using old data since these aren't in MEEDE
  dta_fuel_full = dta_fuel_mid %>% left_join(fuel_old, by = c("region", "fuel_type", "year")) %>%
    mutate(Value = ifelse(is.na(Value_mid),Value_old,Value_mid)) %>%
    select(region, fuel_type, year, Value)
  
  if (ONLY_2020) {
    dta_fuel_full = dta_fuel_full %>% filter(year == 2020)
  }
  
  write.csv(dta_fuel_full, "../updated_data/PE_pf.csv", row.names = F)
}

# wholesale cost
# dimensions: 
# units: $ per MWh
if (STATE_LEVEL) {
  dta_wholesale = dta_merged %>% select(state, plant_type_full, FERC_annual,blr_netgen_tot) %>%
    mutate(spend = FERC_annual * blr_netgen_tot) %>% 
    group_by(state, plant_type_full) %>%
    summarize(total_gen = sum(blr_netgen_tot, na.rm = T), total_spend = sum(spend, na.rm = T)) %>%
    mutate(avg_wholesale = total_spend / total_gen)
} else {
  dta_wholesale = dta_merged %>% select(region, plant_type_full, FERC_annual,blr_netgen_tot) %>%
    mutate(spend = FERC_annual * blr_netgen_tot) %>% 
    group_by(region, plant_type_full) %>%
    summarize(total_gen = sum(blr_netgen_tot, na.rm = T), total_spend = sum(spend, na.rm = T)) %>%
    mutate(avg_wholesale = total_spend / total_gen)
}
  
# combined table to compare with PE data
if (STATE_LEVEL) {
  dta_combined = dta_capacity %>% left_join(plant_map, by = c("plant_type_full" = "unit")) %>%
    left_join(dta_vomcost_full, by = c("state", "plant_type_full")) %>%
    left_join(dta_fomcost_full, by = c("state", "plant_type_full")) %>%
    left_join(dta_netgen, by = c("state", "plant_type_full")) %>%
    left_join(
      select(dta_wholesale,c(state,plant_type_full,avg_wholesale)),
      by = c("state", "plant_type_full")) %>%
    mutate(variable_cost = netgen * Value.y, fixed_cost = Value.x * Value) %>%
    mutate(total_cost = variable_cost + fixed_cost) %>%
    select(state, plant_type_full, status, plant_type, renewable, year, netgen, Value.y, variable_cost,
           Value.x, Value, fixed_cost, total_cost, avg_wholesale)
  
  colnames(dta_combined) = c("state", "unit", "status", "plant_type", "renewable", "time", 
                             "generation", "vomcost", "variable_cost", "capacity", "fomcost", "fixed_cost",
                             "total_cost", "wholesale_cost")
  
  write.csv(dta_combined, "../updated_data/2020_data_comparison_MEEDE_STATE.csv", row.names = F)  
} else {
  dta_combined = dta_capacity %>% left_join(plant_map, by = c("plant_type_full" = "unit")) %>%
    left_join(dta_vomcost_full, by = c("region", "plant_type_full")) %>%
    left_join(dta_fomcost_full, by = c("region", "plant_type_full")) %>%
    left_join(dta_netgen, by = c("region", "plant_type_full")) %>%
    left_join(
      select(dta_wholesale,c(region,plant_type_full,avg_wholesale)),
             by = c("region", "plant_type_full")) %>%
    mutate(variable_cost = netgen * Value.y, fixed_cost = Value.x * Value) %>%
    mutate(total_cost = variable_cost + fixed_cost) %>%
    select(region, plant_type_full, status, plant_type, renewable, year, netgen, Value.y, variable_cost,
           Value.x, Value, fixed_cost, total_cost, avg_wholesale)
  
  colnames(dta_combined) = c("region", "unit", "status", "plant_type", "renewable", "time", 
                             "generation", "vomcost", "variable_cost", "capacity", "fomcost", "fixed_cost",
                             "total_cost", "wholesale_cost")
  
  write.csv(dta_combined, "../updated_data/2020_data_comparison_MEEDE.csv", row.names = F)
}

# One-off data joins for converting data from region to state level
# emis_factor = read.csv("../updated_data/PE_emis_factor.csv", header = T, stringsAsFactors = F)
# emis_factor = emis_factor %>% left_join(regions, by = "region") %>%
#   select(state, plant_type, pollutant, Value)
# write.csv(emis_factor, "../updated_data/PE_emis_factor_STATE.csv", row.names = F)
# 
# load_pct = read.csv("../updated_data/PE_loadpct.csv", header = T, stringsAsFactors = F)
# load_pct = load_pct %>% left_join(regions, by = "region") %>%
#   select(state, load_segment, Value)
# write.csv(load_pct, "../updated_data/PE_loadpct_STATE.csv", row.names = F)
# 
# hours = read.csv("../updated_data/PE_hours.csv", header = T, stringsAsFactors = F) %>%
#   mutate(Value = ifelse(region == "NEG" & load_segment == "sum_mrnevn", Value + 2, Value))
# hours = hours %>% left_join(regions, by = "region") %>%
#   select(state, load_segment, Value)
# write.csv(hours, "../updated_data/PE_hours_STATE.csv", row.names = F)
# 
# maxCF = read.csv("../updated_data/PE_maxCF.csv", header = T, stringsAsFactors = F)
# maxCF = maxCF %>% left_join(regions, by = "region") %>%
#   select(state, plant_type, load_segment, Value)
# write.csv(maxCF, "../updated_data/PE_maxCF_STATE.csv", row.names = F)



setwd(wd)
library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)

wd = getwd()
setwd('C:/Users/sweisberg/OneDrive - Research Triangle Institute/Documents/EMA/EMA/Model/Results')

options(dplyr.summarise.inform = FALSE)

NATIONAL_GAS_PRICE = 3.43
MOVER = "gas"
# MOVER = "col"
# MOVER = "oil"

N = 100
# nrow = 65
# nrow_price = nrow / 5
# ncol = 4
# ncol_price = 3

colors = c(
  "#f8766d", # red
  "#00bdc4" # blue
)

colors2 = c(
  "#000077", # dark blue
  "#addde6" # light blue
)


# Read in csv with mapping towards generation types
fuel_map = read.csv("fuel_map.csv", header = T, stringsAsFactors = F)

# Read in csv with details on the shock price for each iteration
# shock_ratios = read.csv("shock_ratio.csv", header = T, stringsAsFactors = F) %>%
#   mutate(iteration = seq(1,N, by = 1))

shock_ratios = read.csv(paste0("shock_ratio_",MOVER,".csv"), header = T, stringsAsFactors = F) %>%
  mutate(iteration = seq(1,N, by = 1))

# Read in baseline data
price_baseline = read.csv("../../Data/updated_data/PE_pf_BASELINE_SAGE.csv", header = T, stringsAsFactors = F) %>%
  filter(fuel_type %in% c("COL","GAS")) %>%
  mutate(fuel_type = ifelse(fuel_type == "COL","COAL",fuel_type))

generation_baseline = read.csv("./sim_generation/PE_output_generation_baseline.csv", header = T, stringsAsFactors = F) %>%
  left_join(fuel_map, by = "unit") %>%
  group_by(region, gen_type) %>% summarize(value = sum(value)) %>%
  left_join(price_baseline, by = c("region" = "region", "gen_type" = "fuel_type"))

colnames(generation_baseline) = c("region", "gen_type", "generation", "year", "fuel_price")

# Build a dataframe with all of the data from the simulations
# Also read in all of the price data
# data_full = data.frame(matrix(rep(0,nrow * ncol * N), ncol = ncol))
# prices_full = data.frame(matrix(rep(0,nrow_price * ncol_price * N), ncol = ncol_price))
data_full = data.frame()
prices_full = data.frame()
for (i in 1:N) {
  filename = paste("./sim_generation/PE_output_generation_", MOVER, "_", i, ".csv", sep = "")
  temp = read.csv(filename, header = T, stringsAsFactors = F) %>% 
    select(-c(vintage, time)) %>%
    left_join(fuel_map, by = "unit") %>%
    group_by(region, gen_type) %>% summarize(value = sum(value)) %>%
    mutate(iteration = i)
  
  data_full = bind_rows(data_full, temp)
  # data_full[(nrow * (i - 1) + 1) : (nrow * i),] = temp[1:nrow,]
  
  filename = paste("../../Data/updated_data/fuel_shock/PE_pf_SHOCK_", MOVER, "_", i, ".csv", sep = "")
  temp = read.csv(filename, header = T, stringsAsFactors = F) %>% 
    filter(fuel_type == "GAS") %>%
    select(-c(year,fuel_type)) %>%
    mutate(iteration = i)
  
  prices_full = bind_rows(prices_full, temp)
  # prices_full[(nrow_price * (i - 1) + 1) : (nrow_price * i),] = temp[1:nrow_price,]
}
colnames(data_full) = c("region", "gen_type", "value","iteration")
colnames(prices_full) = c("region", "fuel_price", "iteration")

# merge with the ratios by iteration--might be useful later
data_full = data_full %>% left_join(shock_ratios, by = "iteration")

# Construct kernel density estimations
generation_distribution_plot = function(dataframe, baseline, region_filter = "USA") {
  gen_types = unique(data_full$gen_type)
  if (region_filter == "USA") {
    dataframe = dataframe %>% group_by(gen_type, iteration) %>% summarize(value = sum(value)) %>% mutate(region = "USA")
    baseline = baseline %>% group_by(gen_type) %>% summarize(value = sum(generation)) %>% mutate(region = "USA")
  } else {
    dataframe = dataframe %>% filter(region == region_filter)
    baseline = baseline %>% filter(region == region_filter)
  }
  
  p = dataframe %>% 
    filter(gen_type %in% c("COAL","GAS")) %>% # RENEW is always constant, NUC is almost always constant, who cares about OTHER...
    ggplot() +
    geom_density(aes(x = value, color = gen_type, fill = gen_type), alpha = 0.1, adjust = 0.75, linewidth = 0.8) +
    scale_color_manual(values = colors2, name = "Generation") + 
    scale_fill_manual(values = colors2, name = "Generation") + 
    geom_vline(aes(xintercept = mean(baseline$value[baseline$gen_type == "COAL"])), color = colors2[1], linetype = "longdash", linewidth = 0.6) + 
    geom_vline(aes(xintercept = mean(baseline$value[baseline$gen_type == "GAS"])), color = colors2[2], linetype = "longdash", linewidth = 0.6) + 
    scale_y_continuous(expand = expansion(mult = c(0,0.1))) +
    scale_x_continuous(limits = c(0,3000), expand = expansion(mult = c(0,0.05))) +
    labs(x = "Total Generation (TWh)", y = "Density", title = paste("Distributions of Gas/Coal Generation under Gas Price Shocks: ", region_filter, sep = "")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),axis.ticks = element_blank(),
          axis.line.x = element_line(linewidth = 0.5, color = "black", linetype=1),
          axis.line.y = element_line(linewidth = 0.5, color = "black", linetype=1), 
          axis.text.x = element_text(size = 10), axis.text.y = element_blank(),
          plot.title = element_blank(), legend.position = "None")
  return(p)
}

#p = generation_distribution_plot(data_full, generation_baseline, region_filter = "USA")
#print(p)
#ggsave("C:/Users/sweisberg/Research Triangle Institute/Henry, Candise - MEEDE Present/Figures_1112/gen_mix_distribution.png", width = 6.67, height = 4, units = "in")


fuel_shock_scatter = function(dataframe, region_filter = "USA") {
  gen_types = unique(data_full$gen_type)
  if (region_filter == "USA") {
    dataframe = dataframe %>% group_by(gen_type, iteration, ratio) %>% summarize(value = sum(value)) %>% mutate(region = "USA")
    #baseline = baseline %>% group_by(gen_type) %>% summarize(value = sum(value)) %>% mutate(region = "USA")
  } else {
    dataframe = dataframe %>% filter(region == region_filter) %>%
      group_by(region, gen_type, value) %>% summarize(ratio = mean(ratio)) %>% ungroup()
      #arrange(gen_type, desc(value))
    #baseline = baseline %>% filter(region == region_filter)
  }
  
  dataframe = dataframe %>% filter(gen_type %in% c("COAL", "GAS"))
  #baseline = baseline %>% filter(gen_type %in% c("COAL","GAS"))
  #View(dataframe)
  
  jitter <- position_jitter(width = 0.0, height = 0.025)
  p = dataframe %>% ggplot() + 
    geom_point(aes(y = ratio, x = value, color = gen_type)) +
    geom_line(aes(y = ratio, x = value, color = gen_type), size = 0.8) +
    #geom_smooth(aes(y = ratio, x = value, color = gen_type), method='gam', formula= y~x, se = F) +
    #geom_segment(aes(y = ratio, yend = ratio, x = 0, xend = 40), color = "black", position = jitter) + 
    #geom_segment(aes(x = ratio, xend = ratio, y = 0, yend = 90), color = "red") + 
    geom_hline(aes(yintercept = 1), color = "gray", linetype = "longdash", linewidth = 0.6) +
    scale_y_continuous(limits = c(0,4), expand = expansion(mult = c(0,0.05))) +
    scale_x_continuous(limits = c(0,NA), expand = expansion(mult = c(0,0.05))) +
    scale_color_manual(values = colors2, name = "Generation") + 
    labs(title = region_filter[1], y = "Gas Price Ratio (Gas Price / Coal Price)", x = "Total Generation (TWh)") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),axis.ticks = element_blank(),
          axis.line.x = element_line(linewidth = 0.5, color = "black", linetype=1),
          axis.line.y = element_line(linewidth = 0.5, color = "black", linetype=1), 
          axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
  return(p)
}

p = fuel_shock_scatter(data_full, region_filter = c("USA"))
print(p)
p = fuel_shock_scatter(data_full, region_filter = c("South"))
print(p)
p = fuel_shock_scatter(data_full, region_filter = c("Midwest"))
print(p)
p = fuel_shock_scatter(data_full, region_filter = c("Northeast"))
print(p)
p = fuel_shock_scatter(data_full, region_filter = c("West"))
print(p)
#ggsave("C:/Users/sweisberg/Research Triangle Institute/Henry, Candise - MEEDE Present/Figures_1112/gen_mix_scatter_sticks_rev.png", width = 6.67, height = 4, units = "in")

# method either "OLS" or "relative"
calculate_elasticity = function(dataframe, prices, baseline = NA, gen_type_filter = "GAS", region_filter = "USA", method = "OLS") {
  if (region_filter == "USA") {
    dataframe = dataframe %>% group_by(gen_type, iteration, ratio) %>% summarize(value = sum(value)) %>% 
      #filter(gen_type == gen_type_filter) %>%
      mutate(region = "USA", baseline_price = NATIONAL_GAS_PRICE, fuel_price = baseline_price * ratio, 
             value = 10^6 * value, # convert from TWh to MWh
             fuel_price = fuel_price / 0.29307,
             baseline_price = baseline_price / 0.29307) # convert from $/mmbtu to $/mwh
    if (method == "OLS") {
      dataframe = dataframe %>% filter(gen_type == gen_type_filter)
    }
    
    baseline = baseline %>% group_by(gen_type) %>% summarize(fuel_price = mean(fuel_price), generation = sum(generation), ) %>%
      mutate(region = "USA", generation = 10^6 * generation, fuel_price = fuel_price / 0.29307)
  } else {
    #View(dataframe)
    dataframe = dataframe %>% filter(region == region_filter) %>% 
      #filter(gen_type == gen_type_filter) %>%
      left_join(prices, by = c("iteration", "region")) %>%
      mutate(value = 10^6 * value,
             fuel_price = fuel_price / 0.29307)
    if (method == "OLS") {
      dataframe = dataframe %>% filter(gen_type == gen_type_filter)
    }
    
    baseline = baseline %>% filter(region == region_filter) %>%
      mutate(value = 10^6 * generation, 
             fuel_price = fuel_price / 0.29307)
    #print(dataframe)
  }
  
  # run an OLS and calculate the slope (elasticity)
  if (method == "OLS") {
    if (sum(dataframe$value) != 0) {
      dataframe = dataframe %>% filter(value != 0, fuel_price != 0)
      reg = lm(log(fuel_price) ~ log(value), data = dataframe, na.action = na.omit)
      slope = round(as.numeric(coef(reg)[2]),2)
    } else {
      slope = 0
    }
  } else if (method == "relative") {
    # this won't be specific to a single fuel type since it's relative
    # the opposite would be the inverse of the fractions, I guess
    
    # ps and qs for starting point
    qg0 = filter(baseline, region == region_filter,gen_type == "GAS")$generation
    pg0 = filter(baseline, region == region_filter,gen_type == "GAS")$fuel_price
    qc0 = filter(baseline, region == region_filter,gen_type == "COAL")$generation
    pc0 = filter(baseline, region == region_filter,gen_type == "COAL")$fuel_price
    
    #qg0 = log(qg0)
    #pg0 = log(pg0)
    #qc0 = log(qc0)
    #pc0 = log(pc0)
    
    p_ratio0 = pg0 / pc0
    q_ratio0 = qg0 / qc0
    if (gen_type_filter == "COAL") {
      q_ratio0 = 1/q_ratio0
    }
    
    # ps and qs for each simulated point and take the average
    elasticities = c()
    for(i in 1:100) {
      qg1 = filter(dataframe, gen_type == "GAS", iteration == i)$value
      pg1 = filter(dataframe, gen_type == "GAS", iteration == i)$fuel_price
      qc1 = filter(dataframe, gen_type == "COAL", iteration == i)$value
      pc1 = pc0 # coal price doesn't change, only the relative price changes
      
      #qg1 = log(qg1)
      #pg1 = log(pg1)
      #qc1 = log(qc1)
      
      p_ratio1 = pg1 / pc1
      q_ratio1 = qg1 / qc1
      if (gen_type_filter == "COAL") {
        q_ratio1 = 1/q_ratio1
      }
      
      # percent change in p and q
      pct_ch_q = (q_ratio1 - q_ratio0) / q_ratio0
      pct_ch_p = (p_ratio1 - p_ratio0) / p_ratio0
      
      elasticity = pct_ch_q / pct_ch_p
      #print(elasticity)
      if(!is.nan(elasticity)) {
        if (elasticity != -Inf & elasticity != Inf) {
          elasticities = c(elasticities, elasticity)
        }
      }
    }
    if(length(elasticities) >= 1) {
      slope = round(mean(elasticities),2)
    } else {
      slope = NA
    }
  }  
  return(slope)
}

#elasticity = calculate_elasticity(data_full, prices_full, generation_baseline, gen_type_filter = "GAS", region_filter = "USA", method = "relative")
#print(elasticity)

# elasticities = c()
# for (i in 1:99) {
#   elasticity = calculate_elasticity(data_full, prices_full, generation_baseline, gen_type_filter = "GAS", region_filter = "USA", method = "relative", sim_number = i)
#   elasticities = c(elasticities, elasticity)
# }
# elasticities
# 
# for (region in unique(data_full$region)) {
#     e = calculate_elasticity(data_full, prices_full, generation_baseline, gen_type_filter = "GAS", region_filter = region, method = "relative")
#     print(paste("Region: ", region, ", ", e, sep = ""))
# }

#region_order = c("ESC", "TX", "ENC", "USA", "NEG", "MTN", "FL", "WSC", "CA", "SAC", "PAC", "WNC", "MID", "NY")
#region_order = c("NY", "MID", "WNC", "PAC", "SAC", "CA", "WSC", "FL", "MTN", "NEG", "USA", "ENC", "TX", "ESC")
#region_order = c("CA", "PAC", "MID", "NEG", "ENC", "TX", "USA", "MTN", "SAC", "NY", "WSC", "FL", "WNC", "ESC")
#elasticities = read.csv("elasticity_table.csv", header = T, stringsAsFactors = F)
# NY: 46.79, FL: 16.56


# xgap = 0.56
# p = elasticities %>% ggplot() +
#   #geom_col(aes(x = elasticity, y = factor(region, levels = region_order), fill = fuel), position = position_dodge(width = 0.0), width = 0.85) +
#   #geom_text(aes(x = elasticity + sign(elasticity + 0.001) * xgap, y = factor(region, levels = region_order),
#   #              label = round(elasticity,2), group = fuel), position = position_dodge(width = 0.0), color = "black", size = 3, vjust = 0.4) +
#   geom_col(aes(x = elasticity_relative, y = factor(region, levels = region_order), fill = fuel), position = position_dodge(width = 0.0), width = 0.85) +
#   geom_text(aes(x = elasticity_relative + sign(elasticity_relative + 0.001) * xgap, y = factor(region, levels = region_order),
#                 label = round(elasticity_relative,2), group = fuel), position = position_dodge(width = 0.0), color = "black", size = 3, vjust = 0.4) +
#   geom_vline(aes(xintercept = 0), linetype = "solid", color = "black", size = 0.5) +
#   scale_y_discrete(expand = expansion(add = c(0.5,.5))) +
#   #scale_x_continuous(limits = c(-2.5,2.5)) +
#   scale_x_continuous(limits = c(-11,11)) +
#   scale_fill_manual(values = colors2, name = "Elasticity") +
#   labs(x = "Gas Price Elasticity", y = "Region") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(),axis.ticks = element_blank(),
#         axis.line.x = element_line(size = 0.5, color = "black", linetype=1),
#         axis.line.y = element_blank(), 
#         axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold"),
#         legend.position = "None")
# print(p)
# ggsave("C:/Users/sweisberg/Research Triangle Institute/Henry, Candise - MEEDE Present/Figures_1112/elasticity_bars_relative.png", width = 6.67, height = 4, units = "in")

#data_full_usa = data_full %>% group_by(gen_type, iteration, ratio) %>% summarize(value = sum(value)) %>% mutate(region = "USA")

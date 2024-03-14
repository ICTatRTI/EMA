# Empirical Supply Curves

library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)
library(tidyr)
library(patchwork)
library(stringr)
options(dplyr.summarise.inform = FALSE)


wd = getwd()
setwd('C:/Users/sweisberg/OneDrive - Research Triangle Institute/Documents/EMA/EMA/Model/Results')

source('supply_curve_preprocessing.R') 
# data will be put in the dataframe: supply_curve_data
# meede data for heatrate calculations in: meede_data

colors2 = c(
  "#000077", # dark blue
  "#addde6" # light blue
)

## price as a function of quantity (units: $ per MMBtu)
# from sim data
# two options: linear fit, step function
# quantity must be in MWh
# supply_curve_price = function(quantity, dataframe, region_filter = "USA", gen_type_filter = "GAS", method = "linear"){
#   dataframe = dataframe %>% filter(region == region_filter, gen_type == gen_type_filter)
#   # generate an OLS fit and estimate the price as a function of quantity using that linear fit
#   if (method == "linear") {
#     reg = lm(gas_price ~ generation, data = dataframe, na.action = na.omit)
#     intercept = as.numeric(coef(reg)[1])
#     slope = as.numeric(coef(reg)[2])
#     price = intercept + slope * quantity
#   } else if(method == "step") { # generate a step function and estimate the price as a function of quantity using that fit
#     # sort by increasing generation
#     dataframe = dataframe %>% arrange(generation)
#     # loop until we find the correct step and return the corresponding price
#     for (i in 1:nrow(dataframe)) {
#       price = dataframe$gas_price[i]
#       generation = dataframe$generation[i]
#       if (quantity < generation) {
#         break()
#       }
#     }
#   }
#   return(price) # $/MMBtu
# }

#print(supply_curve_price(1.8 * 10^6, supply_curve_data, region_filter = "NEG", method = "step"))

# heatrate as a function of quantity (units: MMBtu per MW or some equivalent)
# from MEEDE?
# not sure yet
supply_curve_marginal_cost = function(quantity, dataframe, region_filter = "USA", gen_type_filter = "GAS") {
  if (region_filter == "USA") {
    dataframe = dataframe %>% filter(gen_type == gen_type_filter) %>%
      arrange(marginal_cost)
  } else {
    dataframe = dataframe %>% filter(region == region_filter, gen_type == gen_type_filter)
  }
  
  quantity_mw = quantity / N_HOURS # convert quantity from MWh -> MW by dividing by 8760 hours per year
  # this assumes each plant/boiler selected is operating constantly throughout the year, which is probably not a great assumption
  
  dataframe = dataframe %>% ungroup() %>% 
    mutate(rolling_NP = cumsum(NP)) %>% # running total of capacity
    mutate(rolling_NP_lag = lag(rolling_NP)) %>% # we need to include the plant after the threshold as well to make sure we meet the full capacity
    mutate(rolling_NP_lag = ifelse(is.na(rolling_NP_lag),0,rolling_NP_lag)) %>%
    filter(rolling_NP_lag <= quantity_mw)
  
  #heatrate = weighted.mean(dataframe$heat_rt, w = dataframe$NP)
  marginal_cost = weighted.mean(dataframe$marginal_cost, w = dataframe$NP)

  return(marginal_cost) # MMBtu/MW
}

supply_curve_all_gen = function(dataframe, region_filter = "USA") {
  if (region_filter == "USA") {
    t = 5
  } else {
    dataframe = dataframe %>% filter(region == region_filter)
  }
  dataframe = dataframe %>% arrange(marginal_cost, gen_type) %>% ungroup() %>%
    mutate(rolling_NP = cumsum(NP)) %>% # running total of capacity
    mutate(rolling_avg_cost = cummean(marginal_cost))
  
  return(dataframe)
}

#supply_all = supply_curve_all_gen(meede_data, "USA")

#print(supply_curve_marginal_cost(13 * 10^8, meede_data, region_filter = "USA", gen_type_filter = "GAS"))
                            
# price of electricity as a function of quantity (supply curve)
# multiplying the two previous functions together
# supply_curve_final = function(quantity, heatrate_data, region_filter = "NEG", gen_type_filter = "GAS") {
#   #price = supply_curve_price(quantity, price_data, region_filter, gen_type_filter, price_method) # $/MMBtu
#   marginal_cost = supply_curve_marginal_cost(quantity, heatrate_data, region_filter, gen_type_filter) # MMBtu/MW
#   #print(price)
#   #print(heatrate)
#   #pe = price * heatrate # ($/MMBtu) * (MMBtu/MW) = $/MW
#   pe = marginal_cost # better labels :(
#   return(pe)
# }

#print(supply_curve_final(6 * 10^7, supply_curve_data, meede_data, region_filter = "NEG", gen_type_filter = "GAS", price_method = "linear"))

generate_supply_curves = function(heatrate_data, capacity_data, axis_data, region_filter = "USA") {
  if (region_filter != "USA") {
    axis_data_coal = axis_data %>% filter(region == region_filter, gen_type == "COAL")
    axis_data_gas = axis_data %>% filter(region == region_filter, gen_type == "GAS")
    xmin = 0
    if (nrow(axis_data_coal) > 0) {
      xmax_coal = axis_data_coal$x_axis_max * 1000000
    } else {
      xmax_coal = 0
    }
    if (nrow(axis_data_gas) > 0) {
      xmax_gas = axis_data_gas$x_axis_max * 1000000
    } else {
      xmax_gas = 0
    }
    
    coal_capacity = capacity_data %>% filter(region == region_filter, gen_type == "COAL")
    gas_capacity = capacity_data %>% filter(region == region_filter, gen_type == "GAS")
    if (nrow(coal_capacity) > 0) {
      max_coal = coal_capacity$capacity_max * 1000000
    } else {
      max_coal = 0
    }
    if (nrow(gas_capacity) > 0) {
      max_gas = gas_capacity$capacity_max * 1000000
    } else {
      max_gas = 0
    }
    
  } else {
    axis_data_coal = axis_data %>% filter(gen_type == "COAL")
    axis_data_gas = axis_data %>% filter(gen_type == "GAS")
    xmax_gas = sum(axis_data_gas$x_axis_max) * 1000000
    xmax_coal = sum(axis_data_coal$x_axis_max) * 1000000
    xmin = 0
    
    coal_capacity = capacity_data %>% filter(gen_type == "COAL")
    gas_capacity = capacity_data %>% filter(gen_type == "GAS")
    max_coal = sum(coal_capacity$capacity_max) * 1000000
    max_gas = sum(gas_capacity$capacity_max) * 1000000
  }
  xmax = xmax_gas + xmax_coal
  
  # xmin = case_when (
  #   region_filter == "USA" ~ 0,
  #   region_filter == "CA" ~ 0,
  #   region_filter == "ENC" ~ 0,
  #   region_filter == "ESC" ~ 0,
  #   region_filter == "FL" ~ 0,
  #   region_filter == "MID" ~ 0,
  #   region_filter == "MTN" ~ 0,
  #   region_filter == "NEG" ~ 0,
  #   region_filter == "NY" ~ 0,
  #   region_filter == "PAC" ~ 0,
  #   region_filter == "SAC" ~ 0,
  #   region_filter == "TX" ~ 0,
  #   region_filter == "WNC" ~ 0,
  #   region_filter == "WSC" ~ 0,
  # )
  # xmax_gas = case_when (
  #   region_filter == "USA" ~ 2550e6,
  #   region_filter == "CA" ~ 73e6,
  #   region_filter == "ENC" ~ 289e6,
  #   region_filter == "ESC" ~ 218e6,
  #   region_filter == "FL" ~ 237e6,
  #   region_filter == "MID" ~ 238e6,
  #   region_filter == "MTN" ~ 230e6,
  #   region_filter == "NEG" ~ 62e6,
  #   region_filter == "NY" ~ 79e6,
  #   region_filter == "PAC" ~ 57e6,
  #   region_filter == "SAC" ~ 328e6,
  #   region_filter == "TX" ~ 373e6,
  #   region_filter == "WNC" ~ 123e6,
  #   region_filter == "WSC" ~ 201e6,
  # )
  # xmax_coal = case_when (
  #   region_filter == "USA" ~ 1900e6,
  #   region_filter == "CA" ~ 0.44e6,
  #   region_filter == "ENC" ~ 381e6,
  #   region_filter == "ESC" ~ 207e6,
  #   region_filter == "FL" ~ 89e6,
  #   region_filter == "MID" ~ 96e6,
  #   region_filter == "MTN" ~ 223e6,
  #   region_filter == "NEG" ~ 12e6,
  #   region_filter == "NY" ~ 46e6,
  #   region_filter == "PAC" ~ 0,
  #   region_filter == "SAC" ~ 266e6,
  #   region_filter == "TX" ~ 202e6,
  #   region_filter == "WNC" ~ 233e6,
  #   region_filter == "WSC" ~ 142e6,
  # )
  
  pe_gas = c()
  pe_coal = c()
  q_list = c()
  q_list_rev = c()
  for (i in seq(xmin, xmax, length = 100)) {
    if (i <= max_gas) {
      pe_gas = c(pe_gas, supply_curve_marginal_cost(i, heatrate_data, region_filter, gen_type_filter = "GAS"))
      q_list_rev = c(i, q_list_rev)
    }
  }
  
  for (i in seq(xmin, xmax, length = 100)) {
    if (i <= max_coal) {
      pe_coal = c(pe_coal, supply_curve_marginal_cost(i,heatrate_data, region_filter, gen_type_filter = "COAL"))
      q_list = c(q_list, i)
    }
  }
  
  gas_frame = data.frame(q_list_rev, pe_gas)
  colnames(gas_frame) = c("quantity", "price")
  gas_frame = gas_frame %>% mutate(fuel_type = "GAS")
  
  coal_frame = data.frame(q_list, pe_coal)
  colnames(coal_frame) = c("quantity", "price")
  coal_frame = coal_frame %>% mutate(fuel_type = "COAL")

  if (max_gas <= xmax) {
    gas_frame = gas_frame %>% mutate(quantity = quantity + (xmax - max_gas))
  }
  
  # if (xmax_coal > xmax_gas) {
  #   gas_frame = gas_frame %>% mutate(quantity = quantity + (xmax_coal - xmax_gas))
  # } else {
  #   w = 4
  # }
  # 
  full = rbind(gas_frame, coal_frame)
  return(full)
}

plot_supply_curves = function(heatrate_data, capacity_data, axis_data, region_filter = "USA", return = "plot") {
  ymax = case_when (
    region_filter == "USA" ~ 40,
    region_filter == "CA" ~ 50,
    region_filter == "ENC" ~ 45,
    region_filter == "ESC" ~ 100,
    region_filter == "FL" ~ 120,
    region_filter == "MID" ~ 160,
    region_filter == "MTN" ~ 75,
    region_filter == "NEG" ~ 350,
    region_filter == "NY" ~ 50,
    region_filter == "PAC" ~ 35,
    region_filter == "SAC" ~ 85,
    region_filter == "TX" ~ 125,
    region_filter == "WNC" ~ 70,
    region_filter == "WSC" ~ 125,
  )
  
  dataframe = generate_supply_curves(heatrate_data, capacity_data, axis_lengths, region_filter)
  dataframe = dataframe %>% mutate(price = ifelse(is.nan(price),0,price))  
  if (return == "frame") {
    return(dataframe)
  }
  View(dataframe)
  p = dataframe %>%
    ggplot() +
    geom_line(aes(x = (quantity / 10^6), # convert from generation (TWh) -> capacity (TW)
                  y = price, color = fuel_type), size = 1.3) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0,0.05))) +
    scale_x_continuous(limits = c(0,NA), expand = expansion(mult = c(0,0.05))) +
    scale_color_manual(values = colors2, name = "Generation") + 
    labs(x = "Total Capacity (GW)", y = "Price ($/MWh)", title = region_filter) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),axis.ticks = element_blank(),
          axis.line.x = element_line(size = 0.5, color = "black", linetype=1),
          axis.line.y = element_line(size = 0.5, color = "black", linetype=1), 
          axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
          #plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
          plot.title = element_blank())
  return(p)
}

plot_supply_curves_multi = function(ratio_frame, point_frame, region_filter = "USA") {
  point_frame = point_frame %>% filter(price != -1, quantity != -1)
  p = ggplot() +
    geom_line(data = ratio_frame, aes(x = (quantity / 10^6), # convert from generation (TWh) -> capacity (TW)
                  y = price, color = fuel_type, alpha = ratio), size = 1.3) +
    geom_point(data = point_frame, aes(x = quantity, y = price), fill = "black", color = "black", size = 2) + 
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0,0.05))) +
    scale_x_continuous(limits = c(0,NA), expand = expansion(mult = c(0,0.05))) +
    scale_color_manual(values = colors2, name = "Generation") + 
    scale_alpha_manual(values = c(0.25,0.6,1,0.6,0.25)) +
    labs(x = "Total Capacity (GW)", y = "Price ($/MWh)", title = region_filter) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),axis.ticks = element_blank(),
          axis.line.x = element_line(size = 0.5, color = "black", linetype=1),
          axis.line.y = element_line(size = 0.5, color = "black", linetype=1), 
          axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
          #plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
          plot.title = element_blank(), legend.position = "None")
  return(p)
}

plot_supply_curves_facet = function(combined_frame, rows) {
  p = combined_frame %>% ggplot() +
    geom_line(aes(x = (quantity / 10^6), # convert from generation (TWh) -> capacity (TW)
                  y = price, color = fuel_type), size = 0.6) +
    facet_wrap(~region, nrow = rows) +
    scale_y_continuous(limits = c(0,NA), n.breaks = 3, expand = expansion(mult = c(0,0.05))) +
    scale_x_continuous(limits = c(0,NA), n.breaks = 3, expand = expansion(mult = c(0,0.05))) +
    scale_color_manual(values = colors2, name = "Generation") + 
    labs(x = "Total Capacity (GW)", y = "Price ($/MWh)") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),axis.ticks = element_blank(),
          axis.line.x = element_line(size = 0.5, color = "black", linetype=1),
          axis.line.y = element_line(size = 0.5, color = "black", linetype=1), 
          axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
          #plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
          plot.title = element_blank(), strip.text.x = element_text(size = 8))
  return(p)
}
#p = plot_supply_curves(meede_data, capacities, axis_lengths, region_filter = "ESC")
#print(p)
#ggsave("C:/Users/sweisberg/Research Triangle Institute/Henry, Candise - MEEDE Present/Figures_1112/supply_stepwise_usa.png", width = 6.67, height = 4, units = "in")


plot_supply_curve_tech = function(dataframe, region_filter = "USA") {
  dataframe = supply_curve_all_gen(dataframe, region_filter)
  
  p = dataframe %>% ggplot() +
    geom_point(aes(x = rolling_NP / 10^3, y = rolling_avg_cost, color = gen_type)) +
    #geom_line(aes(x = rolling_NP / 10^3, y = rolling_avg_cost, color = gen_type), size = 1.2) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0.005,0.05))) +
    scale_x_continuous(limits = c(0,NA), expand = expansion(mult = c(0,0.05))) +
    #scale_color_manual(values = colors2, name = "Generation") + 
    scale_color_brewer(type = "qual", palette = "Accent", ) +
    labs(x = "Total Capacity (GW)", y = "Price ($/MWh)", title = region_filter) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),axis.ticks = element_blank(),
          axis.line.x = element_line(size = 0.5, color = "black", linetype=1),
          axis.line.y = element_line(size = 0.5, color = "black", linetype=1), 
          axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
          #plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
          plot.title = element_blank())
  
  return(p)
}

intersection = function(dataframe, mode = "simple") {
  quantities = sort(unique(dataframe$quantity))
  
  smaller = "none"
  smaller_prev = "none"
  q_prev = "none"
  gas_price_prev = "none"
  coal_price_prev = "none"
  for (q in quantities) {
    temp = dataframe %>% filter(quantity == q)
    gas_frame = temp %>% filter(fuel_type == "GAS")
    coal_frame = temp %>% filter(fuel_type == "COAL")
    
    if(nrow(gas_frame) == 0 | nrow(coal_frame) == 0) {
      return(list("price" = -1, "quantity" = -1))
    }
    
    gas_price = gas_frame$price
    coal_price = coal_frame$price
    
    if (gas_price <= coal_price) {
      smaller = "gas"
    } else {
      smaller = "coal"
    }
    
    if (smaller != "none" & smaller_prev != "none" & smaller != smaller_prev) {
      if (mode == "simple") {
        avg_q = mean(c(q, q_prev))
        avg_p = mean(c(coal_price, gas_price, coal_price_prev, gas_price_prev))
        outputs = list(
          'price' = avg_p,
          'quantity' = avg_q / 1000000
        )
      } else {
        m_gas = (gas_price - gas_price_prev) / (q - q_prev)
        m_coal = (coal_price - coal_price_prev) / (q - q_prev)
        x = (m_gas * q - gas_price - m_coal * q + coal_price) / (m_gas - m_coal)
        y = m_gas * x - m_gas * q + gas_price
        outputs = list(
          'price' = y,
          'quantity' = x / 1000000
        )
      }
      return(outputs)
    }
    
    gas_price_prev = gas_price
    coal_price_prev = coal_price
    smaller_prev = smaller
    q_prev = q
  }
  return(list('price' = -1, 'quantity' = -1))
}

price_shock_elasticity = function(intersections) {
  baseline = intersections %>% filter(ratio == "1")
  endpoints = intersections %>% filter(ratio != "1", price != -1, quantity != -1)
  
  p_start = baseline$price
  q_start = baseline$quantity
  
  elasticities = c()
  for (i in 1:nrow(endpoints)) {
    p_end = endpoints[i,]$price
    q_end = endpoints[i,]$quantity
    delta_p = (p_end - p_start) / p_start
    delta_q = (q_end - q_start) / q_start
    e = delta_q / delta_p
    elasticities = c(elasticities, e)
  }
  print(elasticities)
  avg = mean(elasticities[elasticities >= 0.01 & elasticities <= 100 & !(is.na(elasticities)) & !(is.infinite(elasticities)) & !(is.nan(elasticities))])
  outputs = list(
    'mean' = avg,
    'elasticities' = data.frame(elasticities,endpoints$ratio)
  )
  return(outputs)
}

#p = plot_supply_curve_tech(meede_data, region_filter = "NEG")
#print(p)

## To-do: 
#finish heatrate function to include USA
# better plots
# Deal with the constant operation assumption?

setwd(wd)

# 4 largest states in 2x2 grid, rest in 5x5 grid, overlay elasticities
# bar chart showing individual elasticities for each state
# Process outputs from PE model under different fuel-price scenarios

# Inputs: generation data by plant type and region from PE

# Outputs: 
# 1) Comparison of current simulation vs baseline in terms of generation mix
# 2) Distribution of generation mix over many simulations <- think I'll do this separately in a bash script probably

library(dplyr)
library(ggplot2)
library(lubridate)

wd = getwd()
setwd('C:/Users/sweisberg/OneDrive - Research Triangle Institute/Documents/EMA/EMA/Model/Results')

colors = c(
  "gray", # black
  "#8b0000", # dark red
  "#008b8b", # green
  "#000077", # dark blue
  "#addde6" # light blue
)

colors2 = c(
  "#000077", # dark blue
  "#addde6" # light blue
)

# Read in generation (and wholesale cost too, just in case) for the baseline case
generation_baseline = read.csv("PE_output_generation_baseline.csv", header = T, stringsAsFactors = F)
#wholesale_baseline = read.csv("PE_output_wholesaleprice_baseline.csv", header = T, stringsAsFactors = F)

# Read in generation (and wholesale cost too, just in case) for the simulated case
generation_sim = read.csv("PE_output_generation.csv", header = T, stringsAsFactors = F)
#wholesale_sim = read.csv("PE_output_wholesaleprice.csv", header = T, stringsAsFactors = F)

# Read in csv with mapping towards generation types
fuel_map = read.csv("fuel_map.csv", header = T, stringsAsFactors = F)

# Read in csv with the fuel price ratio
ratio = read.csv("shock_ratio.csv", header = T, stringsAsFactors = F)[1,1]

# Roll up generation, sim data and merge with map file
generation_baseline = generation_baseline %>% 
  left_join(fuel_map, by = "unit") %>%
  group_by(region, gen_type) %>% summarize(value = sum(value)) %>%
  mutate(case = "Baseline")

generation_sim = generation_sim %>%
  left_join(fuel_map, by = "unit") %>%
  group_by(region, gen_type) %>% summarize(value = sum(value)) %>%
  mutate(case = "Shocked")

# Combine datasets for plotting
generation_full = rbind(generation_baseline, generation_sim)

# plot_type = "stacked" or "clustered"
# region = "USA" or "CA","ENC","ESC","FL","MID","MTN","NEG","NY","PAC","SAC","TX","WNC","WSC"
fuel_shock_comparison = function(dataframe, ratio, plot_type = "stacked", region_filter = "USA") {
  if (region_filter == "USA") {
    dataframe = dataframe %>% group_by(gen_type, case) %>% summarize(value = sum(value)) %>%
      mutate(region = "USA")
  } else {
    dataframe = dataframe %>% filter(region == region_filter)
  }
  
  y_gap = max(dataframe$value) * 0.025
  ratio_print = 100 * (ratio - 1)
  ratio_sign = ifelse(ratio >= 1, "+","")
  
  if (plot_type == "stacked") {
    dataframe = dataframe %>% mutate(gen_type = factor(gen_type, levels = c("OTHER", "NUCLEAR","RENEW","COAL","GAS")))
    p = dataframe %>%
      ggplot() +
      geom_col(aes(x = case, y = value, fill = gen_type), position = "stack") + 
      geom_text(aes(x = case, y = value, label = round(value,0), group = gen_type), position = position_stack(vjust = 0.5), color = "white", fontface = "bold") +
      scale_y_continuous(expand = expansion(mult = c(0,0.1))) +
      scale_fill_manual(values = colors, name = "Generation") +
      labs(x = "Scenario", title = paste("", ratio_sign, ratio_print, "% Gas Price Shock, ", region_filter, sep = "")) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),axis.ticks = element_blank(),
            axis.line.x = element_line(size = 0.5, color = "black", linetype=1),
            axis.line.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
            axis.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "None")
  } else if (plot_type == "clustered") {
    dataframe = dataframe %>% mutate(gen_type = factor(gen_type, levels = c("GAS","COAL","RENEW","NUCLEAR","OTHER")))
    p = dataframe %>%
      ggplot() + 
      geom_col(aes(x = gen_type, y = value, fill = case), position = "dodge") +
      geom_text(aes(x = gen_type, y = value + y_gap, label = round(value,0), group = case), position = position_dodge(width = 0.8), color = "black", fontface = "bold") +
      scale_y_continuous(expand = expansion(mult = c(0,0.1))) +
      scale_fill_manual(values = colors2, name = "Scenario") +
      labs(x = "Generation Mix", title = paste("", ratio_sign, ratio_print, "% Gas Price Shock, ", region_filter, sep = "")) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),axis.ticks = element_blank(),
          axis.line.x = element_line(size = 0.5, color = "black", linetype=1),
          axis.line.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
          axis.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "None")
  }
  
  return(p)
}

p = fuel_shock_comparison(generation_full, 1.18, plot_type = "stacked", region_filter = "USA")
print(p)
ggsave("C:/Users/sweisberg/Research Triangle Institute/Henry, Candise - MEEDE Present/Figures_1112/gas_shock_stack_high.png", width = 3.25, height = 4, units = "in")



setwd(wd)

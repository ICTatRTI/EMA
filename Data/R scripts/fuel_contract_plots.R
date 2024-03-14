# Graphs for EIA 923 Fuel Price data

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

wd = getwd()
setwd('C:/Users/sweisberg/OneDrive - Research Triangle Institute/Documents/EMA/EMA/Data/R Scripts')

colors = c(
  "#363537", # blackish
  "#DCED31", # reddish
  "#ED7D3A", # orangeish
  "blue"
)

# read and format purchase data
dta = read.csv("./sources/eia_923_compiled.csv", header = T, stringsAsFactors = F) %>%
  select(YEAR, MONTH, Plant.Id, Plant.Name, Plant.State, Purchase.Type, Contract.Expiration.Date, 
         FUEL_GROUP, QUANTITY, Average.Heat.Content, FUEL_COST, Operator.Name, Operator.Id) %>%
  filter(Purchase.Type %in% c("C","NC","S"), FUEL_GROUP %in% c("Coal", "Natural Gas"), FUEL_COST != ".") %>%
  mutate(FUEL_COST = as.numeric(FUEL_COST) / 100, # convert from c/MMBtu -> $/MMBtu
         Purchase.Type = ifelse(Purchase.Type == "C", "Contract",
                                ifelse(Purchase.Type == "NC", "New Contract", 
                                       ifelse(Purchase.Type == "S", "Spot Purchase","-"))),
         #YEAR = as.factor(YEAR), 
         MONTH = as.factor(MONTH), Plant.Id = as.character(Plant.Id)
         )

tbl = table(dta$YEAR, dta$MONTH, dta$Purchase.Type)
counts = c(mean(tbl[,,1]), mean(tbl[,,2]), mean(tbl[,,3]))
counts = data.frame(counts, c("Contract", "New Contract", "Spot Purchase"))
colnames(counts) = c("count", "series")

contract_breakdown_plot = function(counts) {
  p = counts %>% ggplot() +
    geom_col(aes(x = 1, y = count, fill = factor(series,levels = c("New Contract", "Contract", "Spot Purchase"))), position = "stack", width = 0.5) +
    scale_y_continuous(expand = expansion(mult = c(0,0.1))) +
    scale_fill_manual(breaks = c("Contract", "Henry Hub", "New Contract", "Spot Purchase"),values = colors, name = "Purchase Type") + 
    labs(x = "Average", y = "Contracts") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),axis.ticks = element_blank(),
          axis.line.x = element_line(size = 0.5, color = "black", linetype=1),
          axis.line.y = element_blank(), axis.text.y = element_blank(),
          axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "None")
  return(p)
}
p = contract_breakdown_plot(counts)
print(p)
ggsave("C:/Users/sweisberg/Research Triangle Institute/Henry, Candise - MEEDE Present/Figures_1112/contract_breakdown.png", width = 1, height = 4, units = "in")

dta_gen = read.csv("./sources/eia_923_generation.csv", header = T, stringsAsFactors = F) %>%
  select(YEAR, Plant.Id, Plant.Name, Plant.State, Reported.Prime.Mover, Generator.Id,
         Net.Generation.January, Net.Generation.February, Net.Generation.March, Net.Generation.April, 
         Net.Generation.May, Net.Generation.June, Net.Generation.July, Net.Generation.August, 
         Net.Generation.September, Net.Generation.October, Net.Generation.November, Net.Generation.December) %>%
  pivot_longer(cols = starts_with("Net.Generation."), values_to = "Net.Generation",
               names_to = "MONTH", names_prefix = "Net.Generation.") %>%
  mutate(Net.Generation = ifelse(Net.Generation == ".", 0.0, Net.Generation)) %>%
    mutate(Net.Generation = as.numeric(Net.Generation)) %>%
    mutate(Plant.Id = as.character(Plant.Id), MONTH = case_when(
    MONTH == "January" ~ 1,
    MONTH == "February" ~ 2,
    MONTH == "March" ~ 3,
    MONTH == "April" ~ 4,
    MONTH == "May" ~ 5,
    MONTH == "June" ~ 6,
    MONTH == "July" ~ 7,
    MONTH == "August" ~ 8,
    MONTH == "September" ~ 9,
    MONTH == "October" ~ 10,
    MONTH == "November" ~ 11,
    MONTH == "December" ~ 12
  )) %>%
  mutate(MONTH = as.factor(MONTH))

dta_hh = read.csv("./sources/Henry_Hub_Natural_Gas_Spot_Price.csv", header = T, stringsAsFactors = F) %>%
  filter(YEAR >= 2013 & YEAR <= 2019) %>% 
  mutate(Plant.State = "USA", MONTH = as.factor(MONTH))

dta_fueluse = read.csv("./sources/eia_923_fueluse.csv", header = T, stringsAsFactors = F) %>%
  select(Plant.Id, Plant.Name, Plant.State, Reported.Prime.Mover, Reported.Fuel.Type.Code, AER.Fuel.Type.Code, Physical.Unit.Label, YEAR,
         Elec_MMBtu.January, Elec_MMBtu.February, Elec_MMBtu.March, Elec_MMBtu.April, 
         Elec_MMBtu.May, Elec_MMBtu.June, Elec_MMBtu.July, Elec_MMBtu.August, 
         Elec_MMBtu.September, Elec_MMBtu.October, Elec_MMBtu.November, Elec_MMBtu.December) %>%
  filter(AER.Fuel.Type.Code == "NG") %>%
  pivot_longer(cols = starts_with("Elec_MMBtu."), values_to = "Fuel.Consumed",
               names_to = "MONTH", names_prefix = "Elec_MMBtu.") %>%
  mutate(Fuel.Consumed = ifelse(Fuel.Consumed == ".", 0.0, Fuel.Consumed)) %>%
  mutate(Fuel.Consumed = as.numeric(Fuel.Consumed)) %>%
  mutate(Plant.Id = as.character(Plant.Id), MONTH = case_when(
    MONTH == "January" ~ 1,
    MONTH == "February" ~ 2,
    MONTH == "March" ~ 3,
    MONTH == "April" ~ 4,
    MONTH == "May" ~ 5,
    MONTH == "June" ~ 6,
    MONTH == "July" ~ 7,
    MONTH == "August" ~ 8,
    MONTH == "September" ~ 9,
    MONTH == "October" ~ 10,
    MONTH == "November" ~ 11,
    MONTH == "December" ~ 12
  )) %>%
  mutate(MONTH = as.factor(MONTH))


plot_923 = function(dataframe, dataframe_hh = NA, FUEL_GROUP_filter = "Natural Gas", region_filter = "USA", YEAR_filter = "All") {
  if (region_filter != "USA") {
    dataframe = dataframe %>% filter(Plant.State == region_filter, FUEL_GROUP == FUEL_GROUP_filter) %>%
      group_by(YEAR, MONTH, Plant.State, Purchase.Type, FUEL_GROUP) %>% summarize(AVERAGE_FUEL_COST = weighted.mean(FUEL_COST, QUANTITY))
  } else {
    dataframe = dataframe %>% filter(FUEL_GROUP == FUEL_GROUP_filter) %>%
      group_by(YEAR, MONTH, Purchase.Type, FUEL_GROUP) %>% summarize(AVERAGE_FUEL_COST = weighted.mean(FUEL_COST, QUANTITY))
  }
  if (FUEL_GROUP_filter == "Natural Gas") {
    dataframe = rbind(dataframe, dataframe_hh)
  } else {
    dataframe = dataframe %>% mutate(Purchase.Type = factor(Purchase.Type, levels = c("Contract", "Henry Hub", "New Contract", "Spot Purchase")))
  }
  
  #View(dataframe)
  if (YEAR_filter == "All") {
    p = dataframe %>% filter(YEAR >= 2016) %>%
      ggplot() +
      #geom_point(aes(x = MONTH, y = AVERAGE_FUEL_COST, group = Purchase.Type, color = Purchase.Type, alpha = YEAR), size = 2.25) +
      #geom_line(aes(x = MONTH, y = AVERAGE_FUEL_COST, group = interaction(Purchase.Type,YEAR), color = Purchase.Type, alpha = YEAR), size = 1.0) + 
      geom_line(aes(x = MONTH, y = AVERAGE_FUEL_COST, group = interaction(Purchase.Type,YEAR), color = Purchase.Type), size = 1.0) + 
      facet_wrap(~YEAR, ncol = 2, strip.position = "top") +
      scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0,0.05))) +
      scale_x_discrete(expand = expansion(mult = c(0,0.05))) +
      scale_color_manual(breaks = c("Contract", "Henry Hub", "New Contract", "Spot Purchase"),values = colors, name = "Purchase Type") + 
      labs(y = "Average Fuel Price ($/MMBtu)", x = "Month") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),axis.ticks = element_blank(),
            axis.line.x = element_line(size = 0.5, color = "black", linetype=1),
            axis.line.y = element_line(size = 0.5, color = "black", linetype=1), 
            axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
            plot.title = element_blank(), legend.position = "None",
            strip.background = element_blank(),strip.placement = "outside", 
            strip.text = element_text(face = "bold", size = 11), strip.switch.pad.wrap = unit(0.2, "cm"))
    
  } else {
    p = dataframe %>% filter(YEAR == YEAR_filter) %>%
      ggplot() +
      #geom_point(aes(x = MONTH, y = AVERAGE_FUEL_COST, group = Purchase.Type, color = Purchase.Type), size = 2.25) +
      #geom_line(aes(x = MONTH, y = AVERAGE_FUEL_COST, group = interaction(Purchase.Type,YEAR), color = Purchase.Type, alpha = YEAR), size = 1.0) + 
      geom_line(aes(x = MONTH, y = AVERAGE_FUEL_COST, group = Purchase.Type, color = Purchase.Type), size = 1.0) + 
      #facet_wrap(~YEAR, ncol = 1, strip.position = "right") +
      scale_y_continuous(limits = c(0,10), expand = expansion(mult = c(0,0.05))) +
      scale_x_discrete(expand = expansion(mult = c(0,0.05))) +
      scale_color_manual(values = colors, name = "Purchase Type") + 
      labs(y = "Average Fuel Price ($/MMBtu)", x = "Month") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),axis.ticks = element_blank(),
            axis.line.x = element_line(size = 0.5, color = "black", linetype=1),
            axis.line.y = element_line(size = 0.5, color = "black", linetype=1), 
            axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
            plot.title = element_blank(), legend.position = "None")
  }
    
  return(p)
}


p = plot_923(dta, dta_hh, "Natural Gas", "USA", "All")
print(p)
ggsave("C:/Users/sweisberg/Research Triangle Institute/Henry, Candise - MEEDE Present/Figures_1112/923_panel_gas.png", width = 6.67, height = 4, units = "in")

for (y in seq(2013, 2019)) {
  p = plot_923(dta, dta_hh, "Natural Gas", "USA", y)
  print(p)
  ggsave(paste("C:/Users/sweisberg/Research Triangle Institute/Henry, Candise - MEEDE Present/Figures_1112/923_individual_gas", y, ".png", sep = ""), width = 6.67, height = 4, units = "in")
}

generation_scatter = function(price_data, gen_data, fueluse_data, threshold = 80, mode = 2) {
  # ID which plants ONLY run on gas

  # create a frame with all combinations of plant, year, month
  plants = unique(price_data$Plant.Id)
  months = seq(1,12)
  years = seq(2013,2019)
  plants_full = sort(rep(plants, length(months) * length(years)))
  months_full = rep(months, length(years) * length(plants))
  years_full = rep(sort(rep(years,length(months))),length(plants))
  combos = data.frame(plants_full, months_full, years_full)
  colnames(combos) = c("Plant.Id", "MONTH", "YEAR")
  combos = combos %>% mutate(zeros = 0) %>% 
    mutate(MONTH = as.factor(MONTH), YEAR = as.factor(YEAR))
  
  # 
  temp = data.frame(table(price_data %>% select(Plant.Id, YEAR, MONTH, FUEL_GROUP))) %>% mutate(Plant.Id = as.character(Plant.Id))
  temp_coal = temp %>% filter(FUEL_GROUP == "Coal") %>% rename("Coal" = "Freq") %>% select(-FUEL_GROUP)
  temp_gas = temp %>% filter(FUEL_GROUP == "Natural Gas") %>% mutate(FUEL_GROUP = "Gas") %>% rename("Gas" = "Freq") %>% select(-FUEL_GROUP)
  temp_merged = temp_coal %>% full_join(temp_gas, by = c("Plant.Id", "YEAR", "MONTH")) %>% full_join(combos, by = c("Plant.Id", "YEAR", "MONTH")) %>%
    mutate(gas_only = ifelse(Gas > 0 & Coal == 0, 1, 0)) %>%
    group_by(Plant.Id) %>% summarize(gas_only = sum(gas_only)) %>%
    filter(gas_only >= threshold)
  
  # finally, yikes!
  gas_only_plants = unique(temp_merged$Plant.Id)
  print(length(gas_only_plants))

  # group price data by plant and filter out coal
  price_data = price_data %>% filter(FUEL_GROUP == "Natural Gas", Purchase.Type %in% c("Contract", "Spot Purchase"), Plant.Id %in% gas_only_plants) %>%
    group_by(Plant.Id, YEAR, MONTH) %>% summarize(QUANTITY = sum(QUANTITY))
  
  # group generation data by plant
  gen_data = gen_data %>% filter(Plant.Id %in% gas_only_plants) %>%
    group_by(Plant.Id, YEAR, MONTH) %>% summarize(GENERATION = sum(Net.Generation))
  
  fueluse_data = fueluse_data %>% filter(Plant.Id %in% gas_only_plants) %>%
    group_by(Plant.Id, YEAR, MONTH) %>% summarize(CONSUMPTION = sum(Fuel.Consumed))
  
  # merge data
  data_merged = price_data %>% 
    left_join(gen_data, by = c("Plant.Id", "YEAR", "MONTH")) %>% 
    left_join(fueluse_data, by = c("Plant.Id", "YEAR", "MONTH")) %>%
    mutate(QUANTITY = QUANTITY * 1.037) # convert to btu
  #View(data_merged)
  
  if (mode == 1) {
    p = data_merged %>% mutate(yr = ifelse(YEAR > 2014,1,0)) %>% #filter(YEAR > 2012) %>%
      ggplot() +
      geom_point(aes(x = QUANTITY, y = GENERATION / 10^6, color = as.factor(yr))) +
      scale_y_continuous(limits = c(0, 2.5), expand = expansion(mult = c(0,0.0))) +
      scale_x_continuous(limits = c(0,15000000), expand = expansion(mult = c(0,0.1))) +
      scale_color_brewer(palette = "Set1") +
      labs(x = "Monthly Fuel Purchased (MMBtu)", y = "Monthly Generation (TWh)") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),axis.ticks = element_blank(),
            axis.line.x = element_line(size = 0.5, color = "black", linetype=1),
            axis.line.y = element_line(size = 0.5, color = "black", linetype=1), 
            axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
            plot.title = element_blank(), legend.position = "None")
  } else if (mode == 2) {
    p = data_merged %>% mutate(yr = ifelse(YEAR > 2014,1,0)) %>% #filter(YEAR > 2012) %>%
      ggplot() +
      geom_point(aes(x = QUANTITY, y = CONSUMPTION, color = as.factor(yr))) +
      scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0,0.1))) +
      scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0,0.1))) +
      scale_color_brewer(palette = "Set1") +
      labs(x = "Monthly Fuel Purchased (MMBtu)", y = "Monthly Fuel Consumption (MMBtu)") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),axis.ticks = element_blank(),
            axis.line.x = element_line(size = 0.5, color = "black", linetype=1),
            axis.line.y = element_line(size = 0.5, color = "black", linetype=1), 
            axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
            plot.title = element_blank(), legend.position = "None")
  }
  return(p)
}

# plot fuel purchased vs fuel consumed

p = generation_scatter(dta, dta_gen, dta_fueluse, mode = 1)
print(p)
ggsave("C:/Users/sweisberg/Research Triangle Institute/Henry, Candise - MEEDE Present/Figures_1112/quantity_vs_gen.png", width = 4, height = 4, units = "in")

setwd(wd)
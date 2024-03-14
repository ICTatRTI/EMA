library(dplyr)
library(tidyr)
library(patchwork)
library(ggplot2)
library(stringr)

wd = getwd()
setwd('C:/Users/sweisberg/OneDrive - Research Triangle Institute/Documents/EMA/EMA/Model/Results')

capacity = read.csv("PE_output_capacity.csv", header = T, stringsAsFactors = F) # Units: GW
#demandreg = read.csv("EMA_output_demandreg.csv", header = T, stringsAsFactors = F)
#demandseg = read.csv("EMA_output_demandseg.csv", header = T, stringsAsFactors = F)
#emissions = read.csv("EMA_output_emissions.csv", header = T, stringsAsFactors = F)
#generation = read.csv("EMA_output_generation.csv", header = T, stringsAsFactors = F)
#generation = read.csv("EMA_output_generation_OG.csv", header = T, stringsAsFactors = F)
generation = read.csv("PE_output_generation.csv", header = T, stringsAsFactors = F) # Units: TWh
#generation = read.csv("EMA_output_generation_carbtax20.csv", header = T, stringsAsFactors = F)
fomcost = read.csv("PE_output_fomcost.csv", header = T, stringsAsFactors = F) # Units: $ per kW-yr
vomcost = read.csv("PE_output_vomcost.csv", header = T, stringsAsFactors = F) # Units: $ per MWh
#colnames(generation) = c("region","unit","load_segment","vintage","time","value")
#transmission = read.csv("EMA_output_transmission.csv", header = T, stringsAsFactors = F)
wholesale = read.csv("PE_output_wholesaleprice.csv", header = T, stringsAsFactors = F)

# state-level results
capacity_STATE = read.csv("PE_output_capacity_STATE.csv", header = T, stringsAsFactors = F) # Units: GW
generation_STATE = read.csv("PE_output_generation_STATE.csv", header = T, stringsAsFactors = F) # Units: TWh
fomcost_STATE = read.csv("PE_output_fomcost_STATE.csv", header = T, stringsAsFactors = F) # Units: $ per kW-yr
vomcost_STATE = read.csv("PE_output_vomcost_STATE.csv", header = T, stringsAsFactors = F) # Units: $ per MWh
wholesale_STATE = read.csv("PE_output_wholesaleprice_STATE.csv", header = T, stringsAsFactors = F)

plant_map = read.csv("new_existing.csv", header = T, stringsAsFactors = F)
region_map = read.csv("region_map.csv", header = T, stringsAsFactors = F)

MEEDE_df = read.csv("../../Data/updated_data/2020_data_comparison_MEEDE.csv", header = T, stringsAsFactors = F)
MEEDE_df_STATE = read.csv("../../Data/updated_data/2020_data_comparison_MEEDE_STATE.csv", header = T, stringsAsFactors = F)

## generation
# roll up vintage
generation = generation %>% group_by(region, unit, load_segment, time) %>% summarize(value = sum(value)) %>%
  left_join(plant_map, by = "unit") %>%
  select(region, unit, status, plant_type, renewable, load_segment, time, value)

# same for state data
generation_STATE = generation_STATE %>% group_by(region, unit, load_segment, time) %>% summarize(value = sum(value)) %>%
  left_join(plant_map, by = "unit") %>%
  select(region, unit, status, plant_type, renewable, load_segment, time, value)

capacity = capacity %>% group_by(region, unit, time) %>% summarize(value = sum(value)) %>%
  left_join(plant_map, by = "unit") %>%
  select(region, unit, status, plant_type, renewable, time, value)

# same for state data
capacity_STATE = capacity_STATE %>% group_by(region, unit, time) %>% summarize(value = sum(value)) %>%
  left_join(plant_map, by = "unit") %>%
  select(region, unit, status, plant_type, renewable, time, value)

wholesale = wholesale %>% filter(load_segment == "avg")#, time == 2020)

wholesale_STATE = wholesale_STATE %>% filter(load_segment == "avg")

# Functions to calculate costs
calc_cost = function(gen_data, cap_data, fom_data, vom_data, wholesale_data) {
  # merge with costs
  var_data = gen_data %>% 
    group_by(region, unit, status, plant_type, renewable, time) %>% # roll up load_segment data
    summarize(value = sum(value)) %>%
    left_join(vom_data, by = c("region","unit"))
  fixed_data = cap_data %>% left_join(fom_data, by = c("region", "unit"))
  
  # organization
  colnames(var_data) = c("region", "unit", "status","plant_type","renewable",
                         "time","generation","vomcost")
  colnames(fixed_data) = c("region","unit","status","plant_type","renewable",
                           "time","capacity","fomcost")
  
  # calculate costs ($million)
  var_data = var_data %>% mutate(variable_cost = generation * vomcost)
  fixed_data = fixed_data %>% mutate(fixed_cost = capacity * fomcost)
  
  cost_data = var_data %>% inner_join(fixed_data, by = c("region", "unit", "status", "plant_type", "renewable", "time")) %>%
    mutate(total_cost = variable_cost + fixed_cost) %>%
    left_join(
      select(wholesale_data,c(region,year,value)),
             by = c("region" = "region","time" = "year")
    ) %>%
    ungroup()
  colnames(cost_data)[length(colnames(cost_data))] = "wholesale_cost"
  
  return(cost_data)
}

combined_df = calc_cost(generation, capacity, fomcost, vomcost, wholesale)
combined_df = combined_df %>% filter(time == 2020, status == "existing")

combined_df_STATE = calc_cost(generation_STATE, capacity_STATE, fomcost_STATE, vomcost_STATE, wholesale_STATE)
combined_df_STATE = combined_df_STATE %>% filter(time == 2020, status == "existing")

sage_region_rollup = function(dataframe, region_map) {
  data_merged = dataframe %>% 
    left_join(region_map,by = c("region")) %>%
    group_by(sage_region, plant_type, status, renewable, time) %>%
    summarize(generation = sum(generation),
              capacity = sum(capacity),
              variable_cost = sum(variable_cost),
              fixed_cost = sum(fixed_cost),
              total_cost = sum(total_cost),
              wholesale_cost = mean(wholesale_cost))
  colnames(data_merged)[1] = "region"
  return(data_merged) 
}

usa_region_rollup = function(dataframe) {
  data_merged = dataframe %>%
    group_by(plant_type, status, renewable, time) %>%
    summarize(generation = sum(generation),
              capacity = sum(capacity),
              variable_cost = sum(variable_cost),
              fixed_cost = sum(fixed_cost),
              total_cost = sum(total_cost),
              wholesale_cost = mean(wholesale_cost)) %>%
    mutate(region = "USA")
  return(data_merged) 
}

#test = usa_region_rollup(combined_df)
#test2 = sage_region_rollup(combined_df, region_map)
#test = generation_table(combined_df, rows = "region", columns = "plant_type", year = "All")


# build a table from the generation data
# build a list of rows from c("region","unit","plant_type","status","time", "load_segment", "renewable")
# choose one column from the above list
# optionally filter the data by year or use "All" to keep all data
generation_table = function(gen_data, rows = c("region"), columns = "plant_type", cell_value = "generation", year = "All") {
  # determine which fields to group by and which to drop
  fields = colnames(gen_data)
  include = c()
  for (f in fields[1:length(fields) - 1]) {
    if ((columns == f) | (f %in% rows)) {
      include = c(include,f)
    }
  }
  if (year != "All" & !("time" %in% include)) {
    include = c(include,"time")
  }
  
  # group by the chosen variables and aggregate
  dta = gen_data %>% group_by_at(vars(one_of(include))) %>% summarize_(.dots = paste("sum(",cell_value,", na.rm = T)",sep = ""))
  colnames(dta)[length(colnames(dta))] = "value"
  
  # filter by year if necessary
  if (year != "All") {
    dta = dta %>% filter(time == year)
    if ((columns != "time") & !("time" %in% rows)) {
      dta = dta %>% select(-time)
    }
  }
  
  # reformat data into wide format
  dta = dta %>% 
   pivot_wider(names_from = columns, values_from = value)
  
  return(dta)
}

# create a barplot from the generation data
generation_barplot = function(gen_data, x_var = "plant_type", year_filter = "All", region_filter = "All", plant_type_filter = "All", load_segment_filter = "All", ylim = "None") {
  if (year_filter == "All" & x_var != "time") {
    print("WARNING: data will be aggregated across years. Consider filtering by year or setting the x_variable to be 'time'")
  }
  
  dta = gen_data
  
  # filter data 
  if (year_filter != "All") {
    dta = dta %>% filter(time == year_filter)
  } 
  if (region_filter != "All") {
    dta = dta %>% filter(region == region_filter)
  }
  if (plant_type_filter != "All") {
    dta = dta %>% filter(plant_type == plant_type_filter)
  } 
  if (load_segment_filter != "All") {
    dta = dta %>% filter(load_segment == load_segment_filter)
  }
  
  # group data
  dta = dta %>% group_by_at(vars(one_of(x_var))) %>% summarize(value = sum(value))
  
  # fomatting
  ymax = max(dta$value)
  text_gap = 0.03 * ymax
  expansion_gap = 0.085 * ymax
  
  if (ylim != "None") {
    scaley_object = scale_y_continuous(limits = c(0,ylim),expand = expansion(add = c(0,expansion_gap)))
  } else {
    scaley_object = scale_y_continuous(expand = expansion(add = c(0,expansion_gap)))
  }
  
  # Formatting for x-axis
  x_labels = list("plant_type" = "Plant Type",
                  "time" = "Year",
                  "region" = "Region",
                  "load_segment" = "Load Segment",
                  "renewable" = "Renewable")
  
  if (x_var != "time") {
    col_object = geom_col(aes_string(x = paste("reorder(",x_var,",-value)",sep = ""), y = "value"),fill = "#bcdfeb")
    text_object = geom_text(aes_string(x = paste("reorder(",x_var,",-value)",sep = ""), y = paste("value+",text_gap,sep = ""),label = "round(value,0)"),size = 3.5)
  } else {
    col_object = geom_col(aes_string(x = x_var, y = "value"),fill = "#bcdfeb")
    text_object = geom_text(aes_string(x = x_var, y = paste("value+",text_gap,sep = ""),label = "round(value,0)"),size = 3.5)
  }
  
  # make the plot
  p = dta %>%
    ggplot() + 
   # reorder(player_of_match, -most_award)
    #geom_col(aes_string(x = paste("reorder(",x_var,",-value)",sep = ""), y = "value"),fill = "#bcdfeb") +
    #geom_text(aes_string(x = paste("reorder(",x_var,",-value)",sep = ""), y = paste("value+",text_gap,sep = ""),label = "round(value,0)"),size = 3.5) +
    col_object +
    text_object +
    scaley_object +
    labs(x = x_labels[x_var], title = paste("Electricity Generation (TWh)-Year: ",year_filter,", Region: ", region_filter, ", Plant Type: ", plant_type_filter, ", Load Segment: ", load_segment_filter,sep = "")) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),axis.ticks = element_blank(),
          axis.line.x = element_line(size = 0.5, colour = "black", linetype=1),
          axis.text.y = element_blank(),axis.title.y = element_blank(),
          axis.text.x = element_text(size = 10))

  
  # display the plot
  return(p)
}

# possible ys: "total generation", "percent renewable"
# possible colors: "region, plant_type, load_segment"
generation_linegraph = function(gen_data, y_var = "total", color_by = "None", region_filter = "All", plant_type_filter = "All", load_segment_filter = "All", ylim = "None", wrap_width = "None") {
  dta = gen_data
  
  # filter data 
  if (region_filter != "All") {
    dta = dta %>% filter(region == region_filter)
  }
  if (plant_type_filter != "All") {
    dta = dta %>% filter(plant_type == plant_type_filter)
  } 
  if (load_segment_filter != "All") {
    dta = dta %>% filter(load_segment == load_segment_filter)
  }
  
  # group data
  if (color_by == "None") {
    if (y_var == "total") {
      dta = dta %>% group_by(time) %>% summarize(value = sum(value))
    } else if (y_var == "percent renewable"){
      dta = dta %>% group_by(time,renewable) %>% summarize(value = sum(value)) %>% 
        mutate(pct = 100 * (value / sum(value))) %>%
        filter(renewable == "yes")
    }
  } else {
    if (y_var == "total") {
      group_vars = c("time",color_by)
      dta = dta %>% group_by_at(vars(one_of(group_vars))) %>% summarize(value = sum(value))  
    } else if (y_var == "percent renewable"){
      group_vars = c("time",color_by,"renewable")
      dta = dta %>% group_by_at(vars(one_of(group_vars))) %>% summarize(value = sum(value))  %>%
        mutate(pct = 100 * (value / sum(value))) %>%
        filter(renewable == "yes")
    }
  }
  
  if(wrap_width == "None") {
    wrap_width = 100000
  }
  
  # set up the y variable
  if (y_var == "total") {
    y_var_plot = "value"
    label_object = labs(x = "Year", y = "Generation (TWh)", title = str_wrap(paste("Electricity Generation (TWh)-Region: ", region_filter, ", Plant Type: ", plant_type_filter, ", Load Segment: ", load_segment_filter,sep = ""),wrap_width)) 
  } else if (y_var == "percent renewable") {
    y_var_plot = "pct"
    label_object = labs(x = "Year", y = "Renewable Generation (%)", title = str_wrap(paste("Renewable Electricity Generation (%)-Region: ", region_filter, ", Plant Type: ", plant_type_filter, ", Load Segment: ", load_segment_filter,sep = ""),wrap_width)) 
  }
  
  if (color_by == "None") {
    line_object = geom_line(aes_string(x = "time", y = y_var_plot), color = "black", size = 1)
  } else {
    line_object = geom_line(aes_string(x = "time", y = y_var_plot, color = color_by), size = 0.5)
  }
  
  # make the plot
  p = dta %>%
    ggplot() + 
    line_object + 
    label_object +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),axis.ticks = element_blank(),
          axis.line.x = element_line(size = 0.5, colour = "black", linetype=1),
          axis.text.x = element_text(size = 10))
  
  if (ylim != "None") {
    p = p + scale_y_continuous(limits = c(0,ylim))
  }
  
  return(p)
  
  #ymax = max(dta$value)
  #text_gap = 0.03 * ymax
  
}

comparison_bargraphs = function(MEEDE_data, EMA_data, y_var = "generation", region_filter = "CA", plant_type_filter = c("slr"), ylim = 100, grid = TRUE) {
  my_columns = c("plant_type",y_var)
  MEEDE_filtered = MEEDE_data %>% filter(region == region_filter, plant_type %in% plant_type_filter) %>%
    group_by(plant_type) %>% summarize_(.dots = paste("sum(",y_var,")",sep = "")) %>%
    #select(all_of(my_columns)) %>%
    mutate(source = "MEEDE")
  
  EMA_filtered = EMA_data %>% filter(region == region_filter, plant_type %in% plant_type_filter) %>%
    group_by(plant_type) %>% summarize_(.dots = paste("sum(",y_var,")",sep = "")) %>%
    #select(all_of(my_columns)) %>%
    mutate(source = "EMA")

  plot_data = rbind(MEEDE_filtered, EMA_filtered)
  colnames(plot_data) = c("plant_type", "value", "source")
  
  title_list = list("generation" = "Generation (TWh)",
                    "capacity" = "Capacity (GW)",
                    "variable_cost" = "Variable O&M Costs ($M)",
                    "fixed_cost" = "Fixed O&M Costs ($M)",
                    "total_cost" = "Total O&M Costs ($B)",
                    "wholesale_cost" = "Average Wholesale Price ($/MWh)")
  if (grid) {
    text_gap = 0.1 * ylim
  } else {
    text_gap = 0.03 * ylim
  }
  
  if (y_var == "total_cost") {
    text_obj = geom_text(aes(x = plant_type, y = value + text_gap, label = round(value/1000,1), fill = source),size = 3.5, position = position_dodge2(width = 0.9, preserve = "single"))
  } else {
    text_obj = geom_text(aes(x = plant_type, y = value + text_gap, label = round(value,0), fill = source),size = 3.5, position = position_dodge2(width = 0.9, preserve = "single"))
  }
  
  if (grid) {
    title_obj = labs(x = "Plant Type", title = paste("Region: ", region_filter,sep = ""))
  } else {
    title_obj = labs(x = "Plant Type", title = paste("MEEDE/PE Comparison: ", title_list[y_var], ", Region: ", region_filter,sep = ""))
  }
  
  p = plot_data %>% ggplot() +
    geom_col(aes(x = plant_type, y = value, fill = source), position = "dodge") + 
    text_obj + 
    title_obj +
    scale_y_continuous(limits = c(0,ylim),expand = expansion(add = c(0,5))) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),axis.ticks = element_blank(),
          axis.line.x = element_line(size = 0.5, colour = "black", linetype=1),
          axis.text.y = element_blank(),axis.title.y = element_blank(),
          axis.text.x = element_text(size = 10))
  
  if (grid) {
    p = p + theme(legend.position = "none")
  }
  
  return(p)
}

comparison_bargraph_wholesale = function(MEEDE_data, EMA_data, region_filter = "CA", plant_type_filter = "slr", ylim = 100, grid = TRUE) {
  MEEDE_filtered = MEEDE_data %>% filter(region == region_filter, plant_type %in% plant_type_filter)
  MEEDE_avg = mean(MEEDE_filtered$wholesale_cost, na.rm = TRUE)
  
  EMA_filtered = EMA_data %>% filter(region == region_filter, plant_type %in% plant_type_filter)
  EMA_avg = mean(EMA_filtered$wholesale_cost, na.rm = TRUE)
  
  new_df = data.frame(c("EMA","MEEDE"),c("Average","Average"),c(EMA_avg, MEEDE_avg))
  colnames(new_df) = c("source","unit_type","value")
  
  if (grid) {
    text_gap = 0.1 * ylim
  } else {
    text_gap = 0.03 * ylim
  }
  
  if (grid) {
    title_obj = labs(x = "Plant Type", title = paste("Region: ", region_filter,sep = ""))
  } else {
    title_obj = labs(x = "Plant Type", title = paste("MEEDE/PE Comparison: ","Average Wholesale Price ($/MWh)", ", Region: ", region_filter,sep = ""))
  }
  
  p = new_df %>% ggplot() +
    geom_col(aes(x = unit_type, y = value, fill = source), position = "dodge") + 
    geom_text(aes(x = unit_type, y = value + text_gap, label = round(value,0)),size = 3.5, position = position_dodge2(width = 0.9, preserve = "single")) + 
    title_obj +
    scale_y_continuous(limits = c(0,ylim),expand = expansion(add = c(0,5))) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),axis.ticks = element_blank(),
          axis.line.x = element_line(size = 0.5, colour = "black", linetype=1),
          axis.text.y = element_blank(),axis.title.y = element_blank())
  
  if (grid) {
    p = p + theme(legend.position = "none")
  }
  
  return(p)
}

#comparison_bargraph_wholesale(MEEDE_df, combined_df)
#comparison_bargraphs(MEEDE_df, combined_df, y_var = "capacity", plant_type_filter = c("slr", "wnd", "col"))

generation_report_data = function(gen_data) {
  # What do I want to see:
  # Total generation over time by region
    # possibly broken down
  # Percent renewable over time for each region and overall
  # Options to zoom in on one or more breakdowns maybe
}
#test = generation_table(generation, rows = c("region","time"), columns = "plant_type", year = "All")
#generation_barplot(generation, x_var = "renewable", year_filter = "2030", region = "PAC")
#generation_linegraph(generation, y_var = "total", color_by = "region")

#generation_rollsegment = generation %>% group_by(region, unit, time) %>% summarize(value = sum(value))

#generation %>% group_by(region, renewable) %>% summarize(value = sum(value)) %>% mutate(pct = value / sum(value))

wd = getwd()
setwd('C:/Users/sweisberg/OneDrive - Research Triangle Institute/Documents/EMA/EMA/Model/Results')

source("empirical_supply_curve.R")
source("../../Data/R scripts/sim_results_distribution.R")

regions = unique(meede_data$region)

## Empirical Supply Curves

# plot = plot_supply_curves(meede_data, capacities, axis_lengths, region_filter = "TX")
# plot = plot + plot_annotation(title = "TX", theme = theme(plot.title = element_text(face = "bold", hjust = 0.5)))
# print(plot)
# ggsave("C:/Users/sweisberg/Research Triangle Institute/Henry, Candise - MEEDE Present/Figures_1210/supply_curve_tx_state.png", width = 6.67, height = 4, units = "in")
# 
# for (r in regions) {
#   if (r != "USA") {
#     plot = plot_supply_curves(meede_data, capacities, axis_lengths, region_filter = r)
#     plot = plot + plot_annotation(title = r, theme = theme(plot.title = element_text(face = "bold", hjust = 0.5)))
#     print(plot)
#     #ggsave(paste("C:/Users/sweisberg/Research Triangle Institute/Henry, Candise - MEEDE Present/Figures_1210/supply_curve_", r, ".png",sep = ""), width = 6.67, height = 4, units = "in")
#   }
# }

exclude = c("MA", "DC", "NY","RI","VT","ID", "ME", "DE", "NH", "NJ", "CA", "WA", "SD", "CT","NV","OR","ND","NM","MS")
include_big = c("FL", "TX", "MI", "PA")
include_small = c("AL", "AR", "CO", "CT", "GA", "IA", "IL",
                  "IN", "KS", "KY", "MD", "MN",
                  "MO", "MT", "NC", "NE", "OH",
                  "OK", "SC", "TN", "VA", "WI",
                  "WV", "WY","WA")
state_frames_small = NA
for (r in regions) {
  #print(r)
  #if (r != "USA" & !(r %in% exclude)) {
  if (r %in% include_small) {
    print(r)
    temp_frame = plot_supply_curves(meede_data, capacities, axis_lengths, region_filter = r, return = "frame")
    temp_frame = temp_frame %>% mutate(region = r)
    if (is.na(state_frames_small)) {
      state_frames_small = temp_frame
    } else {
      state_frames_small = rbind(state_frames_small, temp_frame)
    }
  }
}

plot = plot_supply_curves_facet(state_frames_small,5)
print(plot)
ggsave("C:/Users/sweisberg/Research Triangle Institute/Henry, Candise - MEEDE Present/Figures_1210/supply_curve_5x5.png", width = 6.67, height = 4, units = "in")

## Empirical Supply Curves: All Technologies

# plot = plot_supply_curve_tech(meede_data, region_filter = "USA")
# plot = plot + plot_annotation(title = "USA", theme = theme(plot.title = element_text(face = "bold", hjust = 0.5)))
# print(plot)
# #ggsave("C:/Users/sweisberg/Research Triangle Institute/Henry, Candise - MEEDE Present/Figures_1210/supply_curve_tech_usa.png", width = 6.67, height = 4, units = "in")
# 
# for (r in regions) {
#   if (r != "USA") {
#     plot = plot_supply_curve_tech(meede_data, region_filter = r)
#     plot = plot + plot_annotation(title = r, theme = theme(plot.title = element_text(face = "bold", hjust = 0.5)))
#     print(plot)
#     #ggsave(paste("C:/Users/sweisberg/Research Triangle Institute/Henry, Candise - MEEDE Present/Figures_1210/supply_curve_tech_", r, ".png",sep = ""), width = 6.67, height = 4, units = "in")
#   }
# }


## Tomorrow:
# X Fix the USA graph
# X See if the other generator mapping scheme resolves the CA coal issue
# X NaNs
# X Function to pull the intersection point from two curves/dataframes
# X convert from region to state
# X 5x10ish plot of supply curves
# calculate elasticities for each state
# one for texas with the fuel shocked curves to demo elasticity calculations
# VRE plot for TX
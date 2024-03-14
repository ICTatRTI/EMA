wd = getwd()
setwd('C:/Users/sweisberg/OneDrive - Research Triangle Institute/Documents/EMA/EMA/Model/Results')

source("empirical_supply_curve.R")
#source("../../Data/R scripts/sim_results_distribution.R")

regions = unique(meede_data$region)
exclude = c("MA", "DC", "NY","RI","VT","ID", "ME", "DE", "NH", "NJ", "CA", "WA", "SD", "CT","NV","OR","ND","NM","MS")
exclude = c("AZ", "CA", "DC", "DE", "IA", "ID", "KS", "KY", "LA", "MA", "ME", "MS", "MT", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OR", "RI", "SD", "TX", "UT", "VT", "WV", "WY")

ratios_sort = sort(shock_ratios$ratio)
ratios = c(ratios_sort[10], ratios_sort[30],1,ratios_sort[70], ratios_sort[90])

e_regions = c()
elasticities = c()
points_all = NA
ind_elasticity_frame = NA
for (region in "USA") {
  if (!(region %in% exclude)) {
    print(region)
    #region = "MN" #PA
    
    full_frame = NA
    qs = c()
    ps = c()
    for (r in ratios) {
      #print(r)
      adjusted_data = adjust_fuel_px(meede_data, r)
      temp_frame = plot_supply_curves(adjusted_data, capacities, axis_lengths, region_filter = region, return = "frame")
      if (r == 1) {
        temp_frame = temp_frame %>% mutate(ratio = as.character(r))
      } else {
        temp_frame = temp_frame %>% mutate(ratio = as.character(r))
      }
      o = intersection(temp_frame, mode = "interpolation")
      #print(o)
      qs = c(qs,o$quantity)
      ps = c(ps,o$price)
      if (is.na(full_frame)) {
        full_frame = temp_frame
      } else {
        full_frame = rbind(full_frame, temp_frame) 
      }
      #plot = plot_supply_curves(adjusted_data, region_filter = "USA")
      #plot = plot + plot_annotation(title = paste("USA--ratio: ", r,sep = ""), theme = theme(plot.title = element_text(face = "bold", hjust = 0.5)))
      #print(plot)
    }
    points = data.frame(ps,qs, ratios)
    colnames(points) = c("price", "quantity", "ratio")
    points2 = points %>% mutate(state = region)
    points_all = rbind(points_all, points2)
    
    plot = plot_supply_curves_multi(full_frame, points, region_filter = region)
    plot = plot + plot_annotation(title = region, theme = theme(plot.title = element_text(face = "bold", hjust = 0.5)))
    print(plot)
    ggsave(paste("C:/Users/sweisberg/Research Triangle Institute/Henry, Candise - MEEDE Present/Figures_1210/supply_curve_shock", region, ".png",sep = ""), width = 6.67, height = 4, units = "in")
    
    #print(price_shock_elasticity(points))
    output = price_shock_elasticity(points)
    e = output$mean
    e_frame_temp = output$elasticities %>% mutate(state = region)
    colnames(e_frame_temp) = c("elasticity", "ratio", "region")
    ind_elasticity_frame = rbind(ind_elasticity_frame, e_frame_temp)
    
    e_regions = c(e_regions, region)
    elasticities = c(elasticities, e)
    
  }
}
elasticities_frame = data.frame(e_regions, elasticities)
colnames(elasticities_frame) = c("State", "Elasticity")

points_all_merged = points_all %>% left_join(ind_elasticity_frame, by = c("ratio" = "ratio", "state" = "region"))

p = points_all_merged %>% filter(!(is.na(state)), price >= 0, ratio != 1, elasticity <= 50, elasticity >= 0) %>%
  ggplot() +
  geom_col(aes(x = state, y = elasticity, fill = as.factor(ratio)), position = "dodge") +
  #scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0,0.05))) +
  #scale_x_discrete(limits = c(0,NA), expand = expansion(mult = c(0,0.05))) +
  labs(x = "State", y = "Elasticity") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.ticks = element_blank(),
        axis.line.x = element_line(size = 0.5, color = "black", linetype=1),
        axis.line.y = element_line(size = 0.5, color = "black", linetype=1),
        axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        plot.title = element_blank(), legend.position = "None")

print(p)
ggsave(paste("C:/Users/sweisberg/Research Triangle Institute/Henry, Candise - MEEDE Present/Figures_1210/elasticity_bars.png","",sep = ""), width = 6.67, height = 4, units = "in")

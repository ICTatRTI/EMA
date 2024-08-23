# Generate distribution of historical fuel prices and modify inputs to PE model to simulate fuel price shock
# Updated March, 2024

library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(eia)

#eia_set_key("qjenMV6UKSSJ2C6d45QXo4GvfYZCBOZ8eqMZUHiC")


wd = getwd()
setwd('C:/Users/sweisberg/OneDrive - Research Triangle Institute/Documents/EMA/EMA/Data/R Scripts')

MULTI = TRUE
# MOVER = "GAS"
MOVER = "OIL"
# MOVER = "COL"


# clear out directory in preparation
sapply(paste0("../updated_data/fuel_shock/PE_pf_SHOCK_",tolower(MOVER),"_", 1:1000, ".csv"), unlink)

## Read historical data
# monthly prices ($/MMbtu) for delivered gas to electricity generating plants from EIA
# STEO.CLEUDUS.M; STEO.NGEUDUS.M

hist = eia_data("steo/data/", data = "value", facets = list(seriesId = c("CLEUDUS", "DKEUDUS", "NGEUDUS")), 
                freq = "monthly", start = "2014-01", end = "2023-12")
hist = hist %>% # units: $/MMBtu
  select(period, seriesId, value) %>%
  mutate(value = as.numeric(value)) %>%
  mutate(fuel = case_when(seriesId == "NGEUDUS" ~ "gas",
                          seriesId == "DKEUDUS" ~ "oil",
                          seriesId == "CLEUDUS" ~ "coal",
                          TRUE ~ "NA")) %>%
  select(-seriesId) %>%
  mutate(my_year = year(parse_date_time(hist$period, "%Y-%m")), my_month = month(parse_date_time(hist$period, "%Y-%m"))) %>%
  filter(my_month != 2 | my_year != 2021) %>%
  pivot_wider(names_from = fuel, values_from = value) %>%
  mutate(gas_to_coal = gas / coal,
         oil_to_gas = oil / gas,
         oil_to_coal = oil / coal) %>%
  mutate(weight = 1 + ((my_year - min(my_year)) / 5) , weight = weight / sum(weight))


#hist = read.csv('sources/gas_price_historical_m.csv', header = T, stringsAsFactors = F) %>% filter
#hist = hist %>% 
#  mutate(my_year = year(mdy(hist$month))) %>%
#  filter(my_year >= 2021) %>%
#  filter(gas_price <= 15) %>%
#  mutate(weight = 1 + ((my_year - min(my_year)) / 5plot(hist$gasu, type = 'l', ylim = c(-5,5))


## Create distribution
# distribution = density(hist$gas_price)
# distribution_w = density(hist$gas_price, weights = hist$weight)
distribution_g2c = density(hist$gas_to_coal)
distribution_g2cw = density(hist$gas_to_coal, weights = hist$weight)
distribution_o2g = density(hist$oil_to_gas)
distribution_o2gw = density(hist$oil_to_gas, weights = hist$weight)
distribution_o2c = density(hist$oil_to_coal)
distribution_o2cw = density(hist$oil_to_coal, weights = hist$weight)


# plot the distribution
# plot(distribution)
# lines(distribution_w, col = 'blue')
plot(distribution_g2c)
lines(distribution_g2cw, col = 'blue')
plot(distribution_o2g)
lines(distribution_o2gw, col = 'blue')
plot(distribution_o2c)
lines(distribution_o2cw, col = 'blue')

plot(hist$gas_to_coal, type = 'l', ylim = c(0,11))
lines(hist$oil_to_gas, col = 'blue')
lines(hist$oil_to_coal, col = 'red')

# p = hist %>% ggplot() + 
#   geom_density(aes(x = gas_price), color = "#addde6", fill = "#addde6", alpha = 0.3, adjust = 1, size = 0.8) + 
#   scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0,0.1))) +
#   scale_x_continuous(limits = c(0,10), expand = expansion(mult = c(0,0.1))) +
#   labs(x = "Gas Price ($/MMBtu)", y = "Density") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(),axis.ticks = element_blank(),
#         axis.line.x = element_line(size = 0.5, color = "black", linetype=1),
#         axis.line.y = element_line(size = 0.5, color = "black", linetype=1), 
#         axis.text.x = element_text(size = 10), axis.text.y = element_blank(),
#         plot.title = element_text(hjust = 0.5))
# print(p)
# ggsave("C:/Users/sweisberg/Research Triangle Institute/Henry, Candise - MEEDE Present/Figures_1112/hist_gas_dist.png", width = 6.7, height = 4, units = "in")
# #geom_density(aes(x = value, color = gen_type, fill = gen_type), alpha = 0.2, adjust = 0.75, size = 0.8) +

# create a cumulative distribution
# cdf = cumsum(distribution$y) / cumsum(distribution$y)[length(distribution$y)] 
# cdf_w = cumsum(distribution_w$y) / cumsum(distribution_w$y)[length(distribution_w$y)] 
cdf_g2c = cumsum(distribution_g2c$y) / cumsum(distribution_g2c$y)[length(distribution_g2c$y)] 
cdf_g2cw = cumsum(distribution_g2cw$y) / cumsum(distribution_g2cw$y)[length(distribution_g2cw$y)] 
cdf_o2g = cumsum(distribution_o2g$y) / cumsum(distribution_o2g$y)[length(distribution_o2g$y)] 
cdf_o2gw = cumsum(distribution_o2gw$y) / cumsum(distribution_o2gw$y)[length(distribution_o2gw$y)] 
cdf_o2c = cumsum(distribution_o2c$y) / cumsum(distribution_o2c$y)[length(distribution_o2c$y)] 
cdf_o2cw = cumsum(distribution_o2cw$y) / cumsum(distribution_o2cw$y)[length(distribution_o2cw$y)] 




# plot the cdf
plot(cdf_g2c, type = 'l', main='Cumulative Distribution')
lines(cdf_g2cw, col = 'blue')
#plot(cdf_w, type = 'l', main='Cumulative Distribution')

# test the distribution
N_test = 1000
generation_g2c = replicate(N_test, distribution_g2c$x[findInterval(runif(1), cdf_g2c) + 1])
generation_g2cw = replicate(N_test, distribution_g2cw$x[findInterval(runif(1), cdf_g2cw) + 1])

# plot distribution vs draws
plot(distribution_g2cw)
lines(density(generation_g2cw),col='red')
legend('topright',legend=c('Density Function estimate',
                          'Generated sample density'),
       col=c("black", "red"),
       lty=c(1,1),
       cex=1)

## Draw from distribution
if (MULTI) {
  N = 100
} else {
  N = 1
}


#new_gas_price = replicate(N, distribution_w$x[findInterval(runif(1), cdf_w) + 1])
#NATIONAL_GAS_PRICE = 6.24 # from meede, previously # 3.43
#new_gas_price_ratio = new_gas_price / NATIONAL_GAS_PRICE

MEEDE_G2C = 2.37
MEEDE_O2G = 2.86
MEEDE_O2C = 6.66
new_g2c_ratio = replicate(N, distribution_g2cw$x[findInterval(runif(1), cdf_g2cw) + 1])
new_o2g_ratio = replicate(N, distribution_o2gw$x[findInterval(runif(1), cdf_o2gw) + 1])
new_o2c_ratio = replicate(N, distribution_o2cw$x[findInterval(runif(1), cdf_o2cw) + 1])
# new_g2c_ratio = new_g2c / MEEDE_G2C
# new_o2g_ratio = new_o2g / MEEDE_O2G
# new_o2c_ratio = new_o2c / MEEDE_O2C

### think more about this starting here^


## Read default fuel data
fuel_data = read.csv("../updated_data/PE_pf.csv", header = T, stringsAsFactors = F)
fuel_data_wide = fuel_data %>% pivot_wider(names_from = fuel_type, values_from = Value)
  

for (i in 1:N) {
  ## Create price ratio
  if (!MULTI) {
    print(round(new_gas_price_ratio[i],2))
  }
  
  ## Apply Price ratio
  #fuel_data_mult = fuel_data %>% mutate(Value = ifelse(fuel_type == "GAS", Value * new_gas_price_ratio[i], Value))
  #fuel_data_mult = fuel_data %>% mutate(Value = ifelse(fuel_type == "GAS", Value * new_g2c_ratio[i], Value))
  if (MOVER == "GAS") {
    fuel_data_mult = fuel_data_wide %>% mutate(GAS = COL * new_g2c_ratio[i]) %>%
      pivot_longer(cols = c("COL","GAS","OIL","bio","nuc"), names_to = "fuel_type", values_to = "Value") %>%
      select(region, fuel_type, year, Value)
  } else if (MOVER == "OIL") {
    fuel_data_mult = fuel_data_wide %>% mutate(OIL = COL * new_o2c_ratio[i]) %>%
      pivot_longer(cols = c("COL","GAS","OIL","bio","nuc"), names_to = "fuel_type", values_to = "Value") %>%
      select(region, fuel_type, year, Value)
  } else if (MOVER == "COL") {
    fuel_data_mult = fuel_data_wide %>% mutate(COL = GAS / new_g2c_ratio[i]) %>%
      pivot_longer(cols = c("COL","GAS","OIL","bio","nuc"), names_to = "fuel_type", values_to = "Value") %>%
      select(region, fuel_type, year, Value)
  }
  
  ## Output new data for PE model
  if (MULTI) {
    write.csv(fuel_data_mult, paste("../updated_data/fuel_shock/PE_pf_SHOCK_",tolower(MOVER),"_",i,".csv",sep = ""), row.names = F)
  } else {
    write.csv(fuel_data_mult, "../updated_data/PE_pf_SHOCK.csv", row.names = F)
  }
  
}
if (MOVER == "GAS") {
  ratios = as.data.frame(c(round(new_g2c_ratio, 2)))
} else if (MOVER == "OIL") {
  ratios = as.data.frame(c(round(new_o2c_ratio, 2)))
} else if (MOVER == "COL") {
  ratios = as.data.frame(c(round(1/new_g2c_ratio, 2)))
}
colnames(ratios) = c("ratio")
write.csv(ratios,paste0("../../Model/Results/shock_ratio_",tolower(MOVER),".csv"), row.names = F)


setwd(wd)
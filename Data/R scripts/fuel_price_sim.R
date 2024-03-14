# Generate distribution of historical fuel prices and modify inputs to PE model to simulate fuel price shock

library(dplyr)
library(lubridate)

wd = getwd()
setwd('C:/Users/sweisberg/OneDrive - Research Triangle Institute/Documents/EMA/EMA/Data/R Scripts')

MULTI = TRUE

# clear out directory in preparation
sapply(paste0("../updated_data/fuel_shock/PE_pf_SHOCK_", 1:1000, ".csv"), unlink)

## Read historical data
# monthly prices ($/MMbtu) for delivered gas to electricity generating plants from EIA
# STEO.CLEUDUS.M
hist = read.csv('sources/gas_price_historical_m.csv', header = T, stringsAsFactors = F) 
hist = hist %>% 
  mutate(my_year = year(mdy(hist$month))) %>%
  mutate(weight = 1 + ((my_year - min(my_year)) / 5), weight = weight/sum(weight))

## Create distribution
distribution = density(hist$gas_price)
distribution_w = density(hist$gas_price, weights = hist$weight)

# plot the distribution
plot(distribution)
lines(distribution_w, col = 'blue')

p = hist %>% ggplot() + 
  geom_density(aes(x = gas_price), color = "#addde6", fill = "#addde6", alpha = 0.3, adjust = 1, size = 0.8) + 
  scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0,0.1))) +
  scale_x_continuous(limits = c(0,10), expand = expansion(mult = c(0,0.1))) +
  labs(x = "Gas Price ($/MMBtu)", y = "Density") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.ticks = element_blank(),
        axis.line.x = element_line(size = 0.5, color = "black", linetype=1),
        axis.line.y = element_line(size = 0.5, color = "black", linetype=1), 
        axis.text.x = element_text(size = 10), axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
print(p)
ggsave("C:/Users/sweisberg/Research Triangle Institute/Henry, Candise - MEEDE Present/Figures_1112/hist_gas_dist.png", width = 6.7, height = 4, units = "in")
#geom_density(aes(x = value, color = gen_type, fill = gen_type), alpha = 0.2, adjust = 0.75, size = 0.8) +

# create a cumulative distribution
#cdf = cumsum(distribution$y) / cumsum(distribution$y)[length(distribution$y)] 
cdf_w = cumsum(distribution_w$y) / cumsum(distribution_w$y)[length(distribution_w$y)] 

# plot the cdf
#plot(cdf, type = 'l', main='Cumulative Distribution')
#plot(cdf_w, type = 'l', main='Cumulative Distribution')

# test the distribution
#N_test = 1000
#generation = replicate(N_test, distribution$x[findInterval(runif(1), cdf) + 1])
#generation_w = replicate(N_test, distribution_w$x[findInterval(runif(1), cdf_w) + 1])

# plot distribution vs draws
# plot(distribution_w)
# lines(density(generation_w),col='red')
# legend('topright',legend=c('Density Function estimate', 
#                           'Generated sample density'), 
#        col=c("black", "red"), 
#        lty=c(1,1), 
#        cex=1)

## Draw from distribution
if (MULTI) {
  N = 100
} else {
  N = 1
}

new_gas_price = replicate(N, distribution_w$x[findInterval(runif(1), cdf_w) + 1])
NATIONAL_GAS_PRICE = 3.43
new_gas_price_ratio = new_gas_price / NATIONAL_GAS_PRICE

## Read default fuel data
fuel_data = read.csv("../updated_data/PE_pf.csv", header = T, stringsAsFactors = F)

for (i in 1:length(new_gas_price_ratio)) {
  ## Create price ratio
  if (!MULTI) {
    print(round(new_gas_price_ratio[i],2))
  }
  
  ## Apply Price ratio
  fuel_data_mult = fuel_data %>% mutate(Value = ifelse(fuel_type == "GAS", Value * new_gas_price_ratio[i], Value))
  
  ## Output new data for PE model
  if (MULTI) {
    write.csv(fuel_data_mult, paste("../updated_data/fuel_shock/PE_pf_SHOCK_",i,".csv",sep = ""), row.names = F)
  } else {
    write.csv(fuel_data_mult, "../updated_data/PE_pf_SHOCK.csv", row.names = F)
  }
  
}
ratios = as.data.frame(c(round(new_gas_price_ratio,2)))
colnames(ratios) = c("ratio")
write.csv(ratios,"../../Model/Results/shock_ratio.csv", row.names = F)


setwd(wd)
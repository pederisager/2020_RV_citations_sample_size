library(ggplot2)
library(viridis)
library(metR)

#### define function to calculate RV formula #### 
repVal <- function(c, y, n) {
  ## c = current citation count
  ## y = years since publication
  ## n = sample size
  
  RV <- (c/(y+1))*1/(n)  # RV formula based on equation 5
  RV
}



#### Plot formula behavior for range of input values on heatmap ####

set.seed(1)  # set seed to ensure reproducibility of reported values and plots

c.heat <- seq(1, 60)  # generate citation count values
y.heat <- 10  # for simplicity, years since publication is held constant. This number could also be varied. For demonstrating formula behavior over a range of input, varying C, Y, or C/Y produce the same result.
n.heat <- seq(11, 60)  # generate sample size values that will be used to calculate Vz
rv.heat <- sapply(c.heat, function(x) {  # calculate formula replication value for all combinarions of input parameters
  repVal(c = x, y = y.heat, n = n.heat)
})
df.heat <- expand.grid("C" = c.heat, "Y" = y.heat, "n" = n.heat)
df.heat$rv <- apply(df.heat, 1, function(x) {
  repVal(c = x["C"], y = x["Y"], n = x["n"])
})


ggplot(df.heat, aes(x = n, y = C/Y, z = rv)) +  
  geom_contour_fill(bins = 15) +
  scale_fill_viridis(option="plasma", 
                     breaks = seq(0, 0.8, by = 0.1),
                     guide = guide_colorbar(barheight = 20, 
                                            raster = F,
                                            nbin = 16, 
                                            frame.colour = "black", ticks.colour = "black")) +
  theme_bw(base_size = 16) + 
  theme(axis.text=element_text(size=12)
  ) +
  scale_x_continuous(limits = c(11, 60), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.1, 6), expand = c(0, 0)) +
  labs(x = "Sample size", y = "Average yearly citation rate", fill = expression("RV"["Cn"]), title = "Distribution of replication value over input parameters")

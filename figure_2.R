
library(ggplot2)
library(gridExtra)

set.seed(20210808)

years <- 1:50  # Arbitrary number of years used for demonstration purposes


# Plot figure 4A 

citations.lin <- 10+rnorm(length(years))  # Simulate citation counts for each year, assuming a constant citation rate + noise

slope.lin <- sum(citations.lin[1:(length(citations.lin)/2)])/(length(citations.lin)/2)  # predicted future citation count 

a <- ggplot(data = data.frame(x=years, y=citations.lin), aes(x=x, y=y)) +  # Plot actual vs. predicted count
  geom_bar(stat="identity", position="dodge", fill="grey70") +
  geom_bar(data=data.frame(x=years[1:(length(years)/2)], y=citations.lin[1:(length(years)/2)]), fill="#FFBF00", stat="identity", position="dodge") +
  geom_segment(aes(x = length(years)/2, xend = length(years), y = slope.lin, yend = slope.lin), size = 2, col="#FFBF00") +
  geom_label(aes(x = length(years)/(4/3), y = slope.lin, label = "Predicted count"), fill="#FFBF00", size = 5) + 
  theme_classic() +
  theme(axis.text.y = element_blank(),
        text = element_text(size = 18)) +
  labs(x="",
       y="Yearly citations",
       title="A")


# Plot figure 4B

citations.gam <- abs(3*dgamma(years/5, shape = 2, rate = 0.8) + rnorm(length(years))/40) # Simulate citation counts for each year, assuming a gamma function citation rate + noise. The actual numbers are arbitrary - the point is to show how the shape of the distribution matters for prediction.

slope.gam <- sum(citations.gam[1:(length(years)/2)])/(length(years)/2)  # predicted future citation count

d <- ggplot(data = data.frame(x=years, y=citations.gam), aes(x=x, y=y)) +  # Plot actual vs. predicted count
  geom_bar(stat="identity", position="dodge", fill="grey70") +
  geom_bar(data=data.frame(x=years[1:(length(years)/2)], y=citations.gam[1:(length(years)/2)]), fill="#FFBF00", stat="identity", position="dodge") +
  geom_segment(aes(x = length(years)/2, xend = length(years), y = slope.gam, yend = slope.gam), size = 2, col="#FFBF00") +
  geom_label(aes(x = length(years)/(4/3), y = slope.gam, label = "Predicted count"), fill="#FFBF00", size = 5) + 
  theme_classic() +
  theme(axis.text.y = element_blank(),
        text = element_text(size = 18)) +
  labs(x="Year since publication",
       y="Yearly citations",
       title="B")

# Combine plots into figure 4
grid.arrange(a, d)  

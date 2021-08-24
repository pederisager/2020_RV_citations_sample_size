


set.seed(20210808)

years <- 1:50

citations.lin <- 10+rnorm(length(years))

slope.lin <- sum(citations.lin[1:(length(citations.lin)/2)])/(length(citations.lin)/2)
slope.lin2 <- sum(citations.lin[1:3])/3

a <- ggplot(data = data.frame(x=years, y=citations.lin), aes(x=x, y=y)) +
  geom_bar(stat="identity", position="dodge", fill="grey70") +
  geom_bar(data=data.frame(x=years[1:(length(years)/2)], y=citations.lin[1:(length(years)/2)]), fill="#FFBF00", stat="identity", position="dodge") +
  geom_segment(aes(x = length(years)/2, xend = length(years), y = slope.lin, yend = slope.lin), size = 2, col="#FFBF00") +
  geom_label(aes(x = length(years)/(4/3), y = slope.lin, label = "Predicted count"), fill="#FFBF00", size = 5) + 
  # geom_bar(data=data.frame(x=years[1:3], y=citations.lin[1:3]), fill="brown", stat="identity", position="dodge") +
  # geom_segment(aes(x = 3, xend = length(years)/2, y = slope.lin2, yend = slope.lin2), size = 2, col="brown") +
  # geom_label(aes(x = 15.5, y = slope.lin2, label = "Predicted slope"), col="brown") + 
  theme_classic() +
  theme(axis.text.y = element_blank(),
        text = element_text(size = 18)) +
  labs(x="",
       y="Yearly citations",
       title="A")



# citations.log <- log(years)+0.2*rnorm(length(years))
# 
# slope.log <- sum(citations.log[1:(length(years)/2)])/(length(years)/2)
# 
# b <- ggplot(data = data.frame(x=years, y=citations.log), aes(x=x, y=y)) +
#   geom_bar(stat="identity", position="dodge", fill="grey70") +
#   geom_bar(data=data.frame(x=years[1:(length(years)/2)], y=citations.log[1:(length(years)/2)]), fill="blue", stat="identity", position="dodge") +
#   geom_segment(aes(x = length(years)/2, xend = length(years), y = slope.log, yend = slope.log), size = 2, col="blue") +
#   geom_label(aes(x = length(years)/(4/3), y = slope.log, label = "Predicted count"), col="blue") + 
#   theme_classic() +
#   theme(axis.text.y = element_blank()) +
#   labs(x="",
#        y="",
#        title="B")



# citations.exp <- abs(years^2+100*rnorm(length(years)))
# 
# slope.exp <- sum(citations.exp[1:(length(years)/2)])/(length(years)/2)
# 
# c <- ggplot(data = data.frame(x=years, y=citations.exp), aes(x=x, y=y)) +
#   geom_bar(stat="identity", position="dodge", fill="grey70") +
#   geom_bar(data=data.frame(x=years[1:(length(years)/2)], y=citations.exp[1:(length(years)/2)]), fill="blue", stat="identity", position="dodge") +
#   geom_segment(aes(x = length(years)/2, xend = length(years), y = slope.exp, yend = slope.exp), size = 2, col="blue") +
#   geom_label(aes(x = length(years)/(4/3), y = slope.exp, label = "Predicted count"), col="blue") + 
#   theme_classic() +
#   theme(axis.text.y = element_blank()) +
#   labs(x="Year since publication",
#        y="Citation count",
#        title="C")



citations.gam <- abs(3*dgamma(years/5, shape = 2, rate = 0.8) + rnorm(length(years))/40)

slope.gam <- sum(citations.gam[1:(length(years)/2)])/(length(years)/2)
slope.gam2 <- sum(citations.gam[1:2])/2

d <- ggplot(data = data.frame(x=years, y=citations.gam), aes(x=x, y=y)) +
  geom_bar(stat="identity", position="dodge", fill="grey70") +
  geom_bar(data=data.frame(x=years[1:(length(years)/2)], y=citations.gam[1:(length(years)/2)]), fill="#FFBF00", stat="identity", position="dodge") +
  geom_segment(aes(x = length(years)/2, xend = length(years), y = slope.gam, yend = slope.gam), size = 2, col="#FFBF00") +
  geom_label(aes(x = length(years)/(4/3), y = slope.gam, label = "Predicted count"), fill="#FFBF00", size = 5) + 
  # geom_bar(data=data.frame(x=years[1:2], y=citations.gam[1:2]), fill="brown", stat="identity", position="dodge") +
  # geom_segment(aes(x = 3, xend = length(years)/2, y = slope.gam2, yend = slope.gam2), size = 2, col="brown") +
  # geom_label(aes(x = 15.5, y = slope.gam2, label = "Predicted slope"), col="brown") + 
  theme_classic() +
  theme(axis.text.y = element_blank(),
        text = element_text(size = 18)) +
  labs(x="Year since publication",
       y="Yearly citations",
       title="B")

grid.arrange(a, d)

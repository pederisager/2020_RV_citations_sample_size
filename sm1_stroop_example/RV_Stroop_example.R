#-----------------------------------------------------------#
# The following script containts the code underlying the
# Stroop example case in the Replication Value manuscript.
#
# The replication value formula used for this example:
# Yearly citation rate multiplied by variance of Fisher's Z.
#
# This script depends on data and analysis script provided
# in the OSF directory https://osf.io/yw7d9/. 
#-----------------------------------------------------------#



#### set working dir, download necessary files & load packages #### 

library(tidyverse)
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # When opened in Rstudio, sets path to whereever analysis script is stored. Replace with whatever path is appropriate on local system

#unzip(zipfile = "Burns_et_al_data.zip", exdir = "Burns_etal_data")
#source("Burns_etal_data/Final_Analsis_Code_Modified.R")
#setwd("C:/Users/peder/Dropbox/jobb/PhD/Projects/2020_RV_citations_sample_size/sm2_stroop_example/")  # Change dir back after running Burns_etal script.


#### Replication value for study 2 in Stroop (1935) #### 

citations <- 9423  # Based on crossref citation entry for DOI (10.1037/h0054651). Timestamp: 2018-11-07 11:53:40 CET
years_since_pub <- 2018 - 1935
n <- 100


# Estimate repeated measures correlation "r" using data from a close replication of Stroop (1935)
df.stroop <- read.csv("Burns_etal_data/Data/cleanData.csv")  # Grab cleaned data from https://osf.io/yw7d9/ by downloading data (https://osf.io/3fu7q/) and R script (https://osf.io/ub46a/), and running data cleaning from their analysis script.
df.stroop <- subset(df.stroop, cond %in% c("neutral", "incongruent"))  # Keep oncly conditions relevant for Stroop (1935) study 2
means.stroop <- df.stroop %>% group_by(cond, ID) %>% summarize(mean_rt = mean(rt))  # Calculate mean reaction time for each subject in each condition
sd1 <- sd(means.stroop$mean_rt[means.stroop$cond=="incongruent"])  # Calculate standard deviation for mean scores on incongruent trials
sd2 <- sd(means.stroop$mean_rt[means.stroop$cond=="neutral"])  # Calculate standard deviation for mean scores on neutral trials
sddiff <- sd(means.stroop$mean_rt[means.stroop$cond=="incongruent"] - means.stroop$mean_rt[means.stroop$cond=="neutral"])  # calculate standard error of the difference between mean scores
r <- (sd1^2 + sd2^2 - sddiff^2) / (2*sd1*sd2)  # Calculate repeated measures correlation based on Lakens (2013). See supplementary materials 1 in this manuscript for details.

# Calculate CIs around r (see Bohrenstein ch. 6 for equations)
Z <- (1/2)*log((1+r)/(1-r))
Z_CIu <- Z + qnorm(1-.05/2) * sqrt(1/(100-3))
r_CIu <- (exp(2*Z_CIu)-1) / (exp(2*Z_CIu)+1)
Z_CIl <- Z - qnorm(1-.05/2) * sqrt(1/(100-3))
r_CIl <- (exp(2*Z_CIl)-1) / (exp(2*Z_CIl)+1)

a <- 2 # Each subject contributes data to two conditions
n_adjusted <- (n*a)/(1-r)  # Adjust sample size based on design. See supplementary materials 1 for further details.

se = 1/sqrt(n_adjusted)  # calculate variance
RV_stroop_orig <- citations / (years_since_pub+1) * se  # Calculate replication value for Stroop (1935) study 2
print(paste("Stroop (1935) study 2 has an estimated replication value of", round(RV_stroop_orig, 3)))

# Inferential statistics for Stroop (1935) Study 2 (NC-NCWd completion time difference)
n <- 100
mean.nc <- 63.3
sd.nc <- 10.8
mean.ncwd <- 110.3
sd.ncwd <- 18.8
r.within <- r  # Use repeated measures correlation from close replication as estimate
mean.diff <- (mean.ncwd - mean.nc)
sd.diff <- sqrt(sd.nc^2 + sd.ncwd^2 - 2 * r.within * sd.nc * sd.ncwd)

d.rm <- mean.diff / sqrt(sd.nc^2 + sd.ncwd^2 - 2 * r.within * sd.nc * sd.ncwd) * sqrt(2*(1-r))
ci.95 <- -qt(0.05/2, df = 99)*(sd.diff/sqrt(n))
mean.ci.u <- mean.diff + ci.95
mean.ci.l <- mean.diff - ci.95
t <- d.rm * sqrt(n)
p <- pt(t, df = n-1, lower.tail = FALSE)
bf <- BayesFactor::ttest.tstat(t = t, n1 = n, rscale = sqrt(2)/2)



#### Stroop (1935) replications reported in Verhaegen (1998), excluding non-close replications ####

names.rep <- c("Cohn", "Hartman", "Houx 1", "Houx 2", "Kieley 1", "Kieley 2", "Kwong", "Li", "Panek 1", "Panek 2", "Salthouse", "Salthouse_Meinz", "Spieler", "Weir 1", "Weir 2", "Stroop")  # Study identifiers based on first author name and study number in Verhaeghen & De Meersman (1998) table 1.
pub.year <- c(1984, 1991, 1993, 1993, 1997, 1997, 1995, 1996, 1984, 1984, 1996, 1995, 1996, 1997, 1997, 1935)  # Publication years of included replications
n.rep <- c(20, 44, 42, 18, 16, 45, 82, 35, 19, 31, 40, 49, 27, 17, 24, 100)  # Sample size (young participants) of included replications, as reported in Verhaeghen & De Meersman (1998) table 1.
stroop_reps <- data.frame("study" = names.rep, "pub_year" = pub.year, "n" = n.rep)  # gather variables in a data frame
stroop_reps <- arrange(stroop_reps, pub.year)  # sort replications by publication year
stroop_reps$n_adj <- (stroop_reps$n*a)/(1-r)  # Adjust sample size based on design. See supplementary materials 1 for further details. 
# Calculate replication from fixed-effect meta-analytic variance estimate (see supplementary materials 1 for details.) 
# Replication value is calculated sequentially by adding the variance weight from each consequetive replication to the total meta-analytic estimate.
stroop_reps$rv_seq <- citations / (years_since_pub+1) * (sapply(1:length(stroop_reps$n_adj), function(x) {  
  v <- 1/(stroop_reps$n_adj[1:x])
  w <- 1/v
  sqrt(1/sum(w))
  }))

stroop.p <- ggplot(data = stroop_reps, aes(x = 1:nrow(stroop_reps), y = rv_seq, label = study)) + 
  geom_line() + xlim(0, 17) + 
  geom_text(angle = 25, hjust = "left", size = 5) + 
  theme_classic() +
  theme(tex = element_text(size = 20)) +
  labs(x = "Replications ordered by publication year", y = expression("RV"["fixed"]))


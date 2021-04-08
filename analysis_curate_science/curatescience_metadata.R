
#### Load functions and data ####

library(rcrossref)
library(rAltmetric)

source("../Meta-Data/MetaData_functions.R")
source("../2018_OSC_Replication_Value/RV_functions.R")

#### Get rcrossref data ####

cursci <- read.csv(file = "curatescience.csv", sep = ",", header = TRUE, na.strings = c("", "NA"))

#### Add DOI-based data into the curate science dataset ####

cursci <- getCRcitations(table = cursci, doi.col = "orig.study.article.DOI")
cursci <- getAltmetrics(table = cursci, doi.col = "orig.study.article.DOI")
cursci$x_altscore <- as.numeric(cursci$x_altscore)


#### Extract p values from strings for the orig.pvalue variable

library(stringr)
matches <- as.numeric(str_extract_all(cursci$orig.pvalue, "\\.[[:digit:]]+"))
cursci$orig.pvalue.numeric <- matches

write.csv(cursci, "curatescience_appended.csv")


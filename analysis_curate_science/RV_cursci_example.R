
#-------------------------------------------------------#
# The following script containts the code underlying the
# Curate Science example case in the Replication Value 
# manuscript.
#-------------------------------------------------------#


#### Load dataset and relevant packages, and set working dir #### 

setwd("C:/Users/peder/Dropbox/jobb/PhD/Projects/2020_RV_citations_sample_size/analysis_curate_science/")
library(rcrossref)
library(tidyverse)
library(ggpubr)
library(stringr)
library(rcompanion)
library(scales)
library(rcrossref)

#curate_science <- read.csv("https://raw.githubusercontent.com/eplebel/science-commons/master/CS.rep.table.csv", na.strings = "", stringsAsFactors = FALSE)  # Download latest version of Curate Science data
#write.csv(x = curate_science, file = "curate_science.csv", row.names = F)  # Save latest version of Curate Science data

## Retrieve crossref metrics for original studies in the dataset (only needed once)

# cursci_orig_cr <- cr_cn(dois = unique(curate_science$orig.study.article.DOI), "citeproc-json")  # Download DOI meta-data from crossref for each unique DOI
# write_json(cursci_orig_cr, "curate_science_orig_study_crossref.json", pretty = TRUE)
# saveRDS(cursci_orig_cr, "curate_science_orig_study_crossref.Rds")




# Psychological Bulletin sample

### Update citation counts for Psych. Bulletin dataset
#pbul_citations <- cr_citation_count(doi = pbul.df$x_doi)  
#saveRDS(pbul_citations, "psych_bulletin_crossref_citation_count_downloaded_2020-10-20.Rds")

## Load data
pbul.df <- readRDS("psybull_meta_data.rds")
pbul_citations <- readRDS("psych_bulletin_crossref_citation_count_downloaded_2020-10-20.Rds") # Download most recent citation count data
names(pbul_citations) <- c("x_doi", "x_crcited")

## Update citation counts in pbul.df with citation counts in pbul_citations. 
pbul.df$x_crcited <- NULL
x <- unique(pbul_citations)  # remove duplicates of doi-citation count combinations
dups <- x[x$x_doi %in% x[duplicated(x$x_doi), "x_doi"],]  # dois with two matching citation counts - likely caused by crossref updating citation counts as data were being generated, since in all cases, citation count only differs by 1. Solution: delete lower citation count duplicate.
dups <- dups[order(dups$x_doi, dups$x_crcited),]  # order dups. Now, odd rows represents lower citation count duplicates we want to delete.
delete <- as.numeric(rownames(dups[seq(1, nrow(dups), by = 2),]))  # row numbers in x for the odd rows in dups that are to be deleted.
x <- x[!rownames(x) %in% delete,]  # remove rows from x to get completely unique doi-citation count combinations
pbul.df <- left_join(pbul.df, x, by = "x_doi")  # update citation counts in pbul.df
### Update done

## Calculate replication value
pbul.df$years_since_pub <- 2019 - pbul.df$x_pubyear  # Years since publication
pbul.df$cit_p_year <- pbul.df$x_crcited / pbul.df$years_since_pub  # average number of citations per year
pbul.df$RV <- pbul.df$cit_p_year * (1/sqrt(pbul.df$x_n))  # RV per record
pbul.df <- pbul.df[!is.na(pbul.df$RV), ]  # Reduce data to those records for which RV can be calculated (i.e. has info on all input parameters and sample size >4)
pbul.cit.df <- pbul.df[!duplicated(pbul.df$x_doi), c("x_doi", "x_crcited", "x_pubyear", "years_since_pub", "cit_p_year", "x_n", "RV")]  # Aggregate citation data over duplicate article references

## Delete vestigials
rm(list=c("dups", "pbul_citations", "pbul.df", "x", "delete"))



# Curate Science sample

### Update citation counts for Curate Science dataset
#cursci_citations <- cr_citation_count(doi = curate_science$orig.study.article.DOI)  
#saveRDS(cursci_citations, "curate_science_crossref_citation_count_downloaded_2020-10-20.Rds")


## Load Curate Science data
curate_science <- read.csv("curate_science.csv", na.strings = "", stringsAsFactors = FALSE)  # Load curate science data
curate_science$orig.N <- as.numeric(curate_science$orig.N)  # Interpret sample size as numeric variable
cursci_orig_cr <- readRDS("curate_science_orig_study_crossref.Rds")  # Load bibliometrics

### Update citation counts in curate_science data with citation counts from Crossref.
cursci_citations <- readRDS("curate_science_crossref_citation_count_downloaded_2020-10-20.Rds") # Load most recent citation count data
names(cursci_citations) <- c("orig.study.article.DOI", "orig.citations")
curate_science$orig.citations <- NULL
x <- unique(cursci_citations)  # remove duplicates of doi-citation count combinations
dups <- x[x$orig.study.article.DOI %in% x[duplicated(x$orig.study.article.DOI), "orig.study.article.DOI"],]  # dois with two matching citation counts - likely caused by crossref updating citation counts as data were being generated, since in all cases, citation count only differs by 1. Solution: delete lower citation count duplicate.
dups <- dups[order(dups$orig.study.article.DOI, dups$orig.citations),]  # order dups. Now, odd rows represents lower citation count duplicates we want to delete.
delete <- as.numeric(rownames(dups[seq(1, nrow(dups), by = 2),]))  # row numbers in x for the odd rows in dups that are to be deleted.
x <- x[!rownames(x) %in% delete,]  # remove rows from x to get completely unique doi-citation count combinations
curate_science <- left_join(x = curate_science, y = x, by = "orig.study.article.DOI")  # update citation counts 
### Update done



# extract data from crossref data to be merged with curate_science
rep.years <- as.numeric(str_extract(curate_science$rep.study.number, "[[:digit:]]+"))
curate_science$rep.publ.year <- rep.years
curate_science$rep.publ.year[curate_science$rep.publ.year<1000] <- NA

# citations <- unlist(lapply(cursci_orig_cr, function(x) ifelse(is.null(x$`is-referenced-by-count`[1]), NA, x$`is-referenced-by-count`[1])))
cr_numberofreps <- nrow(curate_science)  # Make a note of the number of replications in the dataset before non-matches are removed.
cr_numberoforiginals <- sum(unique(curate_science$orig.study.article.DOI) != "NA")  # Count number of original articles before non-matches are removed.
years <- unlist(lapply(cursci_orig_cr, function(x) ifelse(is.null(x$`issued`$`date-parts`[1]), NA, x$`issued`$`date-parts`[1])))
doi <- unlist(lapply(cursci_orig_cr, function(x) ifelse(is.null(x$`DOI`[1]), NA, x$`DOI`[1]))) 
cr.df <- data.frame(orig.publ.year = years, orig.study.article.DOI = doi)  
curate_science$orig.study.article.DOI <- tolower(curate_science$orig.study.article.DOI)  # set DOI characters in curate_science to lower case to match DOI formatting in cr.df
curate_science <- merge(curate_science, cr.df, by = "orig.study.article.DOI")  # Some DOIs do not match, leading to a smaller dataset.

# Calculate replication value for original studies in the Curate Science dataset
curate_science$orig.RV <- (curate_science$orig.citations / (2020 - curate_science$orig.publ.year)) * (1/sqrt(curate_science$orig.N) )  # RV=(citation_count/years) * (1/sqrt(sample_size))

# Aggregate results 
RVdata <- aggregate(curate_science$orig.N, by=list(orig.DOI=curate_science$orig.study.article.DOI,
                                                   orig.study.number=curate_science$orig.study.number,  
                                                   target.effect=curate_science$target.effect,
                                                   RV=curate_science$orig.RV, 
                                                   #sum.RV=curate_science$sum.RV, 
                                                   citations=curate_science$orig.citations,
                                                   y.cit=(curate_science$orig.citations/(2019-curate_science$orig.publ.year))
), FUN=mean)
names(RVdata)[length(names(RVdata))] <- "sample_size"
RVdata <- RVdata[-which(RVdata$target.effect == "sex difference in implicit math attitudes"),]  # Removing a duplicate in which the same sample from the same study is testing two different effects. Since our RV only uses sample size, this creates a duplicate, which we remove here. 
RVdata <- RVdata[order(-RVdata$RV),]
RVdata$order <- 1:nrow(RVdata)

# Delete vestigials
rm(dups, cr.df, cursci_citations, cursci_orig_cr, x, doi, rep.years, years, delete)


#### Analyses ####




## Citation count replicated studies vs. general studies

### Curate science data
cs.c.n <- length(unique(RVdata[, c("orig.DOI", "citations")])[[2]])
cs.c <- unique(RVdata[, c("orig.DOI", "citations")])[[2]]
cs.c.iqr <- quantile(x = cs.c, probs = c(.25, .5, .75))
### Psychological bulletin data
pbul.c.n <- nrow(pbul.cit.df)
pbul.c <- pbul.cit.df$x_crcited
pbul.c.iqr <- quantile(x = pbul.c, probs = c(.25, .5, .75))
### Common language effect size 
set.seed(20202010)
#c.vda <- vda(x = cs.c, y = pbul.c, ci = T, conf = .99)  # VDA for probability x>y
### Mann-Whitney U test
wilcox.test(cs.c, pbul.c)
### Plot
p.c <- ggplot(data = pbul.cit.df)  +
  geom_density(aes(x = x_crcited^(1/3)), fill = "red", alpha = 0.5, na.rm = T, adjust = 0.5) + 
  geom_density(data = as.data.frame(cs.c^(1/3)), aes(x = cs.c^(1/3)), fill = "blue", alpha = 0.5, na.rm = T, adjust = 0.5) +
  theme_classic(base_size = 16) + 
  labs(x = "Citations", title = "A") +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20), labels = c(0, 5, 10, 15, 20)^3)


## Median average yearly citation count of original findings

### Curate science data
cs.cy.n <- length(unique(RVdata[, c("orig.DOI", "y.cit")])[[2]])
cs.cy <- unique(RVdata[, c("orig.DOI", "y.cit")])[[2]]
cs.cy.iqr <- quantile(x = cs.cy, probs = c(.25, .5, .75))
### Psychological bulletin data
pbul.cy.n <- length(pbul.cit.df$x_crcited/(2019-pbul.cit.df$x_pubyear))
pbul.cy <- pbul.cit.df$x_crcited/(2019-pbul.cit.df$x_pubyear)
pbul.cy.iqr <- quantile(x = pbul.cy, probs = c(.25, .5, .75))
### Common language effect size 
set.seed(20202010)
#cy.vda <- vda(x = cs.cy, y = pbul.cy, ci = T, conf = .99)  # VDA for probability x>y
### Mann-Whitney U test
wilcox.test(cs.cy, pbul.cy)
### Plot
p.cy <- ggplot(data = pbul.cit.df)  +
  geom_density(aes(x = cit_p_year^(1/3)), fill = "red", alpha = 0.5, na.rm = T, adjust = 0.5) + 
  geom_density(data = as.data.frame(cs.cy^(1/3)), aes(x = cs.cy^(1/3)), fill = "blue", alpha = 0.5, na.rm = T, adjust = 0.5) +
  theme_classic(base_size = 16) + 
  labs(x = "Citations per year", title = "B") +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8), labels = c(0, 2, 4, 6, 8)^3)


## Median sample size of original findings

### Curate science data
cs.n.n <- length(unique(RVdata[, c("orig.DOI", "orig.study.number", "sample_size")])[[2]])
cs.n <- unique(RVdata[, c("orig.DOI", "orig.study.number", "sample_size")])[[3]]
cs.n.iqr <- quantile(x = cs.n, probs = c(.25, .5, .75))
### Psychological bulletin data
pbul.n.n <- length(pbul.cit.df$x_n)
pbul.n <- pbul.cit.df$x_n
pbul.n.iqr <- quantile(x = pbul.n, probs = c(.25, .5, .75))
### Common language effect size 
set.seed(20202010)
#n.vda <- vda(x = cs.n, y = pbul.n, ci = T, conf = .99)  # VDA for probability x>y
### Mann-Whitney U test
wilcox.test(cs.n, pbul.n)
### Plot 
p.n <- ggplot(data = pbul.cit.df)  +
  geom_density(aes(x = x_n^(1/3)), fill = "red", alpha = 0.5, na.rm = T, adjust = 0.5) + 
  geom_density(data = as.data.frame(cs.n^(1/3)), aes(x = cs.n^(1/3)), fill = "blue", alpha = 0.5, na.rm = T, adjust = 0.5) +
  theme_classic(base_size = 16) + 
  labs(x = "Sample size", title = "C") +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20), labels = c(0, 5, 10, 15, 20)^3, limits = c(0, 20))


## Median replication value of original findings

### Curate science data
cs.RV.n <- length(unique(RVdata[, c("orig.DOI", "orig.study.number", "RV")])[[2]])
cs.RV <- unique(RVdata[, c("orig.DOI", "orig.study.number", "RV")])[[3]]
cs.RV.iqr <- quantile(x = cs.RV, probs = c(.25, .5, .75))
### Psychological bulletin data
pbul.RV.n <- length(pbul.cit.df$RV)
pbul.RV <- pbul.cit.df$RV
pbul.RV.iqr <- quantile(x = pbul.RV, probs = c(.25, .5, .75))
### Common language effect size 
set.seed(20202010)
#RV.vda <- vda(x = cs.RV, y = pbul.RV, ci = T, conf = .99)  # VDA for probability x>y
### Mann-Whitney U test
wilcox.test(cs.RV, pbul.RV)
### Plot
p.RV <- ggplot(data = pbul.cit.df)  +
  geom_density(aes(x = RV^(1/3), fill = 'red'), alpha = 0.5, na.rm = T, adjust = 0.5) + 
  geom_density(data = as.data.frame(cs.RV^(1/3)), aes(x = cs.RV^(1/3), fill = 'blue'), alpha = 0.5, na.rm = T, adjust = 0.5) +
  theme_classic(base_size = 16) + 
  labs(x = "Replication value", title = "D") +
  scale_x_continuous(breaks = c(0, 0.5, 1.0, 1.5, 2.0), labels = c(0, 0.5, 1.0, 1.5, 2.0)^3) +
  scale_fill_manual(name = 'sample', values =c('blue'='blue','red'='red'), labels = c('replicated','comparison'))

## Save/read all vda values in a file so simulations do not have to be rerun every time script is sourced. 
# vdas <- rbind(c.vda, cy.vda, n.vda, RV.vda)
# rownames(vdas) <- c("c", "cy", "n", "RV")
# saveRDS(vdas, file = "vda_values.Rds")
vdas <- readRDS("vda_values.Rds")  # Load saved vda values

## Combine plots into grid for manuscript

p.all <- ggarrange(p.c, p.cy, p.n, p.RV, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")

## Combine summary stats into table for manuscript

summary.tab <- data.frame(variable = c("citation count", 
                                       "citation count", 
                                       "citations per year", 
                                       "citations per year", 
                                       "sample size", 
                                       "sample size", 
                                       "replication value", 
                                       "replication value"),
                          group = c("replicated", 
                                    "comparison", 
                                    "replicated", 
                                    "comparison",
                                    "replicated", 
                                    "comparison", 
                                    "replicated", 
                                    "comparison"),
                          n = c(cs.c.n, 
                                pbul.c.n, 
                                cs.cy.n, 
                                pbul.cy.n, 
                                cs.n.n, 
                                pbul.n.n, 
                                cs.RV.n, 
                                pbul.RV.n), 
                          median = c(cs.c.iqr[[2]], 
                                     pbul.c.iqr[[2]], 
                                     cs.cy.iqr[[2]], 
                                     pbul.cy.iqr[[2]], 
                                     cs.n.iqr[[2]], 
                                     pbul.n.iqr[[2]], 
                                     cs.RV.iqr[[2]], 
                                     pbul.RV.iqr[[2]]),
                          Q1 = c(cs.c.iqr[[1]], 
                                 pbul.c.iqr[[1]], 
                                 cs.cy.iqr[[1]], 
                                 pbul.cy.iqr[[1]], 
                                 cs.n.iqr[[1]], 
                                 pbul.n.iqr[[1]], 
                                 cs.RV.iqr[[1]], 
                                 pbul.RV.iqr[[1]]),
                          Q3 = c(cs.c.iqr[[3]], 
                                 pbul.c.iqr[[3]], 
                                 cs.cy.iqr[[3]], 
                                 pbul.cy.iqr[[3]], 
                                 cs.n.iqr[[3]], 
                                 pbul.n.iqr[[3]], 
                                 cs.RV.iqr[[3]], 
                                 pbul.RV.iqr[[3]]), 
                          A = c(paste0(vdas["c","VDA"], " 99%CI[", vdas["c","lower.ci"], ", ", vdas["c","upper.ci"], "]"),
                                "",
                                paste0(vdas["cy","VDA"], " 99%CI[", vdas["cy","lower.ci"], ", ", vdas["cy","upper.ci"], "]"),
                                "",
                                paste0(vdas["n","VDA"], " 99%CI[", vdas["n","lower.ci"], ", ", vdas["n","upper.ci"], "]"),
                                "",
                                paste0(vdas["RV","VDA"], " 99%CI[", vdas["RV","lower.ci"], ", ", vdas["RV","upper.ci"], "]"),
                                "")
                          )

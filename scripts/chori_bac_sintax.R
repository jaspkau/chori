#setwd("C:/Users/jaspkaur/Google Drive/data_analysis/chorizanthe/chori")

#source("https://bioconductor.org/biocLite.R")
#biocLite("phyloseq")
library(phyloseq)
library(readxl)
library(vegan)
library(devtools)
#devtools::install_github("benjjneb/decontam")
library(decontam)

# Make phyloseq object ----------------------------------------------------

otu <- read.delim(file = "data/otu_table_no_singletons_sintax_bac.txt", 
                  sep = "\t", header = T)
tax = read.delim(file = "data/tax_bac.sintax", sep = "\t", header = F)
met <- as.data.frame(read_excel("data/met.xlsx", sheet = 2))

source("scripts/make_phyloseq.R")

d = subset_taxa(d, Kingdom == "d:Bacteria")
d
##"Remove taxa not seen more than 3 times in at least 5% of the samples. 
#This protects against an OTU with small mean & trivially large C.V.
d = filter_taxa(d, function(x) sum(x > 2) > (0.05*length(x)), TRUE)
d
decon.d = subset_samples(d, plot == "mon"| plot == "NC")
decon.d

source("scripts/decontaminate_phyloseq.R")

###select populations

d.fin2 = subset_samples(d.fin, Population == "EO12"| 
                          Population == "EO14"| Population == "EO16")

# Alpha diversity ---------------------------------------------------------

#plot_richness(d.fin, x= "Population", measures=c("Shannon", "Simpson") )

temp = estimate_richness(d.fin2)
temp = merge(met, temp, by = "row.names")

ggplot(temp, aes(Population, Shannon)) + geom_point() + facet_grid(. ~ Year)
ggplot(temp, aes(Population, Simpson)) + geom_point() + facet_grid(. ~ Year)

a = summary(aov(Simpson ~ Population*Year, data = temp))
a
a = summary(aov(Shannon ~ Population*Year, data = temp))
a

# Beta diversity with bray ------------------------------------------------

otu2 = t(data.frame(otu_table(d.fin2)))
otu2 = decostand(otu2, method = "hellinger")
rowSums(otu2)
otu2 = otu_table(as.matrix(otu2), taxa_are_rows = F)

d.ado = merge_phyloseq(tax2, otu2, sample_data(d))
rel_otu_code = data.frame(otu_table(d.ado))

dist_w = vegdist(rel_otu_code, method = "bray")

###PERMANOVA

a = adonis(dist_w ~ sample_data(d.ado)$Population*sample_data(d.ado)$Year, permutations = 999)
a

# Hierarchial clustering --------------------------------------------------

d.popyear = merge_samples(d.fin2, "pop.year")
otu3 = data.frame(otu_table(d.popyear))
otu3 = decostand(otu3, method = "hellinger")
rel_otu_int = otu3
rowSums(otu3)
otu3 = round(otu3, 2)

dist_w_int = vegdist(otu3, method = "bray")

otu3_tab = otu_table(as.matrix(otu3), taxa_are_rows = F)
d4 = merge_phyloseq(tax2, otu_table(as.matrix(otu3_tab), 
                                    taxa_are_rows = F), sample_data(d.popyear))

#weighted distance analysis
h = hclust(dist_w_int, method = "average")
dhc <- as.dendrogram(h)
nodePar <- list(lab.cex = 1, pch = c(NA, 19), cex = 0.7, col = "blue")
p = plot(dhc,  xlab = "Weighted Bray-Curtis distance", nodePar = nodePar, horiz = TRUE)
p

# ###Relative abundance plots ---------------------------------------------

source("scripts/relative_abundance_plots.R")

###relative abundances at OTU level
p.otus

###relative abundances at phylum level
p.phy

###relative abundances at family level
p.fam

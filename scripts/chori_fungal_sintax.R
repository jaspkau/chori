#setwd("C:/Users/jaspkaur/Google Drive/data_analysis/chorizanthe/chori")

#source("https://bioconductor.org/biocLite.R")
#biocLite("phyloseq")
library(phyloseq)
library(readxl)
library(vegan)
library(devtools)
#devtools::install_github("benjjneb/decontam")
library(decontam)

###read files and make phyloseq object

otu <- read.delim(file = "data/otu_table_no_singletons_sintax_fungal.txt",
                  sep = "\t", header = T)
tax = read.delim(file = "data/tax_fungal.sintax", sep = "\t", header = F)
met <- as.data.frame(read_excel("data/met.xlsx", sheet = 1))
met$replicate = as.factor(met$replicate)
##make phyloseq object
source("scripts/make_phyloseq.R")

d = subset_taxa(d, Kingdom == "d:Fungi")
d
decon.d = subset_samples(d, plot == "mon" |plot == "ger"| plot == "NC")
decon.d

##decontaminate phyloseq object
source("scripts/decontaminate_phyloseq.R")

###select populations
d.fin

d.fin2 = subset_samples(d.fin, plot == "ger"| plot == "mon") ####remove NC
d.fin2 = prune_taxa(taxa_sums(d.fin2) >= 1, d.fin2)
d.fin2

# Species accumulation curves ---------------------------------------------

###species accumulation curves
d_rf = merge_samples(d.fin2, "pop.year")
otu_rf = data.frame(otu_table(d_rf))
library(iNEXT)
otu_rc = data.frame(t(otu_rf)) ####columns should be samples
m <- c(1000, 2000, 5000, 10000, 20000, 30000)
out = iNEXT(otu_rc, q=0, datatype="abundance", size=m, nboot = 100)
g = ggiNEXT(out, type=1, se = FALSE, facet.var="none")

###plot Figure S2a

g1 = g + scale_color_manual(values=c("wheat4", "violetred4", "turquoise3", "tomato2", "springgreen2",
                                     "slateblue2", "navyblue", "magenta", "blue2", "black", "seagreen4",
                                     "dodgerblue1", "orangered4", "yellow4", "slategray4", "olivedrab1","deeppink4", "aquamarine",
                                     "hotpink", "yellow1", "tan2", "red3", "pink1"))
g1

####Relative abundance plots ---------------------------------------------

source("scripts/relative_abundance_plots.R")

###relative abundances at phylum level (Fig. S2b)
p.phy

# Alpha diversity ---------------------------------------------------------

temp = estimate_richness(d.fin2)
temp = merge(met, temp, by = "row.names")
temp = temp[,-1]
row.names(temp) = temp[,1]

####plot Fig. S2c

bp <- ggplot(temp, aes(x=Population, y=Simpson)) + 
  geom_boxplot(aes(fill= "slategray4")) + 
  theme_classic() +
  labs(x = paste("Site"), 
       y = paste("Simpson diversity index (H)")) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
bp

alpha.kw = c()
for(i in c(4)){
  column = names(temp[i])
  k.demo = kruskal.test(Simpson ~ as.factor(temp[,i]), data = temp)$"p.value"
  results = data.frame(otu = paste(column), pval = as.numeric(paste(k.demo)))
  alpha.kw = rbind(alpha.kw, results)
}

alpha.kw$p.ad = p.adjust(alpha.kw$pval, method = "bonferroni")

avg = temp %>%
  group_by(pop.year) %>%
  summarise(simp = mean(Simpson))
avg

# Beta diversity with bray ------------------------------------------------

otu2 = t(data.frame(otu_table(d.fin2)))
otu2 = decostand(otu2, method = "hellinger") ##transform count data
rowSums(otu2)
otu2 = otu_table(as.matrix(otu2), taxa_are_rows = F)

d.ado = merge_phyloseq(tax2, otu2, sample_data(d))
rel_otu_code = data.frame(otu_table(d.ado))

dist_w = vegdist(rel_otu_code, method = "bray")

###PERMANOVA

a = adonis(dist_w ~ sample_data(d.ado)$Population, permutations = 999)
a

# Hierarchial clustering --------------------------------------------------
sample_data(d.fin2)$int = paste(sample_data(d.fin2)$Population, ".", sample_data(d.fin2)$replicate)
sample_data(d.fin2)$int = gsub(" ", "", sample_data(d.fin2)$int)
d.popyear = merge_samples(d.fin2, "int")
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

###plot Figure 7c
p = plot(dhc,  xlab = "Weighted Bray-Curtis distance", nodePar = nodePar, horiz = TRUE)
p


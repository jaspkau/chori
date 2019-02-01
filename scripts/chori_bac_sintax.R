#setwd("C:/Users/jaspkaur/Google Drive/data_analysis/chorizanthe/chori")

#source("https://bioconductor.org/biocLite.R")
#biocLite("phyloseq")
library(phyloseq)
library(readxl)
library(vegan)
library(devtools)
#devtools::install_github("benjjneb/decontam")
library(decontam)
library(dplyr)

# Make phyloseq object ----------------------------------------------------

otu <- read.delim(file = "data/otu_table_no_singletons_sintax_bac.txt", 
                  sep = "\t", header = T)
tax = read.delim(file = "data/tax_bac.sintax", sep = "\t", header = F)
met <- as.data.frame(read_excel("data/met.xlsx", sheet = 2))

source("scripts/make_phyloseq.R")

d

d = subset_taxa(d, Kingdom == "d:Bacteria") 
d
d = subset_taxa(d, Phylum != "p:Cyanobacteria/Chloroplast")
d 

##"Remove taxa not seen more than 3 times in at least 5% of the samples. 
#This protects against an OTU with small mean & trivially large C.V.
d = filter_taxa(d, function(x) sum(x > 1) > (0.05*length(x)), TRUE)
d
#decon.d = subset_samples(d, plot == "mon"| plot == "NC")
#decon.d
decon.d = subset_samples(d, plot == "mon"| plot == "ger"| plot == "NC")
decon.d

source("scripts/decontaminate_phyloseq.R")

###select populations

d.fin
d.fin2 = subset_samples(d.fin, plot == "ger"| plot == "mon")
d.fin2 = prune_taxa(taxa_sums(d.fin2) >= 1, d.fin2)
d.fin2

# Species accumulation curves ---------------------------------------------

###species accumulation curves
d_rf = merge_samples(d.fin2, "Population")
otu_rf = data.frame(otu_table(d_rf))
library(iNEXT)
otu_rc = data.frame(t(otu_rf)) ####columns should be samples
m <- c(1000, 2000, 5000, 10000, 20000, 30000, 100000)
out = iNEXT(otu_rc, q=1, datatype="abundance", size=m, nboot = 1)
g = ggiNEXT(out, type=1, se = FALSE, facet.var="none")

g1 = g + scale_color_manual(values=c("wheat4", "violetred4", "turquoise3", "tomato2", "springgreen2",
                                     "slateblue2", "navyblue", "magenta", "blue2", "black", "seagreen4",
                                     "dodgerblue1", "orangered4", "yellow4", "slategray4", "olivedrab1","deeppink4", "aquamarine",
                                     "hotpink", "yellow1", "tan2", "red3", "pink1"))
g1

# Alpha diversity ---------------------------------------------------------

#plot_richness(d.fin, x= "Population", measures=c("Shannon", "Simpson") )
temp = estimate_richness(d.fin2)
temp = merge(met, temp, by = "row.names")
temp = temp[,-1]
row.names(temp) = temp[,1]

bp <- ggplot(temp, aes(x=Population, y=Simpson)) + 
  geom_boxplot(aes(fill= "slategray4")) + 
  labs(x = paste("Site"), 
       y = paste("Simpson diversity index (D)")) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
bp

alpha.kw = c()
for(i in c(4)){
  column = names(temp[i])
  k.demo = kruskal.test(Simpson ~ as.factor(temp[,i]), data = temp)$"p.value"
  results = data.frame(otu = paste(column), pval = as.numeric(paste(k.demo)))
  alpha.kw = rbind(alpha.kw, results)
}

alpha.kw$p.ad = p.adjust(alpha.kw$pval, method = "bonferroni")
alpha.kw

avg = temp %>%
  group_by(Population) %>%
  summarise(simp = mean(Simpson))
avg

# Beta diversity with bray ------------------------------------------------

otu2 = t(data.frame(otu_table(d.fin2)))
otu2 = decostand(otu2, method = "hellinger")
rowSums(otu2)
otu2 = otu_table(as.matrix(otu2), taxa_are_rows = F)

d.ado = merge_phyloseq(tax2, otu2, sample_data(d))
rel_otu_code = data.frame(otu_table(d.ado))

dist_w = vegdist(rel_otu_code, method = "bray")

###PERMANOVA

a = adonis2(dist_w ~ sample_data(d.ado)$Population, permutations = 999)
a

# Hierarchial clustering --------------------------------------------------

sample_data(d.fin2)$int = paste(sample_data(d.fin2)$Population, ".", sample_data(d.fin2)$replicate)
sample_data(d.fin2)$int = gsub(" ","",sample_data(d.fin2)$int)
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
p = plot(dhc,  xlab = "Weighted Bray-Curtis distance", nodePar = nodePar, horiz = TRUE)
p

####Relative abundance plots ---------------------------------------------

source("scripts/relative_abundance_plots.R")

###relative abundances at OTU level
p.otus

###relative abundances at phylum level
p.phy

###relative abundances at family level
p.fam

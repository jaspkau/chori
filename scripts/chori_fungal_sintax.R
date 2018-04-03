#setwd("C://Users//jaspkaur//Google Drive//Metagenomics//pico_comb_run//pico/")
#setwd("C:/Users/jas/Google Drive/Metagenomics/pico_comb_run/pico (1)/")
#setwd("/Users/administrator/Documents/jaspreet/chori/chori/")

library(phyloseq)

# Make phyloseq object ----------------------------------------------------

otu <- read.delim(file = "data/otu_table_no_singletons_sintax_fungal.txt",
                  sep = "\t", header = T)
otu = otu[,-ncol(otu)]
row.names(otu) = paste(gsub("denovo", "o", otu[,1]))
otu = otu[,-1]
#Rarefy(otu, depth = min(rowSums(otu)))
#otu = otu[,colSums(otu) > 0]
#site_list = colnames(otu)
otu_tab = otu_table(as.matrix(otu), taxa_are_rows = T)

###Format SINTAX taxonomy table

library(reshape2)

tax = read.delim(file = "data/tax_fungal.sintax", sep = "\t", header = F)
row.names(tax) = tax$V1
list = tax$V2
tax2 = colsplit(list, pattern ="\\(|\\),", c("Kingdom", "Kingdom_conf", "Phylum", "Phylum_conf", "Class", "Class_conf", "Order", "Order_conf", "Family", "Family_conf", "Genus", "Genus_conf", "Species", "Species_conf"))
tax2$Species_conf = gsub("\\)", "", tax2$Species_conf)
tax2$Species_conf = as.numeric(tax2$Species_conf)
tax2[is.na(tax2)] <- 0
row.names(tax2) = row.names(tax)

source("scripts/tax_func.R") #90% conf
level = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
for (i in 1:7){ ###i 1 = kingdom, i 2 = phylum etc
  tax2 = tax_assign(tax2, paste(level[i], "_conf", sep = ""), level[i])
}

tax2 = tax2[,-c(2,4,6,8,10,12,14)]

tax2$row = row.names(tax2)
tax2[,8:9] = colsplit(tax2$row, " ", c("otu", "seq"))
row.names(tax2) = paste(gsub("denovo", "o", tax2$row))
tax2 = tax2[,-c(8:9)]
tax2 = tax_table(as.matrix(tax2))

#meta data
library(readxl)
met <- as.data.frame(read_excel("data/met.xlsx", sheet = 1))
row.names(met) = met$code
met$Year = as.factor(met$Year)
met$pop.year = paste(met$Population, ".", gsub("20", "", met$Year))
met$pop.year = gsub(" ", "", met$pop.year)

#phyloseq object

d = merge_phyloseq(tax2, otu_tab, sample_data(met))
d
d = subset_taxa(d, Kingdom == "d:Fungi")
d
decon.d = subset_samples(d, plot == "mon"| plot == "NC")
decon.d

####decontaminate phyloseq object based on frequency and prevelence
library(devtools)
#devtools::install_github("benjjneb/decontam")
library(decontam)

df <- as.data.frame(sample_data(decon.d)) # Put sample_data into a ggplot-friendly d
df$LibrarySize <- sample_sums(decon.d)
df <- df[order(df$LibrarySize),]
df$Index <- seq(nrow(df))
library(ggplot2)
p = ggplot(data=df, aes(x=Index, y=LibrarySize, color=Sample_or_Control)) + geom_point()

###prevelanec based
sample_data(decon.d)$is.neg <- sample_data(decon.d)$Sample_or_Control == "Control"
contamdf.prev <- isContaminant(decon.d, method="prevalence", neg="is.neg", threshold=0.5)
table(contamdf.prev$contaminant)
which(contamdf.prev$contaminant)
decon.d <- prune_taxa(!contamdf.prev$contaminant, decon.d)
decon.d

contamdf.freq <- isContaminant(decon.d, method="frequency", conc="DNA_conc")
table(contamdf.freq$contaminant)
which(contamdf.freq$contaminant == "TRUE")
decon.d <- prune_taxa(!contamdf.freq$contaminant, decon.d)
decon.d

d.fin = subset_samples(decon.d, Month == "Feb")
d.fin
d.fin = prune_taxa(taxa_sums(d.fin) >= 1, d.fin)
d.fin

####scale envt data according to above sample selection
met2 = data.frame(sample_data(d.fin))
env_met = met2[,cbind(1,2,3,4,5,6,7,40,41)]
env = met2[,8:39]
env = scale(env)
met3 = merge(env_met, env, by = "row.names")
row.names(met3) = met3$Row.names

d.fin = merge_phyloseq(tax_table(d.fin), otu_table(d.fin), sample_data(met3))
d.fin

# Alpha diversity ---------------------------------------------------------

plot_richness(d.fin, x= "Population", measures=c("Observed", "Shannon", "Simpson") )

plot_richness(d.fin, x= "Year", measures=c("Observed", "Shannon", "Simpson"))

temp = estimate_richness(d.fin)
temp = merge(met, temp, by = "row.names")

a = summary(aov(Observed ~ Population, data = temp))
a
a = summary(aov(Simpson ~ Population, data = temp))
a
a = summary(aov(Shannon ~ Population, data = temp))
a
a = summary(aov(Observed ~ Year, data = temp))
a
a = summary(aov(Simpson ~ Year, data = temp))
a
a = summary(aov(Shannon ~ Year, data = temp))
a

# Beta diversity with bray ------------------------------------------------
library(vegan)
otu2 = t(data.frame(otu_table(d.fin)))
otu2 = decostand(otu2, method = "hellinger")
rowSums(otu2)
otu2 = otu_table(as.matrix(otu2), taxa_are_rows = F)

d3 = merge_phyloseq(tax2, otu2, sample_data(d))
rel_otu_code = data.frame(otu_table(d3))

dist_w = vegdist(rel_otu_code, method = "bray")

###PERMANOVA

a = adonis(dist_w ~ sample_data(d3)$Population*sample_data(d3)$Year, permutations = 999)
a

# Hierarchial clustering --------------------------------------------------

#compressing the phyloseq object at level which is significantly different

d2 = merge_samples(d.fin, "pop.year")
otu3 = data.frame(otu_table(d2))
otu3 = decostand(otu3, method = "hellinger")
rel_otu_int = otu3
rowSums(otu3)
otu3 = round(otu3, 2)

dist_w_int = vegdist(otu3, method = "bray")

otu3_tab = otu_table(as.matrix(otu3), taxa_are_rows = F)
d4 = merge_phyloseq(tax2, otu_table(as.matrix(otu3_tab), 
                                    taxa_are_rows = F), sample_data(d2))

#weighted distance analysis
h = hclust(dist_w_int, method = "average")
dhc <- as.dendrogram(h)
nodePar <- list(lab.cex = 1, pch = c(NA, 19), cex = 0.7, col = "blue")
p = plot(dhc,  xlab = "Weighted Bray-Curtis distance", nodePar = nodePar, horiz = TRUE)
p

# Realtive abundance plots at OTU level ------------------------------------------------

d_f = merge_samples(d.fin, "pop.year")
gen_f = data.frame(otu_table(d_f))
gen_f = t(gen_f)
gen_f = merge(gen_f, tax_table(d_f), by = "row.names")
#gen_f = merge(gen_f, sim.kw.popsize, by.x = "Row.names", by.y = "otu")
gen_f$rank = paste(as.character(gen_f$Row.names),"|",substr(gen_f$Family, 3, 5))
list = as.character(gen_f$rank)
gen_f = gen_f[,-1]
drops <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "rank", "otu", "pval", "p.ad")
gen_f = gen_f[ , !(names(gen_f) %in% drops)]
gen_f = data.frame(t(gen_f))
gen_f = gen_f/rowSums(gen_f)
names(gen_f) = list
who = names(sort(colMeans(gen_f), decreasing = TRUE))[1:50]
f = gen_f[,names(gen_f) %in% who]
f$Other = 1-rowSums(f)
who = c(who, "Other")
dd = f
dd$sl = row.names(dd)
m = melt(dd, id.vars = c("sl"), measure.vars = who)
library(RColorBrewer)
state_col2 = scale_fill_manual(name = "State3", values=c(brewer.pal(n = 5, name = "Blues"),brewer.pal(n = 10, name = "Paired"), "azure3", "burlywood1", "cornflowerblue", "wheat4", "cyan4", "turquoise3", "gold1", "tan2", 
                                                         "springgreen2", "slateblue2", "red3", "navyblue", 
                                                         "magenta", "olivedrab1", "blue2", "black", "yellow1",
                                                         "dodgerblue1", "orangered4", "yellow4", "deeppink4", 
                                                         "slategray4", "seagreen4" , "aquamarine",
                                                         "tomato2", brewer.pal(n = 11, name = "Spectral")))

library(scales)

p = ggplot(m, aes(sl, fill = variable)) + geom_bar(aes(weight = value)) + 
  theme_bw(base_size = 20) + state_col2 + theme(axis.text.x = element_text(angle = 0, hjust=.5, size = 12)) +
  xlab("Sample") + ylab("Relative Abundance") + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black")) +
  theme(legend.text = element_text(face = "italic")) + guides(fill = guide_legend(ncol = 1, reverse=T))+ scale_y_continuous(labels = percent_format())
p$data$variable = factor(p$data$variable, ordered = TRUE, levels = rev(who))
p

# Realtive abundance plots at Family level ------------------------------------------------

d_f = tax_glom(d.fin, taxrank = "Family")
d_f = merge_samples(d_f, "pop.year")
gen_f = data.frame(otu_table(d_f))
gen_f = t(gen_f)
gen_f = merge(gen_f, tax_table(d_f), by = "row.names")
gen_f$rank = as.character(gen_f$Family)
#gen_f$rank = paste(as.character(gen_f$Row.names), "_", gen_f$Family)
gen_f$rank = ifelse(gen_f$Phylum == "unidentified", paste(as.character(gen_f$Kingdom), as.character(gen_f$Phylum), sep = ";"), gen_f$rank)
gen_f$rank = ifelse(gen_f$Phylum != "unidentified" &  gen_f$Class == "unidentified", paste(as.character(gen_f$Phylum), as.character(gen_f$Class), sep = ";"), gen_f$rank)
gen_f$rank = ifelse(gen_f$Class != "unidentified" &  gen_f$Order == "unidentified", paste(as.character(gen_f$Class), as.character(gen_f$Order), sep = ";"), gen_f$rank)
gen_f$rank = ifelse(gen_f$Order != "unidentified" &  gen_f$Family == "unidentified", paste(as.character(gen_f$Order), as.character(gen_f$Family), sep = ";"), gen_f$rank)
list = as.character(gen_f$rank)
list = paste(list, "_", rep(1:length(list)), sep = "")
gen_f = gen_f[,-1]
drops <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "rank")
gen_f = gen_f[ , !(names(gen_f) %in% drops)]
gen_f = data.frame(t(gen_f))
gen_f = gen_f/rowSums(gen_f)
names(gen_f) = list
#met$Sample = ordered(met$Sample, levels = c("A", "B", "C", "D", "E", "F", "G"))
who = names(sort(colMeans(gen_f), decreasing = TRUE))[1:25]
f = gen_f[,names(gen_f) %in% who]
f$Other = 1-rowSums(f)
who = c(who, "Other")
dd = f
dd$sl = row.names(dd)
m = melt(dd, id.vars = c("sl"), measure.vars = who)
library(RColorBrewer)
state_col2 = scale_fill_manual(name = "State3", values=c(brewer.pal(n = 3, name = "Pastel1"), "azure3", "burlywood1", "cornflowerblue", "wheat4", "cyan4", "turquoise3", "hotpink", "tan2", 
                                                         "springgreen2", "slateblue2", "red3", "navyblue", 
                                                         "magenta", "olivedrab1", "blue2", "black", "yellow1",
                                                         "dodgerblue1", "orangered4", "yellow4", "deeppink4", 
                                                         "slategray4", "seagreen4" , "aquamarine",
                                                         "tomato2", brewer.pal(n = 8, name = "Accent")))
library(scales)

p = ggplot(m, aes(sl, fill = variable)) + geom_bar(aes(weight = value)) + 
  theme_bw(base_size = 20) + state_col2 + theme(axis.text.x = element_text(angle = 0, hjust=.5, size = 12)) +
  xlab("Sample") + ylab("Relative Abundance") + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black")) +
  theme(legend.text = element_text(face = "italic")) + guides(fill = guide_legend(ncol = 1, reverse=T))+ scale_y_continuous(labels = percent_format())
p$data$variable = factor(p$data$variable, ordered = TRUE, levels = rev(who))
p

#ggsave(file="jc.treatment.nms.jpg")

###......................................................

# MRM and varition partioning ---------------------------------------------

d.mrm = subset_samples(d.fin, Population == "EO12"| Population == "EO14"| Population == "EO16")
d.mrm2 = merge_samples(d.mrm, "pop.year")
otu.mrm = data.frame(otu_table(d.mrm2))
otu.mrm = decostand(otu.mrm, method = "hellinger")
dist_w_mrm = vegdist(otu.mrm, method = "bray")

growth = read_excel("data/growth_germ.xlsx", sheet = 1)
growth$int = paste(growth$Site,".",gsub("20", "", growth$Year))
growth$int = gsub(" ", "", growth$int)
library(dplyr)
growth2 = group_by(growth, int) 
growth3 = data.frame(summarise_each(growth2, funs(mean(., na.rm = TRUE))))
row.names(growth3) = growth3$int
growth.var = c("Diameter", "Branches", "Involucres")
growth.df = growth3[,growth.var] ####order of rows should be same as community data matrix

my.soil = growth3[,8:31] ####order of rows should be same as community data matrix
my.env = growth3[,32:40]
#install.packages("ecodist")
library(ecodist)

x = MRM(dist(growth.df) ~ dist_w_mrm + dist(my.soil) + dist(my.env), nperm=1000)
summary(lm(dist(growth.df) ~ dist_w_mrm + dist(my.soil) + dist(my.env)))

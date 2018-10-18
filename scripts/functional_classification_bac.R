setwd("C:\\Users\\jaspkaur\\Google Drive\\data_analysis\\chorizanthe\\chori/")

library(readxl)
library(ggplot2)
library(pscl)
library(lme4)
library(vegan)
library(dplyr)

guild <- read.delim(file = "data/functional_table.tsv", 
                    sep = "\t", header = T)
row.names(guild) = guild[,1]
guild = guild[,-1]
guild = t(guild)
guild = data.frame(guild)
guild = select(guild, -c(chloroplasts))

met <- as.data.frame(read_excel("data/met.xlsx", sheet = 2))
row.names(met) = met$code
met$Year = as.factor(met$Year)
met$pop.year = paste(met$Population, ".", gsub("20", "", met$Year))
met$pop.year = gsub(" ", "", met$pop.year)

guil = merge(met, guild, by = "row.names")
#guil = subset(guil, Population == "EO12" | Population  == "EO14" | Population == "EO16")
guil = subset(guil, plot == "mon" | plot == "ger")
guil.bd = guil[,43:131] ##select only numeric columns for permanova
rownames(guil.bd) = guil$code
guil.bd = decostand(guil.bd, method = "hellinger")
rowSums(guil.bd)

dist_w_guild = vegdist(guil.bd, method = "bray")

###PERMANOVA

a = adonis(dist_w_guild ~ guil$Population, permutations = 999)
a

###ajust P-values
p.adjust(a$aov.tab$`Pr(>F)`, method = "bonferroni")

sim = simper(guil.bd, guil$Population)
sim.sum = summary(sim)
sim.df.pop = data.frame(sim.sum$EO12_NPS2)

write.csv(sim.df.pop, file = "simper_bac_func.csv")
#weighted distance analysis

guil$int = paste(guil$Population,".",guil$replicate)
guil2 = group_by(guil, int)
guil2 = guil2[, c(132, 43:131)]
tally(guil2)
guil2 = data.frame(summarise_each(guil2, funs(sum(., na.rm = TRUE))))

rownames(guil2) = guil2$int
guil2 = guil2[,-1]

rel.abun = guil2/rowSums(guil2)

guil3 = decostand(guil2, method = "hellinger")
rel_otu_int = guil3
rowSums(guil3)
guil3 = round(guil3, 2)

dist_w_guild.py = vegdist(guil3, method = "bray")

h = hclust(dist_w_guild.py, method = "average")
dhc <- as.dendrogram(h)
nodePar <- list(lab.cex = 1, pch = c(NA, 19), cex = 0.7, col = "blue")
p = plot(dhc,  xlab = "Weighted Bray-Curtis distance", nodePar = nodePar, horiz = TRUE)
p

colfunc <- colorRampPalette(c("grey", "black"))
library(gplots)
g1 = heatmap.2(as.matrix(guil3), 
               Rowv = as.dendrogram(h), margins = c(10, 10), col = colfunc(100), 
               xlab = "Weighted Bray Curtis dissimilarity distances",
               trace = "none",
               cellnote = guil3, notecex=0.7,
               notecol="white")

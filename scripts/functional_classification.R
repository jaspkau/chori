#setwd("C:/Users/jaspkaur/Google Drive/data_analysis/chorizanthe/chori")

#source("https://bioconductor.org/biocLite.R")
#biocLite("phyloseq")
library(phyloseq)
library(readxl)
library(vegan)
library(devtools)
#devtools::install_github("benjjneb/decontam")
library(decontam)

guild <- read.delim(file = "data/otu_table_fun_guild.guilds.txt", 
                    sep = "\t", header = T)
drop = c("taxonomy", "Taxon", "Taxon.Level", "Trophic.Mode", "Confidence.Ranking",
         "Growth.Morphology", "Trait", "Notes", "Citation.Source")
guild2 = guild[,!(names(guild) %in% drop)]
row.names(guild2) = paste(gsub("denovo", "o", guild2[,1]))
guild2 = guild2[,-1]

library(dplyr) 
guild_agg = group_by(guild2, Guild)
tally(guild_agg)
guild_agg2 = data.frame(summarise_each(guild_agg, funs(sum(., na.rm = TRUE))))   # calculate the annual mean of airt
row.names(guild_agg2) = guild_agg2$Guild
guild_agg2  = guild_agg2[,-1]
guild_agg2 = t(guild_agg2)

met <- as.data.frame(read_excel("data/met.xlsx", sheet = 1))
row.names(met) = met$code
met$Year = as.factor(met$Year)
met$pop.year = paste(met$Population, ".", gsub("20", "", met$Year))
met$pop.year = gsub(" ", "", met$pop.year)

guil = merge(met, guild_agg2, by = "row.names")
guil$plot = as.factor(guil$plot)
guil = subset(guil, plot == "ger"| plot == "mon")

guil.bd = guil[,44:97] ##select only numeric columns for permanova
rownames(guil.bd) = guil$code
guil.bd = decostand(guil.bd, method = "hellinger")
rowSums(guil.bd)

dist_w_guild = vegdist(guil.bd, method = "bray")

###PERMANOVA

a = adonis(dist_w_guild ~ guil$Population*guil$Year, permutations = 999)
a

###ajust P-values
p.adjust(a$aov.tab$`Pr(>F)`, method = "bonferroni")

sim = simper(guil.bd, guil$Population)
sim.sum = summary(sim)
sim.df.pop = data.frame(sim.sum$EO12_NPS2)

write.csv(sim.df.pop, file = "results/simper_fun_func.csv")

###clustering

guil = guil[,c(42,44:97)]
guil = group_by(guil, pop.year)
tally(guil)
guil2 = data.frame(summarise_each(guil, funs(sum(., na.rm = TRUE))))

rownames(guil2) = guil2$pop.year
guil2 = guil2[,-1]
rel.abun = guil2/rowSums(guil2)
write.csv(rel.abun, file = "results/fungal_guild_rel_abun.csv", sep = ",")
guil3 = decostand(guil2, method = "hellinger")
rel_otu_int = guil3
rowSums(guil3)
guil3 = round(guil3, 2)

dist_w_guild.py = vegdist(guil3, method = "bray")

#weighted distance analysis
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

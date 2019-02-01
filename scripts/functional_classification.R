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
met$Year = as.factor(met$Year)
met$replicate = as.factor(met$replicate)
met$pop.year = paste(met$Population, ".", gsub("20", "", met$Year))
met$pop.year = gsub(" ", "", met$pop.year)
met$int = interaction(met$Population, met$replicate)
row.names(met) = met$code

guil = merge(met, guild_agg2, by = "row.names")
guil$plot = as.factor(guil$plot)
guil = subset(guil, plot == "ger"| plot == "mon")

guil.bd = guil[,44:97] ##select only numeric columns for permanova
rownames(guil.bd) = guil$code
guil.bd = decostand(guil.bd, method = "hellinger")
rowSums(guil.bd)

dist_w_guild = vegdist(guil.bd, method = "bray")

###PERMANOVA

a = adonis(dist_w_guild ~ guil$Population, permutations = 999)
a

###ajust P-values
p.adjust(a$aov.tab$`Pr(>F)`, method = "bonferroni")

###clustering

guil = guil[,c(5,44:105)]
guil = group_by(guil, Population)
tally(guil)
guil2 = data.frame(summarise_each(guil, funs(sum(., na.rm = TRUE))))

rownames(guil2) = guil2$Population
guil2 = guil2[,-1]
rel.abun = guil2/rowSums(guil2)
write.csv(rel.abun, file = "results/fungal_guild_rel_abun.csv", sep = ",")

who = names(sort(colMeans(rel.abun), decreasing = TRUE))[1:10]
f = rel.abun[,names(rel.abun) %in% who]
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

p.phy = ggplot(m, aes(sl, fill = variable)) + geom_bar(aes(weight = value)) +
  theme_bw(base_size = 20) + state_col2 + xlab("Sample") + ylab("Relative Abundance") + theme(axis.text.x = element_text(angle = 45, hjust = 0.9, size = 10, color = "black")) +
  theme(legend.text = element_text(face = "italic", size = 10)) + guides(fill = guide_legend(ncol = 1, reverse=T, keywidth = 0.8, keyheight = 0.8))+ scale_y_continuous(labels = percent_format())
p.phy$data$variable = factor(p.phy$data$variable, ordered = TRUE, levels = rev(who))
p.phy

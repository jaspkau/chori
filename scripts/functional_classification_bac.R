#setwd("C:\\Users\\jaspkaur\\Google Drive\\data_analysis\\chorizanthe\\chori/")

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

############relative abundance plot of guilds

guil2 = group_by(guil, Population)
guil2 = guil2[, c(5, 43:131)]
tally(guil2)
guil2 = data.frame(summarise_each(guil2, funs(sum(., na.rm = TRUE))))

rownames(guil2) = guil2$Population
guil2 = guil2[,-1]

rel.abun = guil2/rowSums(guil2)

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
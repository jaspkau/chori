#SIMPER (similaritypercentage) analyses ----------------------------------------------

ind.df = data.frame(otu2)##the taxa should be columns and this otu table is hellinger tranfromed

#Identification of species most responsible for differences among groups of samples
#SIMPER(similaritypercentage), Based on abundance, does not weigh occurrence frequency as indicator species analysis does.

sim = simper(ind.df, sample_data(d.ado)$Population)
sim.sum = summary(sim)
sim.df.pop = data.frame(sim.sum$EO14_EO12)

sim.pop.otus = row.names(sim.df.pop)[1:50]

library(dplyr)

mann.pop.df = ind.df[,names(ind.df) %in% sim.pop.otus]
met.sim = data.frame(sample_data(subset_samples(d.fin2, 
                                                Population == "EO14"|
                                                  Population == "EO12")))
mann.pop.df2 = merge(mann.pop.df, met.sim, by = "row.names")
mann.pop.df2$Population = as.factor(mann.pop.df2$Population)

sim.kw.pop = c()
for(i in 2:51){
  column = names(mann.pop.df2[i])
  k = kruskal.test(mann.pop.df2[,i]~Population, data = mann.pop.df2)$"p.value"
  results = data.frame(otu = paste(column), pval = as.numeric(paste(k)))
  sim.kw.pop = rbind(sim.kw.pop, results)
} 

sim.kw.pop$p.ad = p.adjust(sim.kw.pop$pval, method = "bonferroni")
sim.kw.pop = sim.kw.pop[sim.kw.pop$p.ad <= 0.05,]

funguild.df = read.delim("data/otu_table_fun_guild.guilds.txt", sep = "\t")
funguild.df$X.OTUID = gsub("denovo", "o", funguild.df$X.OTUID)
keep <- c("X.OTUID","Taxon", "Trophic.Mode", "Guild", "Confidence.Ranking")
funguild.df = funguild.df[, (names(funguild.df) %in% keep)]

sim.kw.pop.guild = merge(sim.kw.pop, funguild.df, by.x = "otu", by.y = "X.OTUID")
write.csv(sim.kw.pop.guild, file = "results/simper.eo14_eo12.csv")

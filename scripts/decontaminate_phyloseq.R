
####decontaminate phyloseq object based on frequency and prevelence
df <- as.data.frame(sample_data(decon.d)) # Put sample_data into a ggplot-friendly d
df$LibrarySize <- sample_sums(decon.d)
df <- df[order(df$LibrarySize),]
df$Index <- seq(nrow(df))
library(ggplot2)
p = ggplot(data=df, aes(x=Index, y=LibrarySize, color=Sample_or_Control)) + geom_point()

###combined method of decomtamination based on prevelanec and frequency
sample_data(decon.d)$is.neg <- sample_data(decon.d)$Sample_or_Control == "Control"
contamdf.prev <- isContaminant(decon.d, method="combined", neg="is.neg", conc="DNA_conc", threshold=0.5)
table(contamdf.prev$contaminant)
which(contamdf.prev$contaminant)
decon.d <- prune_taxa(!contamdf.prev$contaminant, decon.d)
decon.d

d.fin = subset_samples(decon.d, Month == "Feb")
d.fin
d.fin = prune_taxa(taxa_sums(d.fin) >= 1, d.fin)
d.fin

####scale envt data according to above sample selection
met2 = data.frame(sample_data(d.fin))
env_met = met2[,cbind(1,2,3,4,5,6,7,8,41,42)]
env = met2[,9:40]
env = scale(env)
met3 = merge(env_met, env, by = "row.names")
row.names(met3) = met3$Row.names

d.fin = merge_phyloseq(tax_table(d.fin), otu_table(d.fin), sample_data(met3))
d.fin
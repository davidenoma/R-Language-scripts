
library('BinMat')
library('dendextend')
library('vegan')
#Clustering on 
ssr <- read.csv('rotimi/NEW SSR SCORE SHEET.csv')
scot <- read.csv('rotimi/SCOT SCORE SHEET.csv')
combined <- read.csv('rotimi/SCOT AND SSR COMBINED.csv')


rownames(ssr) <- ssr[,1]
rownames(scot) <- scot[,1]
rownames(combined) <- scot[,1]

ssr <- ssr[,2:ncol(ssr)]
scot <- scot[,2:ncol(scot)]
combined <- combined[,2:ncol(combined)]
 

ssrCluster <- hclust(dist(ssr))
plot(ssrCluster, main ="SSR Markers")
avg_dend_obj <- as.dendrogram(ssrCluster)
avg_col_dend <- color_branches(avg_dend_obj)
plot(avg_col_dend, main ="SSR Markers")
circlize_dendrogram(avg_col_dend)

plot(hclust(dist(ssr), method = "average"))


#fOR THE SSR MARKERS
vegdist(ssr,method = "jaccard")
ssrJac <- hclust(vegdist(ssr,method = "jaccard"),method = "average")
plot(ssrJac,main ="SSR Markers")
avg_dend_obj <- as.dendrogram(ssrJac)
avg_col_dend <- color_branches(avg_dend_obj)
plot(avg_col_dend, main ="SSR Markers")

#for the SCOT markers
scotJac <- hclust(vegdist(scot,method = "jaccard"),method = "average")
plot(ssrJac,main ="Scot Markers")
avg_dend_obj <- as.dendrogram(ssrJac)
avg_col_dend <- color_branches(avg_dend_obj)
plot(avg_col_dend, main ="SSR Markers")

#FOR combines
comJac <- hclust(vegdist(combined,method = "jaccard"),method = "average")
plot(comJac,main ="SSR and SCOT Markers")
avg_dend_obj <- as.dendrogram(comJac)
avg_col_dend <- color_branches(avg_dend_obj)
plot(avg_col_dend, main ="SSR AND SCOT Markers")
circlize_dendrogram(avg_col_dend)

ssr_upgma <- upgma(ssr)
#circlize_dendrogram(as.dendrogram(upgma(ssr)))
upgma(scot)



avg_dend_obj <- as.dendrogram(ssrCluster)

avg_col_dend <- color_branches(avg_dend_obj)

plot(avg_col_dend, main ="SSR Markers")

circlize_dendrogram(avg_col_dend)
#
scotCluster <- hclust(dist(scot))
avg_dend_obj <- as.dendrogram(scotCluster)
avg_col_dend <- color_branches(avg_dend_obj)
plot(avg_col_dend, main ="SCOT Markers")
circlize_dendrogram(avg_col_dend)

#Clustering on 
ssr <- read.csv('NEW SSR SCORE SHEET.csv')
scot <- read.csv('SCOT SCORE SHEET.csv')
rownames(ssr) <- ssr[,1]
rownames(scot) <- scot[,1]

ssr <- ssr[,2:ncol(ssr)]
scot <- scot[,2:ncol(scot)]

 
ssrCluster <- hclust(dist(ssr))
avg_dend_obj <- as.dendrogram(ssrCluster)
library('dendextend')
avg_col_dend <- color_branches(avg_dend_obj)
plot(avg_col_dend, main ="SSR Markers")
circlize_dendrogram(avg_col_dend)

scotCluster <- hclust(dist(scot))
avg_dend_obj <- as.dendrogram(scotCluster)
avg_col_dend <- color_branches(avg_dend_obj)
plot(avg_col_dend, main ="SCOT Markers")
circlize_dendrogram(avg_col_dend)

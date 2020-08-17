install.packages("ICAMS")
install.packages("mSigBG")

install.packages('PCAWG7')


#start 2:30
library(ICAMS)
library(mSigBG)
library(PCAWG7)
library(philentropy)
library(gplots)
library(factoextra)
library(nloptr)

path <- "vcfs/HepG2_Cis/"
files <- list.files(path)
vcfs <- ReadAndSplitStrelkaSBSVCFs(paste0(path,files), 
                                   names.of.VCFs = gsub(".vcf.gz","",files))

HepG2_Cis <- VCFsToSBSCatalogs(list.of.SBS.vcfs = vcfs$SBS.vcfs,
                               ref.genome = "GRCh37",
                               trans.ranges = trans.ranges.GRCh37,
                               region = "genome")

HepG2_Cis <- HepG2_Cis$catSBS96
PlotCatalogToPdf(catalog = HepG2_Cis, 
                 file = "HepG2_Cis_SBS96.pdf")

# repeat with MCF10A
path <- "vcfs/MCF10A_Cis/"
files <- list.files(path)
vcfs <- ReadAndSplitStrelkaSBSVCFs(paste0(path,files), 
                                   names.of.VCFs = gsub(".vcf.gz","",files))

MCF10A_Cis <- VCFsToSBSCatalogs(list.of.SBS.vcfs = vcfs$SBS.vcfs,
                                ref.genome = "GRCh37",
                                trans.ranges = trans.ranges.GRCh37,
                                region = "genome")

MCF10A_Cis <- MCF10A_Cis$catSBS96
PlotCatalogToPdf(catalog = MCF10A_Cis, 
                 file = "MCF10A_Cis_SBS96.pdf")

HepG2_Car <- ReadCatalog("spectra/HepG2_Car.csv")
HepG2_Car<-as.catalog(object = HepG2_Car,
                      ref.genome = "GRCh37",
                      region = "genome",
                      catalog.type = "counts")
HepG2_Oxa <-ReadCatalog("spectra/HepG2_Oxa.csv")
HepG2_Oxa<-as.catalog(object = HepG2_Oxa,
                      ref.genome = "GRCh37",
                      region = "genome",
                      catalog.type = "counts")
MCF10A_Car <- ReadCatalog("spectra/MCF10A_Car.csv")
MCF10A_Car <-as.catalog(object = MCF10A_Car,
                        ref.genome = "GRCh37",
                        region = "genome",
                        catalog.type = "counts")
PlotCatalogToPdf(catalog = HepG2_Car, file = "HepG2_Car_SBS96.pdf")
PlotCatalogToPdf(catalog = HepG2_Oxa, file = "HepG2_Oxa_SBS96.pdf")
PlotCatalogToPdf(catalog = MCF10A_Car, file = "MCF10A_Car_SBS96.pdf")

#clustering
all.spectra <- cbind(HepG2_Cis,
                     HepG2_Car,
                     HepG2_Oxa,
                     MCF10A_Cis,
                     MCF10A_Car)
cosine.sim <- distance(t(all.spectra), 
                       method = "cosine" ,
                       use.row.names = T)
colnames(cosine.sim) <- rownames(cosine.sim)
heatmap.2(x = cosine.sim,
          dendrogram = "column",
          margins = c(9, 9),
          cex.axis = 0.5,
          symm = TRUE,
          trace = "none")

all.spectra.as.sigs <- TransformCatalog(all.spectra, 
                                        target.catalog.type = "counts.signature")
pc <- prcomp(t(all.spectra.as.sigs), 
             center = TRUE, scale = FALSE, retx = TRUE)
fviz_pca_ind(pc)

##background
HepG2_BG <- ReadCatalog("spectra/Background_HepG2.csv")
HepG2_BG <- as.catalog(object = HepG2_BG,
                       ref.genome = "GRCh37",
                       region = "genome",
                       catalog.type = "counts")
HepG2_BG <- mSigBG::MakeBackgroundInfo(HepG2_BG,"HepG2_BG")

MCF10A_BG <- ReadCatalog("spectra/Background_MCF10A.csv")
MCF10A_BG <- as.catalog(object = MCF10A_BG,
                        ref.genome = "GRCh37",
                        region = "genome",
                        catalog.type = "counts")
MCF10A_BG <- mSigBG::MakeBackgroundInfo(MCF10A_BG,"MCF10A_BG")

HepG2_Cis_noBG <- SeparateSignatureAndSpectra(spectra = HepG2_Cis,
                                              bg.sig.info = HepG2_BG,
                                              start.b.fraction = 0.5,
                                              sig.name = "HepG2_Cis")

HepG2_Car_noBG <- SeparateSignatureAndSpectra(spectra = HepG2_Car,
                                              bg.sig.info = HepG2_BG,
                                              start.b.fraction = 0.5,
                                              sig.name = "HepG2_Car")

HepG2_Oxa_noBG <- SeparateSignatureAndSpectra(spectra = HepG2_Oxa,
                                              bg.sig.info = HepG2_BG,
                                              start.b.fraction = 0.5,
                                              sig.name = "HepG2_Oxa")

MCF10A_Cis_noBG <- SeparateSignatureAndSpectra(spectra = MCF10A_Cis,
                                               bg.sig.info = MCF10A_BG,
                                               start.b.fraction = 0.5,
                                               sig.name = "MCF10A_Cis")

MCF10A_Car_noBG <- SeparateSignatureAndSpectra(spectra = MCF10A_Car,
                                               bg.sig.info = MCF10A_BG,
                                               start.b.fraction = 0.5,
                                               sig.name = "MCF10A_Car")

par(mfrow = c(2, 1),mar = c(2, 5, 6, 1),cex = 0.8,cex.main = 1.4)
PlotSpectraAsSigsWithUncertainty(HepG2_Cis_noBG$inferred.target.spectra)
PlotSpectraAsSigsWithUncertainty(HepG2_Car_noBG$inferred.target.spectra)
PlotSpectraAsSigsWithUncertainty(HepG2_Oxa_noBG$inferred.target.spectra)
PlotSpectraAsSigsWithUncertainty(MCF10A_Cis_noBG$inferred.target.spectra)
PlotSpectraAsSigsWithUncertainty(MCF10A_Car_noBG$inferred.target.spectra)

#plot background
PlotCatalog(HepG2_BG$background.sig)
PlotCatalog(MCF10A_BG$background.sig)


#clustering of samples without background
all.spectra.noBG <- cbind(HepG2_Cis_noBG$inferred.target.spectra,
                          HepG2_Car_noBG$inferred.target.spectra,
                          HepG2_Oxa_noBG$inferred.target.spectra,
                          MCF10A_Cis_noBG$inferred.target.spectra,
                          MCF10A_Car_noBG$inferred.target.spectra)
cosine.sim.noBG <- distance(t(all.spectra.noBG), 
                            method = "cosine" ,
                            use.row.names = T)
rownames(cosine.sim.noBG) <- rownames(cosine.sim)
colnames(cosine.sim.noBG) <- rownames(cosine.sim.noBG)
par(mar=c(1,1,1,1))
heatmap.2(x = cosine.sim.noBG,
          dendrogram = "column",
          margins = c(9, 9),
          cex.axis = 0.5,
          symm = TRUE,
          trace = "none")

one.opt <- function(sig, method = "cosine", invert = -1) {
  
  SBS31<-PCAWG7::signature$genome$SBS96[,"SBS31"]
  SBS35<-PCAWG7::signature$genome$SBS96[,"SBS35"]  
  
  my.obj.fn <- function(coef) {
    recon <- coef[1]*SBS31 + coef[2]*SBS35 
    return(
      suppressMessages(
        invert * philentropy::distance(
          rbind(recon, as.vector(sig)), method = method)))
  }
  
  g_ineq <- function(coef) { abs(1 - sum(coef)) }
  
  retval <- nloptr::nloptr(
    x0          = c(0.5, 0.5),
    eval_f      = my.obj.fn,
    eval_g_ineq = g_ineq,
    lb          = c(0, 0),
    ub          = c(1, 1),
    opt         = list(algorithm = "NLOPT_LN_COBYLA",maxeval = 2000,
                       xtol_rel  = 1e-6, xtol_abs  = 1e-7))
  
  retval <- list(similarity = invert * retval$objective, 
                 coef = retval$solution / sum(retval$solution))
  
  return(retval)
}

one.opt(HepG2_Cis_noBG$inferred.target.sig.as.catalog)
one.opt(HepG2_Car_noBG$inferred.target.sig.as.catalog)
one.opt(MCF10A_Cis_noBG$inferred.target.sig.as.catalog)
one.opt(MCF10A_Car_noBG$inferred.target.sig.as.catalog)install.packages("BiocManager")
BiocManager::install('BSgenome.Hsapiens.1000genomes.hs37d5')
install.packages("philentropy")
install.packages("gplots")
install.packages("factoextra")


remotes::install_github("steverozen/mSigBG", ref = "1.0-branch", force = TRUE)
remotes::install_github("steverozen/PCAWG7", force = TRUE)

#start 2:30
library(ICAMS)
library(mSigBG)
library(PCAWG7)
library(philentropy)
library(gplots)
library(factoextra)
library(nloptr)

path <- "vcfs/HepG2_Cis/"
files <- list.files(path)
vcfs <- ReadAndSplitStrelkaSBSVCFs(paste0(path,files), 
                                   names.of.VCFs = gsub(".vcf.gz","",files))

HepG2_Cis <- VCFsToSBSCatalogs(list.of.SBS.vcfs = vcfs$SBS.vcfs,
                               ref.genome = "GRCh37",
                               trans.ranges = trans.ranges.GRCh37,
                               region = "genome")

HepG2_Cis <- HepG2_Cis$catSBS96
PlotCatalogToPdf(catalog = HepG2_Cis, 
                 file = "HepG2_Cis_SBS96.pdf")

# repeat with MCF10A
path <- "vcfs/MCF10A_Cis/"
files <- list.files(path)
vcfs <- ReadAndSplitStrelkaSBSVCFs(paste0(path,files), 
                                   names.of.VCFs = gsub(".vcf.gz","",files))

MCF10A_Cis <- VCFsToSBSCatalogs(list.of.SBS.vcfs = vcfs$SBS.vcfs,
                                ref.genome = "GRCh37",
                                trans.ranges = trans.ranges.GRCh37,
                                region = "genome")

MCF10A_Cis <- MCF10A_Cis$catSBS96
PlotCatalogToPdf(catalog = MCF10A_Cis, 
                 file = "MCF10A_Cis_SBS96.pdf")

HepG2_Car <- ReadCatalog("spectra/HepG2_Car.csv")
HepG2_Car<-as.catalog(object = HepG2_Car,
                      ref.genome = "GRCh37",
                      region = "genome",
                      catalog.type = "counts")
HepG2_Oxa <-ReadCatalog("spectra/HepG2_Oxa.csv")
HepG2_Oxa<-as.catalog(object = HepG2_Oxa,
                      ref.genome = "GRCh37",
                      region = "genome",
                      catalog.type = "counts")
MCF10A_Car <- ReadCatalog("spectra/MCF10A_Car.csv")
MCF10A_Car <-as.catalog(object = MCF10A_Car,
                        ref.genome = "GRCh37",
                        region = "genome",
                        catalog.type = "counts")
PlotCatalogToPdf(catalog = HepG2_Car, file = "HepG2_Car_SBS96.pdf")
PlotCatalogToPdf(catalog = HepG2_Oxa, file = "HepG2_Oxa_SBS96.pdf")
PlotCatalogToPdf(catalog = MCF10A_Car, file = "MCF10A_Car_SBS96.pdf")

#clustering
all.spectra <- cbind(HepG2_Cis,
                     HepG2_Car,
                     HepG2_Oxa,
                     MCF10A_Cis,
                     MCF10A_Car)
cosine.sim <- distance(t(all.spectra), 
                       method = "cosine" ,
                       use.row.names = T)
colnames(cosine.sim) <- rownames(cosine.sim)
heatmap.2(x = cosine.sim,
          dendrogram = "column",
          margins = c(9, 9),
          cex.axis = 0.5,
          symm = TRUE,
          trace = "none")

all.spectra.as.sigs <- TransformCatalog(all.spectra, 
                                        target.catalog.type = "counts.signature")
pc <- prcomp(t(all.spectra.as.sigs), 
             center = TRUE, scale = FALSE, retx = TRUE)
fviz_pca_ind(pc)

##background
HepG2_BG <- ReadCatalog("spectra/Background_HepG2.csv")
HepG2_BG <- as.catalog(object = HepG2_BG,
                       ref.genome = "GRCh37",
                       region = "genome",
                       catalog.type = "counts")
HepG2_BG <- mSigBG::MakeBackgroundInfo(HepG2_BG,"HepG2_BG")

MCF10A_BG <- ReadCatalog("spectra/Background_MCF10A.csv")
MCF10A_BG <- as.catalog(object = MCF10A_BG,
                        ref.genome = "GRCh37",
                        region = "genome",
                        catalog.type = "counts")
MCF10A_BG <- mSigBG::MakeBackgroundInfo(MCF10A_BG,"MCF10A_BG")

HepG2_Cis_noBG <- SeparateSignatureAndSpectra(spectra = HepG2_Cis,
                                              bg.sig.info = HepG2_BG,
                                              start.b.fraction = 0.5,
                                              sig.name = "HepG2_Cis")

HepG2_Car_noBG <- SeparateSignatureAndSpectra(spectra = HepG2_Car,
                                              bg.sig.info = HepG2_BG,
                                              start.b.fraction = 0.5,
                                              sig.name = "HepG2_Car")

HepG2_Oxa_noBG <- SeparateSignatureAndSpectra(spectra = HepG2_Oxa,
                                              bg.sig.info = HepG2_BG,
                                              start.b.fraction = 0.5,
                                              sig.name = "HepG2_Oxa")

MCF10A_Cis_noBG <- SeparateSignatureAndSpectra(spectra = MCF10A_Cis,
                                               bg.sig.info = MCF10A_BG,
                                               start.b.fraction = 0.5,
                                               sig.name = "MCF10A_Cis")

MCF10A_Car_noBG <- SeparateSignatureAndSpectra(spectra = MCF10A_Car,
                                               bg.sig.info = MCF10A_BG,
                                               start.b.fraction = 0.5,
                                               sig.name = "MCF10A_Car")

par(mfrow = c(2, 1),mar = c(2, 5, 6, 1),cex = 0.8,cex.main = 1.4)
PlotSpectraAsSigsWithUncertainty(HepG2_Cis_noBG$inferred.target.spectra)
PlotSpectraAsSigsWithUncertainty(HepG2_Car_noBG$inferred.target.spectra)
PlotSpectraAsSigsWithUncertainty(HepG2_Oxa_noBG$inferred.target.spectra)
PlotSpectraAsSigsWithUncertainty(MCF10A_Cis_noBG$inferred.target.spectra)
PlotSpectraAsSigsWithUncertainty(MCF10A_Car_noBG$inferred.target.spectra)

#plot background
PlotCatalog(HepG2_BG$background.sig)
PlotCatalog(MCF10A_BG$background.sig)


#clustering of samples without background
all.spectra.noBG <- cbind(HepG2_Cis_noBG$inferred.target.spectra,
                          HepG2_Car_noBG$inferred.target.spectra,
                          HepG2_Oxa_noBG$inferred.target.spectra,
                          MCF10A_Cis_noBG$inferred.target.spectra,
                          MCF10A_Car_noBG$inferred.target.spectra)
cosine.sim.noBG <- distance(t(all.spectra.noBG), 
                            method = "cosine" ,
                            use.row.names = T)
rownames(cosine.sim.noBG) <- rownames(cosine.sim)
colnames(cosine.sim.noBG) <- rownames(cosine.sim.noBG)
par(mar=c(1,1,1,1))
heatmap.2(x = cosine.sim.noBG,
          dendrogram = "column",
          margins = c(9, 9),
          cex.axis = 0.5,
          symm = TRUE,
          trace = "none")

one.opt <- function(sig, method = "cosine", invert = -1) {
  
  SBS31<-PCAWG7::signature$genome$SBS96[,"SBS31"]
  SBS35<-PCAWG7::signature$genome$SBS96[,"SBS35"]  
  
  my.obj.fn <- function(coef) {
    recon <- coef[1]*SBS31 + coef[2]*SBS35 
    return(
      suppressMessages(
        invert * philentropy::distance(
          rbind(recon, as.vector(sig)), method = method)))
  }
  
  g_ineq <- function(coef) { abs(1 - sum(coef)) }
  
  retval <- nloptr::nloptr(
    x0          = c(0.5, 0.5),
    eval_f      = my.obj.fn,
    eval_g_ineq = g_ineq,
    lb          = c(0, 0),
    ub          = c(1, 1),
    opt         = list(algorithm = "NLOPT_LN_COBYLA",maxeval = 2000,
                       xtol_rel  = 1e-6, xtol_abs  = 1e-7))
  
  retval <- list(similarity = invert * retval$objective, 
                 coef = retval$solution / sum(retval$solution))
  
  return(retval)
}

one.opt(HepG2_Cis_noBG$inferred.target.sig.as.catalog)
one.opt(HepG2_Car_noBG$inferred.target.sig.as.catalog)
one.opt(MCF10A_Cis_noBG$inferred.target.sig.as.catalog)
one.opt(MCF10A_Car_noBG$inferred.target.sig.as.catalog)
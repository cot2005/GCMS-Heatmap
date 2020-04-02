# The function will make simple heat map from a normalized data file and save it as a pdf file.

library(gplots)

sGCMSheatmap<-function(gcmsfile, aaCluster = TRUE, sampleCluster = TRUE, outputFilename = "GCMS_heatmap.pdf",
                     xlabel = "Amino Acid", ylabel = "Sample", colorLabel = "Log2FC of vehicle",
                     pdfwidth = 15, pdfheight = 8.75) {
  gcmsdata <- read.csv(gcmsfile, sep = "\t", header = T, row.names = 1)
  gcmsdata <- as.matrix(gcmsdata)
  if (aaCluster == TRUE && sampleCluster == TRUE) {
    heatmapParams <- c(TRUE, TRUE)
    dendro = "both"
  } else if (aaCluster == FALSE && sampleCluster == TRUE) {
    heatmapParams <- c(FALSE, TRUE)
    dendro = "row"
  } else if (aaCluster == TRUE && sampleCluster == FALSE) {
    heatmapParams <- c(TRUE, FALSE)
    dendro = "column"
  } else {
    heatmapParams <- c(FALSE, FALSE)
    dendro = "none"
  }
  pdf(outputFilename, pdfwidth, pdfheight)
  heatmap.2(gcmsdata, scale = "none", col= colorRampPalette(c("dodgerblue3", "white", "firebrick"))(n = 299), 
            cexCol = 1.75, cexRow = 1, density.info = "none", #breaks = col_breaks,
            dendrogram = dendro, Rowv = heatmapParams[2], 
            Colv = heatmapParams[1], trace = "none", srtCol = 45, 
            key.xlab = "Log2FC of vehicle average", key.title = NA, keysize = .55,
            xlab = "Amino Acid", ylab = "Sample", offsetCol = .25, offsetRow = .25, key = T, margins = c(10,15))
  dev.off()
}

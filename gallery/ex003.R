library (gplots)
par (cex.main = .8)
heatmap.2(as.matrix (Guts$normed), margins = c(8,1), cexCol = .95, labRow = NA,
          labCol = paste (substr (names (metadata (Guts)), 4, 12), names (Guts), sep = "\n"),
          key = FALSE, trace = "none", colsep = 1:7, sepwidth = 0.01, 
          main = "Gut Samples Clustered by Functional Annotation")

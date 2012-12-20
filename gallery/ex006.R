library (gplots)
graphics.off()
plot.new ()
title ("Clustering by Multiple Reference Databases")
split.screen(fig=c(2,2), erase = FALSE)
screen (1, new = FALSE)
image(Guts$normed, asp = 1)
screen (2, new = FALSE)
image(Guts$normed)
heatmap.2(as.matrix (Guts$normed), labRow = NA,
          labCol = paste (names (metadata (Guts)), names (Guts), sep = "\n"),
          cexCol = .7, key = FALSE, trace = "none", colsep = 1:7, sepwidth = 0.01,
          sub = "Greengenes")
screen (2, new = FALSE)
heatmap.2(as.matrix (Guts$normed), labRow = NA,
          labCol = paste (names (metadata (Guts)), names (Guts), sep = "\n"),
          cexCol = .7, key = FALSE, trace = "none", colsep = 1:7, sepwidth = 0.01,
          sub = "Greengenes")
screen (3, new = FALSE)
heatmap.2(as.matrix (Guts$normed), labRow = NA,
          labCol = paste (names (metadata (Guts)), names (Guts), sep = "\n"),
          cexCol = .7, key = FALSE, trace = "none", colsep = 1:7, sepwidth = 0.01,
          sub = "Greengenes")
screen (4, new = FALSE)
heatmap.2(as.matrix (Guts$normed), labRow = NA,
          labCol = paste (names (metadata (Guts)), names (Guts), sep = "\n"),
          cexCol = .7, key = FALSE, trace = "none", colsep = 1:7, sepwidth = 0.01,
          sub = "Greengenes")

library (scatterplot3d)
P <- pco (Coral$normed)
pts <- cbind (x = P$vectors[,1], y = P$vectors[,2], z = P$vectors[,3])
f <- scatterplot3d(pts,
  angle = 50, main = "Initial Principal Coordinates Of Thirteen Coral Samples",
  axis = TRUE, box = FALSE, pch = 19, type = "h", lty.hplot = 3,
  xlab = paste ("R**2 =", format (P$values[1], 3)), 
  ylab = paste ("R**2 =", format (P$values[2], 3)),
  zlab = paste ("R**2 =", format (P$values[3], 3)))
text (f$xyz.convert (pts), substr (names (Coral), 4, 12), cex = .7, pos = 4)

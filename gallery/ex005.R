
cc <- collection (guts, 
                  L1 = view (level = "level1"), 
                  L2 = view (level = "level2"), 
                  L3 = view (level = "level3"))
groups (cc) <- c (1,1,1,2,2,3,3)
P1 <- pco (cc$L1)
P2 <- pco (cc$L2)
P3 <- pco (cc$L3)
plot (x = P1$vectors[,1], y = P1$vectors[,2], pch = 19,
      col = factor (groups (cc), labels = c ("blue","purple","plum")),
      main = "Principal Coordinates Analysis\nBy Varying Function Level")
points (x = P2$vectors[,1], y = P2$vectors[,2], pch = 19,
        col = factor (groups (cc), labels = c ("salmon","orange","gold")))
points (x = P3$vectors[,1], y = P3$vectors[,2], pch = 19,
        col = factor (groups (cc), labels = c ("black","grey60","grey80")))
text (x = P1$vectors[,1], y = P1$vectors[,2], labels = names (cc))
legend (...)

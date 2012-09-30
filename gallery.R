##############################################################################
###
### IMAGE GALLERY
###
### The R/matR code below will work "out of the box" to produce the images shown.
### Begin your session by loading matR with:  library (matR)
###
### The code for each visualization is separate.
###
### Just producing rough-and-ready images is even easier, because the majority of 
### code below represents graphical formatting parameters, that is, finishing touches.
###
##############################################################################


cc <- collection (waters, L1 = view (level = "level1"))
library (Matrix)
image (cc$L1, aspect = 1, 
       xlab = NULL, ylab = NULL, sub = NULL, 
       main = "Fresh and Spring Water Samples\nFunction Abundance at Level 1",
       colorkey = TRUE, scales = list (
         x = list (at=1:24, labels = names(cc), rot=45),
         y = list(at=1:28, labels = rownames(cc$L1))))

##############################################################################

l1 <- metadata (Marine) [,c("metadata","env_package",data","misc_param")]
l2 <- metadata (Marine) [,c("metadata","env_package","data","samp_store_temp")]
l3 <- metadata (Marine) [,c("metadata","env_package","data","diss_carb_dioxide")]
l4 <- metadata (Marine) [,c("metadata","env_package","data","atmospheric_data")]
par ("mar" = c (5.1, 10.5, 4.1, 2.1))
boxplot (Marine$normed, 
         main = "Log-Normalized Diversity of\nFunction Abundance in Marine Samples",
         names = paste(l1, ", ", l2, ",\n", l3, ", ", l4, sep = ""),
         show.names = TRUE, las = 2, outpch = 21, outcex = 0.5, cex.lab = 0.8,
         boxwex = 0.6, cex.axis = 0.7, horizontal = TRUE,
         xlab = "1+log2(N), scaled to [0,1] after mean-centering")

##############################################################################

library (gplots)
heatmap.2(as.matrix (Guts$normed), labRow = NA,
          labCol = paste (names (metadata (Guts)), names (Guts), sep = "\n"),
          cexCol = .7, key = FALSE, trace = "none", colsep = 1:7, sepwidth = 0.01,
          main = "Gut Samples Clustered\nby Functional Annotation")


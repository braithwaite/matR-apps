l1 <- metadata (Marine) [,c("metadata","env_package","data","misc_param")]
l2 <- metadata (Marine) [,c("metadata","env_package","data","samp_store_temp")]
l3 <- metadata (Marine) [,c("metadata","env_package","data","diss_carb_dioxide")]
l4 <- metadata (Marine) [,c("metadata","env_package","data","atmospheric_data")]
par ("mar" = c (5.1, 14, 4.1, 2.1))
boxplot (Marine$normed, 
         main = "Log-Normalized Diversity of\nFunction Abundance in Marine Samples",
         names = paste(l1, ", ", l2, ",\n", l3, ", ", l4, sep = ""),
         show.names = TRUE, las = 2, outpch = 21, outcex = 0.5, cex.lab = 0.8,
         boxwex = 0.6, cex.axis = 0.7, horizontal = TRUE,
         xlab = "1+log2(N), scaled to [0,1] after mean-centering")

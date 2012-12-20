
cc <- collection (waters, L1 = view (level = "level1"))
library (Matrix)
image (cc$L1, aspect = 1, 
       xlab = NULL, ylab = NULL, sub = NULL, 
       main = "Fresh and Spring Water Samples: Raw Function Abundance (Level 1)",
       colorkey = TRUE, scales = list (
         x = list (at = 1:24, labels = names (cc), rot = 45),
         y = list(at = 1:28, labels = rownames (cc$L1))))

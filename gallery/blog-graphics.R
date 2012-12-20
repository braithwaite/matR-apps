






Guts$lev1 <- c(entry = "normed",lev="level1")
Guts$lev2 <- c(entry = "normed",lev="level2")
Guts$lev3 <- c(entry="normed",lev="level3")
Guts$func <- c(entry = "normed",lev="fun")
d1 <- stats::dist(t(Guts$lev1))
d2 <- stats::dist(t(Guts$lev2))
d3 <- stats::dist(t(Guts$lev3))
d4 <- stats::dist(t(Guts$func))
p1 <- ecodist::pco(d1)
p2 <- ecodist::pco(d2)
p3 <- ecodist::pco(d3)
p4 <- ecodist::pco(d4)
ptsx <- c(p1$vectors[,1],p2$vectors[,1],p3$vectors[,1],p4$vectors[,1])
ptsy <- c(p1$vectors[,2],p2$vectors[,2],p3$vectors[,2],p4$vectors[,2])
ptsz <- c(p1$vectors[,3],p2$vectors[,3],p3$vectors[,3],p4$vectors[,3])
scatterplot3d(ptsx, ptsy, ptsz, color = c(rep('darkgreen',7),rep('blue',7),rep('black',7), rep('red',7)), pch = c(rep(15,3),rep(17,2),rep(16,2)),cex=1.5)
legend(.8, -.7, c("Function Level 1", "Function Level 2", "Function Level 3", "Function Level 4"), text.col = c("darkgreen","blue","brown","red"),bty="n")
legend(.9, 2.9, c("Cow Rumen", "Fish Gut", "Mouse Gut"), pch = c(0, 2, 1), text.col = "black",bty="n")
grid(30,lty="dotted")
title("Ordination of Gut Metagenomes by Function Level")


Matrix::image(Matrix(Guts$lev1),
			aspect = 1, 
			xlab = NULL, ylab = NULL, sub = NULL, 
			main = "Fresh and Spring Water Samples: Raw Function Abundance (Level 1)",
			colorkey = TRUE,
scales = list (
	x = list (at = 1:7, labels = names (Guts), rot = 45),
	y = list(at = 1:nrow(Guts$lev1), labels = rownames (Guts$lev1))), col= heat.colors(25))

Matrix::image(Matrix(Guts$lev2),
							aspect = 1, 
							xlab = NULL, ylab = NULL, sub = NULL, 
							main = "Fresh and Spring Water Samples: Raw Function Abundance (Level 1)",
							colorkey = TRUE,
							scales = list (
								x = list (at = 1:7, labels = names (Guts), rot = 45),
								y = list(at = 1:nrow(Guts$lev2), labels = rownames (Guts$lev2))), col= heat.colors(200))








functions at level 2
smaller font to fit all row names
colors more pastel --- need to study colors
DOES THIS REALLY MAKE SENSE :: sort rows from most to least average density (when samples are similar/same)
or sort by significance level of some kind.




> d2 <- stats::dist(t(Guts$lev2))
Error in array(x, c(length(x), 1L), if (!is.null(names(x))) list(names(x),  : 
	attempt to set an attribute on NULL
																																 > Guts$lev2 <- c(entry = "normed",lev="level2")
																																 getting view:   normed : function : level2 : Subsystems
																																 Warning: package ‘RJSONIO’ was built under R version 2.14.1
																																 > d2 <- stats::dist(t(Guts$lev2))
																																 > p2 <- ecodist::pco(d2)
																																 > dev.new(width=8,height=10)
																																 > Matrix::image(Matrix(Guts$lev2),
																																 								+ aspect = 1, 
																																 								+ xlab = NULL, ylab = NULL, sub = NULL, 
																																 								+ main = "Fresh and Spring Water Samples: Raw Function Abundance (Level 1)",
																																 								+ colorkey = TRUE,
																																 								+ scales = list (
																																 									+ x = list (at = 1:7, labels = names (Guts), rot = 45),
																																 									+ y = list(at = 1:nrow(Guts$lev2), labels = rownames (Guts$lev2))), col= heat.colors(200))
																																 Error in Matrix::image(Matrix(Guts$lev2), aspect = 1, xlab = NULL, ylab = NULL,  : 
																																 	error in evaluating the argument 'x' in selecting a method for function 'image': Error: could not find function "Matrix"
																																 											 > library(Matrix)
																																 											 Loading required package: lattice
																																 											 
																																 											 Attaching package: ‘Matrix’
																																 											 
																																 											 The following object(s) are masked from ‘package:base’:
																																 											 	
																																 											 	det
																																 											 
																																 											 > Matrix::image(Matrix(Guts$lev2),
																																 											 								+ aspect = 1, 
																																 											 								+ xlab = NULL, ylab = NULL, sub = NULL, 
																																 											 								+ main = "Fresh and Spring Water Samples: Raw Function Abundance (Level 1)",
																																 											 								+ colorkey = TRUE,
																																 											 								+ scales = list (
																																 											 									+ x = list (at = 1:7, labels = names (Guts), rot = 45),
																																 											 									+ y = list(at = 1:nrow(Guts$lev2), labels = rownames (Guts$lev2))), col= heat.colors(200))
																																 											 > plot.new()
																																 											 > ?plot.new
																																 											 > ?frame
																																 											 > ?plot.window()
																																 											 > quartz(width=6,height=10)
																																 											 > Matrix::image(Matrix(Guts$lev2),
																																 											 								+ aspect = 1, 
																																 											 								+ xlab = NULL, ylab = NULL, sub = NULL, 
																																 											 								+ main = "Fresh and Spring Water Samples: Raw Function Abundance (Level 1)",
																																 											 								+ colorkey = TRUE,
																																 											 								+ scales = list (
																																 											 									+ x = list (at = 1:7, labels = names (Guts), rot = 45),
																																 											 									+ y = list(at = 1:nrow(Guts$lev2), labels = rownames (Guts$lev2))), col= heat.colors(200))
																																 											 > Matrix::image(Matrix(Guts$lev2),
																																 											 								+ aspect = 1, 
																																 											 								+ xlab = NULL, ylab = NULL, sub = NULL, 
																																 											 								+ main = "Fresh and Spring Water Samples: Raw Function Abundance (Level 1)",
																																 											 								+ colorkey = TRUE,
																																 											 								+ scales = list (
																																 											 									+ x = list (at = 1:7, labels = names (Guts), rot = 45),
																																 											 									+ y = list(at = 1:nrow(Guts$lev2), labels = rownames (Guts$lev2))), col= heat.colors(200))
																																 											 > ?image-methods
																																 											 Warning in .helpForCall(topicExpr, parent.frame()) :
																																 											 	no method defined for function ‘-’ and signature ‘e1 = "standardGeneric", e2 = "function"’
																																 											 Error in .helpForCall(topicExpr, parent.frame()) : 
																																 											 	no documentation for function ‘-’ and signature ‘e1 = "standardGeneric", e2 = "function"’
																																 											 > 
																																 											 	> ??image
																																 											 > ?Matrix::image-methods
																																 											 Warning in .helpForCall(topicExpr, parent.frame()) :
																																 											 	no method defined for function ‘-’ and signature ‘e1 = "standardGeneric", e2 = "function"’
																																 											 Error in .helpForCall(topicExpr, parent.frame()) : 
																																 											 	no documentation for function ‘-’ and signature ‘e1 = "standardGeneric", e2 = "function"’
																																 											 > methods?iamge
																																 											 Error in `?`(methods, iamge) : 
																																 											 	no documentation of type ‘methods’ and topic ‘iamge’ (or error in processing help)
																																 											 > methods?image
																																 											 > ?levelplot
																																 											 > par(cex=.5)
																																 											 > Matrix::image(Matrix(Guts$lev2),
																																 											 								+ aspect = 1, 
																																 											 								+ xlab = NULL, ylab = NULL, sub = NULL, 
																																 											 								+ main = "Fresh and Spring Water Samples: Raw Function Abundance (Level 1)",
																																 											 								+ colorkey = TRUE,
																																 											 								+ scales = list (
																																 											 									+ x = list (at = 1:7, labels = names (Guts), rot = 45),
																																 											 									+ y = list(at = 1:nrow(Guts$lev2), labels = rownames (Guts$lev2))), col= heat.colors(200))
																																 											 > ?par
																																 											 > par(cex.axis=.5)
																																 											 > Matrix::image(Matrix(Guts$lev2),
																																 											 								+ aspect = 1, 
																																 											 								+ xlab = NULL, ylab = NULL, sub = NULL, 
																																 											 								+ main = "Fresh and Spring Water Samples: Raw Function Abundance (Level 1)",
																																 											 								+ colorkey = TRUE,
																																 											 								+ scales = list (
																																 											 									+ x = list (at = 1:7, labels = names (Guts), rot = 45),
																																 											 									+ y = list(at = 1:nrow(Guts$lev2), labels = rownames (Guts$lev2))), col= heat.colors(200))
																																 											 > ?par
																																 											 > par(cex.lab=.5)
																																 											 > Matrix::image(Matrix(Guts$lev2),
																																 											 								+ aspect = 1, 
																																 											 								+ xlab = NULL, ylab = NULL, sub = NULL, 
																																 											 								+ main = "Fresh and Spring Water Samples: Raw Function Abundance (Level 1)",
																																 											 								+ colorkey = TRUE,
																																 											 								+ scales = list (
																																 											 									+ x = list (at = 1:7, labels = names (Guts), rot = 45),
																																 											 									+ y = list(at = 1:nrow(Guts$lev2), labels = rownames (Guts$lev2))), col= heat.colors(200))
																																 											 > par(cex.lab=4)
																																 											 > Matrix::image(Matrix(Guts$lev2),
																																 											 								+ aspect = 1, 
																																 											 								+ xlab = NULL, ylab = NULL, sub = NULL, 
																																 											 								+ main = "Fresh and Spring Water Samples: Raw Function Abundance (Level 1)",
																																 											 								+ colorkey = TRUE,
																																 											 								+ scales = list (
																																 											 									+ x = list (at = 1:7, labels = names (Guts), rot = 45),
																																 											 									+ y = list(at = 1:nrow(Guts$lev2), labels = rownames (Guts$lev2))), col= heat.colors(200))
																																 											 > ?lattice
																																 											 > ?lattice::histogram
																																 											 > 
																																 											 	> ?xyplot
																																 											 > ??scales
																																 											 > ?axis.default
																																 											 > ?print.trellis
																																 											 > 
																																 											 	> 
																																 											 	> q()
																																 											 Save workspace image? [y/n/c]: n
																																 											 anlextwls003-196:Versions dan$ 
																																 											 	
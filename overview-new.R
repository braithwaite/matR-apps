
########################################################################
##  matR UPGRADE
########################################################################

#  input IDs with or without supplemental metadata:
ss <- readIDSet (file="my_ids.txt")

#  then, two equivalent retrievals of a "biom" object:
yy <- biomRequest (ss, length=20, evalue=4, wait=TRUE)
yy <- biomRequest (file="my_ids.txt", length=20, evalue=4, wait=TRUE)

#  ... any other parameters accepted by the API could go in that call, too.

#  this workflow works too:
xx <- biomRequest ("mgp9 mgp10 mgp11", wait=FALSE)
xx <- biom (xx, wait=FALSE)				# query ready? and return immediately
xx <- biom (xx, wait=TRUE)				# or, wait for it

# this works, provided the two objects have no columns in common:
merge (xx, yy)

#  a simple data transformation:
xx.derived <- transform (xx, t.NA2Zero)

#  a compound transformation:
xx.derived <- transform (xx, t.NA2Zero, t.Log, t.ColCenter, t.ColScale)

#  ... the result is still a "biom" object (so, metadata retained):
class (xx.derived)

#  easy to do same with plain "matrix", if you want:
mm <- as.matrix (xx, expand=TRUE)
mm.derived <- t.ColScale (t.ColCenter (t.Log (t.NA2Zero (mm))))

#  provenance of computed objects is tracked with BIOM id:
xx.derived$id

#  ... as expected, the raw data refers to MG-RAST:
xx$id

#  btw, easy to use matR analyses with BIOM from another source:
zz <- biom (file="my_BIOM_file_from_qiime")

#  the transformation dropped singleton annotations:
setdiff (rownames (xx), rownames (xx.derived))

#  metadata can be searched (by regexp, actually -- not shown here):
columns (xx.derived, "biome")

#  metadata can match multiple fields of course:
columns (xx.derived, "id")
rows (xx.derived, "Bact")

#  analysis functions applied to "biom" objects are "metadata-aware" where appropriate:
prcomp(
	xx.derived, 
	method="Bray", 
	map=c(
		col="biome", 
		pch="seq_meth"))

#  nice colors and shapes were auto-selected ...
#  ... but you can also specify exactly what you want:
prcomp(
	xx.derived, 
	method="Bray", 
	map=c(
		col="biome", 
		pch="seq_meth"),
	col=c(
		"Deserts and xeric shrubland biome"							= "red",
		"Temperate coniferous forest biome"							= "olivedrab4",
		"Temperate grasslands, savannas, and shrubland biome"		= "chocolate4",
		"Tropical and subtropical moist broadleaf forest biome"		= "darkorange",
		"Tundra biome"												= "slateblue"),
	pch=c(
		"WGS (strict)" = 15,
		"WGS (loose)" = 17,
		"AMP" = 9,
		"WGS -> 16S" = 16,
		"ASM (strict)" = 0,
		"ASM (loose)" = 2,
		"ASM -> 16S (strict)" = 1,
		"ASM -> 16S (loose)" = 8))

# can also make a literal substitution of metadata
prcomp(
	xx.derived, 
	labels="$$sample.id")

#  boxplot works for "biom":
boxplot (xx)

#  ... and like prcomp(), it is metadata-aware:
biom (xx, labels="$$sample.biome")

#  to compare normalizations, boxplot two "biom" objects side-by-side:
boxplot (xx, xx.derived)

#  ...and could "as.biom(nn)" to include any "matrix" nn in that plot.

#  use metadata for grouping in significance tests:
row.signif.test <- rowstats (xx.derived, groups="$$sample.biome")
jj <- (row.signif.text$p.value < 0.05)

#  use "rows=jj" to redo PCO computation with only significant annotations:
pp <- prcomp(
	xx.derived,
	rows=jj)

#  this syntax is equivalent:
prcomp(xx.derived [jj, ])

#  "rerender" lets you skip unnecessary recomputation, when changing only appearance of a plot.
#  valuable for large analyses!  a few examples:
prcomp(xx.derived, rows=jj, main="no new computation", "rerender=pp)
prcomp(xx.derived, rows=jj, dim=c(1,2), "rerender=pp)
prcomp(xx.derived, rows=jj, dim=2, "rerender=pp)
prcomp (n.xx, 
	dim=c(1,2),
	map = c (
		col = "sample.data.biom", 
		pch = "treatment"), 
 	cex = 2,
	labels="")

#  the return object has the results of the PCO computation:
str (pp)

#  again, provenance of the computation is recorded ...
#  (this is an idiomatic technique in R):
pp$call

#  usage and return value of heatmap function are consistent with the above:
image (xx.derived)
hh <- image (xx.derived, rows=jj)
image(xx,derived[jj,], rerender=hh)
image(xx.derived, labCol="$$sample.id", rerender=hh)
str (hh)
hh$call


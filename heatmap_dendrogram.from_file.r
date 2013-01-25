heatmap_dendrogram.from_file <- function (

                                 file_in,
                                        #file_out,
                                 figure_type   = "png",                              # c("jpg" "pdf" "ps" or "png") # added this as an input argument 8-10-10
                                 image_out = gsub(" ", "", paste(file_in, ".HD.", figure_type)),
                                 image_title = image_out, # image_out
                                        #x,                                             # x = input_object that contains the data                 

                                       # bells and whistles ...
                                 heat_color1="red",                             # two colors for the the gradient that will be created for the heatmap
                                 heat_color2="green",
                                 palette_n=12,
                                       #key = FALSE,
                                 labRow = NULL,                                      # Kevin 1-27-10 - Dendrogram row labels (NA to remove)
                                 labCol = NULL,                                      # Kevin 1-27-10 - Dendrogram column labels (NA to remove)
                                       # par (las=2 (labels perp to axis)
                                 hclustfun_method = "ward",            # hclustfun_method = c("ward", "single", "complete", "average", "mcquitty", "median" or "centroid")
                                 
                                       # figure output parameters (units vary depending on selected figure_type (bleow)
                                 figure_width  = 1000,                               # usually pixels, inches if eps is selected; png is default
                                 figure_height = 1000,                               # usually pixels, inches if eps is selected; png is default
                                 figure_res    = NA,                                 # usually pixels, inches if eps is selected; png is default      
                                       #figure_type   = "png",                              # c("jpg" "pdf" "ps" or "png") # added this as an input argument 8-10-10
                                       
                                       # dendrogram control
                                 Rowv = TRUE,                                        # <--- Kevin 1-27-10 - FALSE, data are not hclust sorted by row
                                 Colv = if (symm) "Rowv" else TRUE,                  # <--- Kevin 1-27-10 - FALSE data are not hclust sorted by column
                                 distfun = dist,
                                 hclustfun = hclust,                                 # <------ Kevin 2-8-10 - forces "complete" method # made variable directly below 2-24-10
                                        #hclustfun_method = "complete",       # hclustfun_method = c("ward", "single", "complete", "average", "mcquitty", "median" or "centroid") 
                                 dendrogram = "both",                                # dendrogram = c("both","row", "column", "none")
                                 symm = FALSE,
                                 
                                        # data scaling
                                 scale = "none",                                     # scale = c("none", "row", "column")
                                 na.rm = TRUE,
                                 
                                        # image plot
                                 revC = identical(Colv, "Rowv"),
                                 add.expr,
                                 
                                        # mapping data to colors
                                 breaks,
                                 symbreaks = min(x < 0, na.rm = TRUE) || scale != "none",
                                 
                                        # colors
                                 col = "heat.colors", # <------ Kevin 1-27-10 - MADE VARIABLE in loop below

                                        # block sepration
                                 colsep,
                                 rowsep, 
                                 sepcolor = "white",
                                 sepwidth = c(0.05, 0.05),

                                        # cell labeling
                                 cellnote,
                                 notecex = 1, 
                                 notecol = "cyan",
                                 na.color = par("bg"),
                                       
                                        # level trace
                                 trace = "none",                                     # trace = c("column", "row", "both", "none")
                                 tracecol = "cyan",
                                 hline = median(breaks), 
                                 vline = median(breaks),
                                 linecol = tracecol,

                                        # Row/Column Labeling
                                 margins = c(5, 1),                                  ##### <------ Kevin 1-27-10 - specifcy the size of the margins
                                 ColSideColors,
                                 RowSideColors,
                                 row_lab_mult = 2, # <-----                          # used below to adjust font size of row labels - Kevin 3-9-10
                                 col_lab_mult = 3, # <-----                          # used below to adjust font size of column labels - Kevin 3-9-10
                                 cexRow = row_lab_mult*(1/log10(nr)),                # 0.1 + 1/log10(nr),  ##### <------ Kevin 1-27-10 (Dendogram row font size)  
                                 cexCol = col_lab_mult*(1/log10(nc)),                # 0.1 + 1/log10(nc),   ##### <------ Kevin 1-27-10 (Dendogram column font size)
                                        #labRow = NULL,                                      # Kevin 1-27-10 - Dendrogram row labels (NA to remove)
                                        #labCol = NULL,                                      # Kevin 1-27-10 - Dendrogram column labels (NA to remove)
                                 
                                        # color key + density info
                                 key = FALSE,                                         # <------ Kevin 9-28-10 key scaling needs work
                                 keysize = .9,                                       ##### <------ Kevin 1-27-10 size of the key
                                 key_lines = 1,                                      # Kevin ADDED 1-27-10 0=no 1=yes for trace lines in the key (edited in loop below)
                                 key_text = "Key (min to max)",                      #\nand Histogram", # Kevin  1-27-10 - ADDED - MADE VARIABLE
                                 key_text_cex = 0.5,                                 # Kevin made this variable 4-7-10
                                 key_xlabel = NULL,                                  #"Value", # Kevin  1-27-10 - ADDED - MADE VARIABLE 
                                 key_ylabel = NULL,                                  #"Count", # Kevin  1-27-10 - ADDED - MADE VARIABLE
                                 density.info = c("histogram", "density", "none"),
                                 denscol = tracecol,                                 # <------ Kevin 1-27-10 - spcify color for key traceline
                                 symkey = min(x < 0, na.rm = TRUE) || symbreaks,
                                 densadj = 0.25,
                                 
                                        # plot labels 
                                 xlab = NULL,
                                 ylab = NULL,
                                 
                                        # plot layout
                                 lmat = NULL,
                                 lhei = NULL,                                        # <--- line height multiplier
                                 lwid = NULL,
                                 
                                        # extras ...
                                 
                                 ...) # close input arguments
  
{
  
  
###### load the neccessary packages
  library(Cairo)
  library(gplots)
  library(matlab)

###### sub to import the input_file
#import_data <- function(file_name)
  {
    x = data.matrix(read.table(file_in, row.names=1, check.names=FALSE, header=TRUE, sep="\t", comment.char="", quote=""))
  }
  
###### get the diensions of the input object  
  number_entries = (dim(x)[1]) # number rows
  number_samples = (dim(x)[2]) # number columns
  
###### create the "main" or title for the figure - also used as the name of the output file
  main = gsub(" ", "", paste(image_title, "::", hclustfun_method, "_clustering"))
  main = gsub(" ", "", main)

###### Sub function that creates the color palette for the heatmap from selected colors (red to gren is default) 
  #heat_palette<-function (heat_color1, heat_color2, n=palette_n)  
    { 
      ramp <- colorRamp(c(heat_color1, heat_color2))
      custom_palette<<- rgb(ramp(seq(0, 1, length = palette_n)), max = 255)
    }
  #heat_palette()                   # 
  col = custom_palette             # - custom_palette is the output from heat_palette
 
###### Produce output as png (Default)
  if(identical(figure_type, "png")){
    #png_filename = paste(main, "_heatmap_dendrogram.png") # added 5-12-10
    #png_filename = gsub(" ", "", png_filename) # added 6-15-10
    #CairoPNG(png_filename, width = figure_width, height = figure_height, pointsize = 12, res = fiure_res , units = "px")
    CairoPNG(image_out, width = figure_width, height = figure_height, pointsize = 12, res = figure_res , units = "px")
  }
  
###### Produce output as jpeg
  if(identical(figure_type, "jpg")){
    #jpeg_filename = paste(main, "_heatmap_dendrogram.jpg") # added 2-24-10
    #jpeg_filename = gsub(" ", "", jpeg_filename) # added 6-15-10
    #CairoJPEG(jpeg_filename, quality=100, width = figure_width, height = figure_height, res = figure_res, units = "px")  # moved here 6-14-10
    CairoJPEG(image_out, quality=100, width = figure_width, height = figure_height, res = figure_res, units = "px")
  }
    
###### Produce output as pdf
  if(identical(figure_type, "pdf")){
    #pdf_filename = paste(main, "_heatmap_dendrogram.pdf")
    #pdf_filename = gsub(" ", "", pdf_filename)
    #CairoPDF(file = pdf_filename, width = figure_width, height = figure_height, res = fiure_res, units = "px")
    CairoPDF(file = image_out, width = figure_width, height = figure_height, res = figure_res, units = "px")
  }

###### Produce output as eps *** eps figures have their dimensions in inches, not pixels
  if (identical(figure_type, "ps")){
    #ps_filename = paste(main, "_heatmap_dendrogram.ps")
    #ps_filename = gsub(" ", "", ps_filename)
    #CairoPS(file = ps_filename, width = figure_width, height = figure_height, res = fiure_res, units = "px")
    #CairoPS(file = image_out, width = figure_width, height = figure_height, res = figure_res, units = "px")
    #CairoPS(file = image_out, width = figure_width, height = figure_height, print.it = TRUE)
    #CairoPS(file = image_out)
    #postscript(file = image_out, width = figure_width, height = figure_height)
    postscript(file = image_out)
  }



###### Kevin's heavily edited version of gplots heatmap.2
  if (trace=="none"){                                                           # Kevin ADDED 1-27-10 trace line is no also removed from the key
    key_lines = 0  
  }                  
  
  scale01 <- function(x, low = min(x), high = max(x)) {
    x <- (x - low)/(high - low)
    x
  }
  retval <- list()
  scale <- if (symm && missing(scale)) 
    "none"
  else match.arg(scale)
  dendrogram <- match.arg(dendrogram)
  trace <- match.arg(trace)
  density.info <- match.arg(density.info)
  if (length(col) == 1 && is.character(col)) 
    col <- get(col, mode = "function")
  if (!missing(breaks) && (scale != "none")) 
    warning("Using scale=\"row\" or scale=\"column\" when breaks are", 
            "specified can produce unpredictable results.", "Please consider using only one or the other.")
  if (is.null(Rowv) || is.na(Rowv)) 
    Rowv <- FALSE
  if (is.null(Colv) || is.na(Colv)) 
    Colv <- FALSE
  else if (Colv == "Rowv" && !isTRUE(Rowv)) 
    Colv <- FALSE
  if (length(di <- dim(x)) != 2 || !is.numeric(x)) 
    stop("`x' must be a numeric matrix")
  nr <- di[1]
  nc <- di[2]
  if (nr <= 1 || nc <= 1) 
    stop("`x' must have at least 2 rows and 2 columns")
  if (!is.numeric(margins) || length(margins) != 2) 
    stop("`margins' must be a numeric vector of length 2")
  if (missing(cellnote)) 
    cellnote <- matrix("", ncol = ncol(x), nrow = nrow(x))
  if (!inherits(Rowv, "dendrogram")) {
    if (((!isTRUE(Rowv)) || (is.null(Rowv))) && (dendrogram %in% 
                                                 c("both", "row"))) {
      if (is.logical(Colv) && (Colv)) 
        dendrogram <- "column"
      else dedrogram <- "none"
      warning("Discrepancy: Rowv is FALSE, while dendrogram is `", 
              dendrogram, "'. Omitting row dendogram.")
    }
  }
  if (!inherits(Colv, "dendrogram")) {
    if (((!isTRUE(Colv)) || (is.null(Colv))) && (dendrogram %in% 
                                                 c("both", "column"))) {
      if (is.logical(Rowv) && (Rowv)) 
        dendrogram <- "row"
      else dendrogram <- "none"
      warning("Discrepancy: Colv is FALSE, while dendrogram is `", 
              dendrogram, "'. Omitting column dendogram.")
    }
  }
  if (inherits(Rowv, "dendrogram")) {
    ddr <- Rowv
    rowInd <- order.dendrogram(ddr)
  }
  else if (is.integer(Rowv)) {
    hcr <- hclustfun(distfun(x), method = hclustfun_method) # <--- Does the row dendrogram - Kevin 1-27-10 # added the hclustfun_method argument on 2-24-10
    ddr <- as.dendrogram(hcr)
    ddr <- reorder(ddr, Rowv)
    rowInd <- order.dendrogram(ddr)
    if (nr != length(rowInd)) 
      stop("row dendrogram ordering gave index of wrong length")
  }
  else if (isTRUE(Rowv)) {
    Rowv <- rowMeans(x, na.rm = na.rm) # <--- Does the row dendrogram - Kevin 1-27-10
    hcr <- hclustfun(distfun(x),  method = hclustfun_method) # added the hclustfun_method argument on 2-24-10
    ddr <- as.dendrogram(hcr)
    ddr <- reorder(ddr, Rowv)
    rowInd <- order.dendrogram(ddr)
    if (nr != length(rowInd)) 
      stop("row dendrogram ordering gave index of wrong length")
  }
  else {
    rowInd <- nr:1
  }
  if (inherits(Colv, "dendrogram")) {
    ddc <- Colv
    colInd <- order.dendrogram(ddc)
  }
  else if (identical(Colv, "Rowv")) {
    if (nr != nc) 
      stop("Colv = \"Rowv\" but nrow(x) != ncol(x)")
    if (exists("ddr")) {
      ddc <- ddr
      colInd <- order.dendrogram(ddc)
    }
    else colInd <- rowInd
  }
  else if (is.integer(Colv)) {
    hcc <- hclustfun(distfun(if (symm) # <--- Does the column dendrogram - Kevin 1-27-10
                             x
    else t(x)), method = hclustfun_method) # added the hclustfun_method argument on 2-24-10
    ddc <- as.dendrogram(hcc)
    ddc <- reorder(ddc, Colv)
    colInd <- order.dendrogram(ddc)
    if (nc != length(colInd)) 
      stop("column dendrogram ordering gave index of wrong length")
  }
  else if (isTRUE(Colv)) {
    Colv <- colMeans(x, na.rm = na.rm)  # <--- Does the column dendrogram - Kevin 1-27-10
    hcc <- hclustfun(distfun(if (symm) 
                             x
    else t(x)), method = hclustfun_method) # added the hclustfun_method argument on 2-24-10
    ddc <- as.dendrogram(hcc)
    ddc <- reorder(ddc, Colv)
    colInd <- order.dendrogram(ddc)
    if (nc != length(colInd)) 
      stop("column dendrogram ordering gave index of wrong length")
  }
  else {
    colInd <- 1:nc
  }
  retval$rowInd <- rowInd
  retval$colInd <- colInd
  retval$call <- match.call()
  x <- x[rowInd, colInd]
  x.unscaled <- x
  cellnote <- cellnote[rowInd, colInd]
  if (is.null(labRow)) 
    labRow <- if (is.null(rownames(x))) 
      (1:nr)[rowInd]
    else rownames(x)
  else labRow <- labRow[rowInd]
  if (is.null(labCol)) 
    labCol <- if (is.null(colnames(x))) 
      (1:nc)[colInd]
    else colnames(x)
  else labCol <- labCol[colInd]
  if (scale == "row") {
    retval$rowMeans <- rm <- rowMeans(x, na.rm = na.rm)
    x <- sweep(x, 1, rm)
    retval$rowSDs <- sx <- apply(x, 1, sd, na.rm = na.rm)
    x <- sweep(x, 1, sx, "/")
  }
  else if (scale == "column") {
    retval$colMeans <- rm <- colMeans(x, na.rm = na.rm)
    x <- sweep(x, 2, rm)
    retval$colSDs <- sx <- apply(x, 2, sd, na.rm = na.rm)
    x <- sweep(x, 2, sx, "/")
  }
  if (missing(breaks) || is.null(breaks) || length(breaks) < 
      1) {
    if (missing(col) || is.function(col)) 
      breaks <- 16
    else breaks <- length(col) + 1
  }
  if (length(breaks) == 1) {
    if (!symbreaks) 
      breaks <- seq(min(x, na.rm = na.rm), max(x, na.rm = na.rm), 
                    length = breaks)
    else {
      extreme <- max(abs(x), na.rm = TRUE)
      breaks <- seq(-extreme, extreme, length = breaks)
    }
  }
  nbr <- length(breaks)
  ncol <- length(breaks) - 1
  if (class(col) == "function") 
    col <- col(ncol)
  min.breaks <- min(breaks)
  max.breaks <- max(breaks)
  x[x < min.breaks] <- min.breaks
  x[x > max.breaks] <- max.breaks
  if (missing(lhei) || is.null(lhei)) 
    lhei <- c(keysize, 4)
  if (missing(lwid) || is.null(lwid)) 
    lwid <- c(keysize, 4)
  if (missing(lmat) || is.null(lmat)) {
    lmat <- rbind(4:3, 2:1)
    if (!missing(ColSideColors)) {
      if (!is.character(ColSideColors) || length(ColSideColors) != 
          nc) 
        stop("'ColSideColors' must be a character vector of length ncol(x)")
      lmat <- rbind(lmat[1, ] + 1, c(NA, 1), lmat[2, ] + 
                    1)
      lhei <- c(lhei[1], 0.2, lhei[2])
    }
    if (!missing(RowSideColors)) {
      if (!is.character(RowSideColors) || length(RowSideColors) != 
          nr) 
        stop("'RowSideColors' must be a character vector of length nrow(x)")
      lmat <- cbind(lmat[, 1] + 1, c(rep(NA, nrow(lmat) - 
                                         1), 1), lmat[, 2] + 1)
      lwid <- c(lwid[1], 0.2, lwid[2])
    }
    lmat[is.na(lmat)] <- 0
  }
  if (length(lhei) != nrow(lmat)) 
    stop("lhei must have length = nrow(lmat) = ", nrow(lmat))
  if (length(lwid) != ncol(lmat)) 
    stop("lwid must have length = ncol(lmat) =", ncol(lmat))
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  layout(lmat, widths = lwid, heights = lhei, respect = FALSE)
  if (!missing(RowSideColors)) {
    par(mar = c(margins[1], 0, 0, 0.5))
    image(rbind(1:nr), col = RowSideColors[rowInd], axes = FALSE)
  }
  if (!missing(ColSideColors)) {
    par(mar = c(0.5, 0, 0, margins[2]))
    image(cbind(1:nc), col = ColSideColors[colInd], axes = FALSE)
  }
  par(mar = c(margins[1], 0, 0, margins[2]))
  x <- t(x)
  cellnote <- t(cellnote)
  if (revC) {
    iy <- nr:1
    if (exists("ddr")) 
      ddr <- rev(ddr)
    x <- x[, iy]
    cellnote <- cellnote[, iy]
  }
  else iy <- 1:nr
  image(1:nc, 1:nr, x, xlim = 0.5 + c(0, nc), ylim = 0.5 + 
        c(0, nr), axes = FALSE, xlab = "", ylab = "", col = col, 
        breaks = breaks, ...)
  retval$carpet <- x
  if (exists("ddr")) 
    retval$rowDendrogram <- ddr
  if (exists("ddc")) 
    retval$colDendrogram <- ddc
  retval$breaks <- breaks
  retval$col <- col
  if (!invalid(na.color) & any(is.na(x))) {
    mmat <- ifelse(is.na(x), 1, NA)
    image(1:nc, 1:nr, mmat, axes = FALSE, xlab = "", ylab = "", 
          col = na.color, add = TRUE)
  }
  axis(1, 1:nc, labels = labCol, las = 2, line = -0.5, tick = 0, # las = 2 is perp to axis 8-16-10 see ?par
       cex.axis = cexCol)
  if (!is.null(xlab)) 
    mtext(xlab, side = 1, line = margins[1] - 1.25)
  axis(4, iy, labels = labRow, las = 2, line = -0.5, tick = 0,   # las = 2 is perp to axis 8-16-10 see ?par
       cex.axis = cexRow)
  if (!is.null(ylab)) 
    mtext(ylab, side = 4, line = margins[2] - 1.25)
  if (!missing(add.expr)) 
    eval(substitute(add.expr))
  if (!missing(colsep)) 
    for (csep in colsep) rect(xleft = csep + 0.5, ybottom = rep(0, 
                                                    length(csep)), xright = csep + 0.5 + sepwidth[1], 
                              ytop = rep(ncol(x) + 1, csep), lty = 1, lwd = 1, 
                              col = sepcolor, border = sepcolor)
  if (!missing(rowsep)) 
    for (rsep in rowsep) rect(xleft = 0, ybottom = (ncol(x) + 
                                1 - rsep) - 0.5, xright = nrow(x) + 1, ytop = (ncol(x) + 
                                                   1 - rsep) - 0.5 - sepwidth[2], lty = 1, lwd = 1, 
                              col = sepcolor, border = sepcolor)
  min.scale <- min(breaks)
  max.scale <- max(breaks)
  x.scaled <- scale01(t(x), min.scale, max.scale)
  if (trace %in% c("both", "column")) {
    retval$vline <- vline
    vline.vals <- scale01(vline, min.scale, max.scale)
    for (i in colInd) {
      if (!is.null(vline)) {
        abline(v = i - 0.5 + vline.vals, col = linecol, 
               lty = 2)
      }
      xv <- rep(i, nrow(x.scaled)) + x.scaled[, i] - 0.5
      xv <- c(xv[1], xv)
      yv <- 1:length(xv) - 0.5
      lines(x = xv, y = yv, lwd = 1, col = tracecol, type = "s")
    }
  }
  if (trace %in% c("both", "row")) {
    retval$hline <- hline
    hline.vals <- scale01(hline, min.scale, max.scale)
    for (i in rowInd) {
      if (!is.null(hline)) {
        abline(h = i + hline, col = linecol, lty = 2)
      }
      yv <- rep(i, ncol(x.scaled)) + x.scaled[i, ] - 0.5
      yv <- rev(c(yv[1], yv))
      xv <- length(yv):1 - 0.5
      lines(x = xv, y = yv, lwd = 1, col = tracecol, type = "s")
    }
  }
  if (!missing(cellnote)) 
    text(x = c(row(cellnote)), y = c(col(cellnote)), labels = c(cellnote), 
         col = notecol, cex = notecex)
  par(mar = c(margins[1], 0, 0, 0))
  if (dendrogram %in% c("both", "row")) {
    plot(ddr, horiz = TRUE, axes = FALSE, yaxs = "i", leaflab = "none")
  }
  else plot.new()
  par(mar = c(0, 0, if (!is.null(main)) 5 else 0, margins[2]))
  if (dendrogram %in% c("both", "column")) {
    plot(ddc, axes = FALSE, xaxs = "i", leaflab = "none")
  }
  else plot.new()
  if (!is.null(main)) 
    title(main, cex.main = .9 * op[["cex.main"]])
  if (key) {
    par(mar = c(5, 4, 2, 1), cex = 0.75)
    tmpbreaks <- breaks
    if (symkey) {
      max.raw <- max(abs(c(x, breaks)), na.rm = TRUE)
      min.raw <- -max.raw
      tmpbreaks[1] <- -max(abs(x))
      tmpbreaks[length(tmpbreaks)] <- max(abs(x))
    }
    else {
      min.raw <- min(x, na.rm = TRUE)
      max.raw <- max(x, na.rm = TRUE)
    }
    z <- seq(min.raw, max.raw, length = length(col))
    image(z = matrix(z, ncol = 1), col = col, breaks = tmpbreaks, 
          xaxt = "n", yaxt = "n")
    par(usr = c(0, 1, 0, 1))
    lv <- pretty(breaks)
    xv <- scale01(as.numeric(lv), min.raw, max.raw)
    axis(1, at = xv, labels = lv)
    if (scale == "row") 
      mtext(side = 1, "Row Z-Score", line = 2)
    else if (scale == "column") 
      mtext(side = 1, "Column Z-Score", line = 2)
    else mtext(side = 1, key_xlabel, line = 2) # <---- Kevin 1-27-10 (make option) - the x axis label for key - MADE VARIABLE
    if (density.info == "density") { # This is for the "color key + density info" - Kevin 1-27-10
      dens <- density(x, adjust = densadj, na.rm = TRUE)
      omit <- dens$x < min(breaks) | dens$x > max(breaks)
      dens$x <- dens$x[-omit]
      dens$y <- dens$y[-omit]
      dens$x <- scale01(dens$x, min.raw, max.raw)
      if (key_lines > 0){ # Kevin 1-27-10 added loop to make this optional
        lines(dens$x, dens$y/max(dens$y) * 0.95, col = denscol, # <-- This is the part that adds the line trace to the key - Kevin 1-27-10 (make option)
              lwd = 1)
        axis(2, at = pretty(dens$y)/max(dens$y) * 0.95, pretty(dens$y))  # axis 2 is for the "color key + density info" - Kevin 1-27-10
      }
      title(key_text) # Kevin 1-27-10 (changed to a variable argument)
      par(cex = key_text_cex)
      mtext(side = 2, "Density", line = 2)
    }
    else if (density.info == "histogram") { # axis 2 is for the "color key + density info" - Kevin 1-27-10
      h <- hist(x, plot = FALSE, breaks = breaks)
      hx <- scale01(breaks, min.raw, max.raw)
      hy <- c(h$counts, h$counts[length(h$counts)])
      if (key_lines > 0){ # Kevin 1-27-10 added loop to make this optional
        lines(hx, hy/max(hy) * 0.95, lwd = 1, type = "s", # <-- This is the part that adds the line trace to the key - Kevin 1-27-10 (make option)
              col = denscol)
        axis(2, at = pretty(hy)/max(hy) * 0.95, pretty(hy))
      }
      title(key_text) # Kevin 1-27-10 (changed to a variable argument)
      par(cex = key_text_cex) # Kevin 4-7-10 made variable from = cex = 0.5
      mtext(side = 2, key_ylabel, line = 2) # <---- Kevin 1-27-10 (make option) - the y axis label for key - MADE VARIABLE
    }
    else title(key_text) # Kevin 1-27-10 (changed to a variable argument)
  }
  else plot.new()
  retval$colorTable <- data.frame(low = retval$breaks[-length(retval$breaks)], 
                                  high = retval$breaks[-1], color = retval$col)
  invisible(retval)
  
  dev.off()

##### New section Kevin added to kick out files that have the row and column labels 1-10-12
  
  Row_labels_file = gsub(" ", "", paste(file_in,".HD.Row_labels.txt"))
  
  for ( i in (dim(data.matrix(labRow))[1]):1 ){ # order of the rows in labRow is reverse of how they are plotted
    write( labRow[i], file = Row_labels_file, append=TRUE )  
  }
  
  Col_labels_file = gsub(" ", "", paste(file_in,".HD.Col_labels.txt"))
  for ( j in 1:(dim(data.matrix(labCol))[1]) ){ 
    write( labCol[j], file = Col_labels_file, append=TRUE )
  }
    
}

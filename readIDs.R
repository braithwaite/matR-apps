readIDs <- function (filename, ...) {
	y <- read.table (filename, colClasses = "character", ...)
	if (nrow (y) > 1)
		if (ncol (y) > 1) {
			res <- as.character (y [,2])
			names (res) <- as.character (y [,1])
			res
		}
	else as.character (y [,1])
	else unlist (y [1,], use.names = FALSE)
}


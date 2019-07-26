# Author: Robert J. Hijmans
# Sept 2018
# version 1
# license GPL3


.setExtension <- function(filename, value) {
	value <- trimws(value)
	if (value != "" & substr(value, 1, 1) != ".") {
		value <- paste(".", value, sep="") 
	}
	lfn <- nchar(filename)
	fname <- list()
	for (f in 1:length(filename)) {
		extstart <- -1
		for (i in lfn[f] : 2) {
			if (substr(filename[f], i, i) == ".") {
				extstart <- i
				break 
			}
		}
		if (extstart > 0 & (lfn[f] - extstart) < 8) {
			fname[f] <- paste(substr(filename[f], 1, extstart-1), value, sep="")
		} else { 
			fname[f] <- paste(filename[f], value, sep="")  
		}
	}
	return( unlist(fname) ) 
}   



.extension <- function(filename, value=NULL, maxchar=10) {
	if (!is.null(value)) {
		filename <- .setExtension(filename, value)
		return(filename)
	}   
	lfn <- nchar(filename)
	ext <- list()
	for (f in 1:length(filename)) {
		extstart <- -1
		for (i in lfn[f] : 2) {
			if (substr(filename[f], i, i) == ".") {
				extstart <- i
				break
			}
		}
		if (extstart > 0) {
			ext[f] <- substr(filename[f], extstart, lfn[f])
		} else { 
			ext[f] <- "" 
		}   
	}
	ext <- unlist(ext)
	ext[nchar(ext) > maxchar] <- ''
	return(ext)
}   


.get_data <- function(name, path, ext=".rds") {
	name <- .setExtension(name, ext)
	fn <- system.file(file.path(path, name), package="agrin")
	if (!(file.exists(fn))) {
		stop(paste(name, "is not a valid data set name"))
	}
	readRDS(fn)
}


agrin_data <- function(name) {
	name <- tolower(name[1])
	n <- nchar(name)
	x <- .get_data(name, "rds") 
	sp <- substr(name, n-1, n) == "sp"
	if (sp) {
		return(x)
	} else {
		y <- vect(x$geom, type=x$type, atts=x$df, crs=x$crs)
		return(y)
	}	
}


ibli_data <- function(name) {
	.get_data(name, "ibli")
}


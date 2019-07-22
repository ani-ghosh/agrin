# Author: Robert J. Hijmans
# Sept 2018
# version 1
# license GPL3


agrin_data <- function(name) {
	
	name <- name[1]
	raster::extension(name) <- '.rds'
	fn <- system.file(file.path("rds", name), package="agrin")
	if (!(file.exists(fn))) {
		stop(paste(name, 'is not a valid data set name.'))
	}
	readRDS(fn)
	
}




LINTCAS <- function(weather, crop, soil, management, control, level=3) {
	if (level == 3) {
		LINTCAS3(weather, crop, soil, management, control)
	} else if (level == 2) {
		LINTCAS2(weather, crop, soil, management, control)
	} else {
		LINTCAS1(weather, crop, soil, management, control)	
	}
}


LINTCAS3 <- function(weather, crop, soil, management, control) {
	names(weather) <- tolower(names(weather))
	d <- .LC(crop, weather, soil, management, control)
	m <- data.frame(matrix(d[[1]], ncol=length(d[[2]]), byrow=TRUE))
	names(m) <- d[[2]]
	date <- as.Date(control$startDATE) - 1  + m[, "step"]
	data.frame(date=date, m)
}


LC_parameters <- function(x, irri=TRUE) {
	stopifnot(x %in% c("Adiele", "Ezui"))
	if (x == "Adiele") {
		p <- LINTUL2_CASSAVA_PARAMETERS_ADIELE()
	} else { #if (x == "Ezui") {
		p <- LINTUL2_CASSAVA_PARAMETERS_EZUI() 
	}
	p[["IRRIGF"]] <- as.integer(as.logical(irri))
	p
}
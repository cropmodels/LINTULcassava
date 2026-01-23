
compare <- function(x, y) {
	n <- intersect(names(x[[1]]), names(y[[1]]))
	all(sapply(1:length(x), function(i) all.equal(x[[i]][,n], y[[i]][,n])))
}

library(LINTULcassava)
crop <- LC_crop("Adiele", TRUE)
x <- expand.grid(watlim=c(FALSE, TRUE), site=c("Edo", "CRS", "Benue"), year = c(2016, 2017), stringsAsFactors=FALSE)


run <- function(level) {
	mod <- lapply(1:nrow(x), function(i)  {
	  p <- Adiele(x$site[i], x$year[i])
	  p$control$NPK_model <- FALSE
	  p$control$nutrient_limited=FALSE
	  data.frame(Location=x$site[i], LINTCAS(p$weather, crop, p$soil, p$management, 
			control=c(p$control, water_limited=x$watlim[i]), level=level))
	})
	names(mod) <- apply(x, 1, function(i) paste0(c(ifelse(i[1], "wlim", "pot"), i[2:3]), collapse="_"))
	mod
}

m <- readRDS(file.path(system.file(package="LINTULcassava"), "ex/test.rds"))
#tinytest::expect_true(compare(m, run(1)))
#tinytest::expect_true(compare(m, run(2)))
tinytest::expect_equal(m, run(3))


library(LINTULcassava)
crop <- LC_crop("Adiele", TRUE)
y <- expand.grid(NL=c(TRUE, FALSE), watlim=c(FALSE, TRUE), site=c("Edo", "CRS", "Benue"), year = c(2016, 2017), stringsAsFactors=FALSE)
y$name <- c("NL", "none", "NWL", "WL")

runNPK <- function(level) {
	mod <- lapply(1:nrow(y), function(i)  {
	  p <- Adiele(y$site[i], y$year[i], NPK=TRUE)
	  data.frame(Location=y$site[i], 
	  LINTCAS(p$weather, crop, p$soil, p$management, level=level, 
		control=c(p$control, nutrient_limited=y$NL[i], water_limited=y$watlim[i])))  
	})
	names(mod) <- y$name
	mod
}

#r <- readRDS(system.file(package="LINTULcassava", "ex/testNPK.rds"))
#tinytest::expect_true(compare(r, runNPK(1)))
#tinytest::expect_equal(r, runNPK(2))



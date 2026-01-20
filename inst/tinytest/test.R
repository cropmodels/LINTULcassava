
library(LINTULcassava)
crop <- LC_crop("Adiele")
x <- expand.grid(IRRI=c(TRUE, FALSE), site=c("Edo", "CRS", "Benue"), year = c(2016, 2017), stringsAsFactors=FALSE)


run <- function(level) {
	mod <- lapply(1:nrow(x), function(i)  {
	  p <- Adiele(x$site[i], x$year[i])
	  data.frame(Location=x$site[i], LINTCAS(p$weather, crop, p$soil, p$management, control=c(p$control, IRRIGF=x$IRRI[i]), level=level))
	})
	names(mod) <- apply(x, 1, function(i) paste0(c(ifelse(i[1], "pot", "wlim"), i[2:3]), collapse="_"))
	mod
}

m <- readRDS(file.path(system.file(package="LINTULcassava"), "ex/test.rds"))
tinytest::expect_equal(m, run(3))


library(LINTULcassava)
crop <- LC_crop("Adiele", TRUE)
y <- expand.grid(NL=c(TRUE, FALSE), IRRI=c(TRUE, FALSE), site=c("Edo", "CRS", "Benue"), year = c(2016, 2017), stringsAsFactors=FALSE)
y$name <- c("NL", "none", "NWL", "WL")

runNPK <- function(level) {
	mod <- lapply(1:nrow(y), function(i)  {
	  p <- Adiele(y$site[i], y$year[i], NPK=TRUE)
	  data.frame(Location=y$site[i], 
	  LINTCAS(p$weather, crop, p$soil, p$management, control=c(p$control, NUTRIENT_LIMITED=y$NL[i], IRRIGF=y$IRRI[i]), level=level))  
	})
	names(mod) <- y$name
	mod
}

#r <- readRDS(system.file(package="LINTULcassava", "ex/testNPK.rds"))
#x <- runNPK(2)
#isTRUE(all.equal(r, x))



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

#for (i in 1:2) {
#	d <- run(i)
#	nms <- intersect(names(m[[1]]), names(d[[1]]))
#	tinytest::expect_true(
#		all(sapply(1:length(d), function(j) all.equal(m[[j]][,nms], d[[j]][,nms])))
#	)
#}


## NPK, original model
z <- readRDS(system.file(package="LINTULcassava", "ex/edo17npk2.rds"))
p <- Adiele("Edo", 2017, NPK=TRUE)
crop <- LC_crop("Adiele", TRUE)
x <- list()
x$W <- LINTCAS(p$weather, crop, p$soil, p$management, c(p$control, NUTRIENT_LIMITED=F, IRRIGF=F), level=1)
x$WN <- LINTCAS(p$weather, crop, p$soil, p$management, c(p$control, NUTRIENT_LIMITED=T, IRRIGF=F), level=1)
x$N <- LINTCAS(p$weather, crop, p$soil, p$management, c(p$control, NUTRIENT_LIMITED=F, IRRIGF=T), level=1)
x$none <- LINTCAS(p$weather, crop, p$soil, p$management, c(p$control, NUTRIENT_LIMITED=T, IRRIGF=T), level=1)

all.equal(x, z)


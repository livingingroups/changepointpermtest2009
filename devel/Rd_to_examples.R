library("tools")

# setwd("devel")
rd_files <- normalizePath(dir("../man", full.names = TRUE))
outdir <- normalizePath("./examples", mustWork = FALSE)
if (!dir.exists(outdir)) {
    dir.create(outdir)
}


for (file in rd_files) {
    ofi <- file.path(outdir, gsub("\\.Rd$", ".R", basename(file)))
    Rd2ex(file, out = ofi)
}



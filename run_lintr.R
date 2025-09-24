library("lintr")
options(lintr.linter_file = normalizePath("../.lintr"))

lintr::lint_package()

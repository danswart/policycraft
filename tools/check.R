Sys.setenv(NOT_CRAN = "true")
devtools::document()
devtools::test()
devtools::check(args = "--no-manual")

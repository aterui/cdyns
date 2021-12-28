
# build package -----------------------------------------------------------

usethis::use_mit_license()
usethis::use_roxygen_md()
usethis::use_package_doc()
devtools::document()
devtools::load_all()
devtools::check()


# check syntax ------------------------------------------------------------

lintr::lint_package()


# test --------------------------------------------------------------------

usethis::use_testthat()
usethis::use_test("cdynsim")


# coverage ----------------------------------------------------------------

#devtools::build(path='.')
usethis::use_coverage()
covr::package_coverage()
#file.remove("cdyns_0.1.0.tar.gz")

# vigenettes --------------------------------------------------------------



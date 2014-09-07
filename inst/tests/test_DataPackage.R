context("DataPackage")

test_that("datapackage library loads", {
    library(datapackage)
})

test_that("DataPackage constructors work", {
    library(datapackage)
    sysmeta <- SystemMetadata()
    expect_that(sysmeta@serialVersion, equals(1))
    dpkg <- DataPackage()
    expect_that(class(dpkg)[[1]], equals("DataPackage"))
})

test_that("getting country codes works", {
    expect_equal(get_countrycode("Norway"), "NOR")
    expect_equal(get_countrycode(c("Sweden", "Italy")), c("SWE", "ITA"))
    expect_equal(get_countrycode(c("Yugoslavia", "Rest of the World")), c("YUG", "ROW"))
})

test_that("some country codes works", {
    expect_equal(get_countrycode(c("Sweden", NA)), c("SWE", NA))
})

test_that("country names that will not work", {
    expect_error(get_countrycode(23))
    expect_error(get_countrycode(character()))
})

test_that("getting country names works", {
    expect_equal(get_countryname("NOR"), "Norway")
    expect_equal(get_countryname(c("SWE", "ITA")), c("Sweden", "Italy"))
    expect_equal(get_countryname(c("YUG", "ROW")), c("Yugoslavia", "Rest of the World"))
})

test_that("some country names works", {
    expect_equal(get_countryname(c("SWE", NA)), c("Sweden", NA))
})

test_that("country codes that will not work", {
    expect_error(get_countryname(23))
    expect_error(get_countryname(character()))
})

test_that("returns valid countries", {
    expect_equal(is_valid_country("Netherlands"), TRUE)
    expect_equal(is_valid_country(c("Netherlands", "NOR")), c(TRUE, TRUE))
})

test_that("returns some valid countries", {
    expect_equal(is_valid_country(c("SWE", NA)), c(TRUE, FALSE))
})

test_that("will not work as valid country ", {
    expect_error(is_valid_country(23))
    expect_error(is_valid_country(character()))
})

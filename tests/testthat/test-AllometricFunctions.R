context("AllometricFunctions")

expect_similar <- function(input, expected) {
  eval(bquote(expect_lt(abs(input - expected), 0.01)))
}

test_that("sa_from_mass function works as expected", {
  expect_similar(sa_from_mass(2,"insect"), 0.002263431)
  expect_similar(sa_from_mass(50, "salamander"), 0.01271737)
  expect_similar(sa_from_mass(400, "frog"), 0.02836539)
})

test_that("mass_from_length function works as expected", {
  expect_similar(mass_from_length(0.04,"insect"), 0.2629767)
  expect_similar(mass_from_length(0.6,"turtle"), 23675.08)
  expect_similar(mass_from_length(1.8,"snake"), 4270.384)
})

test_that("sa_from_volume function works as expected", {
  expect_similar(sa_from_volume(V=0.001,"lizard"), 0.11)
  expect_similar(sa_from_volume(V=0.01,"frog"), 0.5105748)
  expect_similar(sa_from_volume(V=1,"sphere"), 4.83)
})

test_that("volume_from_length function works as expected", {
  expect_similar(volume_from_length(l=0.05,"lizard"), 3.478309e-06)
  expect_similar(volume_from_length(l=0.15,"frog"), 0.0002885335)
  expect_similar(volume_from_length(l=1,"sphere"), 0.5244873)
})

test_that("sa_from_length function works as expected", {
  expect_similar(sa_from_length(l=0.04), 0.00187877)
})

test_that("prop_silhouette_area function works as expected", {
  expect_similar(prop_silhouette_area(z=60, taxa= "frog"), 0.2902208)
  expect_similar(prop_silhouette_area(z=30, taxa= "grasshopper"), 0.1381)
  expect_similar(prop_silhouette_area(z=30, taxa= "lizard"), 0.2739208)
  expect_similar(prop_silhouette_area(z=30, taxa= "lizard", posture= "elevated"), 0.2106307)
})

test_that("prop_silhouette_area_shapes function works as expected", {
  expect_similar(prop_silhouette_area_shapes(shape="spheroid", theta=60, h=0.01, d=0.001),0.2748564)
  expect_similar(prop_silhouette_area_shapes(shape="cylinder flat ends", theta=60, h=0.01, d=0.001), 0.2744423)
  expect_similar(prop_silhouette_area_shapes(shape="cylinder hemisphere ends", theta=60, h=0.01, d=0.001), 0.2733313)
})

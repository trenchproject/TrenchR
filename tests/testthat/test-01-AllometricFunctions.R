context("AllometricFunctions")

test_that("surface_area_from_mass function works as expected", {

  expect_equal(surface_area_from_mass(2, "insect"), 0.002263431, tolerance = 1e-4)
  expect_equal(surface_area_from_mass(50, "salamander"), 0.01271737, tolerance = 1e-4)
  expect_equal(surface_area_from_mass(400, "frog"), 0.02836539, tolerance = 1e-4)
  expect_equal(surface_area_from_mass(400, "lizard"), 0.0536, tolerance = 1e-4)

})

test_that("mass_from_length function works as expected", {

  expect_equal(mass_from_length(0.04, "insect"), 0.2629767, tolerance = 1e-4)
  expect_equal(mass_from_length(0.6, "turtle"), 23675.08, tolerance = 1e-4)
  expect_equal(mass_from_length(1.8, "snake"), 4270.384, tolerance = 1e-4)
  expect_equal(mass_from_length(1.8, "lizard"), 96701.59, tolerance = 1e-4)
  expect_equal(mass_from_length(1.8, "salamander"), 76872.99, tolerance = 1e-4)
  expect_equal(mass_from_length(1.8, "frog"), 1216842, tolerance = 1e-4)

})

test_that("surface_area_from_volume function works as expected", {

  expect_equal(surface_area_from_volume(V = 0.001, "lizard"), 0.11, tolerance = 1e-4)
  expect_equal(surface_area_from_volume(V = 0.01, "frog"), 0.5105748, tolerance = 1e-4)
  expect_equal(surface_area_from_volume(V = 1, "sphere"), 4.83, tolerance = 1e-4)

})

test_that("volume_from_length function works as expected", {

  expect_equal(volume_from_length(l = 0.05, "lizard"), 3.478309e-06, tolerance = 1e-4)
  expect_equal(volume_from_length(l = 0.15, "frog"), 0.0002885335, tolerance = 1e-4)
  expect_equal(volume_from_length(l = 1, "sphere"), 0.5244873, tolerance = 1e-4)

})

test_that("surface_area_from_length function works as expected", {

  expect_equal(surface_area_from_length(l = 0.04), 0.00187877, tolerance = 1e-4)

})

test_that("proportion_silhouette_area function works as expected", {

  expect_equal(proportion_silhouette_area(psi = 60, taxon = "frog"), 0.2902208, tolerance = 1e-4)
  expect_equal(proportion_silhouette_area(psi = 30, taxon = "grasshopper"), 0.1381, tolerance = 1e-4)
  expect_equal(proportion_silhouette_area(psi = 30, taxon = "lizard"), 0.2739208, tolerance = 1e-4)
  expect_equal(proportion_silhouette_area(psi = 30, taxon= "lizard", raz = 0, posture = "prostrate"), 0.2739208, tolerance = 1e-4)
  expect_equal(proportion_silhouette_area(psi = 30, taxon= "lizard", raz = 90, posture = "prostrate"), 0.3153245, tolerance = 1e-4)
  expect_equal(proportion_silhouette_area(psi = 30, taxon= "lizard", raz = 180, posture = "prostrate"), 0.254965, tolerance = 1e-4)
  expect_equal(proportion_silhouette_area(psi = 30, taxon= "lizard", raz = 0, posture = "elevated"), 0.2106307, tolerance = 1e-4)
  expect_equal(proportion_silhouette_area(psi = 30, taxon= "lizard", raz = 90, posture = "elevated"), 0.2335728, tolerance = 1e-4)
  expect_equal(proportion_silhouette_area(psi = 30, taxon= "lizard", raz = 180, posture = "elevated"), 0.2358103, tolerance = 1e-4)

})

test_that("proportion_silhouette_area_shapes function works as expected", {

  expect_equal(proportion_silhouette_area_shapes(shape = "spheroid", theta = 60, h = 0.01, d = 0.001), 0.2748564, tolerance = 1e-4)
  expect_equal(proportion_silhouette_area_shapes(shape = "cylinder flat ends", theta = 60, h = 0.01, d = 0.001), 0.2744423, tolerance = 1e-4)
  expect_equal(proportion_silhouette_area_shapes(shape = "cylinder hemisphere ends", theta = 60, h = 0.01, d = 0.001), 0.2733313, tolerance = 1e-4)

})

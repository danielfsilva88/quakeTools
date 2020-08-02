context("quakeTools")

test_that("eq_location_clean return is character", {
  expect_that(eq_location_clean("location : test"), is_a("character"))
})

test_that("eq_clean_data return is character", {
  expect_that(eq_location_clean("location : test"), is_a("character"))
})

# Checking Geom classes


test_that("Checking GeomTimeline", {

  # Checking GeomTimeline class
  expect_is(GeomTimeline, "GeomTimeline")
})

test_that("Checking GeomTimelineLabel", {

  # Checking GeomTimelineLabel class
  expect_is(GeomTimelineLabel, "GeomTimelineLabel")
})

# Checking geom_ objects (made with help from link below)
# https://stackoverflow.com/questions/31038709/how-to-write-a-test-for-a-ggplot-plot

p <- earthquakes %>%
  eq_clean_data() %>%
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
  ggplot2::ggplot(ggplot2::aes(x = DATE)) +
  geom_timeline() + geom_timeline_label()

test_that("Checking geom_timeline", {

  # Checking geom_timeline layer class
  expect_is(p$layers[[1]]$geom, "GeomTimeline")
})

test_that("Checking geom_timeline_label", {

  # Checking geom_timeline_label layer class
  expect_is(p$layers[[2]]$geom, "GeomTimelineLabel")
})

q <- earthquakes %>%
  eq_clean_data() %>%
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
  eq_map(annot_col = "DATE")

eq_map
test_that("Checking eq_map", {

  # Checking GeomTimelineLabel classes
  expect_is(q, "leaflet")
  expect_is(q, "htmlwidget")
})

test_that("eq_create_label return is character", {
  expect_that(eq_create_label(data.frame(LOCATION="X", EQ_PRIMARY=2, TOTAL_DEATHS=7)), is_a("character"))
})

test_that("creating action sections work", {
  scenario <- data.frame(watershed = "Upper Sacramento River",
                         action = c(2, 3, 4, 5),
                         start_year = 1980,
                         end_year = 1999,
                         units_of_effort = 1)

  actions <- get_action_matrices(scenario)
  not_uppersac <- DSMscenario::watershed_labels != "Upper Sacramento River"

  expect_true(all(actions$spawn["Upper Sacramento River", as.character(1980:1999)] == 1))
  expect_equal(actions$spawn["Upper Sacramento River", "1979"], 0)
  expect_true(all(actions$spawn[not_uppersac, as.character(1979:1999)] == 0))

  expect_true(all(actions$inchannel["Upper Sacramento River", as.character(1980:1999)] == 1))
  expect_equal(actions$inchannel["Upper Sacramento River", "2000"], 0)
  expect_true(all(actions$inchannel[not_uppersac, as.character(1980:2000)] == 0))

  expect_true(all(actions$floodplain["Upper Sacramento River", as.character(1980:1999)] == 1))
  expect_equal(actions$floodplain["Upper Sacramento River", "2000"], 0)
  expect_true(all(actions$floodplain[not_uppersac, as.character(1980:2000)] == 0))

  expect_true(all(actions$survival["Upper Sacramento River", as.character(1980:1999)] == 1))
  expect_equal(actions$survival["Upper Sacramento River", "2000"], 0)
  expect_true(all(actions$survival[not_uppersac, as.character(1980:2000)] == 0))
})


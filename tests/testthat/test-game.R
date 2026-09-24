testthat::test_that("daily seed is deterministic", {
  testthat::expect_equal(daily_seed(as.Date("2026-09-23")), daily_seed(as.Date("2026-09-23")))
  testthat::expect_false(daily_seed(as.Date("2026-09-23")) == daily_seed(as.Date("2026-09-24")))
})

testthat::test_that("daily puzzle date changes at 1 AM Eastern", {
  before <- as.POSIXct("2026-01-15 00:59:00", tz = "America/New_York")
  after <- as.POSIXct("2026-01-15 01:00:00", tz = "America/New_York")
  testthat::expect_equal(daily_puzzle_date(before), as.Date("2026-01-14"))
  testthat::expect_equal(daily_puzzle_date(after), as.Date("2026-01-15"))
})

testthat::test_that("daily puzzle keeps every target in one province", {
  game_data <- load_game_data(data_dir = test_data_dir)
  puzzle <- make_daily_puzzle(game_data, as.Date("2026-09-23"))
  province_keys <- vapply(puzzle, function(target) normalize_place_key(target$province_name), character(1))
  testthat::expect_length(unique(province_keys), 1)
  testthat::expect_identical(vapply(puzzle, `[[`, character(1), "type"),
                             c("province", "city", "city", "health_zone", "health_zone"))
})

testthat::test_that("area-scaled distances use DRC scale for question 1 and province scale afterward", {
  game_data <- load_game_data(data_dir = test_data_dir)
  puzzle <- make_daily_puzzle(game_data, as.Date("2026-09-23"))
  testthat::expect_gt(puzzle[[1]]$zero_radius_km, puzzle[[2]]$zero_radius_km)
  testthat::expect_equal(puzzle[[2]]$zero_radius_km, puzzle[[4]]$zero_radius_km)
})

testthat::test_that("polygon boundary scoring is separate from city scoring", {
  polygon <- sf::st_polygon(list(matrix(c(-0.1, -0.1, 0.1, -0.1, 0.1, 0.1,
                                          -0.1, 0.1, -0.1, -0.1), ncol = 2, byrow = TRUE)))
  polygon_target <- list(type = "province", name = "test", geometry = polygon,
                         full_radius_km = 0, zero_radius_km = 202)
  inside <- score_guess(0, 0, polygon_target)
  just_outside <- score_guess(0, 0.109, polygon_target)
  testthat::expect_equal(inside$score, 100)
  testthat::expect_equal(just_outside$score, 90)
  testthat::expect_gte(just_outside$score, 0)
  testthat::expect_lte(just_outside$score, 100)

  city_target <- list(type = "city", name = "test", geometry = sf::st_point(c(20, -5)),
                      full_radius_km = 5, zero_radius_km = 500)
  testthat::expect_equal(score_guess(20, -5, city_target)$score, 100)
})

testthat::test_that("share text does not expose target names", {
  results <- list(list(score = 10, target_name = "Secret Province"), list(score = 20, target_name = "Secret City"), list(score = 30, target_name = "Secret Zone"), list(score = 40, target_name = "Secret Zone 2"), list(score = 50, target_name = "Secret Zone 3"))
  text <- make_share_text(results, as.Date("2026-09-23"))
  testthat::expect_false(grepl("Secret", text, fixed = TRUE))
  testthat::expect_true(grepl("150/500", text, fixed = TRUE))
})

daily_puzzle_date <- function(now = Sys.time()) {
  eastern_date <- as.Date(format(now, "%Y-%m-%d", tz = "America/New_York"))
  eastern_hour <- as.integer(format(now, "%H", tz = "America/New_York"))
  if (eastern_hour < 1L) eastern_date <- eastern_date - 1
  eastern_date
}

daily_seed <- function(puzzle_date) {
  value <- utf8ToInt(format(as.Date(puzzle_date), "%Y-%m-%d"))
  as.integer(sum(value * seq_along(value)) %% .Machine$integer.max)
}

normalize_place_key <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  gsub("[^a-z0-9]", "", tolower(x))
}

focus_province_keys <- normalize_place_key(c(
  "Ituri", "Nord-Kivu", "North Kivu", "Sud-Kivu", "South Kivu",
  "Tshopo", "Bas-Uele", "Haut-Uele"
))

scoring_config <- list(
  area_radius_multiplier = 1.5,
  city_full_radius_km = 5,
  polygon_boundary_score = 90,
  polygon_boundary_transition_km = 1,
  minimum_score = 5
)

area_radius_km <- function(area_km2, multiplier = scoring_config$area_radius_multiplier) {
  sqrt(area_km2 / pi) * multiplier
}

drc_provincial_capitals <- c(
  "Kinshasa", "Matadi", "Bandundu", "Mbandaka", "Boende", "Gemena", "Lisala",
  "Kisangani", "Bunia", "Isiro", "Buta", "Goma", "Bukavu", "Kindu", "Kalemie",
  "Lubumbashi", "Kolwezi", "Mbuji-Mayi", "Kananga", "Tshikapa", "Kabinda",
  "Mwene-Ditu", "Kamina", "Kenge", "Lusambo"
)

city_candidates_for_daily_game <- function(cities) {
  city_keys <- normalize_place_key(cities$name)
  province_keys <- normalize_place_key(cities$province_name)
  capitals <- city_keys %in% normalize_place_key(drc_provincial_capitals)
  focus <- province_keys %in% focus_province_keys
  candidates <- cities[capitals | focus, ]
  if (nrow(candidates) < 2) stop("At least two eligible cities are required for the focus provinces and provincial capitals")
  candidates[!duplicated(candidates$id), ]
}

eligible_daily_provinces <- function(game_data) {
  province_keys <- normalize_place_key(game_data$provinces$name)
  focus_rows <- which(province_keys %in% focus_province_keys)
  eligible_cities <- city_candidates_for_daily_game(game_data$cities)
  city_keys <- normalize_place_key(eligible_cities$province_name)
  zone_keys <- normalize_place_key(game_data$health_zones$province_name)
  keep <- vapply(focus_rows, function(i) {
    key <- province_keys[[i]]
    sum(city_keys == key) >= 2L && sum(zone_keys == key) >= 2L
  }, logical(1))
  game_data$provinces[focus_rows[keep], , drop = FALSE]
}

make_daily_puzzle <- function(game_data, puzzle_date) {
  set.seed(daily_seed(puzzle_date))
  eligible_provinces <- eligible_daily_provinces(game_data)
  if (!nrow(eligible_provinces)) {
    stop("No focus province has at least two eligible cities and two health zones")
  }
  province <- eligible_provinces[sample(seq_len(nrow(eligible_provinces)), 1), ]
  province_key <- normalize_place_key(province$name[[1]])
  drc_zero_radius_km <- area_radius_km(game_data$drc_area_km2)
  province_zero_radius_km <- area_radius_km(province$area_km2[[1]])
  eligible_cities <- city_candidates_for_daily_game(game_data$cities)
  eligible_cities <- eligible_cities[normalize_place_key(eligible_cities$province_name) == province_key, , drop = FALSE]
  eligible_zones <- game_data$health_zones[normalize_place_key(game_data$health_zones$province_name) == province_key, , drop = FALSE]
  if (nrow(eligible_cities) < 2) stop("At least two eligible cities are required for the selected province")
  if (nrow(eligible_zones) < 2) stop("At least two eligible health zones are required in the focus provinces")
  cities <- eligible_cities[sample(seq_len(nrow(eligible_cities)), 2), ]
  zones <- eligible_zones[sample(seq_len(nrow(eligible_zones)), 2), ]
  province_target <- list(type = "province", name = province$name[[1]], province_name = province$name[[1]], geometry = province$geometry[[1]], full_radius_km = 0, zero_radius_km = drc_zero_radius_km)
  city_targets <- lapply(seq_len(nrow(cities)), function(i) {
    list(type = "city", name = cities$name[[i]], province_name = cities$province_name[[i]], geometry = cities$geometry[[i]], full_radius_km = scoring_config$city_full_radius_km, zero_radius_km = province_zero_radius_km)
  })
  zone_targets <- lapply(seq_len(nrow(zones)), function(i) {
    list(type = "health_zone", name = zones$name[[i]], province_name = zones$province_name[[i]], geometry = zones$geometry[[i]], full_radius_km = 0, zero_radius_km = province_zero_radius_km)
  })
  c(list(province_target), city_targets, zone_targets)
}

target_label_text <- function(target) {
  if (target$type == "province") {
    sprintf("%s Province", target$name)
  } else {
    location_name <- if (target$type == "health_zone") paste0(target$name, " Health Zone") else target$name
    province_name <- if (!is.null(target$province_name) && nzchar(target$province_name)) {
      label <- target$province_name
      if (!grepl(" Province$", label)) label <- paste0(label, " Province")
      paste0(", ", label)
    } else ""
    sprintf("%s%s", location_name, province_name)
  }
}

target_prompt_text <- function(target) {
  sprintf("%s %s", target_prompt_prefix(target), target_label_text(target))
}

target_prompt_prefix <- function(target) {
  switch(target$type,
    city = "Tap as close as you can to the city of",
    "Tap as close as you can to"
  )
}

score_guess <- function(lng, lat, target) {
  guess <- st_sfc(st_point(c(lng, lat)), crs = 4326)
  target_geom <- st_sfc(target$geometry, crs = 4326)
  inside <- target$type != "city" && lengths(st_intersects(guess, target_geom)) > 0
  distance_km <- if (inside) 0 else as.numeric(st_distance(guess, target_geom, by_element = TRUE)) / 1000
  if (target$type != "city") {
    if (inside) {
      score <- 100
    } else {
      # Crossing the boundary creates an intentional immediate penalty,
      # after which the score decays linearly with distance.
      transition_km <- min(scoring_config$polygon_boundary_transition_km,
        target$zero_radius_km - 0.001)
      decay <- max(0, min(1, (target$zero_radius_km - max(distance_km, transition_km)) /
        (target$zero_radius_km - transition_km)))
      score <- round(scoring_config$minimum_score +
        (scoring_config$polygon_boundary_score - scoring_config$minimum_score) * decay)
    }
  } else {
    decay <- max(0, min(1, (target$zero_radius_km - distance_km) /
      (target$zero_radius_km - target$full_radius_km)))
    score <- round(scoring_config$minimum_score +
      (100 - scoring_config$minimum_score) * decay)
  }
  list(score = as.numeric(score), distance_km = distance_km, target_name = target$name, target_display = target_label_text(target), type = target$type)
}

reveal_target <- function(proxy, target) {
  if (target$type == "city") {
    addCircleMarkers(proxy, data = st_sf(geometry = st_sfc(target$geometry, crs = 4326)), radius = 10,
                     color = "#9B5DE5", fillColor = "#9B5DE5", fillOpacity = 0.9, weight = 3,
                     group = "revealed target")
  } else {
    addPolygons(proxy, data = st_sf(geometry = st_sfc(target$geometry, crs = 4326)),
                color = "#FF006E", weight = 4, opacity = 1,
                fillColor = "#FF006E", fillOpacity = 0.32,
                group = "revealed target")
  }
}

make_share_text <- function(results, puzzle_date) {
  scores <- vapply(results, function(x) x$score, numeric(1))
  paste(c(sprintf("DRC Daily Map: %d/500", sum(scores)), paste(scores, collapse = " · ")), collapse = "\n")
}

imagery_attribution <- function() {
  configured <- Sys.getenv("DRC_MAP_ATTRIBUTION", unset = "Sentinel-2 cloudless 2024 · EOX IT Services GmbH · Contains modified Copernicus Sentinel data")
  configured
}

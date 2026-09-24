library(sf)

load_game_data <- function(data_dir = "data", use_cache = TRUE) {
  cache_path <- file.path(data_dir, "game_data.rds")
  source_files <- c(
    file.path(data_dir, "GRID_COD_v8_0_province_dissolve_HZ.shp"),
    file.path(data_dir, "GRID3_COD_health_zones_v8_0.shp"),
    file.path(data_dir, "cities.csv")
  )
  cache_exists <- file.exists(cache_path)
  source_files_present <- all(file.exists(source_files))
  cache_current <- cache_exists && (!source_files_present ||
    max(file.info(source_files)$mtime) <= file.info(cache_path)$mtime)
  if (use_cache && cache_current && Sys.getenv("DRC_REBUILD_DATA", unset = "0") != "1") {
    cached <- readRDS(cache_path)
    if ("province_name" %in% names(cached$cities) && "province_name" %in% names(cached$health_zones) &&
        "area_km2" %in% names(cached$provinces) && "drc_area_km2" %in% names(cached) &&
        "health_zones_display_by_province" %in% names(cached)) return(cached)
  }

  find_first <- function(...) {
    candidates <- c(...)
    existing <- candidates[file.exists(candidates)]
    if (!length(existing)) stop(sprintf("Could not find any of: %s", paste(candidates, collapse = ", ")))
    existing[[1]]
  }

  read_layer <- function(path, name_field = NULL) {
    x <- st_read(path, quiet = TRUE)
    if (is.na(st_crs(x))) stop(sprintf("Missing CRS in %s", path))
    x <- st_transform(x, 4326)
    valid <- st_is_valid(x)
    invalid <- which(!is.na(valid) & !valid)
    if (length(invalid)) st_geometry(x)[invalid] <- st_make_valid(st_geometry(x)[invalid])
    if (!is.null(name_field)) {
      if (!name_field %in% names(x)) stop(sprintf("Field '%s' is missing from %s", name_field, path))
      x$name <- as.character(x[[name_field]])
    }
    x
  }

  provinces <- read_layer(
    find_first(file.path(data_dir, "GRID_COD_v8_0_province_dissolve_HZ.shp"), file.path(data_dir, "provinces.shp"), file.path(data_dir, "provinces.geojson")),
    name_field = if (file.exists(file.path(data_dir, "GRID_COD_v8_0_province_dissolve_HZ.shp"))) "province" else NULL
  )
  health_zones <- read_layer(
    find_first(file.path(data_dir, "GRID3_COD_health_zones_v8_0.shp"), file.path(data_dir, "health_zones.shp"), file.path(data_dir, "health_zones.geojson")),
    name_field = if (file.exists(file.path(data_dir, "GRID3_COD_health_zones_v8_0.shp"))) "zonesante" else NULL
  )
  border_path <- file.path(data_dir, "drc_border.shp")
  drc_border <- if (file.exists(border_path)) {
    read_layer(border_path)
  } else {
    st_sf(name = "Democratic Republic of the Congo", geometry = st_union(provinces))
  }
  if (!"name" %in% names(provinces) || !"name" %in% names(health_zones)) stop("Each boundary layer needs a name field")
  cities <- read.csv(file.path(data_dir, "cities.csv"), stringsAsFactors = FALSE)

  required_city_fields <- c("id", "name", "longitude", "latitude")
  if (!all(required_city_fields %in% names(cities))) stop("cities.csv is missing required fields")
  if (anyDuplicated(cities$id) || anyDuplicated(cities$name)) stop("City IDs and names must be unique")
  if (any(!between(cities$longitude, -180, 180)) || any(!between(cities$latitude, -90, 90))) stop("Invalid city coordinates")
  cities$geometry <- st_as_sf(cities, coords = c("longitude", "latitude"), crs = 4326)$geometry
  cities <- st_as_sf(cities)

  province_for_points <- function(points, provinces) {
    matches <- st_intersects(points, provinces)
    vapply(matches, function(index) if (length(index)) provinces$name[[index[[1]]]] else "Democratic Republic of the Congo", character(1))
  }
  cities$province_name <- province_for_points(cities, provinces)
  if ("province" %in% names(health_zones)) {
    health_zones$province_name <- as.character(health_zones$province)
  } else {
    zone_matches <- st_intersects(health_zones, provinces)
    health_zones$province_name <- vapply(zone_matches, function(index) if (length(index)) provinces$name[[index[[1]]]] else "Democratic Republic of the Congo", character(1))
  }

  normalize_data_key <- function(x) {
    x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
    gsub("[^a-z0-9]", "", tolower(x))
  }
  health_zones$province_key <- normalize_data_key(health_zones$province_name)

  display_source <- health_zones[, c("province_name", "province_key")]
  display_projected <- st_transform(display_source, 3857)
  display_projected <- st_simplify(display_projected, dTolerance = 100, preserveTopology = TRUE)
  health_zones_display <- st_transform(display_projected, 4326)
  health_zones_display_by_province <- split(health_zones_display, health_zones_display$province_key)

  area_km2 <- function(x) {
    sum(as.numeric(st_area(st_transform(x, 6933)))) / 1e6
  }
  provinces$area_km2 <- vapply(seq_len(nrow(provinces)), function(i) area_km2(provinces[i, ]), numeric(1))
  drc_area_km2 <- area_km2(drc_border)

  # Keep the original shapefiles authoritative, but store lighter runtime
  # geometries in the cache. The tolerance is far below the game's scoring
  # scale and avoids deserializing unnecessary source attributes at startup.
  simplify_runtime <- function(x, tolerance_m) {
    projected <- st_transform(x, 3857)
    projected <- st_simplify(projected, dTolerance = tolerance_m, preserveTopology = TRUE)
    st_transform(projected, 4326)
  }
  provinces <- provinces[, c("name", "area_km2", "geometry")]
  health_zones <- health_zones[, c("name", "province_name", "province_key", "geometry")]
  provinces <- simplify_runtime(provinces, 50)
  health_zones <- simplify_runtime(health_zones, 25)
  drc_border <- simplify_runtime(drc_border, 50)

  list(provinces = provinces, health_zones = health_zones,
       health_zones_display_by_province = health_zones_display_by_province,
       cities = cities, drc_border = drc_border, drc_area_km2 = drc_area_km2)
}

between <- function(x, low, high) x >= low & x <= high

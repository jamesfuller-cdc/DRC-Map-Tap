source("R/data.R")

game_data <- load_game_data(data_dir = "data", use_cache = FALSE)
saveRDS(game_data, file = "data/game_data.rds", compress = "gzip")

message("Saved preprocessed game data to data/game_data.rds")
message(sprintf("Provinces: %d; health zones: %d; cities: %d", nrow(game_data$provinces), nrow(game_data$health_zones), nrow(game_data$cities)))

# Load required library
library(av)
library(progress)

# === USER PARAMETERS ===
start_time <- as.POSIXct("2024-04-05 20:00:00", tz = "UTC")
end_time <- as.POSIXct("2025-04-30 01:36:00", tz = "UTC")
interval <- 60 * 60 * 3  # in seconds
use_fixed_interval <- FALSE  # TRUE = use fixed interval; FALSE = estimate from end_time
test_mode <- FALSE            # TRUE = metadata only, no copying or image extraction

# Filename metadata
cruise_id <- "NF2402"
time_format <- "%Y%m%dT%H%M%SZ"
lander_number <- "LND02"
vehicle_name <- "ALBEX"
camera_direction <- "FWD"
quality <- "HD"
detail <- "20s-SH16-AWB-ISO200"

# Directories
source_dir <- "J:\\NF2503\\NF2503\\Bender Recovery\\NF2402_URIALBEX02002\\Raw\\Camera_Rayfin\\Videos"
target_dir <- "J:\\NF2503\\NF2503\\Bender Recovery\\NF2402_URIALBEX02002\\Processed\\Camera_Rayfin\\Videos_Corrected"
images_dir <- "J:\\NF2503\\NF2503\\Bender Recovery\\NF2402_URIALBEX02002\\Processed\\Camera_Rayfin\\Stills_from_Videos_Corrected"
file_extension <- ".mp4"
log_csv <- "J:\\NF2503\\NF2503\\Bender Recovery\\NF2402_URIALBEX02002\\Processed\\Camera_Rayfin\\Video_Log_Corrected.csv"

# === SETUP ===
if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)
if (!dir.exists(images_dir) && !test_mode) dir.create(images_dir, recursive = TRUE)

# === HELPER FUNCTIONS ===
format_interval <- function(seconds) {
  hrs <- floor(seconds / 3600)
  mins <- floor((seconds %% 3600) / 60)
  secs <- round(seconds %% 60)
  sprintf("%02d:%02d:%02d", hrs, mins, secs)
}

check_and_fix_moov_atom <- function(input_path, output_path) {
  cmd_fix <- sprintf(
    '"C:/ffmpeg/bin/ffmpeg.exe" -y -i "%s" -c:v libx264 -preset slow -crf 18 -c:a copy -movflags +faststart "%s"',
    input_path,
    output_path
  )
  system(cmd_fix, intern = TRUE)
}


extract_order <- function(filename) {
  base <- tools::file_path_sans_ext(basename(filename))
  match <- regmatches(base, regexpr("(?<=_)([0-9]+)$", base, perl = TRUE))
  if (length(match) == 0) return(-1)
  return(as.numeric(match))
}

sort_files_by_suffix <- function(filenames) {
  extract_keys <- function(filepath) {
    filename <- basename(filepath)
    base <- tools::file_path_sans_ext(filename)
    if (grepl("_[0-9]+$", base)) {
      prefix <- sub("_[0-9]+$", "", base)
      suffix <- as.numeric(sub("^.*_([0-9]+)$", "\\1", base))
    } else {
      prefix <- base
      suffix <- 0
    }
    return(data.frame(filepath = filepath, prefix = prefix, suffix = suffix, stringsAsFactors = FALSE))
  }
  keys_df <- do.call(rbind, lapply(filenames, extract_keys))
  filenames[order(keys_df$prefix, keys_df$suffix)]
}

# === PROCESSING ===
video_files <- list.files(source_dir, pattern = paste0("*\\", file_extension), full.names = TRUE)
ordered_files <- sort_files_by_suffix(video_files)
n_files <- length(ordered_files)

if (!use_fixed_interval) {
  interval <- as.numeric(difftime(end_time, start_time, units = "secs")) / (n_files - 1)
}
interval_hms <- format_interval(interval)
timestamps <- start_time + seq(0, by = interval, length.out = n_files)
timestamp_strings <- format(timestamps, time_format)

log_df <- data.frame(
  original_filename = character(n_files),
  new_filename = character(n_files),
  timestamp = character(n_files),
  file_size_MB = numeric(n_files),
  video_duration_sec = numeric(n_files),
  image_filename = character(n_files),
  interval_hms = character(n_files),
  stringsAsFactors = FALSE
)

pb <- progress_bar$new(
  format = "  Processing videos [:bar] :percent ETA: :eta",
  total = n_files, clear = FALSE, width = 60
)

for (i in seq_along(ordered_files)) {
  original_path <- ordered_files[i]
  new_name <- paste0(cruise_id, "_", timestamp_strings[i], "_", 
                     lander_number, "_", vehicle_name, "_", camera_direction, "_",
                     quality, "_", detail, file_extension)
  new_path <- file.path(target_dir, new_name)
  image_name <- paste0(tools::file_path_sans_ext(new_name), ".jpg")
  image_path <- file.path(images_dir, image_name)

  size_MB <- file.info(original_path)$size / (1024^2)
  metadata <- av_media_info(original_path)
  duration_sec <- metadata$duration

  if (!test_mode) {
    temp_path <- tempfile(fileext = file_extension)
    check_and_fix_moov_atom(original_path, temp_path)
    file.copy(from = temp_path, to = new_path, overwrite = TRUE)
    unlink(temp_path)  # Remove temporary file

    middle_time <- duration_sec / 2
    timestamp <- format(as.POSIXct(middle_time, origin = "1970-01-01", tz = "UTC"), "%H:%M:%S")

    cmd <- sprintf(
      '"C:/ffmpeg/bin/ffmpeg.exe" -y -ss %s -i "%s" -frames:v 1 "%s"',
      timestamp,
      original_path,
      image_path
    )
    system(cmd, intern = TRUE)
  }

  log_df[i, ] <- list(
    original_filename = basename(original_path),
    new_filename = new_name,
    timestamp = timestamp_strings[i],
    file_size_MB = round(size_MB, 2),
    video_duration_sec = round(duration_sec, 2),
    image_filename = image_name,
    interval_hms = interval_hms
  )
  pb$tick()
}

# === WRITE METADATA LOG ===
con <- file(log_csv, "w")
writeLines(paste("start_time,", format(start_time, "%Y-%m-%d %H:%M:%S UTC")), con)
if (!use_fixed_interval) {
  writeLines(paste("end_time,", format(end_time, "%Y-%m-%d %H:%M:%S UTC")), con)
}
write.table(log_df, file = con, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)
close(con)

if (test_mode) message("✅ Test mode: no files were copied or images extracted.")

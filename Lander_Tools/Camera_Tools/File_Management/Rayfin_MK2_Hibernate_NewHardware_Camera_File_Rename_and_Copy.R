# Load required libraries
library(av)
library(progress)
library(stringr)

# === USER PARAMETERS ===
test_mode <- FALSE  # Set TRUE for dry run without copying or extracting

# Metadata constants
cruise_id <- "NF2503"
lander_number <- "LND06"
vehicle_name <- "ALBEX"
camera_direction <- "FWD"
quality <- "HD"
duration <- "5s"

# Directories
videos_or_images <- "Videos"
#videos_or_images <- "Stills"
source_dir <- paste0("Y:\\NF2503\\T-100 Trial (complete)\\NF2503_URIALBEX061\\Raw\\Camera_Rayfin\\", videos_or_images)
target_dir <- paste0("Y:\\NF2503\\T-100 Trial (complete)\\NF2503_URIALBEX061\\Processed\\Camera_Rayfin\\", videos_or_images)
image_dir <- "Y:\\NF2503\\T-100 Trial (complete)\\NF2503_URIALBEX061\\Processed\\Camera_Rayfin\\"
images_dir <- file.path(image_dir, "Stills_from_Videos")
log_csv <- file.path(image_dir, paste0(videos_or_images, "_Log.csv"))
corrupt_dir <- file.path(image_dir, "Corrupted Files")

# File extensions of interest
allowed_ext <- c("mp4", "jpg", "jpeg", "png")

# Setup output directories
if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)
if (!dir.exists(images_dir) && !test_mode && videos_or_images == "Videos") dir.create(images_dir, recursive = TRUE)
if (!dir.exists(corrupt_dir)) dir.create(corrupt_dir, recursive = TRUE)

# Helper to clean filename suffixes like (1)
clean_filename <- function(name) {
  sub("\\(\\d+\\)", "", name)
}

# Helper to parse metadata from filename
parse_metadata <- function(filename) {
  base <- tools::file_path_sans_ext(basename(filename))
  base <- clean_filename(base)
  
  match <- str_match(base, "sh(\\d+)_iso(\\d+)_(video|still)-(\\d{8})-(\\d{6})")
  if (any(is.na(match))) return(NULL)
  
  list(
    shutter = match[2],
    iso = paste0("ISO", match[3]),
    type = match[4],
    timestamp = paste0(match[5], "T", match[6], "Z")
  )
}

# Get all media files
all_files <- list.files(source_dir, full.names = TRUE)
media_files <- all_files[tolower(tools::file_ext(all_files)) %in% allowed_ext]
media_files <- sort(media_files)

# Initialize log dataframe
log_df <- data.frame(
  original_filename = character(),
  new_filename = character(),
  timestamp = character(),
  file_size_MB = numeric(),
  video_duration_sec = numeric(),
  image_filename = character(),
  detail = character(),
  corruption_status = character(),
  ffmpeg_error = character(),
  stringsAsFactors = FALSE
)

pb <- progress_bar$new(
  format = "  Processing files [:bar] :percent ETA: :eta",
  total = length(media_files), clear = FALSE, width = 60
)

for (i in seq_along(media_files)) {
  file <- media_files[i]
  pb$tick()
  info <- parse_metadata(file)
  if (is.null(info)) next
  
  ext <- tolower(tools::file_ext(file))
  extension <- paste0(".", ext)
  shutter <- info$shutter
  iso <- info$iso
  timestamp_str <- info$timestamp
  detail <- paste(duration, paste0("SH", shutter), "AWB", iso, sep = "-")
  corruption_status <- "OK"
  ffmpeg_error <- NA
  
  new_name <- paste(cruise_id, timestamp_str, lander_number, vehicle_name,
                    camera_direction, quality, detail, sep = "_")
  new_name <- paste0(new_name, extension)
  new_path <- file.path(target_dir, new_name)
  
  size_MB <- file.info(file)$size / (1024^2)
  image_filename <- ""
  video_duration_sec <- NA
  copy_source <- file
  
  if (info$type == "video" && ext == "mp4") {
    metadata <- NULL
    try({
      metadata <- av_media_info(file)
    }, silent = TRUE)
    
    if (is.null(metadata) || is.null(metadata$duration)) {
      message(sprintf("⚠️ Corrupt video detected: %s — attempting repair...", basename(file)))
      corruption_status <- "Repaired"
      
      repaired_file <- tempfile(fileext = ".mp4")
      repair_cmd <- sprintf('"C:/ffmpeg/bin/ffmpeg.exe" -y -i "%s" -c copy "%s"', file, repaired_file)
      ffmpeg_output <- tryCatch({
        system(repair_cmd, intern = TRUE)
      }, error = function(e) {
        return(paste("system() error:", e$message))
      })
      
      try({
        metadata <- av_media_info(repaired_file)
      }, silent = TRUE)
      
      if (!is.null(metadata) && !is.null(metadata$duration)) {
        copy_source <- repaired_file
        message(sprintf("✅ Repair successful: %s", basename(file)))
      } else {
        corruption_status <- "Corrupt (unrepairable)"
        ffmpeg_error <- paste(ffmpeg_output, collapse = "\n")
        
        # Save .txt log and copy corrupted file (even in test_mode)
        txt_path <- file.path(corrupt_dir, paste0(tools::file_path_sans_ext(new_name), ".txt"))
        writeLines(c(
          paste("Original filename:", basename(file)),
          "",
          "FFmpeg error:",
          ffmpeg_error
        ), txt_path)
        
        file.copy(file, file.path(corrupt_dir, basename(file)), overwrite = TRUE)
        
        # Copy next good video for repair reference
        for (j in (i+1):length(media_files)) {
          next_file <- media_files[j]
          if (tolower(tools::file_ext(next_file)) != "mp4") next
          next_info <- NULL
          try({
            next_info <- av_media_info(next_file)
          }, silent = TRUE)
          if (!is.null(next_info) && !is.null(next_info$duration)) {
            file.copy(next_file, file.path(corrupt_dir, basename(next_file)), overwrite = TRUE)
            break
          }
        }
        
        # Log and skip further processing
        log_df <- rbind(log_df, data.frame(
          original_filename = basename(file),
          new_filename = basename(new_name),
          timestamp = timestamp_str,
          file_size_MB = round(size_MB, 2),
          video_duration_sec = NA,
          image_filename = "",
          detail = detail,
          corruption_status = corruption_status,
          ffmpeg_error = ffmpeg_error,
          stringsAsFactors = FALSE
        ))
        next
      }
    }
    
    video_duration_sec <- metadata$duration
    middle_time <- video_duration_sec / 2
    timestamp <- format(as.POSIXct(middle_time, origin = "1970-01-01", tz = "UTC"), "%H:%M:%S")
    
    image_filename <- paste0(tools::file_path_sans_ext(new_name), ".jpg")
    image_path <- file.path(images_dir, image_filename)
    
    if (!test_mode) {
      cmd <- sprintf(
        '"C:/ffmpeg/bin/ffmpeg.exe" -y -ss %s -i "%s" -frames:v 1 "%s"',
        timestamp, copy_source, image_path
      )
      system(cmd, intern = TRUE)
    }
  }
  
  if (!test_mode) {
    file.copy(copy_source, new_path, overwrite = TRUE)
  }
  
  # Log
  log_df <- rbind(log_df, data.frame(
    original_filename = basename(file),
    new_filename = basename(new_path),
    timestamp = timestamp_str,
    file_size_MB = round(size_MB, 2),
    video_duration_sec = round(video_duration_sec, 2),
    image_filename = image_filename,
    detail = detail,
    corruption_status = corruption_status,
    ffmpeg_error = ffmpeg_error,
    stringsAsFactors = FALSE
  ))
}

# Write CSV
con <- file(log_csv, "w")
write.table(log_df, file = con, sep = ",", row.names = FALSE, col.names = TRUE)
close(con)

if (test_mode) message("✅ Test mode: no files were copied or images extracted. Corrupted files were saved for repair.")
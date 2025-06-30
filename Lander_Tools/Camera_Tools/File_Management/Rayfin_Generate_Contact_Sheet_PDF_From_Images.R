# -----------------------  contact_sheet_magick_overlay.R  -----------------------
# Load required packages
suppressMessages({
  library(magick)
  library(grid)
  library(gridExtra)
  library(tools)
  library(progress)
  library(qpdf)
})

# ----------------------- CONFIGURATION ----------------------------------------
# Dynamic path settings
videos_or_images <- "Stills_From_Videos"
#videos_or_images <- "Stills"

folder <- "Y:\\NF2503\\Bender Recovery\\NF2402_URIALBEX02002\\Processed\\Camera_Rayfin\\"

images_dir <- paste0(folder, videos_or_images)
output_pdf <- paste0(folder, "NF2503_URIALBEX061", "_Rayfin_", videos_or_images, "_Contact_Sheet.pdf")
log_path   <- paste0(images_dir, "\\contact_sheet_log.txt")
temp_pdf_dir <- tempdir()     # Temporary storage for individual pages
test_mode   <- FALSE           # ✅ Set TRUE to only test image readability
debug_mode  <- FALSE            # ✅ Set TRUE to visualize placement

MAX_DIMENSION <- 5000          # Maximum allowed dimension for width or height

columns <- 3
rows <- 4
images_per_page <- columns * rows
dpi <- 120

page_width_in <- 8.5
page_height_in <- 11
page_width_px  <- page_width_in * dpi
page_height_px <- page_height_in * dpi

# ✅ Updated target dimensions with margins
margin_px <- 10
inner_margin_px <- 5
target_width_px  <- floor((page_width_px / columns) - (2 * margin_px))
target_height_px <- floor((page_height_px / rows) - (2 * margin_px + 25))

# ----------------------- LOGGING ---------------------------------------------
log_con <- file(log_path, open = "wt", encoding = "UTF-8")
writeln <- function(...) writeLines(paste(...), log_con)

writeln("=== Contact sheet run:", Sys.time(), "===")

# ----------------------- IMAGE LIST ------------------------------------------
image_files <- list.files(images_dir,
                          pattern = "\\.(jpg|jpeg|png)$",
                          full.names = TRUE)

total_images <- length(image_files)
total_pages <- ceiling(total_images / images_per_page)

writeln("Total images found:", total_images)
message("🔍 Processing ", total_images, " images.")

# ----------------------- HELPER FUNCTIONS ------------------------------------

# 📝 **Manual string wrap for long filenames (Max 3 lines)**
manual_wrap <- function(text, width = 45) {
  # If the text is shorter than the width, return it as is
  if (nchar(text) <= width) {
    return(text)
  }
  
  # Split the string into fixed chunks
  chunks <- substring(text, seq(1, nchar(text), by = width), seq(width, nchar(text) + width - 1, by = width))
  
  # Limit to a maximum of 3 lines
  if (length(chunks) > 3) {
    chunks <- chunks[1:3]
    chunks[3] <- paste0(chunks[3], "...")
  }
  
  paste(chunks, collapse = "\n")
}

# 🖼️ **Image loading and resizing with aspect ratio**
load_and_resize_image <- function(file) {
  tryCatch({
    img <- image_read(file)
    
    # Get the image's original dimensions
    img_info <- image_info(img)
    img_width <- img_info$width
    img_height <- img_info$height
    
    # Compute the aspect ratio
    aspect_ratio <- img_width / img_height
    
    # Resize proportionally
    if (aspect_ratio > 1) {
      img <- image_scale(img, paste0(target_width_px - inner_margin_px * 2, "x"))
    } else {
      img <- image_scale(img, paste0("x", target_height_px - inner_margin_px * 2))
    }
    
    # Convert to raster format for grid.raster
    img_grob <- rasterGrob(as.raster(img), interpolate = FALSE)
    
    img_grob
  }, error = function(e) {
    writeln("❌ Failed to read or resize:", basename(file), "|", e$message)
    NULL
  })
}

# ----------------------- PDF PAGE GENERATION ---------------------------------
page_files <- character(0)

pb <- progress_bar$new(
  format = "Writing Pages [:bar] :current/:total pages",
  total = total_pages,
  clear = FALSE, width = 60
)

for (page in seq_len(total_pages)) {
  pb$tick()
  
  # Generate temp file for the page
  temp_pdf <- file.path(temp_pdf_dir, paste0("page_", page, ".pdf"))
  page_files <- c(page_files, temp_pdf)
  
  # Write directly to a temporary PDF
  pdf(temp_pdf, width = page_width_in, height = page_height_in)
  
  start_idx <- (page - 1) * images_per_page + 1
  end_idx <- min(page * images_per_page, total_images)
  image_subset <- image_files[start_idx:end_idx]
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(rows, columns)))
  
  for (i in seq_along(image_subset)) {
    file <- image_subset[i]
    
    # Load and resize image
    img_grob <- load_and_resize_image(file)
    if (is.null(img_grob)) {
      writeln("❌ Failed to load image:", file)
      next
    }
    
    row <- ceiling(i / columns)
    col <- (i - 1) %% columns + 1
    
    vp <- viewport(layout.pos.row = row, layout.pos.col = col)
    pushViewport(vp)
    
    # ✅ Draw image first
    grid.rect(gp = gpar(col = "grey80", fill = "white"))
    grid.draw(img_grob)
    
    # 📝 Wrap the caption (Max 3 lines, 45 chars per line):
    caption <- sprintf("%s (%.0f KB)", basename(file), file.info(file)$size / 1024)
    wrapped_caption <- manual_wrap(caption, width = 45)
    
    # Calculate the height of the box based on line count
    line_count <- length(strsplit(wrapped_caption, "\n")[[1]])
    box_height <- 0.1 + 0.05 * (line_count - 1)
    
    # ✅ Draw background rectangle
    grid.rect(y = unit(0.875, "npc"), height = unit(box_height, "npc"), 
              width = unit(1, "npc"), gp = gpar(fill = rgb(0, 0, 0, 0.5), col = NA))
    
    # ✅ Draw the text, centered vertically
    grid.text(wrapped_caption,
              y = unit(0.875, "npc"),
              gp = gpar(fontsize = 7, col = "white", fontface = "bold"),
              just = "center")
    
    popViewport()
    
    # 🔥 Force garbage collection after each image
    rm(img_grob)
    gc()
  }
  
  dev.off()
}

# ----------------------- MERGE ALL PAGES WITH QPDF ---------------------------
message("🔗 Merging pages into final PDF...")

output_pdf_path <- normalizePath(output_pdf)
qpdf::pdf_combine(input = page_files, output = output_pdf_path)

# ----------------------- CLEANUP --------------------------------------------
file.remove(page_files)
close(log_con)

message("✅ Contact sheet saved to: ", output_pdf_path)
message("📝 Log saved to: ", normalizePath(log_path))
# ------------------------------------------------------------------------------
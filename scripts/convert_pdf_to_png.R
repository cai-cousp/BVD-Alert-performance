library(pdftools)
pdfs <- list.files("output", pattern = "\\.pdf$", recursive = TRUE, full.names = TRUE)
cat("Found", length(pdfs), "PDFs to convert\n")
count <- 0
failed <- character(0)

for (p in pdfs) {
  tryCatch({
    n_pages <- pdf_info(p)$pages
    png_base <- sub("\\.pdf$", ".png", p)

    if (n_pages == 1) {
      # Single-page: overwrite with .png
      pdf_convert(p, filenames = png_base, format = "png", dpi = 200)
    } else {
      # Multi-page: append page number to filename
      for (pg in seq_len(n_pages)) {
        f <- sub("\\.pdf$", paste0("_", pg, ".png"), p)
        pdf_convert(p, pages = pg, filenames = f, format = "png", dpi = 200)
      }
      # Remove the original .png placeholder created for single-page case
      if (file.exists(png_base)) unlink(png_base)
    }

    count <- count + 1
    if (count %% 10 == 0) cat("Converted", count, "\n")
  }, error = function(e) {
    failed <<- c(failed, p)
    cat("FAILED:", basename(p), "-", e$message, "\n")
  })
}
cat("Done. Converted", count, "/", length(pdfs), "PDFs to PNG.\n")
if (length(failed) > 0) {
  cat("\nFailed files:\n")
  for (f in failed) cat(" -", f, "\n")
}

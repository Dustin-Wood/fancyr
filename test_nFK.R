suppressMessages({ library(devtools); load_all(quiet = TRUE) })

# Renders test_nFK.Rmd to test_nFK.html in the project root.
# Edit reference cases and prose in the Rmd, not here.
out <- rmarkdown::render(
  input       = "test_nFK.Rmd",
  output_file = "test_nFK.html",
  output_dir  = ".",
  quiet       = TRUE
)
message("Rendered: ", normalizePath(out, winslash = "/"))

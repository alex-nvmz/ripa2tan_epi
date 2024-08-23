# Convert quarto files with R code into R scripts

files <- list.files(path=".", pattern="*.qmd")
out_files <- stringr::str_replace(files, ".qmd", ".R")

# Have to erase the previous R scripts; purl does not overwrite
unlink(out_files)
purrr::map(
  files,
  \(file) knitr::purl(file, documentation = 2)
)

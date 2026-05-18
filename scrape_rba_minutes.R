# ==============================================================
# Title: Pull RBA Board minutes 2007-present
# ==============================================================

# 0. Load packages and paths ------------------------------------------------

library(data.table)
library(rvest)
library(stringi)

minutes_html   <- "C:/Users/rheac/OneDrive - The University of Chicago/Research/monetary_policy/data/rba_minutes"
output_path  <- "C:/Users/rheac/OneDrive - The University of Chicago/Research/monetary_policy/data"
UA <- "research-script/1.0 (academic; thesis on central bank communication)"


# 1. Pull Meeting URLs ---------------------------------------------------------

meeting_urls <- function(year) {
  index_url <- sprintf(
    "https://www.rba.gov.au/monetary-policy/rba-board-minutes/%d/", year
  )
  res <- tryCatch(read_html(index_url), error = function(e) NULL)
  if (is.null(res)) {
    cat(sprintf("  %d: index page failed\n", year))
    return(character(0))
  }
  links <- html_attr(html_elements(res, "a"), "href")
  # Old format: /YYYY/DDMMYYYY.html  (8 digits before .html)
  # New format: /YYYY/YYYY-MM-DD.html (10 chars with dashes)
  meet <- links[grepl(sprintf("/monetary-policy/rba-board-minutes/%d/", year),
                      links)]
  meet <- meet[grepl("\\.html$", meet)]
  meet <- meet[!grepl("index", meet)]
  meet <- unique(meet)
  # Make absolute
  meet <- ifelse(grepl("^https?://", meet), meet,
                 paste0("https://www.rba.gov.au", meet))
  meet
}

rba_urls <- character(0)

for (y in 2007:2026) {
  urls <- meeting_urls(y)
  print(paste(y,":", length(urls),"meetings"))
  all_urls <- c(all_urls, urls)
  Sys.sleep(1.0)
}


url_to_path <- function(url) {
  # extract last segment, e.g. "2015-02-03.html" or "07122010.html"
  fn <- basename(url)
  file.path(minutes_html, fn)
}

url_to_date <- function(url) {
  fn <- basename(url)
  base <- sub("\\.html$", "", fn)
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", base)) {
    return(base)  
  }
  if (grepl("^\\d{8}$", base)) {
    # DDMMYYYY -> YYYY-MM-DD
    dd <- substr(base, 1, 2); mm <- substr(base, 3, 4); yyyy <- substr(base, 5, 8)
    return(sprintf("%s-%s-%s", yyyy, mm, dd))
  }
  NA_character_
}

# 2. Download minutes as HTMLs----------------------------------------------------

download_files <- function(url) {
  dest <- url_to_path(url)
  
  if (!file.exists(dest)) {
    download.file(url, dest, quiet = TRUE, mode = "wb",
                  headers = c("User-Agent" = UA))
    Sys.sleep(1.0)
  }
  print(paste("Running",url_to_date(url)))
  data.table(date = url_to_date(url), url = url, path = dest)
}

rba_minutes <- rbindlist(lapply(rba_urls, download_files))

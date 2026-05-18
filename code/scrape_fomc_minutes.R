# ==============================================================
# Title: Pull Fed Board minutes 2007-present
# ==============================================================

# 0. Load packages and paths ------------------------------------------------

library(data.table)
library(rvest)
library(stringi)

minutes_html   <- "data/fed_minutes"
UA <- "research-script/1.0 (academic; thesis on central bank communication)"


# 1. Pull Meeting URLs ---------------------------------------------------------

# 2007-2020
meeting_urls_fed <- function(year) {
  index_url <- sprintf(
    "https://www.federalreserve.gov/monetarypolicy/fomchistorical%d.htm", year
  )
  res <- tryCatch(read_html(index_url), error = function(e) NULL)
  if (is.null(res)) {
    cat(sprintf("  %d: index page failed\n", year))
    return(character(0))
  }
  links <- html_attr(html_elements(res, "a"), "href")
  # Minutes URLs look like: /monetarypolicy/fomcminutesYYYYMMDD.htm
  meet <- links[grepl("/monetarypolicy/fomcminutes\\d{8}\\.htm$", links)]
  meet <- unique(meet)
  # Make absolute
  meet <- ifelse(grepl("^https?://", meet), meet,
                 paste0("https://www.federalreserve.gov", meet))
  meet
}

# 2021-2026
meeting_urls_fed_recent <- function() {
  res <- tryCatch(
    read_html("https://www.federalreserve.gov/monetarypolicy/fomccalendars.htm"),
    error = function(e) NULL)
  if (is.null(res)) return(character(0))
  links <- html_attr(html_elements(res, "a"), "href")
  meet <- links[grepl("/monetarypolicy/fomcminutes\\d{8}\\.htm$", links)]
  meet <- unique(meet)
  meet <- ifelse(grepl("^https?://", meet), meet,
                 paste0("https://www.federalreserve.gov", meet))
  meet
}

all_urls <- character(0)

for (y in 2007:2020) {
  urls <- meeting_urls_fed(y)
  print(paste(y, ":", length(urls), "meetings"))
  all_urls <- c(all_urls, urls)
  Sys.sleep(1.0)
}

recent_urls <- meeting_urls_fed_recent()

fed_urls <- c(all_urls, recent_urls)

url_to_path_fed <- function(url) {
  file.path(minutes_html, basename(url))   # basename(url) = "fomcminutes20140319.htm"
}

url_to_date_fed <- function(url) {
  ymd <- sub(".*fomcminutes(\\d{8})\\.htm$", "\\1", url)
  sprintf("%s-%s-%s", substr(ymd, 1, 4), substr(ymd, 5, 6), substr(ymd, 7, 8))
}

download_fed <- function(url) {
  dest <- url_to_path_fed(url)
  if (!file.exists(dest)) {
    download.file(url, dest, quiet = TRUE, mode = "wb",
                  headers = c("User-Agent" = UA))
    Sys.sleep(1.0)
  }
  print(paste("Running", url_to_date_fed(url)))
  data.table(date = url_to_date_fed(url), url = url, path = dest)
}

fed_minutes <- rbindlist(lapply(fed_urls, download_fed))
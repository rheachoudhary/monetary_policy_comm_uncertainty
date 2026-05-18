# ==========================================
# Parse RBA minute HTMLs and save codebook
# ==========================================

# 0. Load data and packages --------------------------------------

library(data.table)
library(rvest)
library(stringi)

output_path  <- "C:/Users/rheac/OneDrive - The University of Chicago/Research/monetary_policy/data"

rba_minutes_html <- list.files("C:/Users/rheac/OneDrive - The University of Chicago/Research/monetary_policy/data/rba_minutes", 
                               pattern = "\\.html?$", full.names = TRUE)

# 1: Parse -------------------------------------------------------

# Pull the date from each filename
extract_rba_date <- function(path) {
  base <- sub("\\.html?$", "", basename(path))
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", base)) {
    return(base)                            # already ISO
  }
  if (grepl("^\\d{8}$", base)) {
    # DDMMYYYY -> YYYY-MM-DD
    dd <- substr(base, 1, 2); mm <- substr(base, 3, 4); yyyy <- substr(base, 5, 8)
    return(sprintf("%s-%s-%s", yyyy, mm, dd))
  }
  NA_character_
}

rba_minutes <- data.table(
  date = sapply(rba_minutes_html, extract_rba_date),
  path = rba_minutes_html
)

extract_body_rba <- function(html_path) {
  doc <- tryCatch(read_html(html_path), error = function(e) NULL)
  if (is.null(doc)) return(NA_character_)
  
  candidates <- c("div#content", "main", "article", "div.col-md-9", "body")
  txt <- NA_character_
  for (sel in candidates) {
    nodes <- html_elements(doc, sel)
    if (length(nodes) > 0) {
      txt <- html_text2(nodes[[1]])
      if (nchar(txt) > 1000) break
    }
  }
  if (is.na(txt) || nchar(txt) < 1000) return(NA_character_)
  
  # Trim non-minute-related html text from the top
  start_markers <- c(
    "Members Present", "Minutes of the Monetary Policy Meeting",
    "Minutes of the Reserve Bank Board", "Minutes of the Monetary Policy Board",
    "Members participating", "Members present"
  )
  start_idx <- NA_integer_
  for (m in start_markers) {
    pos <- regexpr(m, txt, fixed = TRUE)
    if (pos > 0 && (is.na(start_idx) || pos < start_idx)) {
      start_idx <- as.integer(pos)
    }
  }
  if (!is.na(start_idx)) txt <- substr(txt, start_idx, nchar(txt))
  
  # Trim end that isn't minute-related using "The Decision" heading 
  end_pattern <- "The [Dd]ecision The Board (decided|resolved|reaffirmed|agreed)"
  end_pos <- regexpr(end_pattern, txt, perl = TRUE)
  if (end_pos > 0) txt <- substr(txt, 1, end_pos - 1)
  
  txt <- gsub("\\s+", " ", txt)
  txt <- gsub("\u00A0", " ", txt)
  trimws(txt)
}

parse_results <- list()

for (i in seq_len(nrow(rba_minutes))) {
  row <- rba_minutes[i]
  body <- extract_body_rba(row$path)
  if (is.na(body)) {
    cat(sprintf("  %s  PARSE FAIL\n", row$date))
    next
  }
  parse_results[[length(parse_results) + 1]] <-
    data.table(date = row$date, body = body, body_len = nchar(body))
}

rba_minute_body <- rbindlist(parse_results)

# Tokenise sentences 

tokenise_sentences <- function(text) {
  s <- stri_split_boundaries(text, type = "sentence")[[1]]
  s <- trimws(s)
  s <- s[nchar(s) > 20]
  s
}

rba_sentences_dt <- rba_minute_body[, {
  sents <- tokenise_sentences(body)
  .(sent_id = seq_along(sents), sentence = sents)
}, by = date]

rba_sentences_dt[, year := as.integer(format(as.Date(date), "%Y"))]


# 3. RBA codebook -----------------------------------------------------------

match_any <- function(s_lower, patterns) {
  for (p in patterns) {
    if (grepl("\\.\\*", p)) {
      if (grepl(p, s_lower, perl = TRUE)) return(p)
    } else {
      if (grepl(p, s_lower, fixed = TRUE)) return(p)
    }
  }
  NA_character_
}

wait_and_see <- c(
  "for the time being", 
  "more time to evaluate", "more time to assess",
  "pending further evaluation",
  "wait until", "wait and see",
  "await information",
  "wait for further", "waiting for more", "waiting for further",
  "wait another", "wait another month",
  "benefits of waiting for", "potential benefits of waiting"
)

caution <- c(
  "guided by the incoming", "guided by the evidence",
  "will be guided", "would be guided",
  "not on a pre-set", "not on a preset",
  "will depend on",
  "data dependent",
  "meeting-by-meeting",
  "appropriate setting",
  "current setting", 
  "best balance the risks", "best balanced the risks",

  "monitor.*carefully",
  "proceeding cautiously", "move cautiously",
  "warranted at this",
  "premature.*tightening", "risk of.*premature",
  "benefits of waiting", "value in waiting",
  "flexibility to increase or reduce"
)

broad_assessing <- c(
  # Scenario language
  "considered.*alternative scenarios",
  "considered.*plausible scenarios",
  "considered.*plausible alternative",
  "considered three scenarios",
  "alternative scenarios",             
  "plausible scenarios",              
  "did not rule out",
  "could not be ruled out",
  "could be considered again",
  "would be reviewed",
  "frequent opportunities to assess",
  "range of options",
  "range of potential scenarios",
  "various scenarios"
)

unclear <- c(
  "not yet possible to judge",
  "not yet possible to determine",
  "too early to know",
  "too early to determine",
  "too early to assess",               
  "too early to tell",
  "not possible to predict",
  "not possible to have a high degree of confidence",
  "difficult either to rule in or rule out"
)

code_indecision_rba <- function(sentence) {
  s <- tolower(sentence)
  
  hit <- match_any(s, wait_and_see)
  if (!is.na(hit)) return(list(code = 1L, pattern = "wait_and_see", hit = hit))
  
  hit <- match_any(s, unclear)
  if (!is.na(hit)) return(list(code = 1L, pattern = "unclear", hit = hit))
  
  hit <- match_any(s, caution)
  if (!is.na(hit)) return(list(code = 1L, pattern = "caution", hit = hit))
  
  hit <- match_any(s, broad_assessing)
  if (!is.na(hit)) return(list(code = 1L, pattern = "broad_assessing", hit = hit))
  
  list(code = 0L, pattern = "no marker", hit = NA_character_)
}

inflation_exp_deanchor <- c(
  "become unanchored", "becoming unanchored", "becoming de-anchored",
  "becoming deanchored", "de-anchored", "de-anchoring", "deanchoring",
  "could become unanchored",
  "risk of.*expectations.*unanchored",
  "expectations.*could.*drift", "expectations.*had drifted",
  "expectations.*drifted higher",
  "rise in.*inflation expectations",
  "increase in.*inflation expectations",
  "longer.term inflation expectations.*sensitive",
  "more sensitive to energy price",
  "expectations.*more sensitive",
  "credibility of the inflation target",
  "credibility of the target",
  "credibility of monetary policy",
  "near-term inflation expectations.*had risen",
  "act forcefully", "act decisively", "decisive action",
  "respond forcefully",
  "ensure that.*inflation expectations",
  "guard against.*expectations"
)

inflation_persistence_uncertainty <- c(
  "inflation could prove more persistent",
  "inflation could prove to be more persistent",
  "inflation could prove persistent",
  "inflation proved more persistent",
  "more persistent than.*staff anticipated",
  "more persistent than expected",
  "more persistent than the staff",
  "stickier than expected", "stickier inflation", "sticky inflation",
  "inflation remaining elevated for longer",
  "inflation remain elevated for longer",
  "elevated for longer than expected",
  "broadening of inflation pressures",
  "broader inflation pressures",
  "broadening inflation",
  "second.round effects",
  "wage.price spiral", "wage-price dynamics", "wage-price feedback",
  "persistence of inflation",
  "indirect and second-round"
)



code_credibility <- function(sentence) {
  s <- tolower(sentence)
  hit <- match_any(s, inflation_exp_deanchor)
  if (!is.na(hit)) return(list(code = 1L, 
                               pattern = "expectations de-anchoring", hit = hit))
  hit <- match_any(s, inflation_persistence_uncertainty)
  if (!is.na(hit)) return(list(code = 1L,
                               pattern = "inflation-persistence", hit = hit))
  list(code = 0L, pattern = "no credibility concern", 
       hit = NA_character_)
}

# 4. Apply RBA codebook ----------------------------------------------------

rba_sentences_dt[, c("indecision", "indecision_pattern", "indecision_hit") := {
  out <- lapply(sentence, code_indecision_rba)
  list(
    vapply(out, function(x) x$code, integer(1)),
    vapply(out, function(x) x$pattern, character(1)),
    vapply(out, function(x) ifelse(is.null(x$hit) || is.na(x$hit),
                                   NA_character_, as.character(x$hit)),
           character(1))
  )
}]

rba_sentences_dt[, c("credibility", "credibility_pattern", "credibility_hit") := {
  out <- lapply(sentence, code_credibility)
  list(
    vapply(out, function(x) x$code, integer(1)),
    vapply(out, function(x) x$pattern, character(1)),
    vapply(out, function(x) ifelse(is.null(x$hit) || is.na(x$hit),
                                   NA_character_, as.character(x$hit)),
           character(1))
  )
}]

# 5. Save results ------------------------------------------------------

fwrite(rba_sentences_dt, file.path(output_path, "rba_sentences.csv"))

rba_indec_dt <- rba_sentences_dt[indecision == 1L]
fwrite(rba_indec_dt, file.path(output_path, "rba_indecision.csv"))

rba_cred_dt <- rba_sentences_dt[credibility == 1L]
fwrite(rba_cred_dt, file.path(output_path, "rba_credibility.csv"))

rba_meta_dt <- rba_sentences_dt[, .(
  year          = year[1],
  n_sentences   = .N,
  n_indecision  = sum(indecision),
  share_indec   = mean(indecision),
  n_credibility = sum(credibility),
  share_cred    = mean(credibility)
), by = date]


fwrite(rba_meta_dt, file.path(output_path, "rba_meta.csv"))
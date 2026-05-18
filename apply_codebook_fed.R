# ==========================================
# Parse Fed minute HTMLs and save codebook
# ==========================================

# 0. Load data and packages --------------------------------------

library(data.table)
library(rvest)
library(stringi)

output_path  <- "C:/Users/rheac/OneDrive - The University of Chicago/Research/monetary_policy/data"

# 1: Parse -------------------------------------------------------

fed_minutes_html <- list.files("C:/Users/rheac/OneDrive - The University of Chicago/Research/monetary_policy/data/fed_minutes", 
                    pattern = "\\.htm$", full.names = TRUE)

# Pull the date from each filename (Fed format: fomcminutesYYYYMMDD.htm)
extract_fed_date <- function(path) {
  fn <- basename(path)
  ymd <- sub(".*fomcminutes(\\d{8})\\.htm$", "\\1", fn)
  sprintf("%s-%s-%s", substr(ymd, 1, 4), substr(ymd, 5, 6), substr(ymd, 7, 8))
}

fed_minutes <- data.table(
  date = sapply(fed_minutes_html, extract_fed_date),
  path = fed_minutes_html
)

# Two HTML formats to handle:
#   - Pre-2010ish: simple HTML, content in body, no section structure markers
#   - Post-2010ish: structured with bold section headers like
#                   "Staff Review of the Economic Situation",
#                   "Participants' Views on Current Conditions and the
#                    Economic Outlook", "Committee Policy Actions"


extract_body <- function(html_path) {
  doc <- tryCatch(read_html(html_path), error = function(e) NULL)
  if (is.null(doc)) return(NA_character_)
  
  # Try common content containers in order of preference
  candidates <- c(
    "div#content",
    "div.col-xs-12.col-sm-8",
    "div#article",
    "main",
    "body"
  )
  txt <- NA_character_
  for (sel in candidates) {
    nodes <- html_elements(doc, sel)
    if (length(nodes) > 0) {
      txt <- html_text2(nodes[[1]])
      if (nchar(txt) > 1000) break
    }
  }
  if (is.na(txt) || nchar(txt) < 1000) return(NA_character_)
  
  # Trim navigation cruft. Find the start of substantive content
  # by locating a phrase that always appears near the top of a minute.
  start_markers <- c(
    "A meeting of the Federal Open Market Committee was held",
    "A joint meeting of the Federal Open Market Committee"
  )
  start_idx <- NA_integer_
  for (m in start_markers) {
    pos <- regexpr(m, txt, fixed = TRUE)
    if (pos > 0 && (is.na(start_idx) || pos < start_idx)) {
      start_idx <- as.integer(pos)
    }
  }
  if (!is.na(start_idx)) txt <- substr(txt, start_idx, nchar(txt))
  
  # Trim end: cut at first signature/footer/notation-vote section we find.
  end_markers <- c(
    "Notation Vote",
    "Notation Votes",
    "By notation vote",
    "Last update:",
    "Last Update:"
  )
  end_idx <- nchar(txt)
  for (m in end_markers) {
    pos <- regexpr(m, txt, fixed = TRUE)
    if (pos > 0 && pos < end_idx) end_idx <- as.integer(pos)
  }
  txt <- substr(txt, 1, end_idx - 1)
  
  # Normalise whitespace
  txt <- gsub("\\s+", " ", txt)
  txt <- gsub("\u00A0", " ", txt)  # nbsp
  trimws(txt)
}

parse_results <- list()

for (i in seq_len(nrow(fed_minutes))) {
  row <- fed_minutes[i]
  body <- extract_body(row$path)
  if (is.na(body)) {
    cat(sprintf("  %s  PARSE FAIL\n", row$date))
    next
  }
  
  parse_results[[length(parse_results) + 1]] <-
    data.table(date = row$date, 
               body = body,
               body_len = nchar(body))
}

fed_minute_bodies_dt <- rbindlist(parse_results)

# ------- step 3: tokenise into sentences ------------------------------------
# Use stringi sentence boundary detection.
tokenise_sentences <- function(text) {
  s <- stri_split_boundaries(text, type = "sentence")[[1]]
  s <- trimws(s)
  s <- s[nchar(s) > 20]  # drop fragments
  s
}

fed_sentences_dt <- fed_minute_bodies_dt[, {
  sents <- tokenise_sentences(body)
  .(sent_id = seq_along(sents), sentence = sents)
}, by = date]

fed_sentences_dt[, year := as.integer(format(as.Date(date), "%Y"))]


# ------- step 4: codebook ----------------------------------------------------
# Helper: does the lowercased sentence match any of the patterns?
# Patterns are plain substrings except where they contain ".*" (then regex).
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

# Fed Board indecision codebook  --------------------------

# Pattern A: explicit policy alternative framed around uncertainty
fed_wait_and_see <- c(
  "await further",
  "await additional",
  "await information",
  "await news",
  "await more",
  "wait for more",
  "wait for further",
  "waiting for more",
  "waiting for further",
  "wait another",
  "before taking any decision",
  "benefits of waiting",
  "flexibility to increase or reduce",
  "flexibility to increase or decrease",
  "flexibility to adjust",
  "flexibility to be patient",
  "agreed to hold",
  "agreed to leave",
  "agreed to wait",
  "agreed to keep",
  "agreed to maintain",
  "decided to leave",
  "decided to hold",
  "decided to maintain",
  "decided to keep",
  "be patient",
  "patience was warranted",
  "exercising patience",
  "remained patient",
  "would remain patient",
  "the case for holding",
  "the case for keeping",
  "the case for waiting",
  "the case for being patient",
  "the case for patience",
  "the case to wait"
)

# Board treading carefully
fed_caution <- c(
  "appropriate to wait",
  "appropriate to proceed cautiously",
  "appropriate to proceed carefully",
  "appropriate to be patient",
  "warrant a measured approach",
  "warranted a measured approach",
  "a measured approach",
  "carefully assess",
  "carefully monitor",
  "carefully evaluate",
  "carefully calibrate",
  "monitor.*carefully",
  "carefully.*monitor",
  "best balance the risks",
  "best balanced the risks",
  "balance the risks",
  "path of least regret",
  "value in proceeding",
  "value in waiting",
  "benefits of waiting",
  "guided by the data",
  "guided by the evidence",
  "guided by the incoming",
  "data-dependent",
  "data dependent",
  "meeting.by.meeting",
  "meeting-by-meeting",
  "not on a pre-set",
  "not on a preset",
  "not pre-committed",
  "not pre.committing",
  "not committing",
  "patient",
  "patience",
  "be nimble",
  "remain nimble",
  "being nimble",
  "nimbly",
  "proceed cautiously",
  "proceeding cautiously",
  "move cautiously",
  "moving cautiously",
  "acting cautiously",
  "tread carefully"
)

# Pattern C: Board's path itself uncertain / narrow
fed_unclear <- c(
  "not yet possible to judge",
  "not yet possible to determine",
  "too early to know",
  "too early to determine",
  "too early to assess",
  "too early to tell",
  "not possible to predict",
  "not possible to have a high degree of confidence",
  "difficult either to rule in or rule out",
  "difficult to rule in",
  "could not predict",
  "clouded in uncertainty",
  "subject to considerable uncertainty",
  "subject to material uncertainty",
  "subject to significant uncertainty",
  "extent of restrictiveness",
  "extent to which.*policy.*restrictive"
)

# hedging via scenarios
fed_scenarios <- c(
  "considered.*alternative scenarios",
  "considered.*plausible scenarios",
  "considered.*plausible alternative",
  "considered three scenarios",
  "considered two.*scenarios",
  "considered.*scenarios",
  "alternative scenarios",
  "plausible scenarios",
  "alternative.*illustrative scenarios",
  "did not rule out",
  "could not be ruled out",
  "could be reviewed",
  "could be considered again",
  "would be reviewed",
  "frequent opportunities to assess",
  "reassess",
  "re-assess",
  "range of options",
  "range of potential scenarios",
  "various scenarios",
  "alternative.*outcomes"
)


code_indecision_fed <- function(sentence) {
  s <- tolower(sentence)
  
  hit <- match_any(s, fed_wait_and_see)
  if (!is.na(hit)) return(list(code = 1L, pattern = "wait_and_see", hit = hit))
  
  hit <- match_any(s, fed_caution)
  if (!is.na(hit)) return(list(code = 1L, pattern = "caution", hit = hit))
  
  hit <- match_any(s, fed_unclear)
  if (!is.na(hit)) return(list(code = 1L, pattern = "unclear", hit = hit))
  
  hit <- match_any(s, fed_scenarios)
  if (!is.na(hit)) return(list(code = 1L, pattern = "scenarios", hit = hit))
  
  list(code = 0L, pattern = "no marker", hit = NA_character_)
}

fed_sentences_dt[, c("indecision", "indecision_pattern", "indecision_hit") := {
  out <- lapply(sentence, code_indecision_fed)
  list(
    vapply(out, function(x) x$code, integer(1)),
    vapply(out, function(x) x$pattern, character(1)),
    vapply(out, function(x) ifelse(is.null(x$hit) || is.na(x$hit),
                                   NA_character_, as.character(x$hit)),
           character(1))
  )
}]

# Expectations de-anchoring concern 
fed_inflation_exp_deanchor <- c(
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
  "ensure that.*inflation expectations",
  "guard against.*expectations",
  "concerned.*expectations.*unanchored",     
  "concerned.*expectations.*could become",    
  "concerned.*expectations.*move higher",     
  "concerned.*expectations.*move up",
  "concerned.*expectations.*rise",
  "could become unhinged",                    
  "expectations.*become unhinged",
  "committee credibility"
)

# Persistence concern 
fed_inflation_persistence_uncertainty <- c(
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
  "wage.price spiral", "wage-price dynamics", "wage-price feedback",
  "persistence of inflation"
)


fed_code_credibility <- function(sentence) {
  s <- tolower(sentence)
  hit <- match_any(s, fed_inflation_exp_deanchor)
  if (!is.na(hit)) return(list(code = 1L, 
                               pattern = "expectations de-anchoring", hit = hit))
  hit <- match_any(s, fed_inflation_persistence_uncertainty)
  if (!is.na(hit)) return(list(code = 1L, 
                               pattern = "inflation-persistence", hit = hit))
  list(code = 0L, pattern = "no marker", hit = NA_character_)
}

fed_sentences_dt[, c("credibility", "credibility_pattern", "credibility_hit") := {
  out <- lapply(sentence, fed_code_credibility)
  list(
    vapply(out, function(x) x$code, integer(1)),
    vapply(out, function(x) x$pattern, character(1)),
    vapply(out, function(x) ifelse(is.null(x$hit) || is.na(x$hit),
                                   NA_character_, as.character(x$hit)),
           character(1))
  )
}]

# Write outputs -----------------------------------------------

fwrite(fed_sentences_dt, file.path(output_path, "fomc_sentences.csv"))

# Indecision-only
fed_indec_dt <- fed_sentences_dt[indecision == 1]

fwrite(fed_indec_dt, file.path(output_path, "fomc_indecision.csv"))

# Credibility-only
fed_cred_dt <- fed_sentences_dt[credibility == 1]
fwrite(fed_cred_dt, file.path(output_path, "fomc_credibility.csv"))

fed_meta_dt <- fed_sentences_dt[, .(
  year          = year[1],
  n_sentences   = .N,
  n_indecision  = sum(indecision),
  share_indec   = sum(indecision) / .N,
  n_credibility = sum(credibility),
  share_cred    = sum(credibility) / .N
), by = date]

fed_meta_dt[, date := as.IDate(date)]

fwrite(fed_meta_dt, file.path(output_path, "fed_meta.csv"))
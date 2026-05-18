# ==============================================================
# Title: Plot RBA vs Fed minutes
# ==============================================================

# 0. Load packages and paths ------------------------------------------------

library(data.table)
library(ggplot2)
library(patchwork)
library(scales)
library(zoo)

theme_set(theme_classic() +
            theme(
              plot.title    = element_text(face = "bold", size = 15, hjust = 0.5),
              plot.subtitle = element_text(size = 13, hjust = 0.5),
              plot.caption  = element_text(size = 11, hjust = 0)))


# ------- file paths ---------------------------------------------------------
dir <- "C:/Users/rheac/OneDrive - The University of Chicago/Research/monetary_policy/data"

graph_output <- "C:/Users/rheac/OneDrive - The University of Chicago/Research/monetary_policy/graphs"

# ------- load + merge -------------------------------------------------------
rba <- fread(paste0(dir, "/rba_meta.csv"))
fed <- fread(paste0(dir, "/fed_meta.csv"))


rba[, share_indec := share_indec * 100]
rba[, share_cred  := share_cred  * 100]
fed[, share_indec := share_indec * 100]
fed[, share_cred  := share_cred  * 100]

# Shared y-axis limits across left/right panels so they're visually comparable
indec_max <- max(c(rba$share_indec, fed$share_indec), na.rm = TRUE) * 1.05
cred_max  <- max(c(rba$share_cred,  fed$share_cred),  na.rm = TRUE) * 1.05

# Add rolling mean line
rba[, rollm_indec := rollapply(share_indec, width = 3, FUN = mean,
                               fill = NA, partial = TRUE, align = "right")]
rba[, rollm_cred  := rollapply(share_cred, width = 3, FUN = mean,
                               fill = NA, partial = TRUE, align = "right")]
fed[, rollm_indec := rollapply(share_indec, width = 3, FUN = mean,
                               fill = NA, partial = TRUE, align = "right")]
fed[, rollm_cred  := rollapply(share_cred, width = 3, FUN = mean,
                               fill = NA, partial = TRUE, align = "right")]

# ------- panel builders -----------------------------------------------------
indec_colour <- "#1f4e78"
cred_colour  <- "#8b1a1f"

make_panel <- function(df, value_col, roll_mean_col, fill_col, title, ymax) {
  ggplot(df, aes(x = date)) +
    geom_col(aes(y = .data[[value_col]]),
             fill = fill_col, alpha = 0.5) +
    geom_line(aes(y = .data[[roll_mean_col]]), colour = fill_col) +
    scale_y_continuous(limits = c(0, ymax), expand = c(0, 0)) +
    labs(title = title, x = NULL, y = "% of sentences") +
    theme(
      plot.title = element_text(hjust = 0, face = "plain", size = 11),
      plot.margin = margin(8, 12, 4, 8)
    )
}

setorder(rba, date)
setorder(fed, date)

p_rba_i <- make_panel(rba, "share_indec", "rollm_indec",indec_colour,
                      "RBA Board indecision", indec_max)

p_fed_i <- make_panel(fed, "share_indec", "rollm_indec",indec_colour,
                      "FOMC indecision", indec_max)

p_rba_c <- make_panel(rba, "share_cred", "rollm_cred",cred_colour,
                      "RBA Commentary on Inflation Credibility", cred_max)

p_fed_c <- make_panel(fed, "share_cred","rollm_cred", cred_colour,
                      "FOMC Commentary on Inflation Credibility", cred_max)

# ------- combine via patchwork ---------------------------------------------
chart <- (p_rba_i | p_fed_i) /
  (p_rba_c | p_fed_c) +
  plot_annotation(
    title    = "Central Bank Communication of Uncertainty in Board minutes",
    subtitle = "Comparing RBA to FOMC, 2007 to 2026",
    caption  = "Indecision: phrases like 'await additional information', 'value in waiting', 'proceed cautiously'.
    \nInflation credibility: 'risk of expectations becoming deanchored', 'inflation could prove more persistent'.")


print(chart)

ggsave(file.path(graph_output, "rba_vs_fed.png"),
       chart, width = 12, height = 12)
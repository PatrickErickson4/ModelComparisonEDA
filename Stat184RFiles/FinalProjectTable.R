library(dplyr)
library(knitr)
library(kableExtra)

data <- read.csv("Stat184ProjectTables/fullmodel.csv")

stats_list <- lapply(data, function(x) {
  c(
    Freq. = sum(!is.na(x)),           
    Min = min(x, na.rm = TRUE),
    `25%-tile` = quantile(x, 0.25, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    `75%-tile` = quantile(x, 0.75, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    SASD = sd(x, na.rm = TRUE)     
  )
})

summary_stats <- as.data.frame(do.call(rbind, stats_list))
summary_stats <- cbind(`Game Statistic` = rownames(summary_stats), summary_stats)
rownames(summary_stats) <- NULL
summary_stats %>%
  kable(
    digits = 2,
    col.names = c("Game Statistic", "Freq.", "Min", "25%-tile", "Median",
                  "75%-tile", "Max", "SASD"),
    format.args = list(big.mark = ","),
    align = "lcccccccccc"
  ) %>%
  kable_classic() %>%
  add_footnote(
    label = c("Width was measured in millimeters."),
    notation = "none",
    threeparttable = TRUE
  )


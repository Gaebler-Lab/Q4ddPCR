# Processing_Q4ddPCR.R
# R script to apply Q4ddPCR decision tree to Output file after using the Multiplex_PCR_Analysis package (https://doi.org/10.5281/zenodo.15791355) 
# with its corresponding Run file.R (https://doi.org/10.5281/zenodo.16414847)

# Usage:
# - Place your output Excel file you received after using the Run file.R to the path below (input_file)
# - Per-participant options may be provided. If they are absent the script will assume that you used IPDA-based Q4ddPCR, no longitudinal samples, 
# did not observe no env- or Psi- failures
#     IPDA_Q4ddPCR         : TRUE/FALSE (if TRUE, IPDA results will be shown additionally to Q4ddPCR results)
#     Q4ddPCR_analyzed     : TRUE/FALSE (if TRUE, Q4ddPCR_readout should be provided)
#     Q4ddPCR_readout      : Q4ddPCR readout in characters "envgag" or "4-color" etc. (used to pick intacts in longitudinal samples)
#     env_failure          : TRUE/FALSE
#     psi_failure          : TRUE/FALSE
#     ignore_psi_failure   : TRUE/FALSE (if TRUE the psi failure will be ignored and decision tree will be applied)

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)
library(tibble)
library(purrr)

# Path of the Q4ddPCR Output File
input_file <- "/path/Output_from Run File.xlsx"   # path to your Multiplex_PCR_Analysis output file
input_sheet <- 1                     # sheet name or index
output_file <- "/path/Q4ddPCR_Summary.xlsx" # path for your summary result file

# Participant specific options: one row per sample you want to set specific settings 
# Sample = Sample ID as in your Output file
# IPDA_Q4ddPCR = Did you use IPDA-based Q4ddPCR for this sample? 
# Q4ddPCR_analyzed = Longitudinal sample where you already analyzed another time point for this participant?
# Q4ddPCR_readout = If Q4ddPCR_analyzed is TRUE, which read-out did you use? type "4-colors", "envgagPsi", "envPsipol", "envPsi", "envgagpol" or "envgag"
# env_failure = Did you observe an env failure in this sample?
# psi_failure = Did you observe a Psi failure in this sample? 
# ignore_Psi_failure = Do you want the script to ignore the Psi failure and proceed to an alternative Q4ddPCR readout?
participant_options <- tibble::tribble(
    ~Sample,  ~IPDA_Q4ddPCR, ~Q4ddPCR_analyzed, ~Q4ddPCR_readout, ~env_failure, ~psi_failure, ~ignore_psi_failure,
    '37.1',    TRUE,          TRUE,             "envgagPsi",          TRUE,         TRUE,         TRUE, # add your participant specifics here
    '33.1',    FALSE,         FALSE,            NA,                FALSE,          FALSE,        TRUE
  )
  
# ---------------------- END OF MANUAL SETTINGS -------------------------
# ---- 1) Read input ----
raw <- readxl::read_excel(input_file, sheet = input_sheet)
# normalize sample names to character and trimmed (avoid join mismatch)
raw <- raw %>% mutate(Sample = as.character(Sample), Sample = str_trim(Sample))

# ----------------- 2) Select columns to import -----------------
all_cols <- colnames(raw)

# include columns with total.HIV.DNA, corrected.for.shearing, and intact.provirus 
pattern_cols <- all_cols[str_detect(all_cols, regex("total\\.HIV\\.DNA|corrected\\.for\\.shearing|intact.provirus", ignore_case = TRUE))]

explicit_cols <- c("Number.of.cells.analysed", "Shearing.Index",
                   "Gag.Mio.cells.Mean.Target.Mio.cells",
                   "Pol.Mio.cells.Mean.Target.Mio.cells",
                   "Psi.Mio.cells.Mean.Target.Mio.cells",
                   "Env.Mio.cells.Mean.Target.Mio.cells")

selected <- unique(c("Sample", pattern_cols, intersect(explicit_cols, all_cols)))
df_sel <- raw %>% select(any_of(selected))

# ----------------- 3) Join participant options (after selection) -----------------
df <- df_sel %>%
  left_join(participant_options, by = "Sample") %>%
  # set defaults if options were not provided for a sample
  mutate(
    IPDA_Q4ddPCR       = coalesce(IPDA_Q4ddPCR,       TRUE),
    Q4ddPCR_analyzed   = coalesce(Q4ddPCR_analyzed,   FALSE),
    Q4ddPCR_readout    = coalesce(Q4ddPCR_readout,    NA_character_),
    env_failure        = coalesce(env_failure,        FALSE),
    psi_failure        = coalesce(psi_failure,        FALSE),
    ignore_psi_failure = coalesce(ignore_psi_failure, TRUE)
  )

# ----------------- 4) Rename long columns to short names and copy numeric values -----------------
rename_map <- c(
  "Total.HIV.DNA..4.based...copies.Mio.cells." = "Total HIV DNA Q4ddPCR",
  "intact.provirus.Mio.cells.Psi.Env..corrected.for.shearing" = "envPsi",
  "intact.provirus.Mio.cells.Env.Gag..corrected.for.shearing" = "envgag",
  "intact.provirus.Mio.cells.Psi.Env.Gag..corrected.for.shearing" = "envgagPsi",
  "intact.provirus.Mio.cells.Psi.Env.Pol..corrected.for.shearing" = "envPsipol",
  "intact.provirus.Mio.cells.Env.Gag.Pol..corrected.for.shearing" = "envgagpol",
  "intact.provirus.Mio.cells.Psi.Env.Gag.Pol..corrected.for.shearing" = "4-color",
  "Total.HIV.DNA..Env.Psi...copies.Mio.cells." = "Total HIV DNA IPDA"
)

for (long_name in names(rename_map)) {
  short_name <- rename_map[[long_name]]
  if (long_name %in% colnames(df)) {
    # copy numeric values from long column into short-name column
    df[[short_name]] <- suppressWarnings(as.numeric(df[[long_name]]))
  } else {
    # if long column missing, create empty numeric column
    df[[short_name]] <- NA_real_
  }
}

# ----------------- 5) Ensure numeric versions exist -----------------
ensure_numeric <- function(df, cols) {
  for (nm in cols) {
    if (!(nm %in% colnames(df))) df[[nm]] <- NA_real_
    df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
  }
  df
}
# ----------------- 6) Per-participant QC warnings and messages -----------------
miocols <- c(
  "Gag.Mio.cells.Mean.Target.Mio.cells",
  "Pol.Mio.cells.Mean.Target.Mio.cells",
  "Psi.Mio.cells.Mean.Target.Mio.cells",
  "Env.Mio.cells.Mean.Target.Mio.cells"
)

df <- df %>%
  mutate(
    # collect numeric probe values as a *named* row-wise list
    probe_values = pmap(select(., all_of(miocols)), c)
  ) %>%
  rowwise() %>%
  mutate(
    Notes = "",
    Notes = ifelse(!is.na(Shearing.Index) & Shearing.Index > 0.45,
                   str_trim(paste(Notes, "High shearing (Shearing.Index =", Shearing.Index, ").")), Notes),
    Notes = ifelse(!is.na(Number.of.cells.analysed) & Number.of.cells.analysed < 40000,
                   str_trim(paste(Notes, "Low number of cell equivalents analysed (<40000).")), Notes),
    
    # numeric minimum across the named vector
    lowest_probe      = suppressWarnings(min(unlist(probe_values), na.rm = TRUE)),
    probe_signal_warn = any(unlist(probe_values) < 10, na.rm = TRUE),
    
    Notes = ifelse(probe_signal_warn,
                   str_trim(paste(Notes,
                                  "Potential signal failure: check QX Manager plots and consider different primer/probes.")),
                   Notes),
    
    lowest_probe      = ifelse(is.infinite(lowest_probe), NA_real_, lowest_probe),
    lowest_probe_name = if (all(is.na(unlist(probe_values))))
      NA_character_
    else
      names(probe_values)[which.min(unlist(probe_values))]
  ) %>%
  ungroup() %>%
  select(-probe_values)    

# ----------------- 7) IPDA handling -----------------
df <- df %>%
  mutate(
    `Total HIV DNA IPDA` = ifelse(IPDA_Q4ddPCR, `Total HIV DNA IPDA`, NA_real_),
    Notes = ifelse(!IPDA_Q4ddPCR,
                   str_trim(paste(Notes, "You are not working with IPDA primer and probes.")),
                   Notes)
  )

# ---- 8) Manual Q4ddPCR override for intacts ----
if (!"Readout_used" %in% colnames(df)) df$Readout_used <- NA_character_
if (!"intacts_per_E6_CD4" %in% colnames(df)) df$intacts_per_E6_CD4 <- NA_real_

for (i in seq_len(nrow(participant_options))) {
  sample_id <- participant_options$Sample[i]
  readout <- participant_options$Q4ddPCR_readout[i]
  analyzed <- participant_options$Q4ddPCR_analyzed[i]
  
  if (!is.na(readout) && analyzed == TRUE) {
    row_idx <- which(df$Sample == sample_id)
    if (length(row_idx) == 1) {
      # case-insensitive match to short column names
      col_idx <- which(tolower(colnames(df)) == tolower(readout))
      if (length(col_idx) == 1) {
        chosen_col <- colnames(df)[col_idx]
        df$intacts_per_E6_CD4[row_idx] <- suppressWarnings(as.numeric(df[[chosen_col]][row_idx]))
        df$Readout_used[row_idx] <- chosen_col
      } else {
        # fallback note if no match
        df$Notes[row_idx] <- paste0(df$Notes[row_idx],
                                    " Q4ddPCR_readout '", readout, "' not found; manual override skipped.")
      }
    }
  }
}
# ----------------- 9) Env / Psi failures override -----------------
df <- df %>% mutate(env.Psi.status = NA_character_)

for (i in seq_len(nrow(df))) {
  # Env failure: either observed or signal = 0
  env_fail_signal <- !is.na(df$`Env.Mio.cells.Mean.Target.Mio.cells`[i]) &&
    df$`Env.Mio.cells.Mean.Target.Mio.cells`[i] == 0
  
  psi_fail_signal <- !is.na(df$`Psi.Mio.cells.Mean.Target.Mio.cells`[i]) &&
    df$`Psi.Mio.cells.Mean.Target.Mio.cells`[i] == 0
  
  if (isTRUE(df$env_failure[i]) || env_fail_signal) {
    df$env.Psi.status[i] <- "env failure, redo Q4ddPCR with alternative env primer and probe"
    df$intacts_per_E6_CD4[i] <- NA_real_
    df$Readout_used[i] <- NA_character_
  } else if ((isTRUE(df$psi_failure[i]) || psi_fail_signal) && !isTRUE(df$ignore_psi_failure[i])) {
    df$env.Psi.status[i] <- "Psi failure, redo Q4ddPCR with alternative primer and probe set"
    df$intacts_per_E6_CD4[i] <- NA_real_
    df$Readout_used[i] <- NA_character_
  }
}

df <- df %>%
  mutate(
    Notes = ifelse(isTRUE(df$env_failure) | (`Env.Mio.cells.Mean.Target.Mio.cells` == 0),
                   str_trim(paste(Notes, "Env failure observed; use alternative env primer/probes.")), Notes),
    Notes = ifelse((isTRUE(df$psi_failure) | (`Psi.Mio.cells.Mean.Target.Mio.cells` == 0)) & !ignore_psi_failure,
                   str_trim(paste(Notes, "Psi failure observed; use other Q4ddPCR primer/probe set")), Notes)
  )
# ----------------- 10) Priority selection for remaining samples -----------------
priority <- c("4-color", "envgagPsi", "envPsipol", "envPsi", "envgagpol", "envgag")

for (i in seq_len(nrow(df))) {
  if (!is.na(df$env.Psi.status[i])) next
  if (!is.na(df$intacts_per_E6_CD4[i])) next
  chosen <- NA_real_; chosen_readout <- NA_character_
  for (p in priority) {
    if (p %in% colnames(df)) {
      val <- suppressWarnings(as.numeric(df[[p]][i]))
      if (!is.na(val) && val > 0) {
        chosen <- val; chosen_readout <- p; break
      }
    }
  }
  if (is.na(chosen)) {
    df$intacts_per_E6_CD4[i] <- 0
    df$Readout_used[i] <- NA_character_
  } else {
    df$intacts_per_E6_CD4[i] <- chosen
    df$Readout_used[i] <- chosen_readout
  }
}

# ----------------- 11) Build summary -----------------
summary <- df %>%
  mutate(
    total_HIV_DNA_Q4ddPCR = as.numeric(`Total HIV DNA Q4ddPCR`),
    intacts = as.numeric(intacts_per_E6_CD4),
    defectives = ifelse(!is.na(total_HIV_DNA_Q4ddPCR) & !is.na(intacts), total_HIV_DNA_Q4ddPCR - intacts, NA_real_),
    intact_over_total = ifelse(!is.na(intacts) & !is.na(total_HIV_DNA_Q4ddPCR) & total_HIV_DNA_Q4ddPCR != 0, intacts / total_HIV_DNA_Q4ddPCR *100, NA_real_),
    IPDA_intacts_per_E6_CD4 = as.numeric(envPsi),
    Total_HIV_DNA_IPDA = as.numeric(`Total HIV DNA IPDA`),
    IPDA_defectives_per_E6_CD4 = ifelse(!is.na(Total_HIV_DNA_IPDA) & !is.na(IPDA_intacts_per_E6_CD4), Total_HIV_DNA_IPDA - IPDA_intacts_per_E6_CD4, NA_real_),
    intact_fraction_IPDA = ifelse(!is.na(Total_HIV_DNA_IPDA) & !is.na(IPDA_intacts_per_E6_CD4) & IPDA_intacts_per_E6_CD4 != 0,
                                  IPDA_intacts_per_E6_CD4 / Total_HIV_DNA_IPDA *100, NA_real_)
  ) %>%
  select(Sample,
         intacts,
         Readout_used,
         total_HIV_DNA_Q4ddPCR,
         defectives,
         intact_over_total,
         IPDA_intacts_per_E6_CD4,
         Total_HIV_DNA_IPDA,
         IPDA_defectives_per_E6_CD4,
         intact_fraction_IPDA,
         Notes)

colnames(summary) <- c("Participant",
                      "Intacts per E6 CD4",
                       "Readout",
                       "Total HIV DNA Q4ddPCR per E6 CD4",
                       "Defectives per E6 CD4",
                       "Intact fraction (%)",
                       "IPDA Intacts per E6 CD4",
                       "Total HIV DNA IPDA per E6 CD4",
                       "IPDA Defectives per E6 CD4",
                       "Intact fraction IPDA (%)",
                       "Notes")

# ----------------- 12) Export to Excel -----------------
wb <- createWorkbook()
addWorksheet(wb, "summary")
addWorksheet(wb, "processed_data")
write_df <- df
if (!("env.Psi.status" %in% colnames(write_df))) write_df$env.Psi.status <- NA_character_
writeData(wb, sheet = "processed_data", x = write_df)
writeData(wb, sheet = "summary", x = summary)

saveWorkbook(wb, output_file, overwrite = TRUE)

message("Processing complete. Output written to: ", output_file)

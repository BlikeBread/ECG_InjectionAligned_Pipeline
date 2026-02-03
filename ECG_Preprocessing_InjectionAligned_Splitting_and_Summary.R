###############################################################################
# ECG pipeline
# Folder structure produced (inside chosen OUTPUT folder):
#
#   Split_Files/
#     Baseline/
#     After_Injection/
#     After_Injection_2min/
#
#   Split_By_Columns/
#     Baseline/Aligned_by_Columns/ + Summary_Averages_Aligned_Baseline.csv
#     After_Injection/Aligned_by_Columns/ + Summary_Averages_Aligned_After_Injection.csv
#     After_Injection_2min/Aligned_by_Columns/ + Summary_Averages_Aligned_After_Injection_2min.csv
#
#   Summary_Clean_*.xlsx (3 files)
#
# Also writes: Updated_Injection_Times_ECG.xlsx (mouse_id + TimeFormatted + TimeFormatted_2min)
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readxl)
  library(openxlsx)
  library(tibble)
  library(tcltk)
})

###############################################################################
#### 0) USER INPUTS (GUI) ######################################################
###############################################################################

data_dir <- tk_choose.dir(caption = "Select INPUT folder containing RAW ECG CSV files")
if (is.na(data_dir) || !nzchar(data_dir)) stop("❌ No RAW input folder selected.")

split_values_file <- tk_choose.files(
  caption = "Select Excel file with injection times",
  filter = matrix(c("Excel files", "*.xlsx"), ncol = 2, byrow = TRUE)
)
if (!length(split_values_file)) stop("❌ No injection-times Excel file selected.")

output_root <- tk_choose.dir(caption = "Select OUTPUT folder (results will be written inside)")
if (is.na(output_root) || !nzchar(output_root)) stop("❌ No output folder selected.")

###############################################################################
#### 1) DEFINE OUTPUT PATHS (NEW STRUCTURE) ####################################
###############################################################################

# Updated injection-time Excel (keep only original + 2min)
updated_injection_times_xlsx <- file.path(output_root, "Updated_Injection_Times_ECG.xlsx")

# Split files root + three subfolders (as requested)
split_root <- file.path(output_root, "Split_Files")
split_dir_baseline <- file.path(split_root, "Baseline")
split_dir_after_inj <- file.path(split_root, "After_Injection")
split_dir_after_inj_2min <- file.path(split_root, "After_Injection_2min")

# Split-by-columns root (aligned only)
split_by_cols_root <- file.path(output_root, "Split_By_Columns")

aligned_cols_baseline_dir <- file.path(split_by_cols_root, "Baseline", "Aligned_by_Columns")
aligned_cols_after_inj_dir <- file.path(split_by_cols_root, "After_Injection", "Aligned_by_Columns")
aligned_cols_after_inj_2min_dir <- file.path(split_by_cols_root, "After_Injection_2min", "Aligned_by_Columns")

summary_baseline_csv <- file.path(split_by_cols_root, "Baseline", "Summary_Averages_Aligned_Baseline.csv")
summary_after_inj_csv <- file.path(split_by_cols_root, "After_Injection", "Summary_Averages_Aligned_After_Injection.csv")
summary_after_inj_2min_csv <- file.path(split_by_cols_root, "After_Injection_2min", "Summary_Averages_Aligned_After_Injection_2min.csv")

# Clean summaries (xlsx)
clean_baseline_xlsx <- file.path(output_root, "Summary_Clean_Baseline.xlsx")
clean_after_inj_xlsx <- file.path(output_root, "Summary_Clean_After_Injection.xlsx")
clean_after_inj_2min_xlsx <- file.path(output_root, "Summary_Clean_After_Injection_2min.xlsx")

###############################################################################
#### 2) LOAD RAW CSVs -> combined_df ###########################################
###############################################################################

csv_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
if (!length(csv_files)) stop("❌ No CSV files found in selected RAW folder.")

ecg_data_list <- lapply(csv_files, function(file) {
  df <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
  df$File_Name <- basename(file)
  df
})

combined_df <- bind_rows(ecg_data_list)

combined_df <- combined_df %>%
  mutate(
    Date = as.Date(TimeDate, format = "%m/%d/%Y"),
    Time = sub(".* ", "", TimeDate)
  ) %>%
  mutate(
    TimeFormatted_ms = case_when(
      grepl("^\\d+\\.\\d+$", Time) ~ sprintf("00:%05.2f", as.numeric(Time)),
      grepl("^\\d+:\\d+\\.\\d+$", Time) ~ Time,
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(TimeFormatted = sub("\\.\\d{2}$", "", TimeFormatted_ms)) %>%
  mutate(
    File_Name = gsub("\\.csv$", "", File_Name),
    TimeFormatted = format(strptime(TimeFormatted, format = "%M:%S"), "%M:%S")
  )

###############################################################################
#### 3) LOAD injection times -> TimeFormatted + TimeFormatted_2min only ########
###############################################################################

split_values_df <- readxl::read_excel(split_values_file)

split_values_df <- split_values_df %>%
  dplyr::rename(mouse_id = ...1, time_date = `Injection time`)

# Keep your approach: Excel time -> "%H:%M" -> coerce to "%M:%S.0" -> strip ".0"
split_values_df <- split_values_df %>%
  mutate(TimeDate = format(as.POSIXct(time_date, origin = "1899-12-30", tz = "UTC"), "%H:%M")) %>%
  select(mouse_id, TimeDate)

split_values_df_2 <- as.data.frame(split_values_df)
split_values_df_2$TimeDate <- format(as.POSIXct(split_values_df_2$TimeDate, format="%M:%S"), "%M:%S.0")
split_values_df_2$TimeFormatted <- gsub("\\.0$", "", split_values_df_2$TimeDate)

split_values_df_2$TimeFormatted_2min <- format(
  as.POSIXct(split_values_df_2$TimeFormatted, format="%M:%S") + 120,
  "%M:%S"
)

# Keep ONLY what you want
split_values_df_2 <- split_values_df_2 %>%
  dplyr::select(mouse_id, TimeFormatted, TimeFormatted_2min)

openxlsx::write.xlsx(split_values_df_2, updated_injection_times_xlsx)
message("✅ Updated injection-time Excel saved to: ", updated_injection_times_xlsx)

# Named split values
split_values_standard <- setNames(split_values_df_2$TimeFormatted, split_values_df_2$mouse_id)
split_values_2min <- setNames(split_values_df_2$TimeFormatted_2min, split_values_df_2$mouse_id)

# Ensure consistent formatting
split_values_standard <- lapply(split_values_standard, function(x) format(strptime(x, "%M:%S"), "%M:%S"))
split_values_2min <- lapply(split_values_2min, function(x) format(strptime(x, "%M:%S"), "%M:%S"))

###############################################################################
#### 4) CHECK IDs ##############################################################
###############################################################################

check_matching_ids <- function(combined_df, split_values_named, label) {
  missing_ids <- setdiff(names(split_values_named), unique(combined_df$File_Name))
  if (length(missing_ids) > 0) {
    message("⚠️ Missing mouse IDs (", label, "):")
    print(missing_ids)
  } else {
    message("✅ All mouse IDs match (", label, ").")
  }
}

check_matching_ids(combined_df, split_values_standard, "standard")
check_matching_ids(combined_df, split_values_2min, "2min")

###############################################################################
#### 5) SPLIT FUNCTION (writes into a single folder per split type) ############
###############################################################################

generate_csv_files_flat <- function(combined_data, split_values, output_dir, baseline_suffix, injected_suffix) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  for (mouse_id in names(split_values)) {
    split_time <- split_values[[mouse_id]]
    
    mouse_data <- combined_data %>% filter(File_Name == mouse_id)
    if (nrow(mouse_data) == 0) {
      message(paste("No data found for mouse_id:", mouse_id))
      next
    }
    
    baseline_data <- mouse_data %>% filter(TimeFormatted < split_time)
    injected_data <- mouse_data %>% filter(TimeFormatted >= split_time)
    
    if (nrow(baseline_data) == 0 && nrow(injected_data) == 0) {
      message(paste("No valid data for mouse_id:", mouse_id))
      next
    }
    
    if (nrow(baseline_data) > 0) {
      write.csv(
        baseline_data,
        file = file.path(output_dir, paste0(mouse_id, baseline_suffix, ".csv")),
        row.names = FALSE
      )
    }
    
    if (nrow(injected_data) > 0) {
      write.csv(
        injected_data,
        file = file.path(output_dir, paste0(mouse_id, injected_suffix, ".csv")),
        row.names = FALSE
      )
    }
  }
}

###############################################################################
#### 6) ALIGNED BY COLUMN + SUMMARY (generic) ##################################
###############################################################################

aligned_by_column_and_summary <- function(files_dir, file_suffix_regex, output_dir, summary_file) {
  
  files <- list.files(files_dir, pattern = "\\.csv$", full.names = TRUE)
  if (!length(files)) stop("❌ No CSV files found in: ", files_dir)
  
  sample_file <- read.csv(files[1], stringsAsFactors = FALSE)
  
  target_columns <- setdiff(
    colnames(sample_file),
    c("X","TimeDate","mouse_id","Used","Edited","Date","Time","TimeFormatted_ms","TimeFormatted","File_Name")
  )
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(summary_file), recursive = TRUE, showWarnings = FALSE)
  
  # Step 1: aligned matrices per column
  for (target_column in target_columns) {
    selected_column_data <- list()
    max_rows <- 0
    
    for (f in files) {
      mouse_id <- sub(file_suffix_regex, "", basename(f))
      mouse_data <- read.csv(f, stringsAsFactors = FALSE)
      
      if (!(target_column %in% colnames(mouse_data))) next
      
      v <- mouse_data[[target_column]]
      max_rows <- max(max_rows, length(v))
      selected_column_data[[mouse_id]] <- v
    }
    
    # pad NA
    for (mouse_id in names(selected_column_data)) {
      cur_len <- length(selected_column_data[[mouse_id]])
      if (cur_len < max_rows) {
        selected_column_data[[mouse_id]] <- c(selected_column_data[[mouse_id]], rep(NA, max_rows - cur_len))
      }
    }
    
    final_dataset <- as.data.frame(selected_column_data)
    write.csv(final_dataset, file = file.path(output_dir, paste0(target_column, ".csv")), row.names = FALSE)
  }
  
  # Step 2: summary means per mouse
  summary_data <- data.frame(mouse_id = character())
  
  for (target_column in target_columns) {
    column_averages <- list()
    
    for (f in files) {
      mouse_id <- sub(file_suffix_regex, "", basename(f))
      mouse_data <- read.csv(f, stringsAsFactors = FALSE)
      
      if (!(target_column %in% colnames(mouse_data))) next
      column_averages[[mouse_id]] <- mean(mouse_data[[target_column]], na.rm = TRUE)
    }
    
    column_averages_df <- data.frame(mouse_id = names(column_averages), value = unlist(column_averages))
    
    if (nrow(summary_data) == 0) {
      summary_data <- column_averages_df
    } else {
      summary_data <- full_join(summary_data, column_averages_df, by = "mouse_id")
    }
    
    colnames(summary_data)[ncol(summary_data)] <- target_column
  }
  
  write.csv(summary_data, file = summary_file, row.names = FALSE)
  message("✅ Summary saved: ", summary_file)
}

###############################################################################
#### 7) CLEAN SUMMARY COLUMN NAMES -> XLSX #####################################
###############################################################################

clean_and_write_summary <- function(summary_csv, out_xlsx) {
  
  df <- read.csv(summary_csv, header = TRUE, sep = ",", check.names = FALSE)
  
  df2 <- df %>%
    rename("RR_Interval_sec" = "RR.Interval..s.") %>%
    rename("Heart_Rate_BPM" = "Heart.Rate..BPM.") %>%
    rename("PR_Interval_sec" = "PR.Interval..s.") %>%
    rename("P_Duration_sec" = "P.Duration..s.") %>%
    rename("QRS_Interval_sec" = "QRS.Interval..s.") %>%
    rename("QT_Interval_sec" = "QT.Interval..s.") %>%
    rename("QTc_sec" = "QTc..s.") %>%
    rename("JT_Interval_sec" = "JT.Interval..s.") %>%
    rename("Tpeak_Tend_Interval_sec" = "Tpeak.Tend.Interval..s.") %>%
    rename("P_Amplitude_mV" = "P.Amplitude..mV.") %>%
    rename("Q_Amplitude_mV" = "Q.Amplitude..mV.") %>%
    rename("R_Amplitude_mV" = "R.Amplitude..mV.") %>%
    rename("S_Amplitude_mV" = "S.Amplitude..mV.") %>%
    rename("ST_Height_mV" = "ST.Height..mV.") %>%
    rename("T_Amplitude_mV" = "T.Amplitude..mV.")
  
  # keep first 16 columns if present (as you did)
  keep_n <- min(16, ncol(df2))
  df_final <- df2[, 1:keep_n, drop = FALSE]
  
  openxlsx::write.xlsx(df_final, out_xlsx)
  message("✅ Clean summary written: ", out_xlsx)
}

###############################################################################
#### 8) RUN: CREATE SPLIT FILES ################################################
###############################################################################

# Create split folder
dir.create(split_root, recursive = TRUE, showWarnings = FALSE)

# Baseline: use standard injection time split, but write ONLY baseline rows
# (we write baseline files to Baseline folder, and injected to After_Injection folder)
generate_csv_files_flat(
  combined_data = combined_df,
  split_values  = split_values_standard,
  output_dir    = split_dir_baseline,
  baseline_suffix = "_Baseline",
  injected_suffix = "_IGNORE" # will still be produced if injected exists; we remove below
)

# After injection (standard): write ONLY injected rows into After_Injection folder
generate_csv_files_flat(
  combined_data = combined_df,
  split_values  = split_values_standard,
  output_dir    = split_dir_after_inj,
  baseline_suffix = "_IGNORE",
  injected_suffix = "_After_Injection"
)

# After injection 2min: write ONLY injected rows into After_Injection_2min folder
generate_csv_files_flat(
  combined_data = combined_df,
  split_values  = split_values_2min,
  output_dir    = split_dir_after_inj_2min,
  baseline_suffix = "_IGNORE",
  injected_suffix = "_After_Injection_2min"
)

# Remove any *_IGNORE.csv created (keeps behavior minimal without complex branching)
rm_ignore <- function(dirp) {
  ign <- list.files(dirp, pattern = "_IGNORE\\.csv$", full.names = TRUE)
  if (length(ign)) file.remove(ign)
}
rm_ignore(split_dir_baseline)
rm_ignore(split_dir_after_inj)
rm_ignore(split_dir_after_inj_2min)

message("✅ Split_Files created at: ", split_root)

###############################################################################
#### 9) RUN: ALIGNED BY COLUMN + SUMMARIES (Baseline + After + After_2min) #####
###############################################################################

# BASELINE aligned + summary
aligned_by_column_and_summary(
  files_dir = split_dir_baseline,
  file_suffix_regex = "_Baseline\\.csv$",
  output_dir = aligned_cols_baseline_dir,
  summary_file = summary_baseline_csv
)

# AFTER INJECTION aligned + summary
aligned_by_column_and_summary(
  files_dir = split_dir_after_inj,
  file_suffix_regex = "_After_Injection\\.csv$",
  output_dir = aligned_cols_after_inj_dir,
  summary_file = summary_after_inj_csv
)

# AFTER INJECTION 2MIN aligned + summary
aligned_by_column_and_summary(
  files_dir = split_dir_after_inj_2min,
  file_suffix_regex = "_After_Injection_2min\\.csv$",
  output_dir = aligned_cols_after_inj_2min_dir,
  summary_file = summary_after_inj_2min_csv
)

###############################################################################
#### 10) CLEAN SUMMARIES -> XLSX ###############################################
###############################################################################

clean_and_write_summary(summary_baseline_csv, clean_baseline_xlsx)
clean_and_write_summary(summary_after_inj_csv, clean_after_inj_xlsx)
clean_and_write_summary(summary_after_inj_2min_csv, clean_after_inj_2min_xlsx)

###############################################################################
#### DONE ######################################################################
###############################################################################

message("✅ ALL DONE")
message("📁 RAW input: ", data_dir)
message("📄 Injection Excel: ", split_values_file)
message("📄 Updated injection Excel (only TimeFormatted + 2min): ", updated_injection_times_xlsx)
message("📁 Output root: ", output_root)
message("📁 Split_Files root: ", split_root)

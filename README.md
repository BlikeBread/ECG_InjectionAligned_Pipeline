# ECG Injection-Aligned Preprocessing Pipeline

This repository contains an R script for preprocessing ECG feature data aligned to drug injection time.  
The pipeline splits recordings into baseline and post-injection windows, generates aligned feature matrices, and produces cleaned summary tables ready for statistical analysis.

---

## What the pipeline does

Starting from raw ECG CSV files and an Excel sheet containing injection times, the script:

1. Loads and merges ECG feature CSV files (one file per mouse)
2. Aligns ECG data to the injection time
3. Splits data into three conditions:
   - Baseline (before injection)
   - After_Injection (from injection time)
   - After_Injection_2min (from injection time + 2 minutes)
4. Generates aligned-by-column datasets for each ECG feature
5. Computes per-mouse average values for each feature
6. Exports cleaned summary tables with standardized column names
7. Writes all outputs to a structured, analysis-ready folder hierarchy

All file paths are selected interactively via GUI dialogs.

---

## Output folder structure

The script generates the following structure inside the selected output directory:

Output/
├── Split_Files/
│ ├── Baseline/
│ ├── After_Injection/
│ └── After_Injection_2min/
│
├── Split_By_Columns/
│ ├── Baseline/
│ │ ├── Aligned_by_Columns/
│ │ └── Summary_Averages_Aligned_Baseline.csv
│ │
│ ├── After_Injection/
│ │ ├── Aligned_by_Columns/
│ │ └── Summary_Averages_Aligned_After_Injection.csv
│ │
│ └── After_Injection_2min/
│ ├── Aligned_by_Columns/
│ └── Summary_Averages_Aligned_After_Injection_2min.csv
│
├── Summary_Clean_Baseline.xlsx
├── Summary_Clean_After_Injection.xlsx
├── Summary_Clean_After_Injection_2min.xlsx
│
└── Updated_Injection_Times_with_2min_only.xlsx

---

## Required inputs

### 1. Raw ECG CSV files
- One CSV file per mouse
- File name must match the `mouse_id` in the injection-time Excel file
- Must contain:
  - `TimeDate` column
  - ECG feature columns (e.g. RR interval, QT interval, amplitudes)

### 2. Injection times Excel file
The Excel file must contain:
- Column 1: `mouse_id`
- Column 2: `Injection time` (Excel time format)

The script automatically generates:
- `TimeFormatted` (original injection time)
- `TimeFormatted_2min` (injection time + 2 minutes)

Only these two time columns are retained in the updated Excel output.

---

## How to run the script

1. Open R or RStudio
2. Install required packages (once):

```r
install.packages(c(
  "dplyr", "tidyr", "stringr", "readxl",
  "openxlsx", "tibble", "tcltk"
))

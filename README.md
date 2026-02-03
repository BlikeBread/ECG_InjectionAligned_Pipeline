# ECG Injection-Aligned Preprocessing Pipeline (Script 01/03)

This repository contains an R script for preprocessing ECG feature data aligned to drug injection time.  

This script represents the first step (01/03) of a modular ECG analysis pipeline.

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

## Cleaned summary outputs

The final Excel files:

- Summary_Clean_Baseline.xlsx
- Summary_Clean_After_Injection.xlsx
- Summary_Clean_After_Injection_2min.xlsx

These files are ready for statistical analysis and plotting.

---

## Typical use cases:

- ECG drug response analysis
- Baseline vs post-injection comparisons
- Multi-animal aggregation of ECG features
- Preparation of summary tables for figures
- Reproducible preprocessing in collaborative projects

---

## Position in the ECG pipeline

This script is Script 02 of a 3-step ECG analysis workflow:

Script 01 (this repository) – Injection-aligned preprocessing and window generation
Script 02 – QRS-based beat selection and First-N averaging
Script 03 – Metadata integration, statistics, and visualization

---

## Methods Description

ECG feature data were aligned to the time of drug injection and split into baseline, post-injection, and post-injection (+2 min) windows using a custom R pipeline. Per-mouse averages and aligned feature matrices were generated for downstream statistical analysis.

---

## Authorship

This script was developed by **Michele Buono** and can be used freely for **research purposes**, provided appropriate citation of the author.
The overall workflow, structure, and clarity of the pipeline were iteratively refined with assistance from **ChatGPT**, which was used as a tool to improve code organization, documentation, and usability.

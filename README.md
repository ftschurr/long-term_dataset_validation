# Git to the Dataset: European multi-trial data set on carbon and nitrogen isotope ratios of winter wheat spanning 170 years

This repository contains the R processing pipeline, data merging scripts, and technical validation code used to produce the **Long-term European Winter Wheat Isotope Dataset (1850–2024)**.

## 🔗 Data Access
The harmonized master files and metadata are hosted on **Zenodo**. For the duration of the peer-review process, the data is accessible via this private reviewer link:

**[Access Dataset on Zenodo](WILL FOLLOW)**

---

## 📂 Repository Contents

### `data/preprocessed/`
This folder must contain the local files necessary to initialize the pipeline:
* **Preprocessed Isotope CSVs:** Extracted measurements for d13C and d15N.
* **treatment_definer.csv:** A lookup table used to map site-specific treatments to a harmonized `nitrogen treatment_level`.
* **Remeasured RDS:** A reference list (`.rds`) for samples that underwent multiple measurements.

### `Scripts/`
1. **`01_DATASET_PREPARATION_merge_iso_and_meta_data.R`**:
    * Connects to the Zenodo API to fetch master metadata.
    * Homogenizes yield data to a consistent Dry Matter (DM) basis across all ten European trials (Swiss, French, German, Danish, and UK sites).
    * Merges isotope concentrations (C%, N%), ratios (C:N), and delta values (d13C, d15N) using **Unique Identifiers (UID)**.
    * Cleans historical outliers (e.g., specific 15N enrichment experiments at Rothamsted).
2. **`02_TECHNICAL_VALIDATION_overview_figures.R`**:
    * Conducts technical validation by generating multi-panel figures.
    * Visualizes distributions of nitrogen input, sowing/harvest dates, yields, and isotopic compositions

---

## 🚀 Usage

### 1. Requirements
The scripts include a self-installing header for all required R packages. Ensure you have an active internet connection to download the following if not already installed:
`readr`, `ggplot2`, `dplyr`, `cowplot`, `tidyr`, `patchwork`, `readxl`, `httr`, `purrr`.

### 2. Execution
1. Clone this repository to your local machine.
2. Open the scripts and update the `base_path` variable to match your local directory structure:
   ```R
   base_path <- "PATH/TO/YOUR/GIT/DIR"

* **Step 3:** Execute the **Merging Script** first to generate the two main publication files:
  * `winter_wheat_isotopes_management.csv`
  * `winter_wheat_yield.csv`
* **Step 4:** Run the **Validation Script** to verify agronomic and physiological consistency through visual plots.

---

## 🧑‍🔬 Contact & Attribution
* **Author:** Flavian Tschurr
* **Email:** [flavian.tschurr@usys.ethz.ch](mailto:flavian.tschurr@usys.ethz.ch)  
* **Institution:** ETH Zürich | [Grassland Sciences Group](https://gl.ethz.ch/)


---

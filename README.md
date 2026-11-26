# AutoSpectral Shiny Implementation

## Overview

This repository contains a Shiny implementation of the existing **AutoSpectral** R package for spectral flow cytometry unmixing.

Original AutoSpectral repository: https://github.com/DrCytometer/AutoSpectral

This Shiny application provides a user-friendly interface to:

- Select the cytometer used for the acquisition (Cytek Aurora or BD FACSDiscover S8).
- Upload single-stained control FCS files and the fully stained (raw) FCS files to unmix.
- Automatically build the control table, with markers pre-filled from a fluorophore → marker reference. A control whose fluorophore cannot be recognised is named after its file, without `(Beads).fcs` or `(Cells).fcs` (`NF violet 690 (Beads).fcs` → `NF violet 690`); a marker that cannot be recognised is left empty; the channel of such a control is left blank.
- Edit the control table in the application, or download it, complete it and upload it again (.csv, .xls, .xlsx).
- Check the control table and highlight the elements to fix.
- Choose the unstained beads and unstained cells used as negatives and for autofluorescence.
- Prepare the controls: automatic gating, cleaning and extraction of the fluorophore spectra.
- Optionally draw manual FSC/SSC gates and regroup the controls that share a gate.
- Visualize the control figures (spectra, similarity heatmap, gates, spectral ribbon, scatter, cleaned controls).
- Unmix the raw files with AutoSpectral, OLS, WLS or FastPoisson.
- Visualize the unmixed files on bidimensional plots.
- Export the unmixed FCS files, the unmixing matrices and all the figures.

## 1. Requirements

Before running the application, make sure you have installed:

- R: https://cran.r-project.org/
- RStudio (recommended): https://posit.co/download/rstudio-desktop/
- A C++ compiler, to build AutoSpectralRcpp from source:
  - Windows: Rtools (https://cran.r-project.org/bin/windows/Rtools/)
  - macOS: Xcode Command Line Tools (`xcode-select --install`)
  - Linux: `build-essential` (or the equivalent for your distribution)

An internet connection is required to install the R packages.

## 2. Installation of required R packages

The application requires packages from CRAN, Bioconductor and GitHub.

### 2.1. Install CRAN packages

Run the following commands in the R console:

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "shinyFiles",
  "shinybusy",
  "DT",
  "ggplot2",
  "readr",
  "openxlsx",
  "readxl",
  "jpeg",
  "base64enc",
  "htmltools",
  "zip",
  "remotes"
))
```

`readxl` is only needed to import a control table saved in the legacy `.xls` format.

### 2.2. Install Bioconductor packages

The application uses flowCore to read FCS files and also loads flowDensity at startup.

AutoSpectral also uses FlowSOM to compute the spectral variants.

Install them using:

```r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

BiocManager::install(c(
  "flowCore",
  "flowDensity",
  "FlowSOM"
))
```

### 2.3. Install AutoSpectral from GitHub

Install the original AutoSpectral package:

```r
remotes::install_github("DrCytometer/AutoSpectral")
```

Then install AutoSpectralRcpp, the fast C++ companion of AutoSpectral. It is strongly recommended: it speeds up the gating, the autofluorescence extraction and the unmixing.

```r
remotes::install_github("DrCytometer/AutoSpectralRcpp")
```

## 3. Check package installation

To check that the main packages are correctly installed, run:

```r
required_packages <- c(
  "shiny",
  "shinydashboard",
  "shinyFiles",
  "shinybusy",
  "DT",
  "flowCore",
  "flowDensity",
  "FlowSOM",
  "AutoSpectral",
  "AutoSpectralRcpp",
  "ggplot2",
  "readr",
  "openxlsx",
  "jpeg",
  "base64enc",
  "htmltools",
  "zip"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) == 0) {
  message("All required packages are installed.")
} else {
  message(
    "Missing packages: ",
    paste(missing_packages, collapse = ", ")
  )
}
```

## 4. Run the Shiny application

1. Download or clone this GitHub repository.
2. Open the project folder in RStudio.
3. Make sure all required packages are installed.
4. Open `autospectral_app/ui.R` and `autospectral_app/server.R`.
5. Launch the application by running:

```r
shiny::runApp("autospectral_app")
```

Alternatively, if the files are in a specific directory:

```r
shiny::runApp("path/to/autospectral_app")
```

Keep `marker_reference.csv` in the same folder as `ui.R` and `server.R`: the application reads it to pre-fill the markers of the control table.

## 5. Application workflow

The application is organized into four main tabs.

### Upload FCS

- Select the cytometer (Aurora or S8).
- Upload the single-stained controls (beads and/or cells, including the unstained controls).
- Upload the raw (fully stained) files to unmix. All raw files are unmixed with the same controls.

### Parameters

- Choose the unstained beads (default: `billes (Beads).fcs`) and the unstained cells used for the autofluorescence.
- Edit the control table built from the uploaded controls (filename, fluorophore, marker, channel, control type, universal negative, viability).
- Download the uncompleted table, complete it and upload it again (.csv, .xls or .xlsx).
- Check the controls: the problems found are listed and the cells to fix are shown in red.

### Control preparation

- Prepare the controls: automatic gating, cleaning and extraction of the fluorophore spectra (a log window follows the run).
- Browse the control figures (spectra, similarity heatmap, gates, spectral ribbon, scatter, cleaned controls) as a grid or one by one.
- Optionally draw a manual FSC/SSC gate by clicking on the plot, save it and apply the manual gates.
- Create gate groups of controls that share one automatic gate.
- Download all the unmixing matrices (OLS unmixing matrix and spillover signature matrix) in a ZIP archive.
- Download all the figures, with a PDF report, in a ZIP archive.

### Analysis

- Choose the unmixing method:
  - **AutoSpectral**: per-cell unmixing that also fits the autofluorescence spectra (from the unstained cells) and the spectral variants. Choose its speed (fast, medium or slow).
  - **OLS**: ordinary least squares.
  - **WLS**: weighted least squares, down-weights the noisiest detectors.
  - **FastPoisson**: iterative maximum-likelihood unmixing assuming Poisson photon noise.
- Start the unmixing of all the raw files.
- Visualize two markers of an unmixed file on a bidimensional plot.
- Download the unmixed FCS files in a ZIP archive.

## 6. References

**AutoSpectral**

Burton, O., Liston, A. & Laniewski, N. AutoSpectral: Tools for Unmixing Spectral Flow Cytometry Data. R package.

GitHub: https://github.com/DrCytometer/AutoSpectral

Documentation: https://drcytometer.github.io/AutoSpectral/

**AutoSpectralRcpp**

GitHub: https://github.com/DrCytometer/AutoSpectralRcpp

## 7. Example dataset

To try the application, you can use the AutoSpectral Aurora example dataset (10 single-stained human PBMC controls and 1 fully stained sample, Cytek Aurora 5L):

Burton, O. & Liston, A. (2025). AutoSpectral Aurora Example. Mendeley Data, V1. https://doi.org/10.17632/xzt3h3gnx9.1

A completed control table for this dataset (`aurora_fcs_control_file.csv`) can be uploaded in the **Parameters** tab.

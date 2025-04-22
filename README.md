# 786-NMA
# NMA Shiny App

An interactive web application built with R Shiny for conducting frequentist Network Meta-Analysis (NMA) of binary outcomes (Risk Ratio or Odds Ratio), based on the `netmeta` package.

## Features

* Supports arm-level and contrast-level data input via CSV upload.
* Performs frequentist NMA using fixed-effect or random-effects models (`netmeta`).
* Includes optional network meta-regression with study-level covariates.
* Generates interactive and static network plots (`visNetwork`, `igraph`).
* Provides customizable forest plots, league tables, treatment rankings, funnel plots, and inconsistency checks.
* Modern dashboard interface using `bs4Dash`.
* Asynchronous processing for potentially long computations (`future`, `promises`).
* Includes sample data and download options for results.

## Installation

1.  **Clone the repository:**
    ```bash
    git clone [https://github.com/mahmood789]
    cd https://github.com/mahmood789/786-NMA/tree/main)]
    ```

2.  **Install R:** Ensure you have R (version 4.0.0 or later recommended) installed.

3.  **Install Dependencies using `renv`:** This project uses `renv` for reproducible dependency management.
    * Open R **within the project directory**.
    * Run the following command in the R console:
        ```R
        renv::restore()
        ```
    * This will install all necessary packages at the versions specified in the `renv.lock` file into a project-local library. Answer 'y' if prompted to proceed.

## Usage

After installation, run the following command in your R console (make sure your working directory is the project's root directory):

```R
shiny::runApp()

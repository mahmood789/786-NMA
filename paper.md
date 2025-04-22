---
title: 'NMA Shiny: An Interactive Application for Frequentist Network Meta-Analysis' # Or choose your own title
tags:
  - R
  - Shiny
  - network meta-analysis
  - evidence synthesis
  - meta-regression
  - data visualization
authors:
  - name: [Mahmood Ahmad]
   
    affiliation: 1 # Royal Free Hospital 
  # Add other authors here if applicable
  # - name: [Najia ahmad]
  #  affiliation: "Royal Free Hospital "
affiliations:
 - name: Kaleem Asad
 -  #  affiliation: "Royal Free Hospital "
 
date: [Date of submission, e.g., 22 April 2025]
bibliography: paper.bib # Assumes your bibliography file is paper.bib
---

# Summary

Network Meta-Analysis (NMA) is a crucial statistical technique for synthesizing evidence from multiple studies comparing different treatments. While powerful R packages exist for NMA, executing these analyses often requires significant R programming expertise. NMA Shiny provides an interactive web application built with R Shiny [@shiny] and the `bs4Dash` [@bs4Dash] framework, designed to make frequentist network meta-analysis more accessible. It specifically leverages the `netmeta` package [@netmeta] to perform NMA for binary outcomes using Risk Ratios (RR) or Odds Ratios (OR). The application accepts both arm-level and contrast-level data via CSV upload, performs the core NMA calculations, offers optional meta-regression using study-level covariates, and generates a suite of standard visualizations and diagnostic outputs, including network plots, forest plots, league tables, funnel plots, and inconsistency checks.

# Statement of Need

Conducting network meta-analyses typically involves writing custom scripts using specialized R packages like `netmeta`. While flexible, this approach presents a barrier for researchers less familiar with R programming or those who prefer a graphical user interface for exploration and standard analyses. Existing tools may lack specific features, focus on different statistical approaches (e.g., Bayesian), or not offer a comprehensive suite of outputs within a single interface. NMA Shiny aims to fill this gap by providing an easy-to-use, interactive front-end specifically for frequentist NMA based on the `netmeta` package. It allows users to upload their data, select analysis options, run the NMA and optional meta-regression, and interactively explore results and diagnostics without writing R code. By integrating standard NMA procedures and visualizations into a Shiny app, it facilitates quicker analysis cycles, easier exploration of results, and enhances reproducibility through self-contained analysis specification via the UI and dependency management using `renv` [@renv]. The target audience includes systematic reviewers, health technology assessment professionals, clinical researchers, and students learning NMA methods.

# Key Functionality

* **Data Input:** Supports CSV file upload for both arm-level (event/n per arm) and contrast-level (TE/seTE per comparison) data for binary outcomes (RR or OR). Includes automatic calculation of contrast-level data from arm-level data using `netmeta::pairwise`. Provides input data previews using `DT` [@DT].
* **Analysis Options:** Allows selection between fixed-effect and random-effects models, choice of outcome measure (RR/OR), confidence level adjustment, and optional log-transformation of input Treatment Effects (TE).
* **Core NMA:** Performs frequentist NMA using `netmeta::netmeta()`.
* **Meta-Regression:** Includes optional network meta-regression (`netmeta::netmetareg()`) using one or more user-selected numeric covariates present in the input data.
* **Visualization:**
    * Interactive network plot using `visNetwork` [@visNetwork].
    * Customizable static network plot using `igraph` [@igraph].
    * Customizable forest plot (`netmeta::forest.netmeta`).
    * Funnel plots (standard and comparison-adjusted, `netmeta::funnel`).
    * Net heat plot (`netmeta::netheat`).
    * Net splitting plot (`netmeta::netsplit`).
    * Treatment frequency bar plot.
    * Basic scatter and residual plots.
* **Tabular & Text Outputs:**
    * Relative effect estimates (league table, `netmeta::netleague`).
    * Treatment rankings (`netmeta::netrank`).
    * Detailed model summaries from `netmeta`.
    * Inconsistency checks (`netmeta::decomp.design`).
    * Raw data summaries.
* **User Interface:** Built with `bs4Dash` for a modern look and feel. Includes clear instructions, sample data examples, loading spinners (`shinycssloaders`), and download buttons for plots and tables.
* **Performance:** Uses `future` [@future] and `promises` [@promises] for asynchronous execution of potentially long-running NMA calculations.

# Example Usage

A typical workflow involves the following steps within the application:

1.  Navigate to the "Data Upload" tab.
2.  Upload a CSV file containing either arm-level or contrast-level data (examples are available in the "Example CSV Files" tab).
3.  Select the appropriate "Data Format" (Arm-level or Contrast-level) and "Outcome Measure" (RR or OR). Check "Log-transform TE" if input TE is not already on the log scale.
4.  Optionally, view the network structure in the "Network Plot" tab.
5.  Go to the "Analysis" tab, choose the effect model (Fixed or Random), confidence level, and optionally select "Run Meta-regression" and choose covariate(s). The analysis runs automatically based on initial settings or updates when options change.
6.  Explore the various outputs in the "Results", "Diagnostics", and "Inconsistency" tabs. Customize plots using the available options.
7.  Download desired plots or tables using the download buttons.

The application includes sample datasets accessible via the "Example CSV Files" tab, allowing users to explore the functionality without needing their own data immediately.

# Acknowledgements

Rui Providencia 

# References

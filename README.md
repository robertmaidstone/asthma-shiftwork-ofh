# Asthma and Shift Work in Our Future Health

![R](https://img.shields.io/badge/R-%3E%3D4.2-1E6FA8)
![Python](https://img.shields.io/badge/Python-3.x-2AA198)
![SQL](https://img.shields.io/badge/SQL-6C757D)
![License: MIT](https://img.shields.io/badge/License-MIT-4CAF50)
![Last Commit](https://img.shields.io/github/last-commit/robertmaidstone/asthma-shiftwork-ofh?cacheSeconds=60)

Functions and code to analyse Our Future Health data. Contains data processing code, helper functions for use on DNA Nexus as well as scripts for plotting results after transferring summary data through the OFH airlock.

## Repository Structure
```
asthma-shiftwork-ofh/
├── data/                     # Summary data from OFH (not included)
├── notes/                     # notes on variables in OFH
      ├── smokingvariables_jan26release.docx #includes definition of packyears and vaping variables
      ├── alcoholvariables_jan26release.docx 
      ├── workandedvariables_jan26release.docx 
      ├── comorbidvariables_jan26release.docx 
├── datafields/                     # .txt files with data fields exported by Table Exporter
      ├── questionaire.txt          # from questionaire entity
      ├── participant.txt          # from participant entity
      ├── nhse_eng_ed.txt          # from emergency department entity
      ├── nhse_eng_inpat.txt         # from inpatient entity 
├── data_processing.R               # defunct data processing pipeline
├── data_processing_functions.R          # functions for loading data and variable derivation
├── functions.R          # functions for running logistic regression models and outputting results
├── plotting.R  # pipeline for plotting outputs from summary data in data/
├── sqltest.py    # attempt to replace Table Exporter pipeline with SQL code - currently doesn't work
└── README.md
└── LICENSE
```

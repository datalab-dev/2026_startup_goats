# 2026 Smith Goats Project

*This is a live document, and will be subject to change as the project progresses*

_Principal Investigator: Fauna Smith_
_Goat Barn Staff: Ben Rupchist, Nora Manring_  
_Project Lead: Colton Baumler_
_Data Lab Consultant: Nick Ulle_
_Collaborators: Camila Chicatto, Hilary Choi, Justin Huang, Rashmit Shrestha, Odelyn Xie_

# Description
This GitHub repository contains code for Fauna Smith’s Goat Appraisal Visualization Interface project. The project started with mathematical equations that were previously used to aid the appraising process by creating a visualization of a goat rear, including the legs and mammary system, on the online graphing calculator Desmos. We have since translated the equations into R ggplot code, and are in progress of creating a UI system for it.

This project’s end goal is an interface in which appraisers can input the linear appraisal values to create a visualization of the rearview of the goat. The purpose of our collaboration will be to improve appraisal accuracy through creating a program for appraisers to use on iPads when working with the goats. The purpose of this tool is to help support training and improve score accuracy by providing instantaneous feedback for new or existing appraisers. 

# Links:
[google]: https://drive.google.com/drive/folders/1k2zZalMFZtyAQk7a1ZUKVJFEten8hQ5k?usp=sharing

## Google Drive and Directory Structure

```
data/                                           Data sets
└── Goat Pictures/                    Images of dairy goats taken from rear 
└── rear udder image library/    Images of miniature goats taken from rear 
scoping documentation/              Supporting documents
2025Linear-SOPDraft1.pdf 	    Collection of data documentation for ADGA’s Linear Appraisal   
                                                    Program
Data Inventory			    Dataset overview of GOAT data 
GOAT Readme.md                     Brief overview of the project
Linear 2025 pptx.                       Visualizations about goats appraised in 2025
Meeting Notes			    Meeting notes from meeting with Project Lead and Principal    
                                                   Investigator
Student Meeting Notes 	    Notes taken during student team meetings
```

## Github File and Directory Structure

The directory structure for the project is:

```
data/           Data sets
docs/           Supporting documents
models/         Trained and serialized models
outputs/        Outputs from the code, including intermediate data sets
reports/        HTML or PDF reports generated from notebooks
└── figures/    Graphics and figures to be used in reporting
src/            Python/Java/... (non-R) source code
R/              R source code
.gitattributes  Paths Git should give special treatment
.gitignore      Paths Git should ignore
LICENSE         License for the project
README.md       This file
```

## Installation
For the coding portion of this project, the following packages will need to be loaded in R:
- Tidyverse package 
- Shiny package

## Contributing
_Colton Baumler, Camila Chicatto, Hilary Choi, Justin Huang, Nora Manring, Ben Rupchis, Rashmit Shrestha, Fauna Smith, Nick Ulle, Odelyn Xie_

([back to top][top])


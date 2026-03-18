# On-the-Potential-for-Strategic-Behavior-in-Jurisdictional-REDD
This repository contains the code and data for reproducing Garcia and Sanford 2026 (In PNAS): "On the Potential for Strategic Behavior in Jurisdictional REDD+".

> **Authors**: Alberto Garcia (albert.garcia@utah.edu), Luke Sanford (luke.sanford@yale.edu)

> **Abstract:** Low-quality, nonadditional project-based carbon credits have undermined confidence in the voluntary carbon market and risk increasing atmospheric CO2 concentrations. As a result, the voluntary carbon market has shifted toward jurisdictional REDD+ programs, which promise to overcome many of the issues that have drawn heightened scrutiny to project-based approaches. Yet little work has explored additionality in jurisdictional settings. By studying forest changes in all eligible jurisdictions, we show theoretically and empirically how jurisdictions could game the system and sell a large volume of credits without reducing emissions; however, we find that this strategic behavior has not driven enrollment to date. In addition, we find evidence of anticipatory deforestation in currently enrolled jurisdictions, illuminating new risks with jurisdictional REDD+.

## Repository structure  

The DOI for this code repository is 10.5281/zenodo.19071812. In order to reproduce analyses, you must download the `data` folder at https://dataverse.yale.edu/ and replace the empty `data` folder in the repository.

```
On-the-Potential-for-Strategic-Behavior-in-Jurisdictional-REDD
  |__ data
  |__ code
  |__ figs
  |__ figs_pdf
  |__ results
```

## Software

This analysis was performed in R. The scripts for reproducing the paper analysis can be found in `code`.

## License

The software code, data, and figures contained within this repository are made available under the [Creative Commons CC BY-NC-ND 2.0](https://creativecommons.org/licenses/by-nc-nd/2.0/) license.

**Please note:** To ensure reproducibility and in order to manage package dependencies, we use the `renv` package linked to [R Studio Package Manager](https://packagemanager.rstudio.com/client/#/). When you first clone this repo onto your machine, run `renv::restore()` to ensure you have all correct package versions installed in the project. Please see the `renv` page for more information: https://rstudio.github.io/renv/articles/renv.html.

## Code scripts
This section describes the code scripts developed to conduct this study. All code related to this study is available in the GitHub repository. The headings below represent folder names and paths.

### code/
`Processing.R`
Builds jurisdiction-level forest loss panel using Hansen GFC dataset
- Inputs: 
  * XX
- Outputs: 
  * data/processed/XX.rds
  * data/processed/XX.rds

`Processing_TMF.R`
Builds jurisdiction-level forest loss panel using Van Cutsem TMF dataset
- Inputs: 
  * XX
- Outputs: 
  * data/processed/XX.rds
  * data/processed/XX.rds
  

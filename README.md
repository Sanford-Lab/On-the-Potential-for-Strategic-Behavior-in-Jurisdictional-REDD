# On-the-Potential-for-Strategic-Behavior-in-Jurisdictional-REDD
This repository contains the code and data for reproducing Garcia and Sanford 2026 (In PNAS): "On the Potential for Strategic Behavior in Jurisdictional REDD+".

> **Authors**: Alberto Garcia (albert.garcia@utah.edu), Luke Sanford (luke.sanford@yale.edu)

> **Abstract:** Given well-known credibility issues in project-based avoided-deforestation credits and over $3 billion in committed credit purchases under ART TREES, the voluntary carbon market is increasingly moving toward jurisdictional approaches to Reducing Emissions from Deforestation and Forest Degradation (REDD+) as a source of credits. We test whether existing approaches to jurisdictional REDD+ create new incentives for strategic behavior that could undermine the additionality of these credits. Using global remotely sensed forest change data from all jurisdictions eligible for jurisdictional REDD+ programs in the voluntary carbon market, we examine existing jurisdictional baseline approaches. We find that jurisdictional approaches create predictable opportunities for jurisdictions to generate credits without requiring new policy action, while discouraging the enrollment of jurisdictions with increasing rates of deforestation. We show that two simple metrics predict over 30% of baseline errors and that if jurisdictions condition their enrollment on such metrics, they could generate millions of nonadditional credits. Despite this, we do not find systematic evidence that jurisdictions have exploited this information thus far. However, approximately half of enrolled jurisdictions exhibit significant temporary increases in deforestation immediately before crediting begins. This suggests anticipatory forest clearing by private landowners expecting future restrictions with jurisdictional REDD+ enrollment. As jurisdictional REDD+ credits account for a growing share of voluntary carbon markets, these findings reveal both reassuring governance outcomes and critical vulnerabilities requiring methodological reform.

## Repository structure  

The DOI for this code repository is [10.5281/zenodo.19071812](https://doi.org/10.5281/zenodo.19071812). In order to reproduce analyses, you must download the `data` folder at https://dataverse.yale.edu/ and add it to the repository.

```
On-the-Potential-for-Strategic-Behavior-in-Jurisdictional-REDD
  |__ data (need to download)
  |__ code
  |__ figs
  |__ figs_pdf
  |__ results
```

## Software

This analysis was performed in R. The scripts for reproducing the paper analysis can be found in `code`.

## License

The software code, data, and figures contained within this repository are made available under the [Creative Commons CC BY-NC-ND 2.0](https://creativecommons.org/licenses/by-nc-nd/2.0/) license.

**Please note:** To ensure reproducibility and in order to manage package dependencies, we use the `renv` package linked to [R Studio Package Manager](https://packagemanager.rstudio.com/client/#/). When you first clone this repo onto your machine, use `renv` to ensure you have all correct package versions installed in the project. Please see the `renv` page for more information: https://rstudio.github.io/renv/articles/renv.html.

## Code scripts
This section describes the code scripts developed to conduct this study. All code related to this study is available in the GitHub repository. The headings below represent folder names and paths.

### code/
`Processing.R`
builds a jurisdiction-level forest loss panel using the Hansen GFC dataset.

`Processing_TMF.R`
builds a jurisdiction-level forest loss panel using the Van Cutsem Tropical Moist Forests (TMF) dataset.
 
`slope_analyses.R`
conducts main regression analyses of slope and last year deviation from reference period associations with baseline errors

`entry_analysis.R`
conducts analysis to determine if already participating ART TREES jurisdictions had downward sloping deforestation or final reference period deforestation relative to non-participants or in other potential enrollment years.
  
`omniscient_enroller_analysis.R`
estimates the extent to which jurisdictions could generate nonadditional credits if they perfectly took advantage of the heuristics in pre-enrollment deforestation patterns.

`syntheticcontrols_Hansen.R`
conducts the main synthetic control analysis to examine the presence and extent of anticipatory deforestation in ART TREES participating jurisdictions. This script also conducts some of the sensitivity analysis based on alternate anticipation windows and donor pools. 

`syntheticcontrols_TMF.R`
conducts additional sensitivity analysis for the synthetic controls based on the Van Cutsem TMF dataset.

`coefficient_plots.R`
creates plots of the synthetic control analyses for data visualization and presentation.

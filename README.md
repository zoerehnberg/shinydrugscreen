# Drug Screening Shiny App

## How to Use This Repository

- **Download the docker image.** It is available on dockerhub () or can be downloaded from . Run the image (`docker run -it --rm -p 127.0.0.1:80:80 shinydrugscreen`) to launch the Shiny app. For archival purposes, a separate docker image containing both the original and processed data also exists ().
- **Download the `shinyDrugScreen` package.** Install the package in R and run `shinyDrugScreen::launchApp()` to launch the Shiny app.

## Visualization

Once the app is running, it provides tools to visualize the raw drug screening data from CCLE and GDSC. Select a data source, a drug-cell line combination, and a replicate. For CCLE, cell lines are identified by cell line name and compounds are identified by drug ID. For both GDSC1 and GDSC2, cell lines are identified by COSMIC ID and compounds are identified by an internal GDSC drug ID. See the *Cell Line Information* and *Drug Information* tabs for more details.

1. **Dose-Response Observation**s. Raw intensities are plotted vs. log<sub>2</sub> drug dose for the selected drug-cell line combination and replicate.
The red dashed line indicates the median of the untreated controls for the plate.

2. **Heatmap**. A heatmap of intensities for the plate upon which the selected drug-cell line combination and replicate was tested.
Grey cells indicate missing or failed wells.

3. **Plate Format**. A depiction of the plate layout, with the location of the selected drug-cell line combination and replicate highlighted in green.
    - CCLE:
*Blank* wells are control wells that contain no cells and no drug.
*Untreated* wells are control wells that contain cells, but no drug.
*Missing* wells are wells for which no intensity measurements were reported by CCLE.
    - GDSC1:
*Blank* wells are control wells that contain no cells and no drug.
*Untreated* wells are control wells that contain cells, but no drug.
*Missing* wells are wells for which no intensity measurements were reported by GDSC.
*Other* and *Unused* wells are wells that were not used for any external analysis.
    - GDSC2:
*Blank* wells are control wells that contain no cells and no drug.
*Untreated* wells are control wells that contain cells, but no drug.
*Untreated + DMSO* wells are control wells that contain cells and DMSO.
*Missing* wells are wells for which no intensity measurements were reported by GDSC.
*Reference Drug* wells contain one of the two reference drugs repeated across plates.
*Positive Control* wells are wells treated with the positive control treatment, with or without titration.
*DMSO* wells are wells treated with DMSO.
*Unused* wells are wells that were not used for any external analysis.

4. **Plate Information**. A table of information about the selected plate, including the location of all drugs screened on that plate.

5. **All Replicates**. Relative viabilities are plotted vs. log<sub>2</sub> drug dose for all replicates of the selected drug-cell line combination.
Relative viabilities were calculated by dividing raw intensities by the median of the untreated control wells (untreated + DMSO control wells for GDSC2) on the plate.
This normalization was necessary for comparing replicates across scanned plates.

## Cell Line Information and Drug Information

- **Cell Line Information**. This tab provides details about the cell lines included in the CCLE and GDSC studies. It also matches cell line names and identifiers between the studies. The `map_CL.rda` file contains more information.
- **Drug Information**. This tab provides details about the drugs included in the CCLE and GDSC studies. It also matches drug names and identifiers between the studies. The `map_drug.rda` file contains more information.

## Data Sources

The following data are needed and should be saved in the `data/original` directory. The data can be automatically downloaded using `data/download.Rmd`.

1. `gdsc1_orig`, `gdsc2_orig`, `CL_gdsc`, and `drug_gdsc`: Available from GDSC release 8.2, archived on the GDSC bulk data download page (https://www.cancerrxgene.org/downloads/bulk_download).
2. `ccle_orig`: Available from the Supplementary Data in the second Addendum to *The Cancer Cell Line Encyclopedia enables predictive modelling of anticancer drug sensitivity* (Barretine, et al. 2019).
3. `annotate_ccle`: Available from the Pharmacological Profiling legacy data provided by CCLE (https://portals.broadinstitute.org/ccle/data).
4. `GDSC_to_CCLE`: Available from the Supplementary Data in *Pharmacogenomic agreement between two cancer cell line data sets* (The Cancer Cell Line Encyclopedia Consortium & The Genomics of Drug Sensitivity in Cancer Consortium 2015)

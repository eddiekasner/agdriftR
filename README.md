# agdriftR

R scripts that compare Tier I AgDRIFT spray drift deposition curves (aerial, ground boom, and orchard/airblast) from several sources: the OPP spreadsheet calculator, the AgDRIFT 2.1.1 model, the EXPRESS pond datasets, and equations from the AgDRIFT User's Guide.

## Source

This is a fork of [puruckertom/agdriftR](https://github.com/puruckertom/agdriftR), written by Tom Purucker and Carmen Kuan. All of the analysis and data come from that repository. This fork only makes the scripts run on any machine (relative paths, `readxl` instead of the Java-based `xlsx` package) and fixes plot legend errors.

## Usage

Install the packages once:

```r
install.packages(c("readxl", "rmarkdown"))
```

Then, from the repo folder:

```r
source("AgDriftR.R")                # draw the comparison plots
rmarkdown::render("agdrift.rmd")    # rebuild agdrift.html (needs pandoc, bundled with RStudio)
```

## License

The MIT license in [LICENSE](LICENSE) covers the changes made in this fork. The original repository has no license, so the original code remains under its authors' terms. The included manual and spreadsheets are third-party EPA/AgDRIFT materials and are not covered by this license.

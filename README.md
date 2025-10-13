# Child stunting and wasting prevalence in Low-and middle-income countries: An analysis of its linkages to protein quality in staple foods by food balances

This is the repository of data and code accompanying the paper above.

The description of the files:
- `data/GBD JME checked cleaned final @CMC 161124.xlsx`: The main data file
- `data/Dataset@Wast.xlsx`: A subset of the above
- `statistics/`: The SAS code for models
- `map/map.R`: The R code for maps


## Generating maps using R

If this is the first time, install R and then the dependencies as

```bash
Rscript -e "install.packages(c('sf', 'dplyr', 'readxl', 'ggplot2', 'RColorBrewer', 'patchwork', 'cowplot', 'terra', 'ggrepel', 'rnaturalearth', 'rnaturalearthdata', 'here'), repos='https://cran.rstudio.com')"
```


Then, just run the map.R file

```bash
R -f map/map.R
```


The following maps will be generated

Main figure: Africa maps showing stunting and wasting (one image)

S44: Africa map sorghum with numbers (instead of names) as per table 39
S45: World map sorghum without number (anyhow all countries are in africa)

S46: Africa map millet with numbers as per table 40
S47: World map millet without numbers

S48: Africa map cassava with numbers as per table 41
S49: World map of cassava

S50: Africa map of maize with numbers as per table 42
S51: World map of maize

S52: Africa map of rice with numbers as per table 43
S53: World map of rice

S54: Africa map of wheat with numbers as per table 44
S55: World map of wheat
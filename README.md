# Child stunting and wasting prevalence in Low-and middle-income countries: An analysis of its linkages to protein quality in staple foods by food balances

This is the repository of data and code accompanying the paper above.

The description of the files:
- `data/GBD JME aggregate 09 Oct.xlsx`: The main data file
- `data/05 Oct Dataset@Wast.xlsx`: A subset of the above
- `statistics/`: The SAS code for models
- `map/map.R`: The R code for maps and supplement section J generation

## Generating maps using R

If this is the first time, install R and then the dependencies as

```bash
Rscript -e "install.packages(c('sf', 'dplyr', 'readxl', 'ggplot2', 'RColorBrewer', 'patchwork', 'cowplot', 'terra', 'ggrepel', 'rnaturalearth', 'rnaturalearthdata', 'here', 'officer', 'flextable'), repos='https://cran.rstudio.com')"
```


Then, just run the map.R file

```bash
R -f map/map.R
```

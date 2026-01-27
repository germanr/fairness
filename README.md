# Replication Materials: Are Fairness Perceptions Shaped by Income Inequality?

This repository contains replication code and data for:

**Gasparini, Leonardo and Germán Reyes. "Are Fairness Perceptions Shaped by Income Inequality? Evidence from Latin America." *The Journal of Economic Inequality* 20(4): 893-913 (2022).**

## Overview

We study the relationship between income inequality and perceptions of distributive fairness in Latin America from 1997-2015. Using data from 18 countries, we find a strong positive correlation between Gini coefficients and the share of individuals who perceive the income distribution as unfair.

## Requirements

- **Stata 15 or higher** (uses `reghdfe`, `oaxaca`, `estout` packages)
- Required Stata packages: `reghdfe`, `ftools`, `oaxaca`, `estout`, `ineqdeco`, `ineqdec0`

To install required packages:
```stata
ssc install reghdfe
ssc install ftools
ssc install oaxaca
ssc install estout
ssc install ineqdeco
ssc install ineqdec0
```

## Data

### Included Data (ready to use)
All processed data needed to replicate the analysis is included:
- `data/perceptions_data.dta` - Individual-level perception data (N=287,616)
- `data/merged_data.dta` - Country-year level merged dataset
- `data/gini.dta` - Gini coefficients by country-year
- `data/inequality_indicators.dta` - Various inequality measures
- `data/fairness_groups.dta` - Fairness perceptions by demographic groups

### External Data (not included)
The following files are too large for GitHub or require external access:
- **Latinobarómetro raw data** - Download from [latinobarometro.org](https://www.latinobarometro.org/) if you want to run `1-create-individual-data.do`
- **SEDLAC household surveys** - Required for `2-calculate-inequality.do` (World Bank internal access only)

**Note**: You can replicate all tables and figures using only the included data. The external data is only needed if you want to rebuild the datasets from scratch.

## Folder Structure

```
fairness/
├── code/               # Stata do-files
│   ├── 0-master.do     # Master file - runs all analyses
│   ├── 1-create-individual-data.do
│   ├── 2-calculate-inequality.do
│   ├── 3-clean-gini-2018.do
│   ├── 4-merge-data.do
│   ├── 5-tables.do     # Generates all tables
│   └── 6-figures.do    # Generates all figures
├── data/               # Processed datasets
├── raw_data/           # Raw data files
├── results/            # Output tables and figures
└── paper/              # LaTeX source files
```

## Replication Instructions

1. **Set up paths**: Open `0-master.do` and verify the path structure matches your system. The code uses portable paths via `c(username)`.

2. **Run the master file**:
```stata
do "code/0-master.do"
```

3. **Note on inequality data**: Do-file 2 (`2-calculate-inequality.do`) requires access to SEDLAC household survey data via the World Bank's `datalib` command. If you don't have access, you can skip this step as the processed `inequality_indicators.dta` file is provided.

## Output Files

### Tables (in `results/`)
- `Data analysis.xlsx` - Descriptive statistics
- `*_lpm.tex` - Linear probability model results
- `*_logit.tex` - Logit regression results
- `*_unrest_*.tex` - Social unrest analysis

### Figures (in `results/`)
- `fig-oaxaca-0213.pdf` - Oaxaca-Blinder decomposition
- `fig-intensity-fairness.pdf` - Fairness perceptions over time
- `fig-chg-fairness-0213.pdf` - Change in unfairness 2002-2013
- `fig-chg-gini-unfair-0213.pdf` - Change in Gini vs. unfairness
- `fig-scatter-gini-unfair.pdf` - Gini vs. unfairness scatterplot
- `fig-binscatter-gini-unfair.pdf` - Binscatter
- `fig-timeseries-gini-unfair.pdf` - Time series
- `fig-fairness-*.pdf` - Fairness by demographic groups

## Citation

```bibtex
@article{gasparini2022fairness,
  title={Are Fairness Perceptions Shaped by Income Inequality? Evidence from Latin America},
  author={Gasparini, Leonardo and Reyes, Germ{\'a}n},
  journal={The Journal of Economic Inequality},
  volume={20},
  number={4},
  pages={893--913},
  year={2022},
  doi={10.1007/s10888-022-09526-w}
}
```

## Contact

Germán Reyes
Assistant Professor of Economics
Middlebury College
Email: greyes@middlebury.edu
Web: [germanr.com](https://www.germanr.com)

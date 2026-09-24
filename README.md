**FOMC Projection Accuracy Analysis**

Analyzes FOMC participant interest rate projections against actual Federal Funds rates to measure prediction accuracy and identify systematic biases.

**What's Included**

- combine_and_visualize.R - Main R script that combines data and creates all visualizations
- data/projections.csv - FOMC projections from SEP dotplots (2012-2026)
- data/FEDFUNDS.csv - Actual Federal Funds rates from FRED (2012-2025)
- data/combined_projections_actual.csv - Combined dataset with error calculations
- output/ - 6 visualizations showing disagreement and prediction accuracy

**Visualizations**

1. Disagreement Over Time - Participant consensus/disagreement across meetings
2. Prediction Error by Year - The inflation shock story 
3. Predictions vs Actual - Scatter plot showing deviation from perfect predictions
4. Error Distribution - Box plots of prediction errors by year
5. Error with Confidence Intervals - Trends with statistical uncertainty
6. Error Heatmap - When predictions were most/least accurate

**How to Use**

Prerequisites
- R 4.0+
- tidyverse, ggplot2, gridExtra packages

Run the Analysis

```bash
Rscript combine_and_visualize.R
```

This will:
1. Combine projections with actual December Fed Funds rates
2. Calculate prediction accuracy metrics
3. Generate 6 visualizations in `output/`
4. Save combined dataset to `data/combined_projections_actual.csv`

Data Sources

- FOMC Projections: [Federal Reserve SEP](https://www.federalreserve.gov/monetarypolicy/fomcprojtabl.htm)
- Federal Funds Rates: [FRED](https://fred.stlouisfed.org/series/FEDFUNDS)

# Australian Wine Sales Forecasting

An interactive R Shiny application for exploring monthly Australian wine sales and comparing forecasting methods using the `fpp3` framework.

Developed by Asher Frank for **ADS-506: Applied Time Series Analysis**, University of San Diego.

**Live app:** Deployment pending.

## Overview

The application analyzes monthly Australian wine sales from January 1980 through December 1994 across six varietals: Red, Rose, Sweet White, Dry White, Sparkling, and Fortified. Users can explore historical patterns, compare three forecasting methods, and generate future sales forecasts.

## Required Features

- Interactive varietal selection and historical analysis period, with a monthly sales plot.
- TSLM with trend and seasonality, automatic ETS, and automatic ARIMA models per selected varietal.
- Configurable chronological training/validation split (60%–90% training).
- Human-readable training-model specifications, including ETS component forms and ARIMA orders.
- Training and validation accuracy comparisons using RMSE, MAE, and MAPE.
- Comparative validation forecasts overlaid on actual sales and future forecasts with prediction intervals.
- User-defined future forecast horizon of 1–36 months.

## Additional Features

- Forecast zoom for closer examination of recent observations and predictions.
- Independent model visibility controls without changing model estimates or accuracy metrics.
- Optional 80% and 95% prediction intervals on future forecasts.
- Actual-sales legend entries and a dashed boundary marking the start of validation.
- A friendly message when fewer than 24 months are available for model fitting.
- An About tab explaining the data, imputation, modeling approach, and reproduction steps.

## Project Structure

- `app.R` — Shiny interface, reactive logic, model fitting, accuracy tables, and plots.
- `preprocessing.R` — Data loading, date parsing, imputation, and monthly tsibble preparation.
- `AustralianWines.csv` — Dataset supplied with the course assignment.
- `manifest.json` — Connect Cloud deployment metadata and package dependencies.

Keep all three files in the same directory. Run the app from that directory so its relative file paths resolve correctly.

## Running the Application

1. Install R, then install the required packages:

   ```r
   install.packages(c("shiny", "fpp3", "readr", "urca"))
   ```

2. Clone or download this repository and open its directory in RStudio or another R environment.
3. Run:

   ```r
   shiny::runApp()
   ```

## Deploying to Connect Cloud

The repository includes `manifest.json` for GitHub-based deployment. Select `app.R` as the primary file in Connect Cloud. Commit and push the manifest and app updates before deploying.

After changing the app or its dependencies, regenerate the manifest from this directory using `rsconnect`:

```r
rsconnect::writeManifest(
  appFiles = c("app.R", "preprocessing.R", "AustralianWines.csv")
)
```

## Using the App

1. Select one or more varietals and the historical analysis period.
2. Set the training percentage. The earliest observations form the training set; the remaining observations form the validation set. Training models require at least 24 months of training data.
3. Open **Historical Sales** to compare observed sales, or **Validation Forecasts** to compare predictions and training/validation accuracy.
4. Open **Future Forecasts** and set the forecast horizon. Future models are refitted using the full selected historical period; displayed accuracy metrics still describe the training/validation evaluation.
5. Use the model checkboxes, prediction-interval controls, and forecast zoom to adjust the plots. Prediction-interval controls affect future forecasts only. Zoom and model visibility do not change accuracy results.

The sidebar displays **training-model specifications**. Refitted future models can have different specifications. Clearing all varietals leaves the corresponding plot and metric sections blank; clearing all models hides forecast lines while retaining actual sales and accuracy tables.

## Data and Methodology

The course dataset is organized into a monthly `tsibble` keyed by `Varietal`. TSLM uses a linear trend and monthly seasonality. ETS and ARIMA use automatic model selection through `fable`.

Two missing Rose observations, July and August 1994, are estimated from June 1994 sales using the median July/June and August/June ratios from 1989–1993. Rose validation metrics may therefore include estimated observations rather than observed sales.

Model rankings depend on the selected historical period and split. The forecast horizon changes future predictions, not the validation window or its accuracy metrics.

### Example: Reproduce the Report's Validation Results

Select **Dry White** or **Fortified**, set the analysis period to **1980–1994**, training data to **80%**, all three forecast models visible, and forecast zoom to **70**. Open **Validation Forecasts**. Training covers January 1980–December 1991; validation covers January 1992–December 1994.

| Varietal | Lowest validation RMSE | Training-model specification |
| --- | --- | --- |
| Dry White | ARIMA: 441.06 | ARIMA(1,0,0)(0,1,1)[12] with drift |
| Fortified | ETS: 247.26 | ETS(M,A,M) |

## R and Package Versions

Verified locally on November 21, 2026, on macOS (Apple silicon):

| Software | Version |
| --- | --- |
| R | 4.6.1 |
| shiny | 1.14.0 |
| fpp3 | 1.0.3 |
| readr | 2.2.0 |
| urca | 1.3.4 |

These are the R version and four packages directly loaded by the app. `urca` is required for automatic ARIMA differencing and is included explicitly for deployment. Installing these packages also installs their dependencies.

## AI Assistance

OpenAI's [ChatGPT](https://chatgpt.com/) and [Codex](https://openai.com/codex/) assisted with code development, debugging, and documentation. Codex also reviewed the assignment requirements, checked reported results, and helped make minor usability and labeling improvements to the app. The author designed, reviewed, and tested the application and its outputs.

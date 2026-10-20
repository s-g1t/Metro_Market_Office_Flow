
# Metro Office Flow Forecast

An econometric forecasting pipeline for U.S. office market fundamentals, built in R with the `fable` package. It forecasts total inventory, vacant space, weighted average rent, and deliveries (new construction) — separately for Total and Class A properties, and for CBD and suburban submarkets — using distributed-lag models that combine lagged economic drivers with the fundamentals' own autoregressive dynamics.

All model choices live in `config.yml`: metros, fundamentals, driver sets, and lag structures are parameters, not hardcoded logic. The pipeline also supports retro-vintage runs — reproducing a forecast as of a past quarter — for ease of back- and out-of-sample testing.

## Approach

**Variable selection.** Six candidate economic drivers — CPI, Gross Metro Product, households, multifamily completions, office workforce population (ages 25–54), and total employment, drawn from the Census Bureau, BLS, and the Federal Reserve — were screened by lagged cross-correlation against each fundamental. The best lag is the one carrying the strongest, most durable relationship. Correlated drivers (notably total employment and office workforce population) are not entered together, to limit multicollinearity.

**Model specification.** Distributed-lag models estimated with `fable`, combining lagged economic factors with lagged values of the fundamental itself. Series are differenced where needed for stationarity; residuals are checked with Augmented Dickey-Fuller tests (stationarity) and Ljung-Box tests (remaining autocorrelation). Model complexity is weighed against fit via AICc and BIC; forecast accuracy is measured by RMSE.

**Evaluation.** Models were trained on 2010 Q1–2021 Q1 and backtested against actuals over 2021 Q1–2026 Q1. Headline findings:

- Distributed-lag models outperformed pure autoregressive (AR(2)) benchmarks across property classes and submarket types.
- The structure forecasts total inventory and total vacancy best; inventory is strongly tied to multifamily completions, rents to Gross Metro Product and CPI.
- CBD absorption is the weak spot, and rents are hard to predict outside Class A suburban space.

## Data

The underlying data are proprietary and are **not included** in this repository. `config.yml` points the pipeline at local input files; supply your own data in the same schema to run it.

## Repository layout

```
R/MOF_Forecast_Fundamentals.R   fundamentals pipeline: ingestion → lag screening → estimation → backtest
config.yml                      every tunable: metros, fundamentals, drivers, lags, vintage
Data/Input/                     local input CSVs (not tracked)
docs/                           the abridged assessment write-up
```

## Outputs and visualization

Forecast tables and backtest diagnostics are written by the pipeline. The results are also published as an interactive Tableau dashboard — forecast fundamentals by metro, property class, and submarket type:

**Live dashboard:** [Metro Office Flow Forecast Dashboard (Tableau Public)](https://public.tableau.com/app/profile/stephen.gardner1701/viz/MetroOfficeFlowForecastDashboard/MetroOfficeFundamentalForecast)

## Running it

1. Install R and the `fable` ecosystem (`fable`, `tsibble`, `feasts`, `urca`).
2. Place input CSVs under `Data/Input/` and confirm the paths in `config.yml`.
3. Run `R/MOF_Forecast_Fundamentals.R`. To reproduce a historical forecast origin, set the vintage in `config.yml` first.

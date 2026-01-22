## High-Frequency Financial Risk Dashboard

An interactive **R Shiny** dashboard for **real-time financial risk analysis**. It pulls live stock and crypto prices and computes **Value at Risk (VaR)** and **Monte Carlo-based risk metrics** in an intuitive, minimalistic UI.

### Features

- **Live market data**
  - Stocks via free Yahoo Finance data using `quantmod::getSymbols`.
  - Optional API-based data (Alpha Vantage, Finnhub, CoinGecko) if you want more control.
- **Risk analytics**
  - Historical VaR (configurable confidence level and horizon).
  - Monte Carlo P&L distribution and simulated VaR based on recent volatility.
  - Key summary stats and simple stress scenarios.
- **Modern, minimal UI**
  - Clean layout with light/dark theme using `bslib`.
  - Asset watchlist, asset details, and dedicated VaR / Monte Carlo tabs.
  - Auto-refresh for a “high-frequency” feel (you can tune the refresh rate).

### Free Data Pathways

- **Stocks / ETFs**
  - **Yahoo Finance via `quantmod` (no API key required)**  
    - Used by default in this app.
    - Function: `quantmod::getSymbols(symbol, src = "yahoo")`.
  - **Alpha Vantage (free tier, API key required)**  
    - Sign up for a free key at their website.
    - Use from R with the `alphavantager` package (`av_get()`), or your own `httr` calls.
- **Crypto**
  - **CoinGecko (free, no key required for basic usage)**  
    - REST API; you can call it via `httr` in R.
  - **Binance public API (free, no key required for public data)**  
    - Good for high-frequency quotes; also via `httr`.

This starter app ships with **Yahoo Finance** for equities and a simple **CoinGecko** integration for crypto to keep everything free.

### Requirements

- **R** (4.1+ recommended)
- Packages:
  - `shiny`
  - `bslib`
  - `plotly`
  - `quantmod`
  - `PerformanceAnalytics`
  - `xts`
  - `httr`
  - `jsonlite`

You can install them via:

```r
install.packages(c(
  "shiny", "bslib", "plotly",
  "quantmod", "PerformanceAnalytics", "xts",
  "httr", "jsonlite"
))
```

### Running the App Locally

1. Open R (or RStudio) in this project folder.
2. Install the packages listed above.
3. Run:

```r
shiny::runApp("app.R", host = "0.0.0.0", port = 3838)
```

4. Open your browser at `http://localhost:3838`.

### Configuring Optional API Keys

If you decide to use extra APIs (e.g. Alpha Vantage, Finnhub), the recommended way is to keep keys out of source code:

- Set environment variables in your OS:
  - `ALPHAVANTAGE_API_KEY`
  - `COINGECKO_API_KEY` (not strictly needed for public endpoints)
- The app reads these with `Sys.getenv()` when present and uses them instead of the default free sources.

This way the app **runs out of the box without any keys**, but becomes more powerful if you add them.

### Docker Usage

Build the image:

```bash
docker build -t high-frequency-risk-dashboard .
```

Run the container:

```bash
docker run --rm -p 3838:3838 high-frequency-risk-dashboard
```

Then open `http://localhost:3838` in your browser.

You can pass API keys as environment variables:

```bash
docker run --rm -p 3838:3838 \
  -e ALPHAVANTAGE_API_KEY=your_key_here \
  high-frequency-risk-dashboard
```

### Roadmap Ideas

- Add portfolio-level VaR and correlation heatmaps.
- Support intraday candles and order book snapshots for crypto.
- Add user authentication and persistent watchlists.
- Deploy to a Shiny hosting service or a cloud VM.


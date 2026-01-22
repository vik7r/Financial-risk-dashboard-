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
    - **Delay: 15-20 minutes** (free tier limitation)
    - Function: `quantmod::getSymbols(symbol, src = "yahoo")`.
  - **Finnhub (FREE API key, real-time for US stocks)** ⚡
    - **Delay: Real-time** (much better than Yahoo Finance!)
    - Sign up for a free key at [finnhub.io/register](https://finnhub.io/register)
    - Free tier: 60 API calls/minute, real-time US market data
    - Set environment variable: `FINNHUB_API_KEY=your_key_here`
    - The app will automatically use Finnhub if the API key is set
  - **Alpha Vantage (free tier, API key required)**  
    - Sign up for a free key at their website.
    - **Note:** Free tier also has 15-minute delay (same as Yahoo)
    - Use from R with the `alphavantager` package (`av_get()`), or your own `httr` calls.
- **Crypto**
  - **CoinGecko (free, no key required for basic usage)**  
    - **Delay: Near real-time (<1 minute)**
    - REST API; you can call it via `httr` in R.
  - **Binance public API (free, no key required for public data)**  
    - Good for high-frequency quotes; also via `httr`.

### 🚀 Reducing Data Delay (FREE Options)

**📖 Detailed Setup Instructions:** See [SETUP_GUIDE.md](SETUP_GUIDE.md) for step-by-step instructions!

**For US Stocks:**
1. **Finnhub (Recommended)** - Get a free API key at [finnhub.io/register](https://finnhub.io/register)
   - **Real-time data** for US stocks (vs 15-20 min delay)
   - 60 calls/minute on free tier
   - Set `FINNHUB_API_KEY` environment variable
   - Select "Finnhub" in the dashboard's "Data Source" dropdown
   - **Quick setup:** See [SETUP_GUIDE.md](SETUP_GUIDE.md) for detailed steps

**For Indian Stocks:**
- ⚠️ **Finnhub does NOT support Indian stocks** - only US stocks are real-time
- ✅ **NEW: Indian API (indianapi.in)** - FREE real-time option!
  - No API key required
  - Real-time NSE/BSE prices
  - Select "Indian API (Real-time, FREE)" in dashboard
  - See [INDIAN_STOCKS_REALTIME.md](INDIAN_STOCKS_REALTIME.md) for details
- Yahoo Finance (15-20 min delay) - still available as fallback
- For professional real-time data, consider paid services:
  - TrueData (authorized NSE/BSE vendor)
  - INDstocks API (WebSocket streaming)
  - Zerodha Kite Connect (₹2,000/month)

**For Crypto:**
- CoinGecko already provides near real-time data (<1 minute delay)

This starter app ships with **Yahoo Finance** for equities and a simple **CoinGecko** integration for crypto to keep everything free. **You can reduce delays significantly by adding a free Finnhub API key!**

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

If you decide to use extra APIs (e.g. Finnhub for real-time data), the recommended way is to keep keys out of source code:

- Set environment variables in your OS:
  - `FINNHUB_API_KEY` - **Recommended!** Enables real-time US stock data (free tier)
  - `ALPHAVANTAGE_API_KEY` - Alternative API (still has 15-min delay on free tier)
  - `COINGECKO_API_KEY` (not strictly needed for public endpoints)
- The app reads these with `Sys.getenv()` when present and uses them instead of the default free sources.

**To reduce delay for US stocks:**
1. Get a free Finnhub API key: [finnhub.io/register](https://finnhub.io/register)
2. Set environment variable: `FINNHUB_API_KEY=your_key_here`
3. In the dashboard, select "Finnhub (Real-time, FREE API key)" from the "Data Source" dropdown

This way the app **runs out of the box without any keys**, but becomes more powerful (and faster!) if you add them.

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
  -e FINNHUB_API_KEY=your_key_here \
  high-frequency-risk-dashboard
```

**To reduce delay:** Use `FINNHUB_API_KEY` for real-time US stock data (free tier available).

### Roadmap Ideas

- Add portfolio-level VaR and correlation heatmaps.
- Support intraday candles and order book snapshots for crypto.
- Add user authentication and persistent watchlists.
- Deploy to a Shiny hosting service or a cloud VM.


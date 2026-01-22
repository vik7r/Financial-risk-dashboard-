library(shiny)
library(bslib)
library(plotly)
library(quantmod)
library(PerformanceAnalytics)
library(xts)
library(httr)
library(jsonlite)

# -----------------------------
# Helper functions
# -----------------------------

# Finnhub API (FREE - Real-time for US stocks, better than Yahoo Finance)
# Get free API key at: https://finnhub.io/register
fetch_equity_prices_finnhub <- function(symbol, from = Sys.Date() - 365, to = Sys.Date(), api_key = NULL) {
  if (is.null(api_key)) {
    api_key <- Sys.getenv("FINNHUB_API_KEY")
  }
  
  # If no API key, fallback to Yahoo
  if (is.null(api_key) || nchar(api_key) == 0) {
    return(fetch_equity_prices(symbol, from, to))
  }
  
  tryCatch({
    # Get historical candles
    url <- "https://finnhub.io/api/v1/stock/candle"
    from_ts <- as.numeric(as.POSIXct(from))
    to_ts <- as.numeric(as.POSIXct(to))
    
    res <- httr::GET(url, query = list(
      symbol = symbol,
      resolution = "D",
      from = from_ts,
      to = to_ts,
      token = api_key
    ))
    
    if (httr::status_code(res) == 200) {
      data <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
      if (data$s == "ok" && length(data$c) > 0) {
        # Convert to xts format
        ts <- as.POSIXct(data$t, origin = "1970-01-01", tz = "UTC")
        result <- xts::xts(
          cbind(Open = data$o, High = data$h, Low = data$l, Close = data$c, Volume = data$v),
          order.by = ts
        )
        colnames(result) <- c("Open", "High", "Low", "Close", "Volume")
        return(result)
      }
    }
    # Fallback to Yahoo if Finnhub fails
    return(fetch_equity_prices(symbol, from, to))
  }, error = function(e) {
    # Fallback to Yahoo on error
    return(fetch_equity_prices(symbol, from, to))
  })
}

# Free Indian Stock Market API - indianapi.in (Real-time, no API key required!)
# Source: https://indianapi.in/documentation/indian-stock-market
# Note: This API provides current/live prices. For historical charts, we still use Yahoo Finance.
fetch_indian_equity_prices_realtime <- function(symbol, from = Sys.Date() - 365, to = Sys.Date()) {
  tryCatch({
    # Remove .NS suffix if present
    base_symbol <- gsub("\\.NS$", "", toupper(symbol))
    
    # Indian Stock Exchange API - Free real-time data (no key required)
    # This API provides real-time NSE/BSE prices
    url <- "https://indianapi.in/api/stock"
    
    # Try to get current real-time price
    res <- httr::GET(url, query = list(company = base_symbol), httr::timeout(10))
    
    if (httr::status_code(res) == 200) {
      data <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
      
      # The API returns current price data
      # We'll use Yahoo Finance for historical data (needed for charts)
      # but mark that we have real-time current price available
      yahoo_data <- fetch_equity_prices(paste0(base_symbol, ".NS"), from, to)
      
      # If we got real-time price data, we could update the last price
      # For now, we return Yahoo data (which has full historical)
      # The real-time price will be shown in the current price metric
      # Future enhancement: Could update the last row of yahoo_data with real-time price
      
      return(yahoo_data)
    }
    
    # Fallback to Yahoo Finance if API fails
    return(fetch_equity_prices(paste0(base_symbol, ".NS"), from, to))
  }, error = function(e) {
    # Always fallback to Yahoo on error
    return(fetch_equity_prices(paste0(gsub("\\.NS$", "", symbol), ".NS"), from, to))
  })
}

# Yahoo Finance (original - 15-20 min delay)
fetch_equity_prices <- function(symbol, from = Sys.Date() - 365, to = Sys.Date()) {
  suppressWarnings({
    getSymbols(
      Symbols = symbol,
      src = "yahoo",
      from = from,
      to = to,
      auto.assign = FALSE
    )
  })
}

fetch_crypto_prices <- function(symbol = "bitcoin", vs_currency = "inr", days = 365) {
  # Simple CoinGecko integration (no key required)
  url <- sprintf(
    "https://api.coingecko.com/api/v3/coins/%s/market_chart",
    symbol
  )
  res <- httr::GET(url, query = list(vs_currency = vs_currency, days = days))
  httr::stop_for_status(res)
  data <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
  prices <- data$prices
  if (is.null(prices) || nrow(prices) == 0) {
    stop("No crypto price data returned.")
  }
  # prices: matrix/data.frame of [timestamp, price]
  if (is.data.frame(prices)) {
    price_vec <- prices[[2]]
    ts_vec <- prices[[1]]
  } else {
    price_vec <- prices[, 2]
    ts_vec <- prices[, 1]
  }
  ts <- as.POSIXct(ts_vec / 1000, origin = "1970-01-01", tz = "UTC")
  result <- xts::xts(price_vec, order.by = ts)
  colnames(result) <- "Close"
  result
}

# Get USD to INR exchange rate
get_usd_to_inr_rate <- function() {
  tryCatch({
    # Using CoinGecko's simple price API for USD/INR (using a stablecoin as proxy)
    # Or use a free forex API
    url <- "https://api.coingecko.com/api/v3/simple/price"
    res <- httr::GET(url, query = list(ids = "tether", vs_currencies = "inr"))
    if (httr::status_code(res) == 200) {
      data <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
      if (!is.null(data$tether$inr)) {
        return(data$tether$inr)
      }
    }
    # Fallback: use a fixed rate (approximate, should be updated)
    return(83.0)
  }, error = function(e) {
    # Fallback rate if API fails
    return(83.0)
  })
}

compute_returns <- function(price_xts) {
  # Handle both OHLC (equity) and single-column (crypto) xts objects
  if (ncol(price_xts) > 1 && "Close" %in% colnames(price_xts)) {
    prices <- Cl(price_xts)
  } else if (ncol(price_xts) > 1 && any(grepl("Close|CLOSE", colnames(price_xts)))) {
    prices <- price_xts[, grep("Close|CLOSE", colnames(price_xts))[1]]
  } else {
    # Single column (crypto) or use first column
    prices <- price_xts[, 1]
  }
  na.omit(PerformanceAnalytics::Return.calculate(prices))
}

compute_var_historical <- function(returns, p = 0.95, method = "historical") {
  PerformanceAnalytics::VaR(returns, p = p, method = method)
}

compute_mc_simulation <- function(returns, n_days = 1, n_sims = 5000, seed = 123) {
  set.seed(seed)
  mu <- mean(returns)
  sigma <- sd(returns)
  S0 <- as.numeric(last(1 + returns))
  sims <- matrix(NA_real_, nrow = n_sims, ncol = n_days)
  for (i in seq_len(n_sims)) {
    shocks <- rnorm(n_days, mean = mu, sd = sigma)
    path <- cumprod(1 + shocks) * S0
    sims[i, ] <- path
  }
  sims
}

compute_mc_var <- function(sims, p = 0.95) {
  # Compute P&L at horizon and get VaR
  S0 <- sims[1, 1]
  ST <- sims[, ncol(sims)]
  pnl <- ST - S0
  quantile(-pnl, probs = p, na.rm = TRUE)
}

# -----------------------------
# UI
# -----------------------------

popular_equities <- c(
  # Mega-cap tech
  "AAPL","MSFT","GOOG","GOOGL","AMZN","META","NVDA","TSLA","AVGO","ADBE",
  # US large-cap diversified
  "JPM","BAC","WFC","V","MA","UNH","JNJ","PG","HD","DIS",
  "XOM","CVX","PFE","CSCO","KO","PEP","INTC","NFLX","CRM","ORCL",
  # Index & sector ETFs
  "SPY","QQQ","DIA","IWM","XLK","XLF","XLE","XLV","XLU","XLY",
  # Global / EM ETFs
  "EEM","EFA","VWO","FXI","EWZ","EWJ","INDA","RSX","GLD","SLV",
  # Additional active names
  "AMD","INTU","PYPL","SQ","SHOP","BA","NKE","C","T","VZ"
)

# Indian stocks (NSE - use .NS suffix for Yahoo Finance)
# Comprehensive list of all major Indian stocks across all sectors
indian_stocks <- c(
  # === LARGE CAP - IT SERVICES ===
  "TCS.NS", "INFY.NS", "HCLTECH.NS", "WIPRO.NS", "TECHM.NS", "LTIM.NS", "MPHASIS.NS",
  "LTTS.NS", "MINDTREE.NS", "PERSISTENT.NS", "COFORGE.NS", "ZENSAR.NS", "CYIENT.NS",
  "SONATA.NS", "HEXAWARE.NS", "NIITTECH.NS", "INTELLECT.NS", "NEWGEN.NS", "KPITTECH.NS",
  
  # === LARGE CAP - BANKING ===
  "HDFCBANK.NS", "ICICIBANK.NS", "SBIN.NS", "KOTAKBANK.NS", "AXISBANK.NS", "INDUSINDBK.NS",
  "PNB.NS", "BANKBARODA.NS", "CANBK.NS", "UNIONBANK.NS", "IDFCFIRSTB.NS", "FEDERALBNK.NS",
  "RBLBANK.NS", "YESBANK.NS", "SOUTHBANK.NS", "CENTRALBK.NS",
  
  # === LARGE CAP - FINANCIAL SERVICES ===
  "HDFC.NS", "BAJFINANCE.NS", "BAJAJFINSV.NS", "SBILIFE.NS", "HDFCLIFE.NS", "ICICIPRULI.NS",
  "ICICIGI.NS", "M&MFIN.NS", "SRTRANSFIN.NS", "CHOLAFIN.NS", "LICHSGFIN.NS", "MUTHOOTFIN.NS",
  "MANAPPURAM.NS", "MOTILALOFS.NS", "IIFL.NS", "EDELWEISS.NS", "RELIANCE.NS",
  
  # === LARGE CAP - FMCG ===
  "ITC.NS", "HINDUNILVR.NS", "NESTLEIND.NS", "BRITANNIA.NS", "MARICO.NS", "DABUR.NS",
  "GODREJCP.NS", "COLPAL.NS", "EMAMILTD.NS", "JUBLFOOD.NS", "VARUNBEV.NS", "RADICO.NS",
  "UNITEDSPR.NS", "UBL.NS", "SOMANY.NS",
  
  # === LARGE CAP - AUTOMOBILES ===
  "MARUTI.NS", "M&M.NS", "TATAMOTORS.NS", "BAJAJ-AUTO.NS", "HEROMOTOCO.NS", "EICHERMOT.NS",
  "ASHOKLEY.NS", "TVSMOTOR.NS", "FORCEMOT.NS", "ESCORTS.NS", "MAHSCOPE.NS", "BALKRISIND.NS",
  "MRF.NS", "APOLLOTYRE.NS", "CEAT.NS", "JKTYRE.NS",
  
  # === LARGE CAP - PHARMACEUTICALS ===
  "SUNPHARMA.NS", "DRREDDY.NS", "CIPLA.NS", "DIVISLAB.NS", "LUPIN.NS", "TORNTPHARM.NS",
  "AUROPHARMA.NS", "CADILAHC.NS", "GLENMARK.NS", "BIOCON.NS", "NATCOPHARM.NS", "ALEMBICLTD.NS",
  "LAURUSLABS.NS", "GRANULES.NS", "IPCALAB.NS", "REDDY.NS", "MANKIND.NS",
  
  # === LARGE CAP - ENERGY (OIL & GAS) ===
  "ONGC.NS", "IOC.NS", "BPCL.NS", "HPCL.NS", "GAIL.NS", "ADANIGREEN.NS", "ADANITRANS.NS",
  "ADANIPORTS.NS", "ADANIENT.NS", "ADANIPOWER.NS", "RELIANCE.NS", "PETRONET.NS", "IGL.NS",
  "MGL.NS", "GSPL.NS", "GUJGAS.NS", "AEGISCHEM.NS",
  
  # === LARGE CAP - TELECOMMUNICATIONS ===
  "BHARTIARTL.NS", "RIL.NS", "IDEA.NS", "TATACOMM.NS",
  
  # === LARGE CAP - METALS & MINING ===
  "TATASTEEL.NS", "JSWSTEEL.NS", "HINDALCO.NS", "VEDL.NS", "JINDALSTEL.NS", "SAIL.NS",
  "NMDC.NS", "MOIL.NS", "NLCINDIA.NS", "COALINDIA.NS", "HINDZINC.NS", "NATIONALUM.NS",
  "RATNAMANI.NS", "APARINDS.NS",
  
  # === LARGE CAP - CEMENT ===
  "ULTRACEMCO.NS", "SHREECEM.NS", "ACC.NS", "AMBUJACEM.NS", "RAMCOCEM.NS", "JKLAKSHMI.NS",
  "ORIENTCEM.NS", "PRISMCEM.NS", "DALMIABHA.NS", "KCP.NS",
  
  # === LARGE CAP - CONSUMER DURABLES ===
  "TITAN.NS", "DMART.NS", "ASIANPAINT.NS", "BERGEPAINT.NS", "KANSAINER.NS", "AKZOINDIA.NS",
  "WHIRLPOOL.NS", "VOLTAS.NS", "BLUESTARCO.NS", "HAVELLS.NS", "ORIENTELEC.NS", "CROMPTON.NS",
  "VGUARD.NS", "BAJAJELEC.NS", "AMBER.NS",
  
  # === LARGE CAP - REAL ESTATE ===
  "DLF.NS", "GODREJPROP.NS", "PRESTIGE.NS", "SOBHA.NS", "BRIGADE.NS", "MAHINDRA.NS",
  "LODHA.NS", "OBEROIRLTY.NS", "PHOENIXLTD.NS", "SHOBHA.NS",
  
  # === LARGE CAP - INFRASTRUCTURE ===
  "LT.NS", "LARSEN.NS", "NTPC.NS", "POWERGRID.NS", "NHPC.NS", "SJVN.NS", "IRCTC.NS",
  "CONCOR.NS", "GMRINFRA.NS", "GVKPIL.NS", "ADANIPORTS.NS",
  
  # === LARGE CAP - MEDIA & ENTERTAINMENT ===
  "ZEE.NS", "SUNTV.NS", "TVTODAY.NS", "NETWORK18.NS", "TV18BRDCST.NS", "DISHTV.NS",
  
  # === LARGE CAP - RETAIL ===
  "TRENT.NS", "SHOPPERSSTOP.NS", "FUTURE.NS", "V-MART.NS", "SPENCERS.NS",
  
  # === MID CAP - TECHNOLOGY ===
  "ZOMATO.NS", "PAYTM.NS", "NYKA.NS", "POLICYBZR.NS", "DELHIVERY.NS", "NAZARA.NS",
  "MAPMYINDIA.NS", "TATAELXSI.NS", "MINDTREE.NS", "LTI.NS",
  
  # === MID CAP - FINANCIAL SERVICES ===
  "AUBANK.NS", "RBLBANK.NS", "IDFCFIRSTB.NS", "EQUITAS.NS", "UJJIVAN.NS", "FINCABLES.NS",
  "SUNDARMFIN.NS", "MASFIN.NS", "FIVESTAR.NS",
  
  # === MID CAP - PHARMACEUTICALS ===
  "TORNTPHARM.NS", "GLENMARK.NS", "ALEMBICLTD.NS", "LAURUSLABS.NS", "GRANULES.NS",
  "IPCALAB.NS", "MANKIND.NS", "AJANTA.NS", "NEULAND.NS",
  
  # === MID CAP - CHEMICALS ===
  "UPL.NS", "RALLIS.NS", "DEEPAKNTR.NS", "SRF.NS", "GHCL.NS", "TATACHEM.NS", "BASF.NS",
  "AARTIIND.NS", "BALKRISIND.NS", "VINATIORGA.NS",
  
  # === MID CAP - TEXTILES ===
  "ARVIND.NS", "WELSPUN.NS", "TRIDENT.NS", "RAYMOND.NS", "KPRMILL.NS", "SPENTEX.NS",
  
  # === MID CAP - PAPER ===
  "JKPAPER.NS", "BILT.NS", "TNPL.NS", "SESAGOA.NS",
  
  # === MID CAP - ENGINEERING ===
  "THERMAX.NS", "KIRLOSENG.NS", "ELGI.NS", "GRINDWELL.NS", "TIMKEN.NS", "SKFINDIA.NS",
  
  # === MID CAP - POWER ===
  "TATAPOWER.NS", "ADANIPOWER.NS", "TORNTPOWER.NS", "CESC.NS", "RPOWER.NS", "JSWENERGY.NS",
  
  # === MID CAP - LOGISTICS ===
  "MAHINDRA.NS", "CONCOR.NS", "GATI.NS", "TCI.NS", "ALLCARGO.NS",
  
  # === SMALL CAP - POPULAR ===
  "IRFC.NS", "RVNL.NS", "IRCON.NS", "RAILTEL.NS", "TATACOMM.NS", "TATAMETALI.NS",
  "TATAINVEST.NS", "TATACOMM.NS",
  
  # === INDICES ===
  "NIFTY.NS", "NIFTY50.NS", "SENSEX.NS", "NIFTYBANK.NS", "NIFTYIT.NS", "NIFTYFMCG.NS",
  "NIFTYPHARMA.NS", "NIFTYAUTO.NS", "NIFTYENERGY.NS", "NIFTYMETAL.NS", "NIFTYREALTY.NS",
  
  # === ADANI GROUP ===
  "ADANIENT.NS", "ADANIPORTS.NS", "ADANIPOWER.NS", "ADANIGREEN.NS", "ADANITRANS.NS",
  "ADANITOTAL.NS", "ADANIWILMAR.NS", "ADANIGAS.NS",
  
  # === TATA GROUP ===
  "TATAMOTORS.NS", "TATASTEEL.NS", "TATACONSUM.NS", "TATAPOWER.NS", "TATACHEM.NS",
  "TATACOMM.NS", "TATAELXSI.NS", "TATACOMM.NS", "TATAMETALI.NS", "TATAINVEST.NS",
  
  # === RELIANCE GROUP ===
  "RELIANCE.NS", "JIOFIN.NS",
  
  # === HDFC GROUP ===
  "HDFC.NS", "HDFCBANK.NS", "HDFCLIFE.NS", "HDFCAMC.NS",
  
  # === ICICI GROUP ===
  "ICICIBANK.NS", "ICICIPRULI.NS", "ICICIGI.NS", "ICICISEC.NS"
)

app_theme <- bs_theme(
  version = 5,
  bootswatch = "cyborg",
  primary = "#00E676",
  secondary = "#00B0FF",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

ui <- page_fluid(
  theme = app_theme,
  tags$head(
    tags$title("High-Frequency Financial Risk Dashboard"),
    tags$script(src = "https://s3.tradingview.com/tv.js"),
    tags$style(HTML("
      body { background-color: #0b0c10; }
      .card {
        background-color: #14161f;
        border-radius: 14px;
        border: 1px solid #1f2230;
        padding: 20px 24px;
        margin-bottom: 20px;
      }
      .card h5 {
        margin-bottom: 16px;
        color: #ffffff;
        font-weight: 600;
        font-size: 1.1rem;
      }
      .metric-value {
        font-size: 2rem;
        font-weight: 700;
        color: #ffffff;
        line-height: 1.2;
        word-break: break-word;
      }
      .metric-label {
        font-size: 0.9rem;
        text-transform: uppercase;
        letter-spacing: 0.1em;
        color: #9ea5b4;
        font-weight: 500;
        margin-bottom: 4px;
      }
      .soft-badge {
        background: rgba(0, 230, 118, 0.1);
        border-radius: 999px;
        padding: 4px 10px;
        font-size: 0.75rem;
        color: #00e676;
      }
      #tradingview_widget {
        background-color: #14161f;
        border-radius: 8px;
      }
    "))
  ),
  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      h4("Controls"),
      tags$small(
        style = "color:#9ea5b4;",
        "Configure assets and risk parameters. Data auto-refreshes for a high-frequency feel."
      ),
      hr(),
      selectInput(
        "asset_type",
        "Asset Type",
        choices = c(
          "US Equity (Yahoo Finance)" = "equity",
          "Indian Equity (NSE)" = "indian_equity",
          "Crypto (CoinGecko)" = "crypto"
        )
      ),
      conditionalPanel(
        condition = "input.asset_type == 'equity'",
        selectInput(
          "data_source",
          "Data Source (Delay)",
          choices = c(
            "Yahoo Finance (15-20 min delay)" = "yahoo",
            "Finnhub (Real-time, FREE API key)" = "finnhub"
          ),
          selected = "yahoo"
        ),
        conditionalPanel(
          condition = "input.data_source == 'finnhub'",
          tags$small(
            style = "color:#00E676;",
            "Get free API key at: ",
            tags$a(href = "https://finnhub.io/register", target = "_blank", "finnhub.io/register"),
            ". Set FINNHUB_API_KEY environment variable."
          )
        )
      ),
      conditionalPanel(
        condition = "input.asset_type == 'indian_equity'",
        selectInput(
          "indian_data_source",
          "Data Source (Delay)",
          choices = c(
            "Yahoo Finance (15-20 min delay)" = "yahoo",
            "Indian API (Real-time, FREE)" = "indianapi"
          ),
          selected = "yahoo"
        ),
        conditionalPanel(
          condition = "input.indian_data_source == 'indianapi'",
          tags$small(
            style = "color:#00E676;",
            "Free real-time API: ",
            tags$a(href = "https://indianapi.in", target = "_blank", "indianapi.in"),
            tags$br(),
            "Note: Current price only (historical charts use Yahoo)"
          )
        )
      ),
      conditionalPanel(
        condition = "input.asset_type == 'equity'",
        selectizeInput(
          "equity_symbol",
          "US Ticker",
          choices = popular_equities,
          selected = "AAPL",
          multiple = FALSE,
          options = list(
            placeholder = "Type or select a US ticker",
            create = TRUE
          )
        )
      ),
      conditionalPanel(
        condition = "input.asset_type == 'indian_equity'",
        selectizeInput(
          "indian_equity_symbol",
          "Indian Stock (NSE)",
          choices = indian_stocks,
          selected = "RELIANCE.NS",
          multiple = FALSE,
          options = list(
            placeholder = "Type any NSE symbol (e.g. RELIANCE, TCS.NS) - 200+ stocks available",
            create = TRUE,
            maxOptions = 500
          )
        )
      ),
      conditionalPanel(
        condition = "input.asset_type == 'crypto'",
        textInput("crypto_symbol", "Crypto (e.g. bitcoin, ethereum)", value = "bitcoin")
      ),
      sliderInput(
        "lookback_days",
        "Lookback Window (days)",
        min = 60, max = 730, value = 365, step = 30
      ),
      sliderInput(
        "var_confidence",
        "VaR Confidence Level",
        min = 0.90, max = 0.99, value = 0.95, step = 0.01
      ),
      sliderInput(
        "horizon_days",
        "Risk Horizon (days)",
        min = 1, max = 10, value = 1
      ),
      sliderInput(
        "mc_sims",
        "Monte Carlo Simulations",
        min = 1000, max = 20000, value = 5000, step = 1000
      ),
      sliderInput(
        "refresh_secs",
        "Auto-Refresh (seconds)",
        min = 10, max = 300, value = 60, step = 10
      ),
      checkboxInput("auto_refresh", "Enable Auto-Refresh", value = TRUE),
      hr(),
      tags$div(
        style = "background: rgba(0, 230, 118, 0.1); border-left: 3px solid #00E676; padding: 10px; margin-top: 10px; border-radius: 4px;",
        tags$strong(style = "color:#00E676;", "💡 Reduce Delay:"),
        tags$p(
          style = "color:#9ea5b4; font-size: 0.85rem; margin: 5px 0 0 0;",
          "Use Finnhub (FREE) for US stocks - real-time data!",
          tags$br(),
          "Get free API key: ",
          tags$a(href = "https://finnhub.io/register", target = "_blank", style = "color:#00B0FF;", "finnhub.io/register"),
          tags$br(),
          "Set environment variable: FINNHUB_API_KEY"
        )
      ),
      tags$small(
        style = "color:#748094; margin-top: 10px; display: block;",
        "Data: free Yahoo Finance (15-20 min delay) & CoinGecko APIs."
      )
    ),
    layout_columns(
      col_widths = c(12),
      div(
        class = "card",
        style = "padding: 24px 28px;",
        fluidRow(
          column(
            4,
            style = "padding: 16px 20px; border-right: 1px solid #1f2230;",
            div(class = "metric-label", "Live Price (INR)"),
            div(textOutput("metric_price"), class = "metric-value", style = "margin: 12px 0;"),
            tags$div(textOutput("metric_symbol"), style = "color:#9ea5b4; font-size:0.85rem; margin-top: 8px;"),
            tags$div(textOutput("metric_delay"), style = "color:#748094; font-size:0.75rem; margin-top: 4px; font-style: italic;")
          ),
          column(
            4,
            style = "padding: 16px 20px; border-right: 1px solid #1f2230;",
            div(class = "metric-label", "Daily VaR"),
            div(textOutput("metric_var"), class = "metric-value", style = "margin: 12px 0;"),
            tags$div(textOutput("metric_var_conf"), style = "color:#9ea5b4; font-size:0.85rem; margin-top: 8px;")
          ),
          column(
            4,
            style = "padding: 16px 20px;",
            div(class = "metric-label", "Volatility (Ann.)"),
            div(textOutput("metric_vol"), class = "metric-value", style = "margin: 12px 0;"),
            tags$div("Last lookback window", style = "color:#9ea5b4; font-size:0.85rem; margin-top: 8px;")
          )
        )
      ),
      tabsetPanel(
        type = "pills",
        tabPanel(
          "Price & Returns",
          div(
            class = "card",
            h5("Price History"),
            plotlyOutput("price_plot", height = "320px")
          ),
          div(
            class = "card",
            h5("Daily Returns"),
            plotlyOutput("returns_plot", height = "280px")
          )
        ),
        tabPanel(
          "VaR & Tail Risk",
          div(
            class = "card",
            h5("Historical VaR"),
            plotlyOutput("var_histogram", height = "320px")
          ),
          div(
            class = "card",
            h5("Risk Summary"),
            tableOutput("risk_table")
          )
        ),
        tabPanel(
          "Monte Carlo",
          div(
            class = "card",
            h5("Simulated P&L Distribution"),
            plotlyOutput("mc_pnl_plot", height = "320px")
          ),
          div(
            class = "card",
            h5("Simulation Snapshot"),
            plotlyOutput("mc_paths_plot", height = "280px")
          )
        ),
        tabPanel(
          "TradingView Chart",
          div(
            class = "card",
            style = "padding: 0; overflow: hidden;",
            div(
              id = "tradingview_widget",
              style = "height: 600px; width: 100%; position: relative;"
            ),
            div(
              id = "tradingview_iframe",
              style = "height: 600px; width: 100%; display: none;"
            ),
            tags$script(HTML("
              var tradingViewWidget = null;
              
              // Map common NSE symbols to TradingView-compatible formats
              var symbolMap = {
                'RELIANCE': 'BSE:RELIANCE',
                'TCS': 'BSE:TCS',
                'INFY': 'BSE:INFY',
                'HDFCBANK': 'BSE:HDFCBANK',
                'ICICIBANK': 'BSE:ICICIBANK',
                'SBIN': 'BSE:SBIN',
                'BHARTIARTL': 'BSE:BHARTIARTL',
                'ITC': 'BSE:ITC',
                'HINDUNILVR': 'BSE:HINDUNILVR',
                'MARUTI': 'BSE:MARUTI',
                'TATAMOTORS': 'BSE:TATAMOTORS',
                'SUNPHARMA': 'BSE:SUNPHARMA',
                'WIPRO': 'BSE:WIPRO',
                'ONGC': 'BSE:ONGC',
                'TATASTEEL': 'BSE:TATASTEEL',
                'NTPC': 'BSE:NTPC',
                'POWERGRID': 'BSE:POWERGRID',
                'ULTRACEMCO': 'BSE:ULTRACEMCO',
                'TITAN': 'BSE:TITAN',
                'ASIANPAINT': 'BSE:ASIANPAINT'
              };
              
              function getTradingViewSymbol(symbol) {
                // Remove exchange prefix if present
                var baseSymbol = symbol.replace(/^(NSE|BSE):/i, '').toUpperCase();
                // Check if we have a mapping
                if (symbolMap[baseSymbol]) {
                  return symbolMap[baseSymbol];
                }
                // Try BSE format (more reliable)
                return 'BSE:' + baseSymbol;
              }
              
              function createTradingViewWidget(symbol) {
                var container = document.getElementById('tradingview_widget');
                var iframeContainer = document.getElementById('tradingview_iframe');
                if (!container || !iframeContainer) return;
                
                // Convert symbol to TradingView format
                var tvSymbol = getTradingViewSymbol(symbol || 'BSE:RELIANCE');
                var baseSymbol = tvSymbol.replace(/^(NSE|BSE):/i, '');
                
                // Use iframe embed for better compatibility with Indian stocks
                container.style.display = 'none';
                iframeContainer.style.display = 'block';
                iframeContainer.innerHTML = '<iframe src=\"https://www.tradingview.com/chart/?symbol=' + 
                  encodeURIComponent('BSE:' + baseSymbol) + 
                  '&theme=dark&interval=D\" style=\"width: 100%; height: 600px; border: none;\" frameborder=\"0\"></iframe>';
                
                // Also try widget approach as fallback
                if (typeof TradingView !== 'undefined') {
                  try {
                    container.innerHTML = '';
                    tradingViewWidget = new TradingView.widget({
                      autosize: true,
                      symbol: tvSymbol,
                      interval: 'D',
                      timezone: 'Asia/Kolkata',
                      theme: 'dark',
                      style: '1',
                      locale: 'en',
                      toolbar_bg: '#14161f',
                      enable_publishing: false,
                      allow_symbol_change: true,
                      container_id: 'tradingview_widget',
                      studies: ['RSI@tv-basicstudies', 'MACD@tv-basicstudies', 'Volume@tv-basicstudies'],
                      height: 600,
                      width: '100%'
                    });
                    // If widget loads successfully, hide iframe
                    setTimeout(function() {
                      if (container.innerHTML && !container.innerHTML.includes('error')) {
                        iframeContainer.style.display = 'none';
                        container.style.display = 'block';
                      }
                    }, 2000);
                  } catch(e) {
                    console.log('Widget approach failed, using iframe');
                  }
                }
              }
              
              $(document).on('shiny:connected', function() {
                setTimeout(function() { createTradingViewWidget('BSE:RELIANCE'); }, 1000);
              });
              
              Shiny.addCustomMessageHandler('updateTradingView', function(message) {
                if (message && message.symbol) {
                  createTradingViewWidget(message.symbol);
                }
              });
            "))
          )
        ),
        tabPanel(
          "About",
          div(
            class = "card",
            h4("High-Frequency Financial Risk Dashboard"),
            tags$p(
              style = "color:#9ea5b4;",
              "This dashboard illustrates how to take quantitative finance workflows in R ",
              "from static notebooks to a responsive, production-ready interface."
            ),
            tags$ul(
              style = "color:#9ea5b4;",
              tags$li("Real-time style updates via auto-refresh."),
              tags$li("Historical and Monte Carlo VaR for single assets."),
              tags$li("Designed for extension to portfolios and intraday feeds.")
            ),
            tags$div(
              class = "soft-badge",
              "Tech: R, Shiny, quantmod, PerformanceAnalytics, Docker-ready"
            )
          )
        )
      )
    )
  )
)

# -----------------------------
# Server
# -----------------------------

server <- function(input, output, session) {
  # Auto-refresh trigger
  data_refresh_trigger <- reactive({
    if (isTRUE(input$auto_refresh)) {
      invalidateLater(input$refresh_secs * 1000, session)
    }
    Sys.time()
  })

  # Cache USD to INR rate
  usd_to_inr_rate <- reactive({
    data_refresh_trigger()
    get_usd_to_inr_rate()
  })

  price_series <- reactive({
    data_refresh_trigger()
    req(input$asset_type)

    lookback_days <- input$lookback_days
    from <- Sys.Date() - lookback_days
    to <- Sys.Date()

    if (input$asset_type == "equity") {
      symbol <- toupper(trimws(input$equity_symbol))
      if (!nzchar(symbol)) {
        shiny::validate(shiny::need(FALSE, "Enter a valid equity ticker (e.g. AAPL, TSLA)."))
      }
      tryCatch({
        # Use selected data source
        data_source <- if (is.null(input$data_source)) "yahoo" else input$data_source
        if (data_source == "finnhub") {
          xts_obj <- fetch_equity_prices_finnhub(symbol, from, to)
        } else {
          xts_obj <- fetch_equity_prices(symbol, from, to)
        }
        if (is.null(xts_obj) || nrow(xts_obj) == 0) {
          shiny::validate(shiny::need(FALSE, paste("No data returned for", symbol)))
        }
        # Convert USD prices to INR
        rate <- usd_to_inr_rate()
        if (ncol(xts_obj) > 1) {
          # Convert all OHLC columns
          for (i in 1:ncol(xts_obj)) {
            xts_obj[, i] <- xts_obj[, i] * rate
          }
        } else {
          xts_obj[, 1] <- xts_obj[, 1] * rate
        }
        attr(xts_obj, "asset_label") <- symbol
        attr(xts_obj, "tradingview_symbol") <- paste0("NASDAQ:", symbol)
        xts_obj
      }, error = function(e) {
        err_msg <- as.character(conditionMessage(e))
        shiny::validate(shiny::need(FALSE, paste("Error fetching equity data for", symbol, ":", err_msg)))
      })
    } else if (input$asset_type == "indian_equity") {
      symbol <- toupper(trimws(input$indian_equity_symbol))
      if (!nzchar(symbol)) {
        shiny::validate(shiny::need(FALSE, "Enter a valid Indian stock ticker (e.g. RELIANCE.NS, TCS.NS, or just RELIANCE)."))
      }
      # Auto-add .NS suffix if not present
      if (!grepl("\\.NS$", symbol)) {
        symbol <- paste0(symbol, ".NS")
      }
      tryCatch({
        # Indian stocks: Use selected data source
        data_source <- if (is.null(input$indian_data_source)) "yahoo" else input$indian_data_source
        
        if (data_source == "indianapi") {
          # Try free real-time API (indianapi.in)
          # Note: This API provides current price, but we still need Yahoo for historical charts
          xts_obj <- fetch_indian_equity_prices_realtime(symbol, from, to)
        } else {
          # Yahoo Finance (15-20 min delay, but has full historical data)
          xts_obj <- fetch_equity_prices(symbol, from, to)
        }
        if (is.null(xts_obj) || nrow(xts_obj) == 0) {
          shiny::validate(shiny::need(FALSE, paste("No data returned for", symbol)))
        }
        # Extract base symbol for TradingView (remove .NS suffix)
        base_symbol <- gsub("\\.NS$", "", symbol)
        attr(xts_obj, "asset_label") <- base_symbol
        # TradingView uses BSE format for Indian stocks (more reliable than NSE)
        attr(xts_obj, "tradingview_symbol") <- paste0("BSE:", base_symbol)
        xts_obj
      }, error = function(e) {
        err_msg <- as.character(conditionMessage(e))
        shiny::validate(shiny::need(FALSE, paste("Error fetching Indian equity data for", symbol, ":", err_msg)))
      })
    } else {
      symbol <- tolower(trimws(input$crypto_symbol))
      if (!nzchar(symbol)) {
        shiny::validate(shiny::need(FALSE, "Enter a valid crypto symbol (e.g. bitcoin, ethereum)."))
      }
      tryCatch({
        xts_obj <- fetch_crypto_prices(symbol, vs_currency = "inr", days = lookback_days)
        if (is.null(xts_obj) || length(xts_obj) == 0) {
          shiny::validate(shiny::need(FALSE, paste("No data returned for", symbol)))
        }
        attr(xts_obj, "asset_label") <- symbol
        # TradingView uses different exchanges for crypto
        tv_symbol <- paste0("COINBASE:", toupper(symbol), "USD")
        attr(xts_obj, "tradingview_symbol") <- tv_symbol
        xts_obj
      }, error = function(e) {
        err_msg <- as.character(conditionMessage(e))
        shiny::validate(shiny::need(FALSE, paste("Error fetching crypto data for", symbol, ":", err_msg)))
      })
    }
  })

  # Update TradingView widget when symbol changes
  observe({
    # React to asset type and symbol changes
    asset_type <- input$asset_type
    if (asset_type == "equity") {
      symbol <- input$equity_symbol
      if (!is.null(symbol) && nzchar(symbol)) {
        tv_symbol <- paste0("NASDAQ:", toupper(symbol))
        session$sendCustomMessage("updateTradingView", list(symbol = tv_symbol))
      }
    } else if (asset_type == "indian_equity") {
      symbol <- input$indian_equity_symbol
      if (!is.null(symbol) && nzchar(symbol)) {
        symbol <- toupper(trimws(symbol))
        # Remove .NS suffix if present for TradingView
        base_symbol <- gsub("\\.NS$", "", symbol)
        # Try BSE format first (more reliable on TradingView)
        tv_symbol <- paste0("BSE:", base_symbol)
        session$sendCustomMessage("updateTradingView", list(symbol = tv_symbol))
      }
    } else if (asset_type == "crypto") {
      symbol <- input$crypto_symbol
      if (!is.null(symbol) && nzchar(symbol)) {
        tv_symbol <- paste0("COINBASE:", toupper(symbol), "USD")
        session$sendCustomMessage("updateTradingView", list(symbol = tv_symbol))
      }
    }
  })

  returns_series <- reactive({
    px <- price_series()
    compute_returns(px)
  })

  # Metrics
  output$metric_symbol <- renderText({
    tryCatch({
      px <- price_series()
      label <- attr(px, "asset_label", exact = TRUE)
      if (is.null(label)) label <- "Unknown"
      as.character(paste0("Asset: ", label))
    }, error = function(e) {
      as.character(paste("Asset error:", as.character(conditionMessage(e))))
    })
  })

  output$metric_price <- renderText({
    tryCatch({
      px <- price_series()
      # Handle both OHLC and single-column data
      if (ncol(px) > 1 && "Close" %in% colnames(px)) {
        last_price <- as.numeric(Cl(last(px)))
      } else if (ncol(px) > 1 && any(grepl("Close|CLOSE", colnames(px)))) {
        last_price <- as.numeric(last(px[, grep("Close|CLOSE", colnames(px))[1]]))
      } else {
        last_price <- as.numeric(last(px[, 1]))
      }
      if (is.na(last_price)) return(as.character("–"))
      # Format with INR symbol and proper formatting
      formatted_price <- format(round(last_price, 2), big.mark = ",", nsmall = 2)
      as.character(paste0("₹", formatted_price))
    }, error = function(e) {
      as.character(paste("Price error:", as.character(conditionMessage(e))))
    })
  })

  output$metric_delay <- renderText({
    tryCatch({
      asset_type <- input$asset_type
      if (asset_type == "crypto") {
        return("Data delay: <1 minute (CoinGecko)")
      } else if (asset_type == "equity") {
        # US stocks - can use Finnhub for real-time
        data_source <- if (is.null(input$data_source)) "yahoo" else input$data_source
        if (data_source == "finnhub") {
          api_key <- Sys.getenv("FINNHUB_API_KEY")
          if (nchar(api_key) > 0) {
            return("Data delay: Real-time (Finnhub)")
          } else {
            return("Data delay: Real-time (Finnhub - API key needed)")
          }
        } else {
          return("Data delay: 15-20 minutes (Yahoo Finance)")
        }
      } else if (asset_type == "indian_equity") {
        # Indian stocks - check selected data source
        data_source <- if (is.null(input$indian_data_source)) "yahoo" else input$indian_data_source
        if (data_source == "indianapi") {
          return("Data delay: Real-time (Indian API - Free)")
        } else {
          return("Data delay: 15-20 minutes (Yahoo Finance)")
        }
      }
      return("")
    }, error = function(e) {
      return("")
    })
  })

  output$metric_var <- renderText({
    tryCatch({
      rets <- returns_series()
      p <- input$var_confidence
      var_val <- tryCatch(
        compute_var_historical(rets, p = p),
        error = function(e) NA_real_
      )
      if (anyNA(var_val)) return(as.character("–"))
      pct <- 100 * as.numeric(var_val)
      as.character(paste0("-", sprintf("%.2f", pct), "%"))
    }, error = function(e) {
      as.character(paste("VaR error:", as.character(conditionMessage(e))))
    })
  })

  output$metric_var_conf <- renderText({
    as.character(paste0("1-day ", round(input$var_confidence * 100), "% historical VaR"))
  })

  output$metric_vol <- renderText({
    tryCatch({
      rets <- returns_series()
      vol_daily <- sd(rets)
      vol_annual <- vol_daily * sqrt(252)
      as.character(paste0(sprintf("%.2f", 100 * as.numeric(vol_annual)), "%"))
    }, error = function(e) {
      as.character(paste("Volatility error:", as.character(conditionMessage(e))))
    })
  })

  # Plots: Price & Returns
  output$price_plot <- renderPlotly({
    px <- price_series()
    label <- attr(px, "asset_label", exact = TRUE)
    if (is.null(label)) label <- "Unknown"
    # Handle both OHLC and single-column data
    if (ncol(px) > 1 && "Close" %in% colnames(px)) {
      prices <- as.numeric(Cl(px))
    } else if (ncol(px) > 1 && any(grepl("Close|CLOSE", colnames(px)))) {
      prices <- as.numeric(px[, grep("Close|CLOSE", colnames(px))[1]])
    } else {
      prices <- as.numeric(px[, 1])
    }
    df <- data.frame(
      time = index(px),
      price = prices
    )
    plot_ly(
      df,
      x = ~time, y = ~price,
      type = "scatter", mode = "lines",
      line = list(color = "#00E676", width = 2),
      hovertemplate = "Date: %{x}<br>Price: ₹%{y:,.2f}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "", gridcolor = "#1f2230"),
        yaxis = list(title = "Price (INR)", gridcolor = "#1f2230", tickformat = ",.0f"),
        margin = list(l = 50, r = 20, t = 30, b = 50),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        showlegend = FALSE,
        title = list(text = paste("Price History -", label), font = list(color = "#ffffff", size = 14))
      )
  })

  output$returns_plot <- renderPlotly({
    rets <- returns_series()
    df <- data.frame(
      time = index(rets),
      ret = as.numeric(rets)
    )
    plot_ly(
      df,
      x = ~time, y = ~ret,
      type = "bar",
      marker = list(color = ifelse(df$ret >= 0, "#00B0FF", "#FF5252"), line = list(width = 0)),
      hovertemplate = "Date: %{x}<br>Return: %{y:.2%}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "", gridcolor = "#1f2230"),
        yaxis = list(title = "Daily Return", gridcolor = "#1f2230", tickformat = ".2%"),
        margin = list(l = 50, r = 20, t = 30, b = 50),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        showlegend = FALSE
      )
  })

  # VaR & Tail Risk
  output$var_histogram <- renderPlotly({
    rets <- returns_series()
    p <- input$var_confidence
    var_val <- tryCatch(
      compute_var_historical(rets, p = p),
      error = function(e) NA_real_
    )
    df <- data.frame(ret = as.numeric(rets))

    h <- plot_ly(df, x = ~ret, type = "histogram", marker = list(color = "#00B0FF"))

    if (!anyNA(var_val)) {
      var_cut <- as.numeric(var_val)
      h <- h %>%
        add_lines(
          x = c(var_cut, var_cut),
          y = c(0, 1),
          line = list(color = "#FF5252", dash = "dash"),
          inherit = FALSE,
          showlegend = FALSE
        )
    }

    h %>%
      layout(
        xaxis = list(title = "Daily Return", gridcolor = "#1f2230", tickformat = ".2%"),
        yaxis = list(title = "Frequency", gridcolor = "#1f2230"),
        margin = list(l = 50, r = 20, t = 30, b = 50),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
  })

  output$risk_table <- renderTable({
    rets <- returns_series()
    p <- input$var_confidence
    horizon <- input$horizon_days

    var_1d <- tryCatch(
      as.numeric(compute_var_historical(rets, p = p)),
      error = function(e) NA_real_
    )
    var_h <- if (!is.na(var_1d)) var_1d * sqrt(horizon) else NA

    vol_daily <- sd(rets)
    vol_annual <- vol_daily * sqrt(252)

    mean_ret <- mean(rets)

    data.frame(
      Metric = c(
        "Mean daily return",
        "Daily volatility",
        "Annualized volatility",
        sprintf("1-day VaR @ %.0f%%", p * 100),
        sprintf("%d-day VaR @ %.0f%% (square-root-of-time)", horizon, p * 100)
      ),
      Value = c(
        sprintf("%.3f%%", 100 * as.numeric(mean_ret)),
        sprintf("%.3f%%", 100 * as.numeric(vol_daily)),
        sprintf("%.3f%%", 100 * as.numeric(vol_annual)),
        sprintf("-%.3f%%", 100 * as.numeric(var_1d)),
        sprintf("-%.3f%%", 100 * as.numeric(var_h))
      ),
      stringsAsFactors = FALSE
    )
  })

  # Monte Carlo
  mc_sims_reactive <- reactive({
    rets_xts <- returns_series()
    rets <- as.numeric(rets_xts)
    horizon <- input$horizon_days
    n_sims <- input$mc_sims
    compute_mc_simulation(rets, n_days = horizon, n_sims = n_sims)
  })

  output$mc_pnl_plot <- renderPlotly({
    sims <- mc_sims_reactive()
    S0 <- sims[1, 1]
    ST <- sims[, ncol(sims)]
    pnl <- ST - S0
    var_mc <- compute_mc_var(sims, p = input$var_confidence)

    df <- data.frame(pnl = pnl)

    h <- plot_ly(df, x = ~pnl, type = "histogram", marker = list(color = "#00E676"))

    h %>%
      add_lines(
        x = c(-var_mc, -var_mc),
        y = c(0, 1),
        line = list(color = "#FF5252", dash = "dash", width = 2),
        inherit = FALSE,
        showlegend = FALSE
      ) %>%
      layout(
        xaxis = list(title = "P&L at Horizon (INR)", gridcolor = "#1f2230", tickformat = ",.0f"),
        yaxis = list(title = "Frequency", gridcolor = "#1f2230"),
        margin = list(l = 50, r = 20, t = 30, b = 50),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
  })

  output$mc_paths_plot <- renderPlotly({
    sims <- mc_sims_reactive()
    n_days <- ncol(sims)
    days <- 0:(n_days - 1)

    # Take a subset of paths to plot
    n_plot <- min(50, nrow(sims))
    sims_plot <- sims[seq_len(n_plot), , drop = FALSE]

    plt <- plot_ly()
    for (i in seq_len(n_plot)) {
      plt <- plt %>%
        add_lines(
          x = days,
          y = sims_plot[i, ],
          line = list(color = "rgba(0,176,255,0.25)", width = 1),
          showlegend = FALSE
        )
    }

    plt %>%
      layout(
        xaxis = list(title = "Day", gridcolor = "#1f2230"),
        yaxis = list(title = "Simulated Price (INR)", gridcolor = "#1f2230", tickformat = ",.0f"),
        margin = list(l = 50, r = 20, t = 30, b = 50),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
  })
}

shinyApp(ui, server)


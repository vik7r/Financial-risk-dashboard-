library(shiny)
library(bslib)
library(plotly)
library(quantmod)
library(PerformanceAnalytics)
library(xts)
library(httr)
library(jsonlite)

# -----------------------------
# SSL Configuration for Windows
# -----------------------------
# Fix Windows SSL certificate issues by disabling SSL verification
# This is necessary on many Windows systems where SSL certificates
# are not properly configured in R's certificate store
if (.Platform$OS.type == "windows") {
  # Set global SSL config to handle Windows certificate issues
  # This is a workaround for SEC_E_NO_CREDENTIALS errors
  httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
}

# -----------------------------
# Helper functions
# -----------------------------

# Alpha Vantage API (FREE - More reliable than Yahoo Finance, no blocking)
# NOTE: Alpha Vantage has rate limits. For development, Yahoo/Finnhub may be better
fetch_equity_prices_alphavantage <- function(symbol, from = Sys.Date() - 365, to = Sys.Date()) {
  stop("Alpha Vantage disabled - please use Finnhub or Yahoo Finance instead. Alpha Vantage has strict rate limits (5 requests/min without key).")
}

# Finnhub API (FREE - Real-time for US stocks, better than Yahoo Finance)
# Get free API key at: https://finnhub.io/register
fetch_equity_prices_finnhub <- function(symbol, from = Sys.Date() - 365, to = Sys.Date(), api_key = NULL) {
  if (is.null(api_key)) {
    api_key <- Sys.getenv("FINNHUB_API_KEY")
  }
  
  # If no API key, fallback to Yahoo with clear message
  if (is.null(api_key) || nchar(api_key) == 0) {
    cat("[INFO] Finnhub API key not set - falling back to Yahoo Finance\n")
    cat("[INFO] To get real-time data:\n")
    cat("[INFO] 1. Sign up at https://finnhub.io/register (FREE, 2 minutes)\n")
    cat("[INFO] 2. Copy your API key\n")
    cat("[INFO] 3. Run: Sys.setenv(FINNHUB_API_KEY='your_key_here')\n")
    cat("[INFO] 4. Reload the app\n")
    return(fetch_equity_prices(symbol, from, to))
  }
  
  tryCatch({
    # Get historical candles
    url <- "https://finnhub.io/api/v1/stock/candle"
    from_ts <- as.numeric(as.POSIXct(from))
    to_ts <- as.numeric(as.POSIXct(to))
    
    # On Windows, use relaxed SSL verification to avoid certificate issues
    ssl_config <- if (.Platform$OS.type == "windows") {
      httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE)
    } else {
      httr::config()
    }
    res <- httr::GET(url, query = list(
      symbol = symbol,
      resolution = "D",
      from = from_ts,
      to = to_ts,
      token = api_key
    ), httr::timeout(60),  # Increased from 30 to 60 seconds
       httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"),
       httr::add_headers(
         "Accept-Language" = "en-US,en;q=0.9",
         "Cache-Control" = "no-cache"
       ),
       ssl_config)
    
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
# Note: This API provides current/live prices. For historical charts, we use Yahoo Finance.
fetch_indian_equity_prices_realtime <- function(symbol, from = Sys.Date() - 365, to = Sys.Date()) {
  tryCatch({
    # Remove .NS suffix if present
    base_symbol <- gsub("\\.NS$", "", toupper(symbol))
    
    # Indian Stock Exchange API - Free real-time data (no key required)
    # This API provides real-time NSE/BSE prices
    url <- "https://indianapi.in/api/stock"
    
    # On Windows, use relaxed SSL verification to avoid certificate issues
    ssl_config <- if (.Platform$OS.type == "windows") {
      httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE)
    } else {
      httr::config()
    }
    
    # Try to get current real-time price
    res <- httr::GET(
      url, 
      query = list(company = base_symbol), 
      httr::timeout(30),
      httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"),
      httr::add_headers(
        "Accept-Language" = "en-US,en;q=0.9",
        "Cache-Control" = "no-cache"
      ),
      ssl_config
    )
    
    if (httr::status_code(res) != 200) {
      cat("[WARN] Indian API returned status:", httr::status_code(res), "- falling back to Yahoo\n")
      return(fetch_equity_prices(paste0(base_symbol, ".NS"), from, to))
    }
    
    data <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
    
    # Get historical data from Yahoo for charts
    yahoo_data <- fetch_equity_prices(paste0(base_symbol, ".NS"), from, to)
    
    # Try to get real-time price and update the last row
    if (!is.null(data) && !is.null(data$price)) {
      current_price <- as.numeric(data$price)
      
      # Update the last close price with current real-time price
      if (!is.na(current_price) && nrow(yahoo_data) > 0) {
        # Create a copy of yahoo data
        result <- yahoo_data
        
        # Update the last row's Close price with real-time data
        if ("Close" %in% colnames(result)) {
          result[nrow(result), "Close"] <- current_price
        } else if (ncol(result) >= 4) {
          # If using OHLC format, update the Close column (typically 4th)
          result[nrow(result), 4] <- current_price
        }
        
        cat("[INFO] Updated with real-time price from Indian API:", current_price, "\n")
        return(result)
      }
    }
    
    # Return Yahoo data as-is if real-time update failed
    cat("[INFO] Using historical data from Yahoo Finance\n")
    return(yahoo_data)
    
  }, error = function(e) {
    # Fallback to Yahoo on error
    tryCatch({
      cat("[WARN] Indian API error:", as.character(conditionMessage(e)), "- trying Yahoo\n")
      return(fetch_equity_prices(paste0(gsub("\\.NS$", "", symbol), ".NS"), from, to))
    }, error = function(e2) {
      # If Yahoo also fails, provide helpful error
      stop(paste("Unable to fetch Indian stock data for", symbol,
                 "\nBoth Indian API and Yahoo Finance failed.",
                 "\n\nPossible causes:",
                 "1. Check internet connection",
                 "2. APIs may be temporarily unavailable",
                 "3. Firewall/proxy blocking connections",
                 "4. Try again in a few minutes",
                 sep = "\n"))
    })
  })
}

# Yahoo Finance (original - 15-20 min delay)
fetch_equity_prices <- function(symbol, from = Sys.Date() - 365, to = Sys.Date()) {
  tryCatch({
    # Try using quantmod first (most reliable)
    result <- suppressWarnings({
      getSymbols(
        Symbols = symbol,
        src = "yahoo",
        from = from,
        to = to,
        auto.assign = FALSE,
        warnings = FALSE
      )
    })
    
    # Validate result
    if (is.null(result) || nrow(result) == 0) {
      stop("No data from Yahoo Finance")
    }
    
    # Ensure proper column names
    if (ncol(result) < 4) {
      stop("Insufficient columns in data")
    }
    
    return(result)
  }, error = function(e) {
    # If quantmod fails, provide helpful error
    error_msg <- as.character(conditionMessage(e))
    
    if (grepl("401|403|blocked|Unauthorized", error_msg, ignore.case = TRUE)) {
      stop(paste("❌ Yahoo Finance is blocking this request.",
                 "\nTry Alpha Vantage or Finnhub instead (see dropdown menu).",
                 "\nOr wait 10 minutes and try again.",
                 sep = "\n"))
    }
    
    stop(paste("Unable to fetch data for", symbol,
               "\n\nError:", error_msg,
               "\n\nSolutions:",
               "1. Check internet connection",
               "2. Try a different data source (Alpha Vantage, Finnhub)",
               "3. Verify symbol is correct (e.g., AAPL, MSFT)",
               sep = "\n"))
  })
}

fetch_crypto_prices <- function(symbol = "bitcoin", vs_currency = "inr", days = 365) {
  tryCatch({
    # Simple CoinGecko integration (no key required)
    url <- sprintf(
      "https://api.coingecko.com/api/v3/coins/%s/market_chart",
      symbol
    )
    
    # Try with timeout and better error handling
    # On Windows, use relaxed SSL verification to avoid certificate issues
    ssl_config <- if (.Platform$OS.type == "windows") {
      httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE)
    } else {
      httr::config()
    }
    res <- httr::GET(
      url, 
      query = list(vs_currency = vs_currency, days = days),
      httr::timeout(60),  # Increased from 30 to 60 seconds
      httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"),
      httr::add_headers(
        "Accept-Language" = "en-US,en;q=0.9",
        "Accept-Encoding" = "gzip, deflate"
      ),
      ssl_config
    )
    
    # Check status
    if (httr::status_code(res) != 200) {
      stop(paste("CoinGecko API returned status", httr::status_code(res)))
    }
    
    # Parse JSON
    data <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
    prices <- data$prices
    
    if (is.null(prices) || (is.data.frame(prices) && nrow(prices) == 0) || 
        (is.matrix(prices) && nrow(prices) == 0)) {
      stop("No crypto price data returned from CoinGecko")
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
  }, error = function(e) {
    err_msg <- conditionMessage(e)
    stop(paste("CoinGecko API error for", symbol, ":", err_msg,
               "\n\nPossible causes:",
               "1. Check internet connection",
               "2. CoinGecko API may be temporarily unavailable",
               "3. Invalid crypto symbol (try: bitcoin, ethereum, etc.)",
               "4. Firewall/proxy blocking connection",
               sep = "\n"))
  })
}

# Get USD to INR exchange rate
get_usd_to_inr_rate <- function() {
  tryCatch({
    # Using CoinGecko's simple price API for USD/INR (using a stablecoin as proxy)
    # Or use a free forex API
    url <- "https://api.coingecko.com/api/v3/simple/price"
    # On Windows, use relaxed SSL verification to avoid certificate issues
    ssl_config <- if (.Platform$OS.type == "windows") {
      httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE)
    } else {
      httr::config()
    }
    res <- httr::GET(
      url, 
      query = list(ids = "tether", vs_currencies = "inr"),
      httr::timeout(30),  # Increased from 15 to 30 seconds
      httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"),
      httr::add_headers(
        "Accept-Language" = "en-US,en;q=0.9",
        "Cache-Control" = "no-cache"
      ),
      ssl_config
    )
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
      /* Modern Dark Theme with Glassmorphism */
      body { 
        background: #000000;
        background-attachment: fixed;
      }
      
      /* Animated Gradient Background */
      body::before {
        content: '';
        position: fixed;
        top: -50%;
        left: -50%;
        width: 200%;
        height: 200%;
        background: radial-gradient(circle, rgba(0,230,118,0.03) 0%, transparent 50%),
                    radial-gradient(circle at 80% 80%, rgba(0,176,255,0.03) 0%, transparent 50%);
        animation: gradientShift 15s ease infinite;
        pointer-events: none;
        z-index: 0;
      }
      
      @keyframes gradientShift {
        0%, 100% { transform: translate(0, 0) rotate(0deg); }
        50% { transform: translate(5%, 5%) rotate(5deg); }
      }
      
      /* Glass Cards with Liquid Effect */
      .card {
        background: rgba(20, 22, 31, 0.7);
        backdrop-filter: blur(20px) saturate(180%);
        -webkit-backdrop-filter: blur(20px) saturate(180%);
        border-radius: 20px;
        border: 1px solid rgba(255, 255, 255, 0.08);
        padding: 20px 24px;
        margin-bottom: 20px;
        box-shadow: 0 8px 32px rgba(0, 0, 0, 0.3),
                    inset 0 1px 0 rgba(255, 255, 255, 0.05);
        position: relative;
        overflow: hidden;
        transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
      }
      
      /* Liquid Glass Glow Effect */
      .card::before {
        content: '';
        position: absolute;
        top: -50%;
        left: -50%;
        width: 200%;
        height: 200%;
        background: radial-gradient(circle, rgba(0,230,118,0.1) 0%, transparent 70%);
        opacity: 0;
        transition: opacity 0.6s ease;
        pointer-events: none;
      }
      
      .card:hover::before {
        opacity: 1;
      }
      
      .card:hover {
        transform: translateY(-2px);
        border-color: rgba(0, 230, 118, 0.3);
        box-shadow: 0 12px 40px rgba(0, 230, 118, 0.15),
                    0 8px 32px rgba(0, 0, 0, 0.4),
                    inset 0 1px 0 rgba(255, 255, 255, 0.1);
      }
      
      .card h5 {
        margin-bottom: 16px;
        color: #ffffff;
        font-weight: 600;
        font-size: 1.1rem;
        text-shadow: 0 2px 8px rgba(0,230,118,0.3);
      }
      
      /* Metric Values with Glow */
      .metric-value {
        font-size: 2rem;
        font-weight: 700;
        background: linear-gradient(135deg, #ffffff 0%, #00E676 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
        line-height: 1.2;
        word-break: break-word;
        filter: drop-shadow(0 2px 8px rgba(0,230,118,0.3));
      }
      
      .metric-label {
        font-size: 0.9rem;
        text-transform: uppercase;
        letter-spacing: 0.15em;
        color: #9ea5b4;
        font-weight: 600;
        margin-bottom: 4px;
      }
      
      /* Glassmorphic Badges */
      .soft-badge {
        background: linear-gradient(135deg, rgba(0,230,118,0.15) 0%, rgba(0,176,255,0.15) 100%);
        backdrop-filter: blur(10px);
        border: 1px solid rgba(0,230,118,0.3);
        border-radius: 999px;
        padding: 6px 14px;
        font-size: 0.75rem;
        color: #00e676;
        font-weight: 600;
        letter-spacing: 0.05em;
        box-shadow: 0 4px 12px rgba(0,230,118,0.2),
                    inset 0 1px 0 rgba(255,255,255,0.1);
      }
      
      /* Modern Select Inputs with Glassmorphism */
      .form-select, .selectize-input, input[type='text'], input[type='number'] {
        background: rgba(31, 34, 48, 0.6) !important;
        backdrop-filter: blur(10px);
        border: 1px solid rgba(255, 255, 255, 0.1) !important;
        border-radius: 12px !important;
        color: #ffffff !important;
        padding: 10px 16px !important;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        box-shadow: inset 0 2px 8px rgba(0, 0, 0, 0.2);
      }
      
      .form-select:hover, .selectize-input:hover, input[type='text']:hover, input[type='number']:hover {
        border-color: rgba(0, 230, 118, 0.4) !important;
        background: rgba(31, 34, 48, 0.8) !important;
        box-shadow: 0 0 0 3px rgba(0, 230, 118, 0.1),
                    inset 0 2px 8px rgba(0, 0, 0, 0.2);
      }
      
      .form-select:focus, .selectize-input.focus, input:focus {
        border-color: rgba(0, 230, 118, 0.6) !important;
        background: rgba(31, 34, 48, 0.9) !important;
        box-shadow: 0 0 0 4px rgba(0, 230, 118, 0.15),
                    0 4px 16px rgba(0, 230, 118, 0.2),
                    inset 0 2px 8px rgba(0, 0, 0, 0.2) !important;
        outline: none !important;
      }
      
      /* Selectize Dropdown - Advanced Design */
      .selectize-dropdown {
        background: rgba(20, 22, 31, 0.95) !important;
        backdrop-filter: blur(20px) saturate(180%);
        border: 1px solid rgba(255, 255, 255, 0.1) !important;
        border-radius: 12px !important;
        box-shadow: 0 16px 48px rgba(0, 0, 0, 0.5),
                    0 0 0 1px rgba(0, 230, 118, 0.1) !important;
        margin-top: 8px;
        padding: 8px;
      }
      
      .selectize-dropdown-content {
        max-height: 300px;
        overflow-y: auto;
      }
      
      .selectize-dropdown-content::-webkit-scrollbar {
        width: 8px;
      }
      
      .selectize-dropdown-content::-webkit-scrollbar-track {
        background: rgba(255, 255, 255, 0.02);
        border-radius: 4px;
      }
      
      .selectize-dropdown-content::-webkit-scrollbar-thumb {
        background: linear-gradient(180deg, rgba(0,230,118,0.3), rgba(0,176,255,0.3));
        border-radius: 4px;
      }
      
      .selectize-dropdown .option {
        padding: 10px 14px;
        border-radius: 8px;
        margin: 2px 0;
        transition: all 0.2s ease;
        color: #9ea5b4;
      }
      
      .selectize-dropdown .option:hover,
      .selectize-dropdown .option.active {
        background: linear-gradient(135deg, rgba(0,230,118,0.15), rgba(0,176,255,0.1));
        color: #00E676 !important;
        transform: translateX(4px);
        box-shadow: 0 4px 12px rgba(0,230,118,0.2);
      }
      
      .selectize-dropdown .option.selected {
        background: rgba(0,230,118,0.1);
        color: #00E676;
      }
      
      /* Slider Inputs - Modern Glow */
      .irs {
        font-family: 'Inter', sans-serif;
      }
      
      .irs--shiny .irs-bar {
        background: linear-gradient(90deg, #00E676, #00B0FF) !important;
        border: none !important;
        box-shadow: 0 2px 8px rgba(0,230,118,0.4);
      }
      
      .irs--shiny .irs-handle {
        background: linear-gradient(135deg, #00E676, #00B0FF) !important;
        border: 2px solid rgba(255,255,255,0.3) !important;
        box-shadow: 0 4px 12px rgba(0,230,118,0.5),
                    0 0 0 4px rgba(0,230,118,0.1);
        transition: all 0.3s ease;
      }
      
      .irs--shiny .irs-handle:hover {
        transform: scale(1.1);
        box-shadow: 0 6px 16px rgba(0,230,118,0.6),
                    0 0 0 6px rgba(0,230,118,0.15);
      }
      
      .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
        background: linear-gradient(135deg, rgba(0,230,118,0.9), rgba(0,176,255,0.9)) !important;
        border-radius: 8px;
        padding: 4px 10px;
        font-weight: 600;
        box-shadow: 0 4px 12px rgba(0,230,118,0.3);
      }
      
      /* Checkbox - Liquid Glass Toggle */
      .form-check-input {
        width: 48px;
        height: 24px;
        background: rgba(31, 34, 48, 0.6);
        backdrop-filter: blur(10px);
        border: 1px solid rgba(255, 255, 255, 0.1);
        border-radius: 12px;
        cursor: pointer;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
      }
      
      .form-check-input:checked {
        background: linear-gradient(135deg, #00E676, #00B0FF);
        border-color: rgba(0, 230, 118, 0.5);
        box-shadow: 0 0 0 4px rgba(0, 230, 118, 0.15),
                    0 4px 12px rgba(0, 230, 118, 0.4),
                    inset 0 1px 0 rgba(255,255,255,0.2);
      }
      
      /* Tab Pills - Advanced Design */
      .nav-pills .nav-link {
        background: rgba(31, 34, 48, 0.4);
        backdrop-filter: blur(10px);
        border: 1px solid rgba(255, 255, 255, 0.05);
        border-radius: 12px;
        color: #9ea5b4;
        padding: 10px 20px;
        margin: 0 6px;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        font-weight: 500;
        position: relative;
        overflow: hidden;
      }
      
      .nav-pills .nav-link::before {
        content: '';
        position: absolute;
        top: 0;
        left: -100%;
        width: 100%;
        height: 100%;
        background: linear-gradient(90deg, transparent, rgba(0,230,118,0.1), transparent);
        transition: left 0.5s ease;
      }
      
      .nav-pills .nav-link:hover::before {
        left: 100%;
      }
      
      .nav-pills .nav-link:hover {
        background: rgba(31, 34, 48, 0.7);
        border-color: rgba(0, 230, 118, 0.3);
        color: #00E676;
        transform: translateY(-2px);
        box-shadow: 0 4px 12px rgba(0,230,118,0.2);
      }
      
      .nav-pills .nav-link.active {
        background: linear-gradient(135deg, rgba(0,230,118,0.2), rgba(0,176,255,0.15));
        border-color: rgba(0, 230, 118, 0.5);
        color: #00E676;
        box-shadow: 0 4px 16px rgba(0,230,118,0.3),
                    inset 0 1px 0 rgba(255,255,255,0.1);
        font-weight: 600;
      }
      
      /* Sidebar - Glass Effect */
      .sidebar {
        background: rgba(20, 22, 31, 0.8) !important;
        backdrop-filter: blur(20px) saturate(180%);
        border-right: 1px solid rgba(255, 255, 255, 0.08);
        box-shadow: 4px 0 24px rgba(0, 0, 0, 0.2);
      }
      
      /* Buttons with Liquid Glow */
      .btn, button {
        background: linear-gradient(135deg, rgba(0,230,118,0.15), rgba(0,176,255,0.15));
        backdrop-filter: blur(10px);
        border: 1px solid rgba(0,230,118,0.3);
        border-radius: 12px;
        color: #00E676;
        padding: 10px 24px;
        font-weight: 600;
        letter-spacing: 0.05em;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        position: relative;
        overflow: hidden;
      }
      
      .btn::before {
        content: '';
        position: absolute;
        top: 50%;
        left: 50%;
        width: 0;
        height: 0;
        border-radius: 50%;
        background: radial-gradient(circle, rgba(0,230,118,0.3), transparent);
        transform: translate(-50%, -50%);
        transition: width 0.6s ease, height 0.6s ease;
      }
      
      .btn:hover::before {
        width: 300px;
        height: 300px;
      }
      
      .btn:hover {
        background: linear-gradient(135deg, rgba(0,230,118,0.25), rgba(0,176,255,0.25));
        border-color: rgba(0,230,118,0.5);
        box-shadow: 0 0 0 4px rgba(0,230,118,0.1),
                    0 8px 24px rgba(0,230,118,0.3);
        transform: translateY(-2px);
      }
      
      .btn:active {
        transform: translateY(0) scale(0.98);
      }
      
      /* HR Dividers */
      hr {
        border-color: rgba(255, 255, 255, 0.08);
        opacity: 1;
      }
      
      /* Links */
      a {
        color: #00B0FF;
        text-decoration: none;
        transition: all 0.3s ease;
        position: relative;
      }
      
      a::after {
        content: '';
        position: absolute;
        bottom: -2px;
        left: 0;
        width: 0;
        height: 2px;
        background: linear-gradient(90deg, #00E676, #00B0FF);
        transition: width 0.3s ease;
      }
      
      a:hover::after {
        width: 100%;
      }
      
      a:hover {
        color: #00E676;
        text-shadow: 0 0 8px rgba(0,230,118,0.5);
      }
      
      /* Code blocks */
      code {
        background: rgba(0,230,118,0.1);
        border: 1px solid rgba(0,230,118,0.2);
        border-radius: 6px;
        padding: 2px 8px;
        color: #00E676;
        font-family: 'Fira Code', monospace;
      }
      
      /* Scrollbar Styling */
      ::-webkit-scrollbar {
        width: 10px;
        height: 10px;
      }
      
      ::-webkit-scrollbar-track {
        background: rgba(20, 22, 31, 0.5);
      }
      
      ::-webkit-scrollbar-thumb {
        background: linear-gradient(180deg, rgba(0,230,118,0.3), rgba(0,176,255,0.3));
        border-radius: 5px;
        border: 2px solid rgba(20, 22, 31, 0.5);
      }
      
      ::-webkit-scrollbar-thumb:hover {
        background: linear-gradient(180deg, rgba(0,230,118,0.5), rgba(0,176,255,0.5));
      }
      
      #tradingview_widget {
        background-color: rgba(20, 22, 31, 0.7);
        backdrop-filter: blur(20px);
        border-radius: 20px;
        border: 1px solid rgba(255, 255, 255, 0.08);
      }
      
      /* Smooth Animations */
      * {
        transition: background-color 0.3s ease, border-color 0.3s ease;
      }
    "))
  ),
  layout_sidebar(
    sidebar = sidebar(
      width = 280,
      open = "open",
      h4("Controls", style = "margin-bottom: 8px;"),
      tags$small(
        style = "color:#9ea5b4; font-size: 0.8rem;",
        "Configure assets & risk parameters"
      ),
      hr(style = "margin: 8px 0;"),
      selectInput(
        "asset_type",
        "Asset Type",
        choices = c(
          "US Equity" = "equity",
          "Indian Equity" = "indian_equity",
          "Crypto" = "crypto"
        )
      ),
      conditionalPanel(
        condition = "input.asset_type == 'equity'",
        selectInput(
          "data_source",
          "Data Source (Delay)",
          choices = c(
            "Finnhub (Real-time, FREE - Recommended)" = "finnhub",
            "Yahoo Finance (15-20 min delay)" = "yahoo"
          ),
          selected = "finnhub"
        ),
        conditionalPanel(
          condition = "input.data_source == 'finnhub'",
          tags$small(
            style = "color:#00E676;",
            "✓ Real-time prices (no delay) - FREE!",
            tags$br(),
            "Don't have API key? Get one at: ",
            tags$a(href = "https://finnhub.io/register", target = "_blank", "finnhub.io/register"),
            " (takes 2 minutes)",
            tags$br(),
            "Then set environment variable: ",
            tags$code("FINNHUB_API_KEY=your_key_here")
          )
        ),
        conditionalPanel(
          condition = "input.data_source == 'yahoo'",
          tags$small(
            style = "color:#FF9800;",
            "⚠ Yahoo Finance has 15-20 minute delay - use Finnhub for live prices"
          )
        )
      ),
      conditionalPanel(
        condition = "input.asset_type == 'indian_equity'",
        selectInput(
          "indian_data_source",
          "Data Source (Delay)",
          choices = c(
            "Indian API (Real-time, FREE - Recommended)" = "indianapi",
            "Yahoo Finance (15-20 min delay)" = "yahoo"
          ),
          selected = "indianapi"
        ),
        conditionalPanel(
          condition = "input.indian_data_source == 'indianapi'",
          tags$small(
            style = "color:#00E676;",
            "✓ Real-time NSE/BSE prices (no delay) - FREE!",
            tags$br(),
            "Works for all Indian stocks (TCS, RELIANCE, INFY, etc.)",
            tags$br(),
            "Note: Current price only - historical charts use Yahoo fallback"
          )
        ),
        conditionalPanel(
          condition = "input.indian_data_source == 'yahoo'",
          tags$small(
            style = "color:#FF9800;",
            "⚠ Yahoo Finance has 15-20 minute delay - use Indian API for live prices"
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
        "Lookback (days)",
        min = 60, max = 730, value = 365, step = 30
      ),
      sliderInput(
        "var_confidence",
        "VaR Confidence",
        min = 0.90, max = 0.99, value = 0.95, step = 0.01
      ),
      sliderInput(
        "horizon_days",
        "Risk Horizon (days)",
        min = 1, max = 10, value = 1
      ),
      sliderInput(
        "mc_sims",
        "MC Simulations",
        min = 1000, max = 20000, value = 5000, step = 1000
      ),
      sliderInput(
        "refresh_secs",
        "Refresh (sec)",
        min = 10, max = 300, value = 60, step = 10
      ),
      checkboxInput("auto_refresh", "Auto-Refresh", value = TRUE),
      hr(style = "margin: 8px 0;"),
      tags$div(
        style = "background: rgba(0, 230, 118, 0.08); border-left: 2px solid #00E676; padding: 6px 8px; margin-top: 8px; border-radius: 3px;",
        tags$strong(style = "color:#00E676; font-size: 0.8rem;", "💡 Real-time Data:"),
        tags$p(
          style = "color:#9ea5b4; font-size: 0.75rem; margin: 3px 0 0 0;",
          "Finnhub (FREE) for US stocks",
          tags$br(),
          tags$a(href = "https://finnhub.io/register", target = "_blank", style = "color:#00B0FF;", "Get API key")
        )
      ),
      tags$small(
        style = "color:#748094; margin-top: 8px; display: block; font-size: 0.7rem;",
        "Data: Yahoo Finance & CoinGecko APIs"
      )
    ),
    layout_columns(
      col_widths = c(12),
      div(
        class = "card",
        style = "padding: 16px 20px;",
        fluidRow(
          column(
            4,
            style = "padding: 10px 16px; border-right: 1px solid #1f2230;",
            div(class = "metric-label", style = "font-size: 0.8rem;", "Live Price (INR)"),
            div(textOutput("metric_price"), class = "metric-value", style = "margin: 8px 0; font-size: 1.6rem;"),
            tags$div(textOutput("metric_symbol"), style = "color:#9ea5b4; font-size:0.8rem; margin-top: 6px;"),
            tags$div(textOutput("metric_delay"), style = "color:#748094; font-size:0.7rem; margin-top: 3px; font-style: italic;")
          ),
          column(
            4,
            style = "padding: 10px 16px; border-right: 1px solid #1f2230;",
            div(class = "metric-label", style = "font-size: 0.8rem;", "Daily VaR"),
            div(textOutput("metric_var"), class = "metric-value", style = "margin: 8px 0; font-size: 1.6rem;"),
            tags$div(textOutput("metric_var_conf"), style = "color:#9ea5b4; font-size:0.8rem; margin-top: 6px;")
          ),
          column(
            4,
            style = "padding: 10px 16px;",
            div(class = "metric-label", style = "font-size: 0.8rem;", "Volatility (Ann.)"),
            div(textOutput("metric_vol"), class = "metric-value", style = "margin: 8px 0; font-size: 1.6rem;"),
            tags$div("Last lookback window", style = "color:#9ea5b4; font-size:0.8rem; margin-top: 6px;")
          )
        )
      ),
      tabsetPanel(
        type = "pills",
        tabPanel(
          "Price & Returns",
          div(
            class = "card",
            h5("Price History", style = "margin-bottom: 10px;"),
            plotlyOutput("price_plot", height = "280px")
          ),
          div(
            class = "card",
            h5("Daily Returns", style = "margin-bottom: 10px;"),
            plotlyOutput("returns_plot", height = "280px")
          )
        ),
        tabPanel(
          "VaR & Tail Risk",
          div(
            class = "card",
            h5("Historical VaR", style = "margin-bottom: 10px;"),
            plotlyOutput("var_histogram", height = "280px")
          ),
          div(
            class = "card",
            h5("Risk Summary", style = "margin-bottom: 10px;"),
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
            h5("Simulation Snapshot", style = "margin-bottom: 10px;"),
            plotlyOutput("mc_paths_plot", height = "250px")
          )
        ),
        tabPanel(
          "Real-Time Candlestick Chart",
          div(
            class = "card",
            h5("Live OHLCV Chart", style = "margin-bottom: 8px;"),
            p(
              style = "margin-bottom: 10px; font-size: 0.9rem;",
              "Real-time data from: ",
              tags$span(textOutput("chart_source"), style = "font-weight: bold; color: #00E676;")
            ),
            plotlyOutput("candlestick_chart", height = "500px")
          )
        ),
        tabPanel(
          "TradingView Chart",
          div(
            class = "card",
            style = "padding: 0; overflow: hidden;",
            div(
              id = "tradingview_widget",
              style = "height: 500px; width: 100%; position: relative;"
            ),
            div(
              id = "tradingview_iframe",
              style = "height: 500px; width: 100%; display: none;"
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
                  '&theme=dark&interval=D\" style=\"width: 100%; height: 500px; border: none;\" frameborder=\"0\"></iframe>';
                
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
                      height: 500,
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
        # Use selected data source - default to Yahoo
        data_source <- if (is.null(input$data_source)) "yahoo" else input$data_source
        
        if (data_source == "finnhub") {
          xts_obj <- fetch_equity_prices_finnhub(symbol, from, to)
        } else {
          # Yahoo Finance (most reliable for most users)
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
        shiny::validate(shiny::need(FALSE, paste("❌ Error fetching", symbol, "\n", err_msg)))
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
        # Indian stocks: Use selected data source - default to Indian API (real-time)
        data_source <- if (is.null(input$indian_data_source)) "indianapi" else input$indian_data_source
        
        if (data_source == "indianapi") {
          # Try free real-time API (indianapi.in) - provides current live prices
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
        shiny::validate(shiny::need(FALSE, paste("❌ Error fetching", symbol, "\n", err_msg)))
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
        # Truncate very long error messages for better UI display
        if (nchar(err_msg) > 200) {
          err_msg <- paste0(substr(err_msg, 1, 200), "...")
        }
        shiny::validate(shiny::need(FALSE, paste("❌ Error fetching", symbol, "\n", err_msg)))
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
      err <- as.character(conditionMessage(e))
      cat("[DEBUG] metric_symbol error:", err, "\n")
      as.character(paste("❌ Data Error:\n", err))
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

  # Real-time Candlestick Chart
  output$chart_source <- renderText({
    asset_type <- input$asset_type
    if (asset_type == "Equity (US)") {
      source <- input$data_source_us
      if (source == "finnhub") "Finnhub (Real-time)" else "Yahoo Finance (15-20 min delay)"
    } else if (asset_type == "Equity (India)") {
      source <- input$data_source_in
      if (source == "indianapi") "Indian API (Real-time)" else "Yahoo Finance (15-20 min delay)"
    } else {
      "CoinGecko API (<1 min delay)"
    }
  })

  output$candlestick_chart <- renderPlotly({
    px <- price_series()
    label <- attr(px, "asset_label", exact = TRUE)
    if (is.null(label)) label <- "Unknown"

    # Extract OHLCV data
    tryCatch({
      # Handle different data formats
      has_ohlc <- all(c("Open", "High", "Low", "Close") %in% colnames(px))
      
      if (has_ohlc) {
        df <- data.frame(
          date = index(px),
          open = as.numeric(Op(px)),
          high = as.numeric(Hi(px)),
          low = as.numeric(Lo(px)),
          close = as.numeric(Cl(px)),
          volume = if ("Volume" %in% colnames(px)) as.numeric(Vo(px)) else rep(NA, nrow(px))
        )
        
        # Show only last 60 days for better detail
        if (nrow(df) > 60) df <- tail(df, 60)
        
        # Determine color: green if close > open, red if close < open
        df$color <- ifelse(df$close >= df$open, "#00E676", "#FF5252")
        
        # Create candlestick chart
        plot_ly(df, x = ~date, open = ~open, high = ~high, low = ~low, close = ~close,
                type = "candlestick",
                increasing = list(line = list(color = "#00E676"), fillcolor = "#00E676"),
                decreasing = list(line = list(color = "#FF5252"), fillcolor = "#FF5252")) %>%
          layout(
            title = list(text = paste("Real-time Candlestick Chart -", label), 
                        font = list(color = "#ffffff", size = 14)),
            xaxis = list(title = "", gridcolor = "#1f2230", rangeslider = list(visible = FALSE)),
            yaxis = list(title = "Price (INR)", gridcolor = "#1f2230", tickformat = ",.0f"),
            margin = list(l = 50, r = 20, t = 40, b = 50),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)",
            font = list(color = "#ffffff"),
            hovermode = "x unified"
          )
      } else {
        # Fallback: show as line chart if OHLCV not available
        prices <- as.numeric(px[, 1])
        df <- data.frame(time = index(px), price = prices)
        
        plot_ly(df, x = ~time, y = ~price, type = "scatter", mode = "lines",
                line = list(color = "#00E676", width = 2),
                hovertemplate = "Date: %{x}<br>Price: ₹%{y:,.2f}<extra></extra>") %>%
          layout(
            title = list(text = paste("Price Chart -", label), 
                        font = list(color = "#ffffff", size = 14)),
            xaxis = list(title = "", gridcolor = "#1f2230"),
            yaxis = list(title = "Price (INR)", gridcolor = "#1f2230", tickformat = ",.0f"),
            margin = list(l = 50, r = 20, t = 40, b = 50),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)",
            font = list(color = "#ffffff")
          )
      }
    }, error = function(e) {
      cat("Candlestick chart error:", as.character(e), "\n")
      plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("Chart Error:", substr(as.character(e), 1, 100)),
                             textposition = "center",
                             showlegend = FALSE) %>%
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE),
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
    })
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


# Troubleshooting Guide - Stock Data Display Issues

## ✅ Recent Fixes Applied

Your app has been updated with the following improvements:

### 1. **Enhanced Error Messages**
- Full error messages are now displayed (no truncation) to help diagnose issues
- Better validation of data responses from APIs
- Detailed troubleshooting steps for each error type

### 2. **Improved Connection Handling**
- **Increased timeouts**: 30s → 60s for Yahoo Finance (more reliable for large date ranges)
- **Better HTTP headers**: Added real browser headers to avoid being blocked by anti-bot systems
- **Windows SSL compatibility**: Maintained SSL workarounds for Windows systems

### 3. **Robust CSV Parsing**
- Validates that required columns (Date, Close) exist before processing
- Detects HTML error responses from Yahoo Finance
- Better handling of empty or malformed responses
- Supports flexible OHLCV column availability

---

## 🔍 How to Diagnose Issues

### **Step 1: Check the Error Message**
When stock data fails to display, you'll now see:
```
❌ Error fetching AAPL
[Full error details]
```

Read the error carefully - it will tell you what went wrong:

| Error Message | Meaning | Solution |
|--------------|---------|----------|
| "HTTP Error: 403" | Yahoo Finance is blocking the request | Wait a few minutes, check firewall |
| "Empty response from Yahoo Finance" | API returned no data | Symbol may be invalid or delisted |
| "CSV parsing failed" | Response format changed | Try a different symbol first |
| "Connection timeout" | Network is too slow | Check internet speed, increase timeout |
| "SSL certificate error" | Windows certificate issue | Already handled by the app |

### **Step 2: Test with a Known Stock**
Try these reliable symbols to verify the app is working:
- **US Stocks**: `AAPL`, `MSFT`, `GOOGL` (mega-cap tech)
- **Indian Stocks**: `RELIANCE.NS`, `TCS.NS`, `INFY.NS` (popular blue chips)
- **Crypto**: `bitcoin`, `ethereum` (high liquidity)

If these work, your issue is with the specific symbol.
If these don't work, continue below.

### **Step 3: Check Your Internet Connection**
```powershell
# In PowerShell, test connectivity to Yahoo Finance:
Invoke-WebRequest -Uri "https://query1.finance.yahoo.com" -TimeoutSec 10
```

Should see: `StatusCode : 200`

### **Step 4: Run App with Debugging**
Start R/RStudio and run:
```r
# Enable verbose output
options(warn = 2)  # Convert warnings to errors
options(error = recover)  # Interactive debugging on error

# Load and run the app
shiny::runApp("app.R", host = "0.0.0.0", port = 3838)
```

When an error occurs, you'll get a debug prompt where you can:
- `ls()` - See available variables
- `print(err_msg)` - Print the full error message
- `c` - Continue execution

---

## 📊 Data Source Selection Guide

### **For US Stocks**
1. **Yahoo Finance (Default, 15-20 min delay)**
   - No API key needed
   - Reliable for all US equities and ETFs
   - Works on Windows out of the box

2. **Finnhub (Real-time, FREE)**
   - Get key: https://finnhub.io/register
   - Much faster than Yahoo (real-time vs 15-20 min)
   - 60 calls/minute on free tier (plenty for manual use)
   - Setup: See [SETUP_GUIDE.md](SETUP_GUIDE.md)

### **For Indian Stocks**
1. **Yahoo Finance (15-20 min delay)**
   - Add `.NS` suffix: `RELIANCE.NS`, `TCS.NS`
   - Reliable historical data
   - Free, no API key needed

2. **Indian API (Real-time, FREE)** ⚡
   - No API key needed
   - Live NSE/BSE prices
   - Better for current price viewing
   - May be slower due to rate limiting
   - See [INDIAN_STOCKS_REALTIME.md](INDIAN_STOCKS_REALTIME.md)

### **For Crypto**
- **CoinGecko (Free, no key, <1 min delay)**
  - Default and recommended
  - All major cryptocurrencies supported
  - Near real-time prices

---

## 🛠️ Common Issues & Solutions

### **Issue 1: "Symbol not found" or "No data returned"**

**Causes:**
- Typo in ticker symbol (case-sensitive for crypto)
- Stock symbol doesn't exist or is delisted
- Using NSE/BSE code instead of symbol

**Solutions:**
```r
# Verify symbol exists by testing with:
# US: Use Yahoo Finance lookup - https://finance.yahoo.com
# India: Use NSE website - https://www.nseindia.com
# Crypto: Use CoinGecko - https://www.coingecko.com

# Common mistakes:
"RIL" # Wrong - should be "RELIANCE.NS" for Yahoo
"AAPL.NS" # Wrong - not an Indian stock
"Bitcoin" # Wrong - should be "bitcoin" (lowercase)
"aapl" # Wrong - should be "AAPL" (uppercase for US stocks)
```

**For Indian stocks**, always use one of these formats:
- `RELIANCE.NS` (with .NS suffix)
- `TCS.NS`
- `INFY.NS`

### **Issue 2: "Connection timeout" or "Network errors"**

**Causes:**
- Firewall/proxy blocking API calls
- Very slow internet connection
- VPN causing issues

**Solutions:**
```powershell
# Test connection to APIs:
Invoke-WebRequest -Uri "https://query1.finance.yahoo.com" -TimeoutSec 10
Invoke-WebRequest -Uri "https://api.coingecko.com" -TimeoutSec 10
Invoke-WebRequest -Uri "https://indianapi.in" -TimeoutSec 10

# If any fail, your firewall/proxy is blocking it
# Solutions:
# 1. Check firewall - allow outbound HTTPS (443)
# 2. Disable VPN temporarily (for testing)
# 3. Check if corporate proxy needs configuration
```

### **Issue 3: "HTTP Error: 403" (Yahoo Finance blocking)**

**Causes:**
- Too many rapid requests from same IP
- Anti-bot protection triggered

**Solutions:**
- Wait 15-30 minutes before trying again
- Try a different stock to see if it's Yahoo-wide
- Use Finnhub API instead (doesn't have this issue)
- Check your network - if using public WiFi, it might be blocked

### **Issue 4: Data shows but prices are very high or very low**

**Possible causes:**
- Currency conversion is applied incorrectly
- Using wrong column (bid/ask instead of close)

**Current behavior:**
- All prices are automatically converted to INR:
  - US stocks: USD × (current USD→INR rate) = INR
  - Crypto: Already in INR from CoinGecko
  - Indian stocks: Already in INR

**If conversion looks wrong:**
- Check the USD→INR rate: `₹83` is ~correct (as of Jan 2026)
- Try without conversion by modifying app.R

---

## 🚀 Performance Optimization

### **If App is Slow:**

1. **Reduce lookback window:**
   - Default: 365 days
   - Try: 90 days for faster loading
   - Less data = faster processing

2. **Reduce Monte Carlo simulations:**
   - Default: 5,000 sims
   - Try: 1,000 for testing
   - Performance increases ~linearly with simulation count

3. **Disable auto-refresh:**
   - Turn off "Enable Auto-Refresh"
   - Or increase refresh interval from 60s to 300s

4. **Switch data source:**
   - Yahoo Finance might be slow
   - Try Finnhub for US stocks (faster API)

### **If Charts are Laggy:**

1. Reduce zoom/date range on TradingView
2. Try a different symbol with less historical data
3. Reduce number of Monte Carlo paths in simulation view

---

## 📝 Log Debugging (Windows)

### **Capture Full Error Logs:**

```powershell
# Run R with output to file
cd "C:\Users\ratho\Desktop\Financial Dashboard"
R --vanilla > app_log.txt 2>&1 <<EOF
library(shiny)
shiny::runApp("app.R", port = 3838)
EOF
```

Then share `app_log.txt` for analysis.

---

## ✅ Verification Checklist

- [ ] Internet connection works (`ping google.com`)
- [ ] Firewall allows HTTPS outbound (port 443)
- [ ] Using valid symbol for chosen data source
- [ ] No typos in ticker name (case matters!)
- [ ] For Indian stocks: Using `.NS` suffix or base name
- [ ] For crypto: Using lowercase symbol names
- [ ] Waited 5+ minutes if Yahoo error (rate limiting)
- [ ] Tried a known-good symbol (AAPL, RELIANCE.NS, bitcoin)

---

## 📞 Still Having Issues?

1. **Collect this information:**
   - What symbol are you trying?
   - What exact error message appears?
   - What's your operating system?
   - Are you using VPN/Proxy?
   - Do other apps on your PC access the internet normally?

2. **Test manually in R:**
   ```r
   library(httr)
   library(quantmod)
   
   # Test Yahoo Finance directly:
   res <- GET("https://query1.finance.yahoo.com/v7/finance/download/AAPL?period1=1609459200&period2=1641081600&interval=1d&events=history")
   print(status_code(res))  # Should be 200
   
   # Test CoinGecko:
   res <- GET("https://api.coingecko.com/api/v3/coins/bitcoin")
   print(status_code(res))  # Should be 200
   ```

3. **Check app.R error handling:**
   - All data fetches now include detailed error messages
   - Error output should guide you to the root cause

---

## 📚 Reference Links

- [Yahoo Finance Help](https://help.yahoo.com/)
- [Finnhub API Documentation](https://finnhub.io/docs/api)
- [Indian API Documentation](https://indianapi.in/documentation)
- [CoinGecko API Docs](https://www.coingecko.com/en/api/documentation)
- [NSE Symbol Lookup](https://www.nseindia.com/)


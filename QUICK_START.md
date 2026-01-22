# Quick Start: Finnhub API Setup

## 🎯 Goal: Get Real-Time US Stock Data (Instead of 15-20 min delay)

---

## Method 1: PowerShell (Easiest - Temporary)

1. **Get API Key:**
   - Go to: https://finnhub.io/register
   - Sign up (free, no credit card)
   - Copy your API key

2. **Open PowerShell** and run:
   ```powershell
   cd "C:\Users\ratho\Desktop\Financial Dashboard"
   $env:FINNHUB_API_KEY="paste_your_key_here"
   ```

3. **Start R/RStudio** and run:
   ```r
   shiny::runApp("app.R", host = "0.0.0.0", port = 3838)
   ```

4. **In Dashboard:**
   - Select "US Equity"
   - Select "Finnhub (Real-time, FREE API key)"
   - Enter ticker (e.g., AAPL)

✅ Done! You should see "Data delay: Real-time (Finnhub)"

---

## Method 2: System Environment (Permanent)

1. **Get API Key** (same as above)

2. **Set System Variable:**
   - Press `Win + R`
   - Type: `sysdm.cpl` → Enter
   - Click "Advanced" → "Environment Variables"
   - Under "User variables" → Click "New"
   - Name: `FINNHUB_API_KEY`
   - Value: Your API key
   - Click OK on all dialogs

3. **Restart R/RStudio**

4. **Run app and select Finnhub in dashboard**

---

## Method 3: R Console (Temporary)

```r
Sys.setenv(FINNHUB_API_KEY = "your_key_here")
shiny::runApp("app.R", host = "0.0.0.0", port = 3838)
```

---

## ⚠️ Important Notes

- **Finnhub ONLY works for US stocks** (AAPL, MSFT, TSLA, etc.)
- **Indian stocks** (RELIANCE.NS, TCS.NS) still use Yahoo Finance (15-20 min delay)
- **Crypto** already has <1 min delay (CoinGecko)

---

## Verify It's Working

Look for this text below the price:
- ✅ "Data delay: Real-time (Finnhub)" = Working!
- ❌ "Data delay: Real-time (Finnhub - API key needed)" = Key not set
- ❌ "Data delay: 15-20 minutes (Yahoo Finance)" = Using Yahoo (not Finnhub)

---

## Troubleshooting

**Key not working?**
```r
# Check if key is set:
Sys.getenv("FINNHUB_API_KEY")

# Should show your key, not empty string
```

**Still seeing delay?**
- Make sure you selected "Finnhub" in "Data Source" dropdown
- Restart R after setting system environment variables
- Check your API key is valid at https://finnhub.io/dashboard

---

For detailed instructions, see [SETUP_GUIDE.md](SETUP_GUIDE.md)

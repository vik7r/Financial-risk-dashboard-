# Step-by-Step Guide: Setting Up Finnhub API Key

## Why Use Finnhub?
- **Real-time data** for US stocks (vs 15-20 min delay with Yahoo Finance)
- **100% FREE** - No credit card required
- 60 API calls per minute on free tier
- Works immediately after setup

## Step 1: Get Your Free Finnhub API Key

1. **Open your web browser** and go to: https://finnhub.io/register
2. **Sign up** with your email address (or use Google/GitHub login)
3. **Verify your email** if required
4. **Log in** to your Finnhub account
5. **Go to your dashboard** - you'll see your API key displayed
6. **Copy your API key** - it looks like: `c1234567890abcdefghij`

## Step 2: Set the API Key as Environment Variable

### Option A: Windows PowerShell (Temporary - Only for current session)

1. **Open PowerShell** (Press `Win + X`, then select "Windows PowerShell" or "Terminal")
2. **Navigate to your project folder:**
   ```powershell
   cd "C:\Users\ratho\Desktop\Financial Dashboard"
   ```
3. **Set the environment variable:**
   ```powershell
   $env:FINNHUB_API_KEY="your_api_key_here"
   ```
   Replace `your_api_key_here` with the actual API key you copied from Finnhub.

4. **Verify it's set:**
   ```powershell
   echo $env:FINNHUB_API_KEY
   ```
   You should see your API key printed.

5. **Run your R Shiny app** from the same PowerShell window:
   ```r
   shiny::runApp("app.R", host = "0.0.0.0", port = 3838)
   ```

### Option B: Windows System Environment Variables (Permanent)

1. **Press `Win + R`** to open Run dialog
2. **Type:** `sysdm.cpl` and press Enter
3. **Click "Advanced" tab**
4. **Click "Environment Variables"** button
5. **Under "User variables"**, click **"New"**
6. **Variable name:** `FINNHUB_API_KEY`
7. **Variable value:** Paste your API key here
8. **Click "OK"** on all dialogs
9. **Restart your R/RStudio** for changes to take effect

### Option C: R/RStudio (Temporary - Only for current session)

1. **Open R or RStudio**
2. **Before running the app**, type in the console:
   ```r
   Sys.setenv(FINNHUB_API_KEY = "your_api_key_here")
   ```
   Replace `your_api_key_here` with your actual API key.

3. **Then run your app:**
   ```r
   shiny::runApp("app.R", host = "0.0.0.0", port = 3838)
   ```

## Step 3: Use Finnhub in the Dashboard

1. **Start your Shiny app** (using one of the methods above)
2. **Open the dashboard** in your browser (usually `http://localhost:3838`)
3. **Select "US Equity"** from the "Asset Type" dropdown
4. **In the "Data Source" dropdown**, select **"Finnhub (Real-time, FREE API key)"**
5. **Enter a US stock ticker** (e.g., AAPL, MSFT, TSLA)
6. **You should now see "Data delay: Real-time (Finnhub)"** below the price!

## Step 4: Verify It's Working

- Look at the price metric section - you should see:
  - **"Data delay: Real-time (Finnhub)"** if the API key is set correctly
  - **"Data delay: Real-time (Finnhub - API key needed)"** if the key is missing

## Troubleshooting

### "API key needed" message appears
- Make sure you set the environment variable correctly
- Restart R/RStudio after setting system environment variables
- Check that the variable name is exactly: `FINNHUB_API_KEY` (case-sensitive)
- Verify the API key is correct by running: `Sys.getenv("FINNHUB_API_KEY")` in R

### Still seeing 15-20 min delay
- Make sure you selected "Finnhub" in the "Data Source" dropdown
- Check that your API key is valid at https://finnhub.io/dashboard
- Verify the environment variable is set: `Sys.getenv("FINNHUB_API_KEY")`

### API rate limit errors
- Free tier: 60 calls per minute
- If you hit the limit, wait 1 minute and try again
- Consider reducing the auto-refresh interval

---

## Indian Equity - Real-Time Options

**Unfortunately, Finnhub does NOT support real-time Indian stocks.** They only provide real-time data for US markets.

### Current Options for Indian Stocks:

1. **Yahoo Finance (Default)**
   - Delay: 15-20 minutes
   - Free, no setup required
   - Works with all NSE stocks

2. **Free Indian Stock APIs (Limited Real-Time)**
   - Some community APIs exist but may still have delays
   - Most real-time Indian stock data requires paid subscriptions
   - NSE/BSE official feeds are commercial services

### For True Real-Time Indian Stocks:
You would need to use paid services like:
- **TrueData** (authorized NSE/BSE vendor)
- **INDstocks API** (WebSocket streaming)
- **NSE/BSE official data feeds** (commercial)

**Note:** The dashboard currently uses Yahoo Finance for Indian stocks, which has a 15-20 minute delay. This is the best free option available.

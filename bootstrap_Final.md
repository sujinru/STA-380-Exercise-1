    library(quantmod)
    library(mosaic)
    library(foreach)
    library(pander)
    library(ggplot2)

    mystocks <- c("SPY", "TLT", "LQD", "EEM", "VNQ")
    getSymbols(mystocks)

    ## [1] "SPY" "TLT" "LQD" "EEM" "VNQ"

    SPYa <- adjustOHLC(SPY)
    TLTa <- adjustOHLC(TLT)
    LQDa <- adjustOHLC(LQD)
    EEMa <- adjustOHLC(EEM)
    VNQa <- adjustOHLC(VNQ)
    myreturns <- cbind(ClCl(SPY),ClCl(TLT),ClCl(LQD),ClCl(EEM),ClCl(VNQ))
    myreturns <- as.matrix(na.omit(myreturns))

    #Find the mean and standard deviation for each asset
    asset_return <- sort(apply(myreturns, 2,mean))
    asset_risk <- sort(apply(myreturns, 2, sd))

### Expected Return for each asset

    pander(asset_return)

<table style="width:81%;">
<colgroup>
<col width="16%" />
<col width="16%" />
<col width="16%" />
<col width="15%" />
<col width="15%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">ClCl.LQD</th>
<th align="center">ClCl.TLT</th>
<th align="center">ClCl.EEM</th>
<th align="center">ClCl.VNQ</th>
<th align="center">ClCl.SPY</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">5.933e-05</td>
<td align="center">0.0001719</td>
<td align="center">0.0002506</td>
<td align="center">0.000264</td>
<td align="center">0.0002802</td>
</tr>
</tbody>
</table>

Now, we rank the five asset from lowest return to highest return based
on sample mean.

### Standard deviation of return for each asset

    pander(asset_risk)

<table style="width:76%;">
<colgroup>
<col width="15%" />
<col width="15%" />
<col width="15%" />
<col width="15%" />
<col width="15%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">ClCl.LQD</th>
<th align="center">ClCl.TLT</th>
<th align="center">ClCl.SPY</th>
<th align="center">ClCl.EEM</th>
<th align="center">ClCl.VNQ</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">0.005468</td>
<td align="center">0.009423</td>
<td align="center">0.01281</td>
<td align="center">0.02058</td>
<td align="center">0.02203</td>
</tr>
</tbody>
</table>

Here, we rank them from lowest risk to highest risk based on sample
standard deviations of the assets.

From the above tables, we can classify our assets into different
categories. Any assets below the 3rd rank will be given a score low.
Those above the third rank will be given a score high, and the middle
rank will be given a score medium.

**SPY** - High return/ Medium risk

**TLT** - Medium return/ Low risk

**LQD** - Low return/ Low risk

**EEM** - Low return/ High risk

**VNQ** - High return/ High risk

    plot(asset_return, asset_risk, pch = 19, cex = 3.5, col = "salmon", xlab = "Expected Return", ylab = "Risk(Standard Devation)",ylim = c(0,0.025), main = "Risk vs Return")
    text(asset_return[1],asset_risk[1]+0.002, "LQD")
    text(asset_return[2],asset_risk[2]+0.002, "TLT")
    text(asset_return[3],asset_risk[3]+0.002, "EEM")
    text(asset_return[4],asset_risk[4]+0.002, "VNQ")
    text(asset_return[5],asset_risk[5]+0.002, "SPY")

![](bootstrap_Final_files/figure-markdown_strict/unnamed-chunk-6-1.png)

    #Create a function to simulate 20 trading day
    sim_stock <- function(myreturns, investment, endperiod, weights, num_sim){
        sim_result <- foreach(i=1:num_sim, .combine='rbind') %do% {
            totalwealth <- investment #reset totalwealth for every simulation
            horizon <- endperiod
            holdings <- weights * totalwealth
            for (today in 1:horizon) {
                return_today <- resample(myreturns, 1, orig.ids = FALSE)
                holdings <- holdings + return_today * holdings
                totalwealth <- sum(holdings)
                holdings <- weights * totalwealth #end of day. redistribute the wealth
            }
            totalwealth
        }
    }

### Setting values for our simulation

We have 100,000 to invest, and we will do our simulation for 20 days.

    investment <- 100000
    endperiod <- 20

For each of the strategy, we will adjust the weight accordingly.

### Even Split Strategy

We will assign equal weights to all five assets

    set.seed(1)
    weights <- c(0.2,0.2,0.2,0.2,0.2)
    even_strategy <- sim_stock(myreturns, investment, endperiod, weights, 3000)
    names(weights) <- mystocks
    pander(weights)

<table style="width:42%;">
<colgroup>
<col width="8%" />
<col width="8%" />
<col width="8%" />
<col width="8%" />
<col width="8%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">SPY</th>
<th align="center">TLT</th>
<th align="center">LQD</th>
<th align="center">EEM</th>
<th align="center">VNQ</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">0.2</td>
<td align="center">0.2</td>
<td align="center">0.2</td>
<td align="center">0.2</td>
<td align="center">0.2</td>
</tr>
</tbody>
</table>

**Weight of each stock for the even split strategy**

### Safe Strategy

For this strategy, we will look at our classification of the five assets
and choose those with low risk properties. We will also include one
medium risk asset. For the weight, we will use 1/standard deviation as
the coefficients and normalize them to add up to 1. SPY, TLT, and LQD
are the three chosen assets.

    set.seed(1)
    spy_coef <- 1/asset_risk[3]
    tlt_coef <- 1/asset_risk[2]
    lqd_coef <- 1/ asset_risk[1]
    total_coef <- spy_coef + tlt_coef + lqd_coef
    spy_coef <- spy_coef/total_coef
    tlt_coef <- tlt_coef/total_coef
    lqd_coef <- lqd_coef/total_coef
    weights <- c(spy_coef, tlt_coef,lqd_coef, 0, 0)
    safe_strategy <- sim_stock(myreturns, investment, endperiod, weights, 3000)
    names(weights) <- mystocks
    pander(weights)

<table style="width:54%;">
<colgroup>
<col width="12%" />
<col width="12%" />
<col width="12%" />
<col width="8%" />
<col width="8%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">SPY</th>
<th align="center">TLT</th>
<th align="center">LQD</th>
<th align="center">EEM</th>
<th align="center">VNQ</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">0.2127</td>
<td align="center">0.2891</td>
<td align="center">0.4982</td>
<td align="center">0</td>
<td align="center">0</td>
</tr>
</tbody>
</table>

**Weight of each stock for the safe strategy**

### Aggressive Strategy

For this strategy, we will not be as diversified as the safe strategy.
Also, we will look mainly at assests which have high returns with
moderate to high risks. Coefficients will be adjusted based on the
expected return values. We will invest in SPY and VNQ

    set.seed(1)
    total_coef_a <- asset_return[4] + asset_return[5]
    spy_coef_a <- asset_return[4]/total_coef_a
    vnq_coef_a <- asset_return[5]/total_coef_a
    weights <- c(spy_coef_a, 0, 0, 0, vnq_coef_a)
    aggressive_strategy <- sim_stock(myreturns, investment, endperiod, weights, 3000)
    names(weights) <- mystocks
    pander(weights)

<table style="width:49%;">
<colgroup>
<col width="12%" />
<col width="8%" />
<col width="8%" />
<col width="8%" />
<col width="11%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">SPY</th>
<th align="center">TLT</th>
<th align="center">LQD</th>
<th align="center">EEM</th>
<th align="center">VNQ</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">0.4851</td>
<td align="center">0</td>
<td align="center">0</td>
<td align="center">0</td>
<td align="center">0.5149</td>
</tr>
</tbody>
</table>

**Weight of each stock for the aggressive strategy**

### Distribution of return values for even split strategy

    profit_split <- even_strategy - investment
    var05_split <- qdata(profit_split, 0.05)[2]
    hist(profit_split, 60,col = "cornsilk", xlab = "Return values", main = "Distribution of Return Values (Even Split Strategy)")
    abline(v = var05_split, col = "firebrick", lwd = 3)
    abline(v = mean(profit_split), col = "cyan4", lwd = 3)
    legend("topright", 
           c("5% value at risk", "expected return"), 
           lty=c(1, 1), 
           col=c("firebrick","cyan4"), 
           bty = "n")

![](bootstrap_Final_files/figure-markdown_strict/unnamed-chunk-12-1.png)

    var_mean_split <- c(var05_split, mean(profit_split),sd(profit_split))
    names(var_mean_split) <- c("Value at Risk at 5%", "Expected Return", "Standard Deviation of Return")
    pander(var_mean_split)

<table style="width:97%;">
<colgroup>
<col width="30%" />
<col width="25%" />
<col width="41%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Value at Risk at 5%</th>
<th align="center">Expected Return</th>
<th align="center">Standard Deviation of Return</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">-6560</td>
<td align="center">446.1</td>
<td align="center">4454</td>
</tr>
</tbody>
</table>

This shows us that if investors invest for 20 traiding days for this
portfolio, 5 percent of the time they will suffer a loss of 6600.
However, on average, they will receive around 491.

**Quantile Values**

    pander(quantile(profit_split))

<table style="width:54%;">
<colgroup>
<col width="12%" />
<col width="11%" />
<col width="11%" />
<col width="9%" />
<col width="9%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">0%</th>
<th align="center">25%</th>
<th align="center">50%</th>
<th align="center">75%</th>
<th align="center">100%</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">-13458</td>
<td align="center">-2480</td>
<td align="center">243.2</td>
<td align="center">3196</td>
<td align="center">19929</td>
</tr>
</tbody>
</table>

The table suggests that the return value in for 20 trading days can
range from a loss of 16958 to a gain of 21481.

### Distribution of return values for safe strategy

    profit_safe <- safe_strategy - investment
    var05_safe <- qdata(profit_safe, 0.05)[2]
    hist(profit_safe, 60,col = "cornsilk", xlab = "Return values", main = "Distribution of Return Values (Safe Strategy)")
    abline(v = var05_safe, col = "firebrick", lwd = 3)
    abline(v = mean(profit_safe), col = "cyan4", lwd = 3)
    legend("topright", 
           c("5% value at risk", "expected return"), 
           lty=c(1, 1), 
           col=c("firebrick","cyan4"), 
           bty = "n")

![](bootstrap_Final_files/figure-markdown_strict/unnamed-chunk-15-1.png)

    var_mean_safe <- c(var05_safe, mean(profit_safe), sd(profit_safe))
    names(var_mean_safe) <- c("Value at Risk at 5%", "Expected Return", "Standard Deviation of Return")
    pander(var_mean_safe)

<table style="width:97%;">
<colgroup>
<col width="30%" />
<col width="25%" />
<col width="41%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Value at Risk at 5%</th>
<th align="center">Expected Return</th>
<th align="center">Standard Deviation of Return</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">-3308</td>
<td align="center">213.5</td>
<td align="center">2175</td>
</tr>
</tbody>
</table>

This shows us that if investors invest for 20 traiding days for this
portfolio, 5 percent of the time they will suffer a loss of 3218.
However, on average, they will receive around 261.

**Quantile Values**

    pander(quantile(profit_safe))

<table style="width:53%;">
<colgroup>
<col width="11%" />
<col width="11%" />
<col width="11%" />
<col width="9%" />
<col width="9%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">0%</th>
<th align="center">25%</th>
<th align="center">50%</th>
<th align="center">75%</th>
<th align="center">100%</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">-8893</td>
<td align="center">-1147</td>
<td align="center">254.9</td>
<td align="center">1577</td>
<td align="center">9976</td>
</tr>
</tbody>
</table>

The table suggests that the return value in for 20 trading days can
range from a loss of 9046 to a gain of 8677.

### Distribution of return values for agressive strategy

    profit_aggressive <- aggressive_strategy - investment
    var05_aggressive <- qdata(profit_aggressive, 0.05)[2]
    hist(profit_aggressive, 60,col = "cornsilk", xlab = "Return values", main = "Distribution of Return Values (Aggressive Strategy)")
    abline(v = var05_aggressive, col = "firebrick", lwd = 3)
    abline(v = mean(profit_aggressive), col = "cyan4", lwd = 3)
    legend("topright", 
           c("5% value at risk", "expected return"), 
           lty=c(1, 1), 
           col=c("firebrick","cyan4"), 
           bty = "n")

![](bootstrap_Final_files/figure-markdown_strict/unnamed-chunk-18-1.png)

    var_mean_aggressive <- c(var05_aggressive, mean(profit_aggressive), sd(profit_aggressive))
    names(var_mean_aggressive) <- c("Value at Risk at 5%", "Expected Return", "Standard Deviation of Return")
    pander(var_mean_aggressive)

<table style="width:97%;">
<colgroup>
<col width="30%" />
<col width="25%" />
<col width="41%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Value at Risk at 5%</th>
<th align="center">Expected Return</th>
<th align="center">Standard Deviation of Return</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">-11025</td>
<td align="center">647.4</td>
<td align="center">7509</td>
</tr>
</tbody>
</table>

This shows us that if investors invest for 20 traiding days for this
portfolio, 5 percent of the time they will suffer a loss of 11065.
However, on average, they will receive around 724.

**Quantile Values**

    pander(quantile(profit_aggressive))

<table style="width:51%;">
<colgroup>
<col width="12%" />
<col width="11%" />
<col width="8%" />
<col width="9%" />
<col width="9%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">0%</th>
<th align="center">25%</th>
<th align="center">50%</th>
<th align="center">75%</th>
<th align="center">100%</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">-24004</td>
<td align="center">-4314</td>
<td align="center">320</td>
<td align="center">5020</td>
<td align="center">36846</td>
</tr>
</tbody>
</table>

The table suggests that the return value in for 20 trading days can
range from a loss of 26063 to a gain of 37979.

### Summary

    final_table <- data.frame(rbind(var_mean_split, var_mean_safe, var_mean_aggressive))
    rownames(final_table) <- c("Split Strategy","Safe Strategy", "Aggressive Strategy")
    colnames(final_table) <- c("Value at Risk at 5%", "Expected Return", "Standard Deviation of Return")
    pander(final_table)

<table style="width:90%;">
<caption>Table continues below</caption>
<colgroup>
<col width="36%" />
<col width="30%" />
<col width="23%" />
</colgroup>
<thead>
<tr class="header">
<th align="center"> </th>
<th align="center">Value at Risk at 5%</th>
<th align="center">Expected Return</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center"><strong>Split Strategy</strong></td>
<td align="center">-6560</td>
<td align="center">446.1</td>
</tr>
<tr class="even">
<td align="center"><strong>Safe Strategy</strong></td>
<td align="center">-3308</td>
<td align="center">213.5</td>
</tr>
<tr class="odd">
<td align="center"><strong>Aggressive Strategy</strong></td>
<td align="center">-11025</td>
<td align="center">647.4</td>
</tr>
</tbody>
</table>

<table style="width:78%;">
<colgroup>
<col width="36%" />
<col width="41%" />
</colgroup>
<thead>
<tr class="header">
<th align="center"> </th>
<th align="center">Standard Deviation of Return</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center"><strong>Split Strategy</strong></td>
<td align="center">4454</td>
</tr>
<tr class="even">
<td align="center"><strong>Safe Strategy</strong></td>
<td align="center">2175</td>
</tr>
<tr class="odd">
<td align="center"><strong>Aggressive Strategy</strong></td>
<td align="center">7509</td>
</tr>
</tbody>
</table>
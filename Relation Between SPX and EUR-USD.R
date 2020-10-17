library(quantmod)
library(dplyr)

Start_D = as.Date('2010-01-01')
End_D = as.Date('2018-08-01')

suppressWarnings({
	getSymbols('^GSPC', return.class = 'zoo', from = Start_D, to = End_D); SPX = GSPC[, 4]
	EUR_USD = get(getSymbols('EURUSD=X', return.class = 'zoo', from = Start_D, to = End_D))[, 4]
})

head(SPX); head(EUR_USD)

cor(merge(rollapply(diff(log(SPX[!is.na(SPX)])), width = 252, FUN = sd, fill = NA, align = 'r') %>% .[!is.na(.)],
			rollapply(diff(log(EUR_USD[!is.na(EUR_USD)])), width = 252, FUN = sd, fill = NA, align = 'r') %>% .[!is.na(.)], all = FALSE)) %>%
	`colnames<-`(c('SPX_SD', 'EURUSD_SD')) %>%
	`rownames<-`(c('SPX_SD', 'EURUSD_SD'))

### http://www.cboe.com/products/stock-index-options-spx-rut-msci-ftse/s-p-500-index-options/s-p-500-index/price-charts-on-spx
### https://www.marketwatch.com/investing/index/spx/historical
### 

## https://www.tizianobellini.com/copy-of-chapter-2-4

Source = "/Users/arunchaitali/Dropbox/ARUN & CHAITALI/Quant-Understanding/Data"

library(caret)
library(dplyr)
library(vars)
library(smbinning)
library(LDPD) ### Low Default Probability

{	### Page : 51
	Get_Data =  read.csv(paste(Source, "/chap2oneypd.csv", sep = "")) %>%
				mutate_at(., vars(contains('date')), funs(as.Date)) %>%
				mutate(max_arrears_12m = round(max_arrears_12m, 4), arrears_months = round(arrears_months, 4)) %>%
				mutate(default_event = if_else(arrears_event == 1 | term_expiry_event == 1 | bankrupt_event == 1, 
												1,
												0)) %>% ### Define the Default
				mutate(default_flag = dplyr::if_else(default_event == 1, 0, 1))
		print(head(Get_Data))

	Train_Index = caret::createDataPartition(Get_Data$default_event, p = .7, list = FALSE)
		Train_Data = Get_Data[Train_Index, ]
		Test_Data  = Get_Data[-Train_Index, ]
}

{	### Page : 52
	InformationValue_Analysis = smbinning.sumiv(df = Train_Data, y = "default_flag")
		# -->> It gives the user the ability to calculate, in one step, the IV for each characteristic of the dataset.

		# http://r-statistics.co/Information-Value-With-R.html 

		# Information Value (IV) is a measure of the [[predictive capability of a categorical x]] variable to accurately predict the goods and bads. 
		# For each category of [[x]], information value is computed as:

		# 	IV = ([[%]] good of all goods−perc bad of all bads) * WOE
		# 	WOE = log([[%]] good of all goods / [[%]] bad of all bads)

		# 	The total IV of a variable is the sum of IV’s of its categories. Here is what the values of IV mean according to Siddiqi (2006):

		# 		Less than 0.02, then the predictor is not useful for modeling (separating the Goods from the Bads)
		# 		0.02 to 0.1, then the predictor has only a weak relationship.
		# 		0.1 to 0.3, then the predictor has a medium strength relationship.
		# 		0.3 or higher, then the predictor has a strong relationship.
}

{	### Page : 80 | EXAMPLE 2.5.1
	Portfolio_Data = data.frame(Rating = LETTERS[1:5], `Number of Accounts` = c(10, 40, 25, 15, 10), `Number of Defaults` = c(2, 1, 0, 0, 0), check.names = FALSE)
		print(Portfolio_Data)

	set.seed(123)

	PD_1_Point = PTOnePeriodPD(Portfolio_Data[, 'Number of Accounts'], Portfolio_Data[, 'Number of Defaults'], conf.interval = 0.90)
		### Pluto Tasche Approach
		### VERY SENSITIVE to 'conf.interval'
		print(PD_1_Point)

	PD_1_Point_CAP_Nethod = VDBCalibratePD(portf.uncond = Portfolio_Data[, 'Number of Accounts'], pd.uncond.old = 0.03, pd.uncond.new = 0.03, AR = 0.90, 
									rating.type = 'RATING')
		print(PD_1_Point_CAP_Nethod)
}
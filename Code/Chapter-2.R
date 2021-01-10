## https://www.tizianobellini.com/copy-of-chapter-2-4

Source = "/Users/arunchaitali/Dropbox/ARUN & CHAITALI/Quant-Understanding/Data"

library(caret)
library(dplyr)
library(vars)
library(smbinning)
library(LDPD)    ### Low Default Probability
library(optiRum) ### Model Validations
library(pROC)    ### ROC -> Receiver Operating Characteristic

{	### Page : 51
	Get_Data =  read.csv(paste(Source, "/chap2oneypd.csv", sep = "")) %>%
				mutate_at(., vars(contains('date')), funs(as.Date)) %>%
				mutate(max_arrears_12m = round(max_arrears_12m, 4), arrears_months = round(arrears_months, 4)) %>%
				mutate(default_event = if_else(arrears_event == 1 | term_expiry_event == 1 | bankrupt_event == 1, 
												1,
												0)) %>% ### Define the Default
				mutate(default_flag = dplyr::if_else(default_event == 1, 0, 1))
		print(head(Get_Data))

	{	### Page : 54 // Bining
		Get_Data = Get_Data %>%
					mutate(woe_bureau_score = rep(NA, length(bureau_score))) %>%
						mutate(woe_bureau_score = ifelse(bureau_score <= 308, 
													-0.7994, 
													woe_bureau_score)) %>%
						mutate(woe_bureau_score = ifelse(bureau_score > 308 & bureau_score <= 404, 
													-0.0545, 
													woe_bureau_score)) %>%
						mutate(woe_bureau_score = ifelse(bureau_score > 404 & bureau_score <= 483, 
													0.7722, 
													woe_bureau_score)) %>%
						mutate(woe_bureau_score = ifelse(bureau_score > 483, 
													1.0375, 
													woe_bureau_score)) %>%
					mutate(woe_bureau_score = ifelse(is.na(woe_bureau_score), -0.0910, woe_bureau_score)) %>%

					mutate(woe_cc_util = rep(NA, length(cc_util))) %>%
						mutate(woe_cc_util = ifelse(cc_util <= 0.55, 
													1.8323, 
													woe_cc_util)) %>%
						mutate(woe_cc_util = ifelse(cc_util > 0.55 & cc_util <= 0.70, 
													-0.4867, 
													woe_cc_util)) %>%
						mutate(woe_cc_util = ifelse(cc_util > 0.70 & cc_util <= 0.85, 
													-1.1623, 
													woe_cc_util)) %>%
						mutate(woe_cc_util = ifelse(cc_util > 0.85, 
													-2.3562, 
													woe_cc_util)) %>%
					mutate(woe_cc_util = ifelse(is.na(woe_cc_util), 0, woe_cc_util)) %>%

					mutate(woe_num_ccj = rep(NA, length(num_ccj))) %>%
						mutate(woe_num_ccj = ifelse(num_ccj <= 0, 
													0.1877, 
													woe_num_ccj)) %>%
						mutate(woe_num_ccj = ifelse(num_ccj > 0 & num_ccj <= 1, 
													-0.9166, 
													woe_num_ccj)) %>%
						mutate(woe_num_ccj = ifelse(num_ccj > 1, 
													-1.1322, 
													woe_num_ccj)) %>%
					mutate(woe_num_ccj = ifelse(is.na(woe_num_ccj), -0.0910, woe_num_ccj)) %>%

					mutate(woe_max_arrears_12m = rep(NA, length(max_arrears_12m))) %>%
						mutate(woe_max_arrears_12m = ifelse(max_arrears_12m <= 0, 
													0.7027, 
													woe_max_arrears_12m)) %>%
						mutate(woe_max_arrears_12m = ifelse(max_arrears_12m > 0 & max_arrears_12m <= 1, 
													-0.8291, 
													woe_max_arrears_12m)) %>%
						mutate(woe_max_arrears_12m = ifelse(max_arrears_12m > 1 & max_arrears_12m <= 1.4, 
													-1.1908, 
													woe_max_arrears_12m)) %>%
						mutate(woe_max_arrears_12m = ifelse(max_arrears_12m > 1.4, 
													-2.2223, 
													woe_max_arrears_12m)) %>%
					mutate(woe_max_arrears_12m = ifelse(is.na(woe_max_arrears_12m), 0, woe_max_arrears_12m)) %>%

					mutate(woe_max_arrears_bal_6m = rep(NA, length(max_arrears_bal_6m))) %>%
						mutate(woe_max_arrears_bal_6m = ifelse(max_arrears_bal_6m <= 0, 
													0.5771, 
													woe_max_arrears_bal_6m)) %>%
						mutate(woe_max_arrears_bal_6m = ifelse(max_arrears_bal_6m > 0 & max_arrears_bal_6m <= 300, 
													-0.7818, 
													woe_max_arrears_bal_6m)) %>%
						mutate(woe_max_arrears_bal_6m = ifelse(max_arrears_bal_6m > 300 & max_arrears_bal_6m <= 600, 
													-1.2958, 
													woe_max_arrears_bal_6m)) %>%
						mutate(woe_max_arrears_bal_6m = ifelse(max_arrears_bal_6m > 600 & max_arrears_bal_6m <= 900, 
													-1.5753, 
													woe_max_arrears_bal_6m)) %>%
						mutate(woe_max_arrears_bal_6m = ifelse(max_arrears_bal_6m > 900, 
													-2.2110, 
													woe_max_arrears_bal_6m)) %>%
					mutate(woe_max_arrears_bal_6m = ifelse(is.na(woe_max_arrears_bal_6m), 0, woe_max_arrears_bal_6m)) %>%

					mutate(woe_emp_length = rep(NA, length(emp_length))) %>%
						mutate(woe_emp_length = ifelse(emp_length <= 2, 
													-0.7514, 
													woe_emp_length)) %>%
						mutate(woe_emp_length = ifelse(emp_length > 2 & emp_length <= 4, 
													-0.3695, 
													woe_emp_length)) %>%
						mutate(woe_emp_length = ifelse(emp_length > 4 & emp_length <= 7, 
													0.1783, 
													woe_emp_length)) %>%
						mutate(woe_emp_length = ifelse(emp_length > 7, 
													0.5827, 
													woe_emp_length)) %>%
					mutate(woe_emp_length = ifelse(is.na(woe_emp_length), 0, woe_emp_length)) %>%

					mutate(woe_months_since_recent_cc_delinq = rep(NA, length(months_since_recent_cc_delinq))) %>%
						mutate(woe_months_since_recent_cc_delinq = ifelse(months_since_recent_cc_delinq <= 6, 
													-0.4176, 
													woe_months_since_recent_cc_delinq)) %>%
						mutate(woe_months_since_recent_cc_delinq = ifelse(months_since_recent_cc_delinq > 6 & months_since_recent_cc_delinq <= 11, 
													-0.1942, 
													woe_months_since_recent_cc_delinq)) %>%
						mutate(woe_months_since_recent_cc_delinq = ifelse(months_since_recent_cc_delinq > 11, 
													1.3166, 
													woe_months_since_recent_cc_delinq)) %>%
					mutate(woe_months_since_recent_cc_delinq = ifelse(is.na(woe_months_since_recent_cc_delinq), 0, woe_months_since_recent_cc_delinq)) %>%

					mutate(woe_annual_income = rep(NA, length(annual_income))) %>%
						mutate(woe_annual_income = ifelse(annual_income <= 35064, 
													-1.8243, 
													woe_annual_income)) %>%
						mutate(woe_annual_income = ifelse(annual_income > 35064 & annual_income <= 41999, 
													-0.8272, 
													woe_annual_income)) %>%
						mutate(woe_annual_income = ifelse(annual_income > 41999 & annual_income <= 50111, 
													-0.3294, 
													woe_annual_income)) %>%
						mutate(woe_annual_income = ifelse(annual_income > 50111 & annual_income <= 65050, 
													0.2379, 
													woe_annual_income)) %>%
						mutate(woe_annual_income = ifelse(annual_income > 65050, 
													0.6234, 
													woe_annual_income)) %>%
					mutate(woe_annual_income = ifelse(is.na(woe_annual_income), 0, woe_annual_income))
	}	

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

{	### Page : 54 & Page : 55
	woe_Variables = Train_Data %>% dplyr::select(starts_with("woe"))
	woe_corr = cor(as.matrix(woe_Variables), method = 'spearman'); print(woe_corr)

	{  ### Fit GLM for the Data
		logit_Stepwise = stepAIC(glm(default_event ~ woe_bureau_score + woe_annual_income+woe_emp_length + woe_max_arrears_12m + 
											woe_months_since_recent_cc_delinq + woe_num_ccj + woe_cc_util,
								family = binomial(link = 'logit'), 
								data = Train_Data), 
							k = qchisq(0.05, 1, lower.tail = FALSE), 
							direction = 'both')
			print(summary(logit_Stepwise))
	}

	Train_Data = Train_Data %>% mutate(predict_logit = predict(logit_Stepwise, newdata = Train_Data, type = 'response'),
								sample = 'Train')
	Test_Data =  Test_Data  %>% mutate(predict_logit = predict(logit_Stepwise, newdata = Test_Data, type = 'response'),
								sample = 'Test')
		Train_Test_Data = rbind(Train_Data, Test_Data)

	Scale_Score_Fn = 
		Vectorize(function(Prob, Odd, Base_Score, Base_Score_Increase) {
						D_Val = Base_Score_Increase / log(2)
						C_Val = Base_Score - D_Val * log(Odd)

						Scales_Score = C_Val + D_Val * log((1 - Prob) / Prob)
						return(Scales_Score)
					})

	{  ### Fit FINAL-GLM for the Data
		logit_Full_Final = glm(default_event ~ score,
							family = binomial(link = 'logit'), 
							data = Train_Test_Data %>% mutate(score = Scale_Score_Fn(predict_logit, 72, 660, 40)))
			print(summary(logit_Full_Final))

		Train_Test_Data = Train_Test_Data %>% mutate(pd = predict(logit_Full_Final, 
														newdata = Train_Test_Data %>% mutate(score = Scale_Score_Fn(predict_logit, 72, 660, 40)), 
														type = 'response'))
	}
}

{	### Page : 59 // Model Validations
	GINI_Train = optiRum::giniCoef(Train_Data$predict_logit, Train_Data$default_event); print(GINI_Train) ### Gini Coef.
	plot(roc(Train_Data$default_event, Train_Data$predict_logit, direction = "<"), 
		col = "blue", lwd = 3, main = "ROC Curve")  ### ROC Curve
}

{	### Page : 61 // Compare : Actual vs. Fitted PDs
	Score_Cust = smbinning.custom(Train_Test_Data %>% mutate(score = Scale_Score_Fn(predict_logit, 72, 660, 40)), 
							y = 'default_flag', 
							x = 'score', 
							cuts= c(517, 576, 605, 632, 667, 716, 746, 773))
	Score_Data = smbinning.gen(Train_Test_Data %>% mutate(score = Scale_Score_Fn(predict_logit, 72, 660, 40)), 
							Score_Cust,
							chrname = 'score_band')  ### Group by bands
	Data_PD = Score_Data %>% dplyr::select(score, score_band, pd, default_event) %>%
				dplyr::group_by(score_band) %>%
				dplyr::summarise(mean_dr = round(mean(default_event), 4), mean_pd = round(mean(pd), 4))

		RMSE = sqrt(mean((Data_PD$mean_dr - Data_PD$mean_pd)^2)); print(RMSE)
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
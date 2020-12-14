## https://www.tizianobellini.com/copy-of-chapter-2-4

Source = "/Users/arunchaitali/Dropbox/ARUN & CHAITALI/Quant-Understanding/Data"

library(caret)
library(dplyr)
library(vars)
library(OIsurv)  ### For Survival Analysis

{	### Page : 106
	Get_Data =  read.csv(paste(Source, "/chap3ltpdpanel.csv", sep = ""))
		print(head(Get_Data))

	Data = dplyr::mutate_at(Get_Data, vars(contains('date')), funs((as.Date(., format = '%Y-%m-%d'))))
		print(head(Data))
		### 1) 'tob'                   : Number of Qs Since Origination
		### 2) 'start_time / end_time' : Represent : The window to include into ***CPH function*** to fit the model

	Default_Rate_Q = Data %>%
					dplyr::group_by(report_date, year) %>%
					dplyr::summarise(dr_QoQ = mean(default_flag)) %>%
					dplyr::select(report_date, year, dr_QoQ)
		print(head(Default_Rate_Q))
}

{	### Page : 107
	Data_Subset = Data %>%
				dplyr::select(id, default_ind) %>%
				dplyr::distinct()

	Data_Default_Mismatch = Data[Data[, 'default_ind'] != Data[, 'default_flag'], , drop = FALSE]  ### [Q] WHY, They are Diff. ???
		print(head(Data_Default_Mismatch, 20))

	set.seed(2122)
		Sample_ID = caret::createDataPartition(Data_Subset$default_ind, p = 0.7, list = FALSE)
			Data_Subset_Train = Data_Subset[Sample_ID,  , drop = FALSE]
			Data_Subset_Test  = Data_Subset[-Sample_ID, , drop = FALSE]

			Data_Subset_Train$sample = 'train'
			Data_Subset_Test$sample  = 'test'
			Data_Subset_All = rbind(Data_Subset_Train, Data_Subset_Test) %>%
								dplyr::select(id, sample)

	Data = dplyr::left_join(Data, Data_Subset_All, by = 'id')
		Data$sample = as.factor(Data$sample)
}

{	### Page : 111
	Get_Data_MV_Q = read.csv(paste(Source, "/chap3drts.csv", sep = "")) %>% 
						mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
						as.data.frame()
		print(head(Get_Data_MV_Q))

	Default_Rate_MV_Q = as.matrix(Get_Data_MV_Q[5:nrow(Get_Data_MV_Q), 2])
	GDP_Lag_MV_Q = as.matrix(Get_Data_MV_Q[1:(nrow(Get_Data_MV_Q) - 4), 3])
	Unepm_Rate_MV_Q = as.matrix(Get_Data_MV_Q[5:nrow(Get_Data_MV_Q), 4])

	MV_Data_Final = as.data.frame(cbind(Default_Rate_MV_Q, GDP_Lag_MV_Q, Unepm_Rate_MV_Q)/100) %>%
					`colnames<-`(c('dr_tt', 'gdp_ttlag', 'uer_tt'))

	Model_Fit = lm(dr_tt ~ gdp_ttlag + uer_tt, data = MV_Data_Final)
	Default_Rate_Predict = predict(Model_Fit, MV_Data_Final)
}

{	### Page : 116 & 117 & 119
	Train_Data = Data %>% dplyr::filter(sample == 'train')
	Test_Data  = Data %>% dplyr::filter(sample == 'test')

	quantile(Train_Data$mob, prob = seq(0, 1, length = 11))
	quantile(Train_Data$mob)
	quantile(Train_Data$remaining_term, prob = seq(0, 1, length = 11))
	quantile(Train_Data$remaining_term)

	Train_Data$default_event = dplyr::if_else(Train_Data$default_flag == 1, 0, 1)
	Train_Data$seasoning     = dplyr::if_else(Train_Data$remaining_term > 36, 1, 0)

	Logit_Model = glm(default_flag ~ seasoning + hpi, family = binomial(link = 'logit'), data = Train_Data)
		print(summary(Logit_Model))

	Train_Data$predict_logit <- round(predict(Logit_Model, newdata = Train_Data, type = 'response'), 7)
		Pred_Train_Data = ROCR::prediction(Train_Data$predict_logit, Train_Data$default_flag)
		Pref_Train_Data = ROCR::performance(Pred_Train_Data, 'tpr', 'fpr')
		print(ROCR::performance(Pred_Train_Data, 'auc'))
}

{	### Page : 120 (EXAMPLE 3.3.5)
	Train_Data1 = Train_Data %>% dplyr::group_by(report_date, year) %>%
								dplyr::summarise(default_rate = mean(default_flag), pd = mean(predict_logit)) %>%
								dplyr::select(report_date, year, default_rate, pd)
		print(head(Train_Data1))
		print(cor(Train_Data1$default_rate, Train_Data1$pd))
}

##### Calculations on : The SURVIVAL ANALYSIS

{	### Page : 123 (EXAMPLE 3.4.1)
	Data_Survival = data.frame(`ACC ID`          = 1:12,
							Time           = c(3, 3, 6, 5, 1, 7, 3, 4, 2, 6, 8, 8),
							`Default Flag` = c(0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0),
							check.names    = FALSE)
		print(Data_Survival)
	
	Survival_Calc = 
		Reduce('rbind', lapply(split(Data_Survival, as.character(Data_Survival[, 'Time'])),
						function(Time_Data) {
							Time = Time_Data[1, 'Time']
							N_j = sum(Data_Survival[, 'Time'] >= Time)  ### This is IMP. Calculation
							D_j = sum(Time_Data[, 'Default Flag'])
							
							return(data.frame(Time = Time, 
											`ACC ID` = paste(Time_Data[, 'ACC ID'], collapse = ", "),
											N_j = N_j, D_j = D_j, S_j = NA, check.names = FALSE))
						})) %>%
			mutate(S_j = cumprod(1 - .[, 'D_j'] / .[, 'N_j']))
		print(Survival_Calc)
}

{	### Page : 125
	Data_Survival = Data_Survival

	Survival_Model = survfit(Surv(Data_Survival$Time, Data_Survival$'Default Flag') ~ 1)
		summary(Survival_Model)
}

{	### Page : 126 (EXAMPLE 3.4.3) & Page : 129 (EXAMPLE 3.4.4) 
	Data_Survival = read.csv(paste(Source, "/chap3coxphx.csv", sep = ""))
		print(Data_Survival)

	Survival_Model_KM = survfit(Surv(Data_Survival$Time, Data_Survival$Status) ~ Data_Survival$Product)  ### Just like Before
		summary(Survival_Model_KM)

	Survival_Model_CPH = coxph(Surv(Data_Survival$Time, Data_Survival$Status) ~ Data_Survival$Product, method = 'breslow') ### Product = PT
		summary(Survival_Model_CPH)

	Survival_Model_CPH = coxph(Surv(start_time, end_time, default_flag) ~ ltv_utd + seasoning + hpi + ir, data = Train_Data, method = 'efron')
		summary(Survival_Model_CPH)	
}

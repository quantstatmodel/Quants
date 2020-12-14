## https://www.tizianobellini.com/copy-of-chapter-2-4

Source = "/Users/arunchaitali/Dropbox/ARUN & CHAITALI/Quant-Understanding/Data"

library(caret)
library(dplyr)
library(vars)

{	### Page : 106
	Get_Data =  read.csv(paste(Source, "/chap3ltpdpanel.csv", sep = ""))
		print(head(Get_Data))

	Data = dplyr::mutate_at(Get_Data, vars(contains('date')), funs((as.Date(., format = '%Y-%m-%d'))))
		print(head(Data))

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

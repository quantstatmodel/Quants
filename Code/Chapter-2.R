## https://www.tizianobellini.com/copy-of-chapter-2-4

Source = "/Users/arunchaitali/Dropbox/ARUN & CHAITALI/Quant-Understanding/Data"

library(caret)
library(dplyr)
library(vars)
library(OIsurv)  ### For Survival Analysis

{	### Page : 51
	Get_Data =  read.csv(paste(Source, "/chap2oneypd.csv", sep = ""))
		print(head(Get_Data))

	Data = dplyr::mutate_at(Get_Data, vars(contains('date')), funs((as.Date(., format = '%Y-%m-%d'))))
		print(head(Data))
		### 1) 'tob'                   : Number of Qs Since Origination
		### 2) 'start_time / end_time' : Represent : The window to include into ***CPH function*** to fit the model

	### This is ***NOT used*** ANYWHERE Here
	Default_Rate_Q = Data %>%
					dplyr::group_by(report_date, year) %>%
					dplyr::summarise(dr_QoQ = mean(default_flag)) %>%
					dplyr::select(report_date, year, dr_QoQ)
		print(head(Default_Rate_Q))
}
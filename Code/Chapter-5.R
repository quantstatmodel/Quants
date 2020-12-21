## https://www.tizianobellini.com/copy-of-chapter-2-4

Source = "/Users/arunchaitali/Dropbox/ARUN & CHAITALI/Quant-Understanding/Data"

library(caret)
library(dplyr)
library(vars)

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
Source = "/Users/arunchaitali/Dropbox/ARUN & CHAITALI/Quant-Understanding/Data"

library(dplyr)
library(vars)

Get_Data =  read.csv(paste(Source, "/chap3ltpdpanel.csv", sep = ""))
	head(Get_Data)

Get_Data = dplyr::mutate_at(Get_Data, vars(contains('date')), funs((as.Date(., format = '%Y-%m-%d'))))

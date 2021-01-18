
chicago <- readRDS('chicago.rds')
dim(chicago)
str(chicago)
names(chicago)
subset <- select(chicago, city:date)
head(subset)

chic.f <- filter(chicago,pm25tmean2   > 30)
str(chic.f)
chic.f <- filter(chicago, pm25tmean2 > 30 & tmpd > 80)
select(chic.f, date, tmpd, pm25tmean2)

chicago <- arrange(chicago, date)
head(select(chicago, date, pm25tmean2), 3)

#sorting the dates in descending order 
chicago <- arrange(chicago, desc(date))

chicago <- rename(chicago, dewpoint = dptp, pm25 = pm25tmean2)
head(chicago[, 1:5], 3)

#with air pollution data, we often want to detrend the data by subtracting the mean from the data. 
chicago <- mutate(chicago, pm25detrend = pm25 - mean(pm25, na.rm = TRUE))

#find information here 
#https://bookdown.org/rdpeng/rprogdatascience/managing-data-frames-with-the-dplyr-package.html 
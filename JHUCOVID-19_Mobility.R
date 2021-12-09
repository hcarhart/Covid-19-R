

#load libraries
```{r}
library(tidyverse)
library(lubridate)
library(reshape2)
```

#read csv
```{r}
cases = read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

mob = read_csv("2021_IT_Region_Mobility_Report.csv")
```

```{r}
head(cases)
```
#transpose and turn into data frame
```{r}
tb1 = cases %>% dplyr:: select(5:length(cases[1,])) %>% as.matrix
tb1 = as.data.frame(t(tb1))
```


#add column names
```{r}
names(tb1) = cases$'Country/Region'
tail(tb1)
```
#change format of dates
```{r}
current_dates = names(cases[5:length(cases[1,])])
tb1$dates = mdy(current_dates)
```

#add time window
```{r}
time_window = 600
end = today(tzone = 'CET')
```

# select for just Italy
```{r}
tb2 = tb1 %>% dplyr:: select(all_of('Italy'), 'dates') %>% dplyr:: filter(between(dates, end-ddays(time_window), end))
#View(tb2)
```

#subtract cumulative cases to get daily cases
```{r}
daily_cases = c(0, diff(tb2[,1]))
tb3_daily = data.frame(daily_cases)
head(tb3_daily)
```

#adding date column
```{r}
tb3_daily$YMD = mdy(row.names(tb2))
head(tb3_daily)
```
#rename column
```{r}
names(tb3_daily)[1] = 'Italy'
```

#plot daily cases for italy
```{r}
daily_cases_plot <- ggplot(tb3_daily, aes(x= YMD, y=tb3_daily[,1]))
daily_cases_plot + geom_point() + stat_smooth(span = 0.3) + ggtitle("Daily COVID-19 Cases in Italy") + ylab("Cases") + xlab ("Date")
```
#view mobility data
```{r}
head(mob)
```

```{r}
str(mob)
```
#select only columns that are averaged for all regions
```{r}
tb_mob = mob[1:237,]
```
#group by date 
```{r}
gm_bydate = tb_mob %>% group_by(date) %>% summarise_if(is.numeric, mean)
```


```{r}
daily_italy = tb3_daily
names(daily_italy) = c("Daily Cases", "date")
head(daily_italy)
```

#merge cases and google mobility
```{r}
mob_cases = merge(x=daily_italy, y=gm_bydate, by="date", all.y = TRUE)
row.names(mob_cases) = mob_cases$date
```
#get columns 2-8
```{r}
mob_cases2 <- mob_cases %>% dplyr:: select(2:8)
```

#scale and create date column
```{r}
mob_cases2_scaled <- data.frame(scale(mob_cases2))
mob_cases2_scaled$date = ymd(row.names(mob_cases2_scaled))
```

#see column names
```{r}
names(mob_cases2_scaled)
```
#select columns
```{r}
select_cols= c("Daily.Cases", "residential_percent_change_from_baseline", "workplaces_percent_change_from_baseline")
```
#melt and plot
```{r}
dfmelt <- melt(mob_cases2_scaled, measure.vars = select_cols, value.name = "Values", variable.name = "variable")


```
```{r}
plot <- ggplot(dfmelt, aes(x=date, y=Values, color=variable)) + stat_smooth(span=0.15)
```

```{r}
plot + ggtitle("Daily COVID-19 Cases and Google Mobility Data - Italy")
```


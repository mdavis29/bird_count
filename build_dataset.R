
## build data set for charleston
library(reshape2)
library(xlsx)
print('loading charleston count data ...')
file_name = "./data/HistoricalResultsByCount_chs.xls"
sheet_name = 'HistoricalResultsByCount'
# set the index of rows to read
row_index = 232:1141
# set the column index
col_index = 6:124
count_data = read.xlsx(file_name, sheetName =sheet_name, rowIndex = row_index, colIndex = col_index)
d = count_data
for (i in 1:(nrow(d))){
  if (is.na(d[i, 1])){
    d[i, 1] = d[i-1, 1]
  }
}
# select which columns to actually use from the
keep_col_index = grep('N', colnames(count_data), invert = T)
d = d[, keep_col_index]

# gets the colnames (uses only the year)
colnames(d) = substr(colnames(d), 2, 5)
colnames(d)[1:2] = c('species', 'val')

# coeces columns to numeric
for ( i in 3:(ncol(d))){
  d[,i] = as.numeric(d[, i])
}
# fills Nulls with zero
d[is.na(d)]= 0

# melts the years columns into a single year column
d_melted = melt(d, id.vars=c('species', 'val'), variable.name='year', value.name='count',  na.rm = T)

# pivot out the val columns
d_cast = dcast(d_melted, species + year + count ~ val, sum, value.var='count')
# map year to a integer from a factor
d_cast$year= as.numeric(as.character(d_cast$year))

counts_chs = d_cast


print('loading charleston weather data ...')

file_name = './data/chs_wx.csv'
wx_data = read.csv(file_name)
# filter to rows with only december
wx_data = wx_data[substr(wx_data$DATE,6, 8) == '12', ]
# changes the Date to use only the year, (keeping in mind this is the December average)
wx_data$DATE = as.integer(substr(wx_data$DATE, 1, 4))
rownames(wx_data) = wx_data$DATE 
wx_chs = wx_data
wx_chs$TSUN = NULL

wx_chs$year = as.integer(substr(wx_chs$DATE, 1, 4))
merged_chs = merge(wx_chs, counts_chs, all=F)



###############################################################

#### SAVANNAH SECTION 

# load the savannah river site data 
print('loading savannah river count data ...')
file_name = "./data/HistoricalResultsByCount_svr.xls"
sheet_name = 'HistoricalResultsByCount'
# set the index of rows to read
row_index = 142:724
# set the column index
col_index = 6:100
count_data = read.xlsx(file_name, sheetName =sheet_name, rowIndex = row_index, colIndex = col_index)
d = count_data
for (i in 1:(nrow(d))){
  if (is.na(d[i, 1])){
    d[i, 1] = d[i-1, 1]
  }
}
# select which columns to actually use from the
keep_col_index = grep('N', colnames(count_data), invert = T)
d = d[, keep_col_index]

# gets the colnames (uses only the year)
colnames(d) = substr(colnames(d), 2, 5)
colnames(d)[1:2] = c('species', 'val')

# coeces columns to numeric
for ( i in 3:(ncol(d))){
  d[,i] = as.numeric(d[, i])
}
# fills Nulls with zero
d[is.na(d)]= 0

# melts the years columns into a single year column
d_melted = melt(d, id.vars=c('species', 'val'), variable.name='year', value.name='count',  na.rm = T)

# pivot out the val columns
d_cast = dcast(d_melted, species + year + count ~ val, sum, value.var='count')
# map year to a integer from a factor
d_cast$year= as.numeric(as.character(d_cast$year))

counts_svr = d_cast

# re index the rows based on sum counts

# load savannah river weather data
print('loading savannah river wx data ...')
file_name = './data/wx_svr.csv'
wx_svr = read.csv(file_name)
# filter to rows with only december
wx_svr = wx_svr[substr(wx_svr$DATE,6, 8) == '12', ]
# changes the Date to use only the year, (keeping in mind this is the December average)
wx_svr$year = as.integer(substr(wx_svr$DATE, 1, 4))
merged_svr= merge(wx_svr, counts_svr, all=F)


final_data = merge(merged_svr, merged_chs, all=F, by=c('species', 'year'))

print('writting data to final_data.csv')
write.csv(final_data, file='final_data.csv')


x = final_data$count.x - final_data$count.y 
final_data$tavg_diff  =  final_data$TAVG.x - final_data$TAVG.y 
plot(x,y )


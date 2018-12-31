import quandl
quandl.ApiConfig.api_key = 'SDYDy_D1kPnB1kYTjwyw'

# get the table for daily stock prices and,
# filter the table for selected tickers, columns within a time range
# set paginate to True because Quandl limits tables API to 10,000 rows per call
# https://www.quandl.com/data/MCX-Multi-Commodity-Exchange-India/usage/quickstart/python

data = quandl.get('MCX/GMQ2018')
print(data)
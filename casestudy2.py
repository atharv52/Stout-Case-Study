import pandas as pd
import numpy as np

df = pd.read_csv("casestudy.csv")
df

#•	Total revenue for the current year (2017)
this_year = df[df['year'] == 2017]
total_revenue = this_year['net_revenue'].sum()
total_revenue


#•	Total Customers Current Year(2017)
this_year = df[df['year'] == 2017]
total_customers =  this_year['customer_email'].count()
total_customers


#•	Total Customers Previous Year(2016)
previous_year = df[df['year'] == 2016]
previous_total_customers =  previous_year['customer_email'].count()
previous_total_customers

#•	New Customer Revenue e.g. new customers not present in previous year only For YEar 2017
customers = df.pivot(index ='customer_email', columns ='year', values ='net_revenue')
customers

customers = customers.fillna(0)
customers = customers.reset_index()
customers.head()

New_customer_revenue_2017 = customers[(customers[2016] == 0.00) & (customers[2017] > 0.00)]
Total_new_customer_revenue_2017 = New_customer_revenue_2017[2017].sum()
Total_new_customer_revenue_2017

new_customers = New_customer_revenue_2017[2017].count()
new_customers

#Lost customers
lost_cusomers = customers[(customers[2017] == 0.00) & (customers[2016] > 0.00)]
total_lost_customers = lost_cusomers[2016].count()
total_lost_customers

#exisisting customers revenue for 2017
exist_customer = customers[((customers[2015] > 0.00) | (customers[2016] > 0.00)) & (customers[2017] > 0.00) ]
exist_customer[2017].sum()

#exisisting customers revenue for 2016
exist_customer = customers[(customers[2015] > 0.00)  & (customers[2016] > 0.00) ]
exist_customer[2016].sum()
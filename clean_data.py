import pandas as pd
import numpy as np
from tqdm import tqdm

data_home = 'C:/Users/602330/BOOZ ALLEN HAMILTON/McMenamin, Robert [USA] - Charleston Data Science/'
data= pd.read_csv(data_home+'all_energy_statistics_201902230853.csv')
cat_to_kwh = pd.read_csv(data_home+'category_to_kwh.csv')
cat_weights = pd.read_csv(data_home+'Category_Weights.csv')
cat_weights_dict = dict(zip(cat_weights['Category'],cat_weights['Kg CO2']))
cat_rollups = pd.read_csv(data_home+'Category_Rollup.csv')
cat_rollups_dict = dict(zip(cat_rollups['Category'],cat_rollups['Rollup Category']))

cat_to_kwh_dict = {}
for i in range(cat_to_kwh.shape[0]):
    print(i)
    cat_to_kwh_dict[cat_to_kwh.loc[i,'Category']] = cat_to_kwh.loc[i,'KWH']


data['Transaction'] = list(map(lambda val:val.split(' - ')[1] if val.__contains__(' - ') else val ,data['commodity_transaction']))
data.loc[np.where(data['category']=='solar_electricity')[0],'Transaction'].unique()
unique_countries = list(data['country_or_area'].unique())
former_countries = [country for country in unique_countries if country.__contains__('former')]

data = data.iloc[np.where(data['country_or_area'].isin(former_countries)==False)[0],:].reset_index(drop=True)
unique_countries = list(data['country_or_area'].unique())

data.columns = ['Country','commodity_transaction','Year','unit','Quantity','quantity_footnotes','Category','Transaction']


tmp_data = data[['Country','Category','Year','Transaction','Quantity']].groupby(['Country','Category','Year']).agg({'Quantity':'max'})

tmp_data.to_csv('data/tmp_data.csv')
data = pd.read_csv('data/tmp_data.csv')

data.columns
data = data[['Country','Category','Year','Quantity']]


data['KWH'] = 0
data['CO2'] = 0
data['KWH_CO2_Convert'] = 0
data['Category_Rollup'] = ''
for i in tqdm(range(data.shape[0])):
    quantity = data.loc[i,'Quantity']
    category = data.loc[i,'Category']
    data.loc[i,'KWH'] = cat_to_kwh_dict[category]*quantity
    data.loc[i, 'CO2'] = cat_weights_dict[category] * data.loc[i,'KWH']
    data.loc[i,'Category_Rollup']  =cat_rollups_dict[category]
    data.loc[i, 'KWH_CO2_Convert'] = cat_weights_dict[category]

data.to_csv(data_home+'cleaned_data.csv')


agg_data = data[['Country','Category_Rollup','Year','KWH','CO2']].groupby(['Country','Category_Rollup','Year']).agg({'KWH':'sum','CO2':'sum'})
agg_data.to_csv(data_home+'cleaned_rollup_data.csv')

total_data = data[['Country','Year','KWH','CO2']].groupby(['Country','Year']).agg({'KWH':'sum','CO2':'sum'})
total_data.to_csv(data_home+'cleaned_totals_data.csv')












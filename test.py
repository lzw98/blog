import pandas as pd
import numpy as np
fileroecsv = r'C:\Users\lzw\Desktop\ROE.csv'
roe = pd.read_csv(fileroecsv)

k,b = roe.shape
allnu = [i for i in range(k)]
length,width = np.where(roe.isnull())
# length, width are null value's axis

for j in set(width):
    length,width = np.where(roe.isnull())
    abscissa = length[width == j]
    # find not null value's abscissa
    no_null = list(set(allnu) - set(abscissa))
    no_null = np.array(no_null)
    
    if roe.iloc[0,j] is np.nan:
        roe.iloc[0,j] = 99999999999999999
        continue
    if roe.iloc[k-1,j] is np.nan:
        low_value = no_null[no_null<k].max()
        row = k-low_value
        for axis in range(1,row+1):
            roe.iloc[axis+low_value,j] = roe.iloc[low_value,j]
    
    length,width = np.where(roe.isnull())
    abscissa = length[width == j]
    # find not null value's abscissa
    no_null = list(set(allnu) - set(abscissa))
    no_null = np.array(no_null)

    if roe.iloc[0,j] is not np.nan:
        for a in abscissa:
            print(a)
            high_value = no_null[no_null>a].min()
            low_value = no_null[no_null<a].max()
            cake = high_value - low_value
            high_portion = high_value - a
            low_portion = a - low_portion
            roe.iloc[a,j] = (low_portion/cake)*roe.iloc[high_value,j]+(high_portion/cake)*roe.iloc[low_value,j]
    
        

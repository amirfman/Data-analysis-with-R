import pandas as pd
import matplotlib.pyplot as plt
# import statsmodels.api as sm
import os
os.listdir()
# Function to Get the current
# working directory
print("Current working directory before",os.getcwd())
path = "C:/uni hamburg/Mini project 2/mini project 2"
os.chdir(path)
print("Current working directory before",os.getcwd())
dfc1= pd.read_csv("CPUE Balitic 21 22 23 24 WBC.csv")
print(dfc1)
dfc1.shape
pd.DataFrame(dfc1)
df2=dfc1.dropna()
dfc3 = dfc1.drop(dfc1.columns[[0, 2, 3,4,5,7]], axis=1)
dfc4= dfc3.groupby(by='Year').describe()
#print(dfc4)
dfmean= dfc3.groupby(by='Year').mean()
#print(dfmean)
dfmean.plot()
plt.show()



import pandas as pd
import statsmodels.api as sm

# create a sample dataset
data = {'temperature': [15, 20, 25, 30, 35],
        'mean_length': [25, 30, 35, 40, 45]}
dfe = pd.DataFrame(data)

# fit a linear regression model
X = sm.add_constant(dfe['temperature'])  # add an intercept
model = sm.OLS(dfe['mean_length'], X).fit()

# print the summary of the model
#print(model.summary())

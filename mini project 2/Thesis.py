import pandas as pd
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression
import statsmodels.api as sm
import statsmodels.formula.api as smf
import numpy as np
import seaborn as sns
from scipy.optimize import curve_fit
import os
import pygam
from pygam import LinearGAM, s
#os.listdir()
path = "C:/uni hamburg/Thesis"
os.chdir(path)
print("Current working directory before", os.getcwd())

#########################################  COD  ######################################################
######################################################################################################

dfC1= pd.read_csv("CF1.csv")
dfC2= pd.read_csv("CF2.csv")
dfC3= pd.read_csv("CF3.csv")
dfC4= pd.read_csv("CF4.csv")
dfC5= pd.read_csv("CF5.csv")

dfC1['CF1TOTAL'] = dfC1.iloc[:, 1:-1].sum(axis=1)
dfC1['CF1FERTILIZATION SUCCESS'] =(dfC1['CF1REGULAR'] / (dfC1['CF1TOTAL'] - dfC1['CF1NONVIABLE'])) * 100

dfC2['CF2TOTAL'] = dfC2.iloc[:, 1:-1].sum(axis=1)
dfC2['CF2FERTILIZATION SUCCESS'] =(dfC2['CF2REGULAR'] / (dfC2['CF2TOTAL'] - dfC2['CF2NONVIABLE'])) * 100

dfC3['CF3TOTAL'] = dfC3.iloc[:, 1:-1].sum(axis=1)
dfC3['CF3FERTILIZATION SUCCESS'] =(dfC3['CF3REGULAR'] / (dfC3['CF3TOTAL'] - dfC3['CF3NONVIABLE'])) * 100

dfC4['CF4TOTAL'] = dfC4.iloc[:, 1:-1].sum(axis=1)
dfC4['CF4FERTILIZATION SUCCESS'] =(dfC4['CF4REGULAR'] / (dfC4['CF4TOTAL'] - dfC4['CF4NONVIABLE'])) * 100

dfC5['CF5TOTAL'] = dfC5.iloc[:, 1:-1].sum(axis=1)
dfC5['CF5FERTILIZATION SUCCESS'] =(dfC5['CF5REGULAR'] / (dfC5['CF5TOTAL'] - dfC5['CF5NONVIABLE'])) * 100

# Turning every individuals in our data frame into Percentage
# deviding each column by 'CFTOTAL' and multiply by 100

columns_to_convert1 = ['CF1REGULAR', 'CF1ABNORMAL', 'CF1NECROTIC', 'CF1UNFERTILIZED', 'CF1NONVIABLE', 'CF1TOTAL']

dfC12 = dfC1[columns_to_convert1].div(dfC1['CF1TOTAL'], axis=0) * 100
dfC12['CF1FERTILIZATION SUCCESS'] =(dfC1['CF1REGULAR'] / (dfC1['CF1TOTAL'] - dfC1['CF1NONVIABLE'])) * 100
dfC12['CMEANTEMP'] = dfC1['CMEANTEMP']

columns_to_convert2 = ['CF2REGULAR', 'CF2ABNORMAL', 'CF2NECROTIC', 'CF2UNFERTILIZED', 'CF2NONVIABLE', 'CF2TOTAL']
dfC22 = dfC2[columns_to_convert2].div(dfC2['CF2TOTAL'], axis=0) * 100
dfC22['CF2FERTILIZATION SUCCESS'] =(dfC2['CF2REGULAR'] / (dfC2['CF2TOTAL'] - dfC2['CF2NONVIABLE'])) * 100
dfC22['CMEANTEMP'] = dfC2['CMEANTEMP']

columns_to_convert3 = ['CF3REGULAR', 'CF3ABNORMAL', 'CF3NECROTIC', 'CF3UNFERTILIZED', 'CF3NONVIABLE', 'CF3TOTAL']
dfC32 = dfC3[columns_to_convert3].div(dfC3['CF3TOTAL'], axis=0) * 100
dfC32['CF3FERTILIZATION SUCCESS'] =(dfC3['CF3REGULAR'] / (dfC3['CF3TOTAL'] - dfC3['CF3NONVIABLE'])) * 100
dfC32['CMEANTEMP'] = dfC3['CMEANTEMP']

columns_to_convert4 = ['CF4REGULAR', 'CF4ABNORMAL', 'CF4NECROTIC', 'CF4UNFERTILIZED', 'CF4NONVIABLE', 'CF4TOTAL']
dfC42 = dfC4[columns_to_convert4].div(dfC4['CF4TOTAL'], axis=0) * 100
dfC42['CF4FERTILIZATION SUCCESS'] =(dfC4['CF4REGULAR'] / (dfC4['CF4TOTAL'] - dfC4['CF4NONVIABLE'])) * 100
dfC42['CMEANTEMP'] = dfC4['CMEANTEMP']

columns_to_convert5 = ['CF5REGULAR', 'CF5ABNORMAL', 'CF5NECROTIC', 'CF5UNFERTILIZED', 'CF5NONVIABLE', 'CF5TOTAL']
dfC52 = dfC5[columns_to_convert5].div(dfC5['CF5TOTAL'], axis=0) * 100
dfC52['CF5FERTILIZATION SUCCESS'] =(dfC5['CF5REGULAR'] / (dfC5['CF5TOTAL'] - dfC5['CF5NONVIABLE'])) * 100
dfC52['CMEANTEMP'] = dfC5['CMEANTEMP']

# Concatenate the individual dataframes into one
df_combined = pd.concat([dfC12, dfC22, dfC32, dfC42, dfC52])

# Filter rows for the specific temperature (-1.5)
df_at_minus_1_5 = df_combined.groupby(['CMEANTEMP'] == -1.5)

# Calculate the mean fertilization success for the specified temperature
mean_fertilization_success = df_at_minus_1_5['CF5FERTILIZATION SUCCESS'].mean()

# Print the result
print("Mean Fertilization Success at -1.5:", mean_fertilization_success)



#print(dfC12)

# 'CMEANTEMP' column as the index for the DataFrame (optional, for better visualization)
#dfC12.set_index('CMEANTEMP', inplace=True)

# Plot the bar chart
#ax = dfC12.plot(kind='bar', stacked=True, figsize=(10, 6))
#plt.xlabel('CMEANTEMP')
#plt.ylabel('Percentage')
#plt.title('Bar Chart of Data in Percentages')

# the legend outside the plot
#ax.legend(title='Columns', bbox_to_anchor=(1.05, 1), loc='upper left')

#plt.tight_layout()
#plt.show()


# scatter plot with regression line using seaborn
#sns.regplot(x=dfC12['CMEANTEMP'], y='CF1FERTILISATION SUCCESS', data=dfC12)

# labels and title
#plt.xlabel('CMEANTEMP')
#plt.ylabel('CF1FERTILISATION SUCCESS')
#plt.title('Scatter Plot with Regression Line')

#plt.tight_layout()
#plt.show()



# Set the index to 'CMEANTEMP'
#dfC12.set_index('CMEANTEMP', inplace=True)

#columns_to_plot1 = ['CF4ABNORMAL', 'CF4NECROTIC', 'CF4UNFERTILIZED', 'CF4NONVIABLE']

# a line graph with different colors for specified columns
#plt.figure(figsize=(10, 6))

# each specified column with a different color
#for column in columns_to_plot1:
 #plt.plot(dfC42.index, dfC42[column], label=column)

# labels and title
#plt.xlabel('CMEANTEMP')
#plt.ylabel('Percentage')
#plt.title('Line Graph of Percentage Values')

# a legend
#plt.legend()
#plt.tight_layout()
#plt.show()

# a line graph for 'CFxFERTILISATION SUCCESS' columns
#plt.figure(figsize=(10, 6))

# each 'CFxFERTILISATION SUCCESS' column with a different color
#plt.plot(dfC12['CMEANTEMP'], dfC12['CF1FERTILIZATION SUCCESS'], label='CF1FERTILIZATION SUCCESS')
#plt.plot(dfC22['CMEANTEMP'], dfC22['CF2FERTILIZATION SUCCESS'], label='CF2FERTILIZATION SUCCESS')
#plt.plot(dfC32['CMEANTEMP'], dfC32['CF3FERTILIZATION SUCCESS'], label='CF3FERTILIZATION SUCCESS')
#plt.plot(dfC42['CMEANTEMP'], dfC42['CF4FERTILIZATION SUCCESS'], label='CF4FERTILIZATION SUCCESS')
#plt.plot(dfC52['CMEANTEMP'], dfC52['CF5FERTILIZATION SUCCESS'], label='CF5FERTILIZATION SUCCESS')

# labels and title
#plt.xlabel('CMEANTEMP')
#plt.ylabel('Percentage')
#plt.title('Line Graph of FERTILIZATION Success Percentage')

# Add a legend
#plt.legend()

#plt.tight_layout()
#plt.show()


# all the data into a single dataframe
# all the 'FERTILISATION SUCCESS' columns from different dataframes
combined_data = pd.concat([dfC12['CF1FERTILIZATION SUCCESS'], dfC22['CF2FERTILIZATION SUCCESS'],
                           dfC32['CF3FERTILIZATION SUCCESS'], dfC42['CF4FERTILIZATION SUCCESS'],
                           dfC52['CF5FERTILIZATION SUCCESS'], dfC12['CMEANTEMP']], axis=1)

# columns for clarity
combined_data.columns = ['CF1FERTILIZATION SUCCESS', 'CF2FERTILIZATION SUCCESS',
                         'CF3FERTILIZATION SUCCESS', 'CF4FERTILIZATION SUCCESS',
                         'CF5FERTILIZATION SUCCESS', 'CMEANTEMP']

# the dataframe for plotting
combined_data = combined_data.melt(id_vars=['CMEANTEMP'], value_vars=['CF1FERTILIZATION SUCCESS',
                                                                   'CF2FERTILIZATION SUCCESS',
                                                                    'CF3FERTILIZATION SUCCESS',
                                                                    'CF4FERTILIZATION SUCCESS',
                                                                     'CF5FERTILIZATION SUCCESS'],
                                   var_name='Dataframe', value_name='FERTILIZATION Success')

print(combined_data)
# scatter plot with a regression line
#plt.figure(figsize=(10, 6))

#sns.regplot(x='CMEANTEMP', y='FERTILIZATION Success', data=combined_data, scatter_kws={'alpha': 0.7})

# labels and title
#plt.xlabel('CMEANTEMP')
#plt.ylabel('FERTILIZATION Success (%)')
#plt.title('Scatter Plot of FERTILIZATION Success against CMEANTEMP with Regression Line')

#plt.tight_layout()
#plt.show()

# a custom nonlinear model function
#def custom_nonlinear_model(CMEANTEMP, a, b, c):
 #   return a * np.sin(b * CMEANTEMP) + c

# the custom nonlinear model to the data
#params, covariance = curve_fit(custom_nonlinear_model, combined_data['CMEANTEMP'], combined_data['FERTILIZATION Success'])

# the estimated parameters
#a, b, c = params

# a scatter plot of the data
#plt.figure(figsize=(10, 6))
#plt.scatter(combined_data['CMEANTEMP'], combined_data['FERTILIZATION Success'], label='Data', alpha=0.7)

# a curve using the fitted parameters
#x_values = np.linspace(min(combined_data['CMEANTEMP']), max(combined_data['CMEANTEMP']), 100)
#y_values = custom_nonlinear_model(x_values, a, b, c)
#plt.plot(x_values, y_values, 'r-', label='Nonlinear Regression Curve')

# labels and title
#plt.xlabel('CMEANTEMP')
#plt.ylabel('FERTILIZATION Success (%)')
#plt.title('Custom Nonlinear Regression of FERTILIZATION Success against CMEANTEMP')

#plt.legend()
#plt.tight_layout()
#plt.show()

# the estimated parameters
#print(f"Estimated Parameters (a, b, c): {params}")


# columns for clarity
#combined_data = pd.concat([dfC12['CF1FERTILIZATION SUCCESS'], dfC22['CF2FERTILIZATION SUCCESS'],
#                           dfC32['CF3FERTILIZATION SUCCESS'], dfC42['CF4FERTILIZATION SUCCESS'],
#                           dfC52['CF5FERTILIZATION SUCCESS'], dfC12['CMEANTEMP']], axis=1)
#
# the dataframe for plotting
#combined_data = combined_data.melt(id_vars=['CMEANTEMP'], value_vars=['CF1FERTILIZATION SUCCESS',
     #                                                               'CF2FERTILIZATION SUCCESS',
    #                                                                'CF3FERTILIZATION SUCCESS',
   #                                                                 'CF4FERTILIZATION SUCCESS',
  #                                                                  'CF5FERTILIZATION SUCCESS'],
 #                                  var_name='FERTILIZATION Type', value_name='FERTILIZATION Success')

# scatter plot with different colors for each dataframe
#plt.figure(figsize=(10, 6))

#sns.scatterplot(x='CMEANTEMP', y='FERTILIZATION Success', data=combined_data, alpha=0.7, hue='FERTILIZATION Type')

# Overlay regression line
#sns.regplot(x='CMEANTEMP', y='FERTILIZATION Success', data=combined_data, scatter=False)

# labels and title
#plt.xlabel('CMEANTEMP')
#plt.ylabel('FERTILIZATION Success (%)')
#plt.title('Scatter Plot of Fertilisation Success against CMEANTEMP with Regression Line')

# legend
#plt.legend()
#plt.tight_layout()
#plt.show()
#print(combined_data)
#selected_columns = dfC12[['CF1FERTILIZATION SUCCESS', 'CMEANTEMP']]
#print(selected_columns)
#max_index = selected_columns['CF1FERTILIZATION SUCCESS'].idxmax()

#max_row = selected_columns.loc[max_index]
#print(max_row)
#print(combined_data)

# Create a Linear Regression model
#model = LinearRegression()

#X = combined_data[['CMEANTEMP']]  # Feature (independent variable)
#y = combined_data[['FERTILIZATION Success']]  # Target variable

# Fit the linear regression model to your data
#model.fit(X, y)

# Print the intercept and coefficient of the linear regression model
#print("Intercept:", model.intercept_[0])
#print("Coefficient:", model.coef_[0][0])


#data = {
 #   'CMEANTEMP': [-1.5, 0.3, 2.2, 3.8, 5.4, 6.9, 8.2, 9.4, 10.8, 12.0,
#                  -1.5, 0.3, 2.2, 3.8, 5.4, 6.9, 8.2, 9.4, 10.8, 12.0,
#                  -1.5, 0.3, 2.2, 3.8, 5.4, 6.9, 8.2, 9.4, 10.8, 12.0,
#                  -1.5, 0.3, 2.2, 3.8, 5.4, 6.9, 8.2, 9.4, 10.8, 12.0,
#                  -1.5, 0.3, 2.2, 3.8, 5.4, 6.9, 8.2, 9.4, 10.8, 12.0],
#    'FERTILIZATION_Type': ['CF1FERTILIZATION SUCCESS'] * 10 +
#                    ['CF2FERTILIZATION SUCCESS'] * 10 +
#                     ['CF3FERTILIZATION SUCCESS'] * 10 +
#                      ['CF4FERTILIZATION SUCCESS'] * 10 +
#                      ['CF5FERTILIZATION SUCCESS'] * 10,
    #'FERTILIZATION_Success': [58.119658, 69.930070, 83.333333, 84.745763, 88.549618, 91.836735, 96.402878, 87.500000, 84.892086, 85.981308,
   #                           50.495050, 83.443709, 89.932886, 95.394737, 91.608392, 94.890511, 94.927536, 86.805556, 82.000000, 88.709677,
  #                            77.981651, 85.470085, 97.894737, 91.150442, 92.920354, 85.714286, 86.554622, 84.070796, 84.482759, 77.876106,
 #                             80.000000, 86.400000, 83.333333, 85.000000, 81.343284, 85.517241, 82.945736, 80.434783, 83.941606, 73.722628,
#                             2.702703, 31.818182, 43.243243, 60.810811, 61.585366, 79.393939, 74.585635, 83.139535, 84.242424, 86.330935]
#}

#df = pd.DataFrame(data)
# Rename the column with a space
#df.rename(columns={'FERTILIZATION_Success': 'FERTILIZATION_Success'}, inplace=True)

# Fit a GLM
#model = smf.glm(
#    formula="FERTILIZATION_Success ~ CMEANTEMP",
#    data=df,
#    family=sm.families.Gaussian()  # Assuming Gaussian distribution
#)

# Fit the model
#result = model.fit()

# Summary of the GLM
#print(result.summary())

# Define a quadratic function for the model
#def quadratic_function(x, a, b, c):
#    return a * x**2 + b * x + c

# Fit the data to the quadratic function
#popt, _ = curve_fit(quadratic_function, df['CMEANTEMP'], df['FERTILIZATION_Success'])

# Generate predicted values using the fitted parameters
#x = np.linspace(min(df['CMEANTEMP']), max(df['CMEANTEMP']), 100)
#y_pred = quadratic_function(x, *popt)

# Create a scatter plot of the data
#plt.scatter(df['CMEANTEMP'], df['FERTILIZATION_Success'], label='Data', color='blue')

# Create the fitted curve
#plt.plot(x, y_pred, 'r-', label='Fit', linewidth=2)

# Add labels and a legend
#plt.xlabel('CMEANTEMP')
#plt.ylabel('FERTILIZATION_Success')
#plt.legend()

# Show the plot
#plt.show()

data = {
    'CMEANTEMP': [-1.5, 0.3, 2.2, 3.8, 5.4, 6.9, 8.2, 9.4, 10.8, 12.0,
                  -1.5, 0.3, 2.2, 3.8, 5.4, 6.9, 8.2, 9.4, 10.8, 12.0,
                  -1.5, 0.3, 2.2, 3.8, 5.4, 6.9, 8.2, 9.4, 10.8, 12.0,
                  -1.5, 0.3, 2.2, 3.8, 5.4, 6.9, 8.2, 9.4, 10.8, 12.0,
                  -1.5, 0.3, 2.2, 3.8, 5.4, 6.9, 8.2, 9.4, 10.8, 12.0],
    'FERTILIZATION_Type': ['CF1FERTILIZATION SUCCESS'] * 10 +
                            ['CF2FERTILIZATION SUCCESS'] * 10 +
                            ['CF3FERTILIZATION SUCCESS'] * 10 +
                            ['CF4FERTILIZATION SUCCESS'] * 10 +
                            ['CF5FERTILIZATION SUCCESS'] * 10,
    'FERTILIZATION_Success': [58.119658, 69.930070, 83.333333, 84.745763, 88.549618, 91.836735, 96.402878, 87.500000, 84.892086, 85.981308,
                              50.495050, 83.443709, 89.932886, 95.394737, 91.608392, 94.890511, 94.927536, 86.805556, 82.000000, 88.709677,
                              77.981651, 85.470085, 97.894737, 91.150442, 92.920354, 85.714286, 86.554622, 84.070796, 84.482759, 77.876106,
                              80.000000, 86.400000, 83.333333, 85.000000, 81.343284, 85.517241, 82.945736, 80.434783, 83.941606, 73.722628,
                              2.702703, 31.818182, 43.243243, 60.810811, 61.585366, 79.393939, 74.585635, 83.139535, 84.242424, 86.330935],
}

df = pd.DataFrame(data)

# List of CF groups
cf_groups = ['CF1FERTILIZATION SUCCESS', 'CF2FERTILIZATION SUCCESS', 'CF3FERTILIZATION SUCCESS',
             'CF4FERTILIZATION SUCCESS', 'CF5FERTILIZATION SUCCESS']

# Calculate the mean value for each CF group
mean_values = []
for cf_group in cf_groups:
    group_df = df[df['FERTILIZATION_Type'] == cf_group]
    mean_value = group_df['FERTILIZATION_Success'].mean()
    mean_values.append(mean_value)

# Create a new DataFrame with the mean values
mean_df = pd.DataFrame({'FERTILIZATION_Type': cf_groups, 'FERTILIZATION_Success': mean_values})

# Create a dictionary to store GAM models for each CF group
gam_models = {}

# Create a dictionary to store colors for each CF group
colors = {'CF1FERTILIZATION SUCCESS': 'b', 'CF2FERTILIZATION SUCCESS': 'g',
          'CF3FERTILIZATION SUCCESS': 'r', 'CF4FERTILIZATION SUCCESS': 'c', 'CF5FERTILIZATION SUCCESS': 'm'}

# Fit a GAM to the mean values for each CF group
for cf_group in cf_groups:
    group_df = mean_df[mean_df['FERTILIZATION_Type'] == cf_group]
    X = group_df[['FERTILIZATION_Success']]
    y = group_df['FERTILIZATION_Type']

    gam = LinearGAM(s(0)).fit(X, y)

    gam_models[cf_group] = gam

    # Summary of the model
    print(f'Summary for {cf_group}:')
    print(gam.summary())
    print('\n')

    # Plot the model with colors
    plt.figure()
    plt.plot(X, y, 'ro', label='Data')
    XX = gam.generate_X_grid(term=0)
    plt.plot(XX, gam.predict(XX), color=colors[cf_group], label=cf_group)
    plt.ylabel('FERTILIZATION_Type')
    plt.xlabel('FERTILIZATION_Success')
    plt.legend()
    plt.title(cf_group)

# Show all plots
plt.show()
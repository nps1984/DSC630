import sys
import pandas as pd
import seaborn as sns
from matplotlib import pyplot as plt
from sklearn.model_selection import train_test_split




if __name__ == '__main__':
    df = pd.read_csv('dodgers.csv')

    # Print the df
    print(df)
    months = {'MAR':3,'APR':4,'MAY':5,'JUN':6,'JUL':7,'AUG':8,'SEP':9,'OCT':10,'NOV':11}
    days = {'Sunday':0,'Monday':1,'Tuesday':2,'Wednesday':3,'Thursday':4,'Friday':5,'Saturday':6}
    df['m'] = df['month'].map(months)
    df['d'] = df['day_of_week'].map(days)

    print(df)

    # Describe some stats
    print(df['attend'].describe())
    print(df['temp'].describe())
    print(df.groupby('m').attend.describe())


    # Plots
    # df.groupby('m').attend.mean().plot(x='temp',y='attend')
    # df.groupby('d').attend.mean().plot(x='d',y='attend')
    #df.plot(x='temp',y='attend', style='o')
    #df.boxplot(by='m',column='attend', grid=False)
    #bplot = sns.boxplot(y='attend',x='m',data=df, width=0.5,palette="colorblind")
    #bplot = sns.boxplot(y='attend', x='d', data=df, width=0.5, palette="colorblind")
    # plt.show()






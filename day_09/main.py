import pandas as pd
import pandas as pd
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split

def predict(df):    
    X = df.iloc[:,:-1]
    y = df.iloc[:,-1]  
    model = LinearRegression()
    model.fit(X, y)
    return model.predict(df.iloc[:,1:]).round().sum()

data = pd.read_csv("input.txt" ,delim_whitespace=True, header=None)

print(predict(data))
print(predict(data[data.columns[::-1]]))
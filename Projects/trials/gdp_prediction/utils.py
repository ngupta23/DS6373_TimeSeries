import pandas as pd
import numpy as np
from sklearn.metrics import mean_squared_error

# convert series to supervised learning
def series_to_supervised(data, n_in=1, n_out=1, dropnan=True):
    """
    data: Sequence of observations as a list or 2D NumPy array. Required.
    n_in: Number of lag observations as input (X). Values may be between [1..len(data)] Optional. Defaults to 1.
    n_out: Number of observations as output (y). Values may be between [0..len(data)-1]. Optional. Defaults to 1.
    dropnan: Boolean whether or not to drop rows with NaN values. Optional. Defaults to True.
    return: Pandas DataFrame of series framed for supervised learning.
    """
    n_vars = 1 if type(data) is list else data.shape[1]
    df = pd.DataFrame(data)
    cols, names = list(), list()
    # input sequence (t-n, ... t-1)
    for i in range(n_in, 0, -1):
        cols.append(df.shift(i))
        names += [('var%d(t-%d)' % (j+1, i)) for j in range(n_vars)]
    # forecast sequence (t, t+1, ... t+n)
    for i in range(0, n_out):
        cols.append(df.shift(-i))
        if i == 0:
            names += [('var%d(t)' % (j+1)) for j in range(n_vars)]
        else:
            names += [('var%d(t+%d)' % (j+1, i)) for j in range(n_vars)]
    # put it all together
    agg = pd.concat(cols, axis=1)
    agg.columns = names
    # drop rows with NaN values
    if dropnan:
        agg.dropna(inplace=True)
    return agg 

def compute_rmse(X, y, scaler, model, n_timesteps, n_features, dtype = 'Train'):
    """
    X: Train or Test features to be used to make a prediction
    y: Train or Test output (actual)
    scaler: Fitted Scaler object used before training
    model: Fitted model
    n_timesteps: How many timesteps were used
    n_features: How many features were used
    dtype = Type of data used, e.g. 'Train', 'Test', etc.
    """
    X_int = X.copy()
    y_int = y.copy()
    
    # make a prediction
    yhat = model.predict(X_int)
    X_int = X_int.reshape((X_int.shape[0], n_timesteps*n_features))
    
    # invert scaling for forecast
    inv_yhat = np.concatenate((yhat, X_int[:, -(n_features-1):]), axis=1)
    inv_yhat = scaler.inverse_transform(inv_yhat)
    inv_yhat = inv_yhat[:,0]
    
    # invert scaling for actual
    y_int = y_int.reshape((len(y_int), 1))
    inv_y = np.concatenate((y_int, X_int[:, -(n_features-1):]), axis=1)
    inv_y = scaler.inverse_transform(inv_y)
    inv_y = inv_y[:,0]
    
    # calculate RMSE
    rmse = np.sqrt(mean_squared_error(inv_y, inv_yhat))
    print(dtype, 'RMSE: %.1f' % rmse)
    
    return(inv_y, inv_yhat, rmse)
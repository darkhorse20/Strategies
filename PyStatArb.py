#!/usr/bin/env python
#
# Copyright 2013 Quantopian, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import matplotlib.pyplot as plt
import numpy as np
import statsmodels.api as sm
from datetime import datetime
import pytz
import pandas as pd

from zipline.algorithm import TradingAlgorithm
from zipline.finance import trading
from zipline.transforms import batch_transform
from zipline.utils.factory import load_from_yahoo

def getReturn(Price_Data):
    # the data has been delete the last row but to get the return from price, we have to 
    # delete another row
    R_data = Price_Data[:-1].copy()
    for i in xrange(0,len(R_data.index)):
        for sid in Price_Data.columns.values:
            R_data[sid][i] = (Price_Data[sid][i+1] - Price_Data[sid][i])/Price_Data[sid][i]
    return R_data

def Linear_Regression(R_data):# return data
    ticker_list = R_data.columns.values
    
    Depend_sid = ticker_list[0]
    Indep_sids = ticker_list[1:len(ticker_list)]
    
    X = R_data[Indep_sids]
    Y = R_data[Depend_sid]
    
    X = sm.add_constant(X, prepend=True)
    
    lm_Result = sm.OLS(Y, X).fit()
    #resid = Y - lm_Result.predict(X)
    #slope, intercept = sm.OLS(Y, X).fit().params
    #return #resid
    return lm_Result
    
#def Normal_Data(R_data):# normalized the data
#    for sid in R_data.columns.values:
#        R_data[sid] = (R_data[sid] - np.mean(R_data[sid]))/np.std(R_data[sid])
#    return R_data

def get_Pridict_Resi(data, Liner_Object):
    """
    calculate the residule  by using the linear regression parameter that we passed into
    """
    ticker_list = data.columns.values
    
    Depend_sid = ticker_list[0]
    Indep_sids = ticker_list[1:len(ticker_list)]
    
    X = data[Indep_sids]
    Y = data[Depend_sid]
        
    resid = Y - Liner_Object.predict(sm.add_constant(X, prepend=True))
    return resid

@batch_transform
def ols_transform(data, sid1, sid2):

    """Computes regression coefficient (slope and intercept)
    via Ordinary Least Squares between two SIDs.
    """
    # the data will include the current price 
    # since we set the window to 20 and we have to calculate the return before current time
    # so we have 20-2 = 18 observations to calculate the sscore.
    R_data = getReturn(data.price[:-1])
    # we do not need to normalize data if we only use them to regression rather than PCA
    #Normal_R_data = Normal_Data(R_data)
    #if counter >= 30 or counter == 1 :
    lm = Linear_Regression(R_data)
    resid = get_Pridict_Resi(R_data, lm)
    Cum_resid = resid
    for i in xrange(1,len(resid)):
        Cum_resid[i] = resid[i]+Cum_resid[i-1]
    # the last one should really close to 0 since this is the residue of the linear regression
    
    # resid is a pd dataframe and slop and intercept are just double 
    intercept, slope = lm.params
    return Cum_resid, intercept, slope
class Pairtrade(TradingAlgorithm):
    """Pairtrading relies on cointegration of two stocks.

    The expectation is that once the two stocks drifted apart
    (i.e. there is spread), they will eventually revert again. Thus,
    if we short the upward drifting stock and long the downward
    drifting stock (in short, we buy the spread) once the spread
    widened we can sell the spread with profit once they converged
    again. A nice property of this algorithm is that we enter the
    market in a neutral position.

    This specific algorithm tries to exploit the cointegration of
    Pepsi and Coca Cola by estimating the correlation between the
    two. Divergence of the spread is evaluated by z-scoring.
    """

    def initialize(self,  window_length=60):
        self.spreads = []
        self.invested = 0
        self.window_length = window_length
        self.ols_transform = ols_transform(refresh_period=1,
                                           window_length=self.window_length)
        #window_length: when you call the ols_transform function, how long the data from current time
        # you will trace back 
        # refresh_period: How long we recalculate the regression?
        # initial the decorator's refresh_period and window_length
        # make is own function so that the Algorithm can update it
        # generate a handler for the current object
        #refresh_period : int
        #Interval to wait between advances in the window.
        #window_length : int
        #How many days the trailing window should have.

    
    def handle_data(self, data):
        # the data is like: {'PEP': SIDData({'volume': 1000, 'sid': 'PEP', 'source_id': 'DataFrameSource-58d0fde27d9bd802ec3f0563c33696bd', 'dt': Timestamp('2000-01-03 00:00:00+0000', tz='UTC'), 'type': 4, 'price': 26.97}), 'KO': SIDData({'volume': 1000, 'sid': 'KO', 'source_id': 'DataFrameSource-58d0fde27d9bd802ec3f0563c33696bd', 'dt': Timestamp('2000-01-03 00:00:00+0000', tz='UTC'), 'type': 4, 'price': 19.81})}
        # which is a dictionary
        ######################################################
        # 1. Compute regression coefficients between PEP and KO
        params = self.ols_transform.handle_data(data, 'PEP', 'KO')# because we @batch_transform so ther eis handle_data function in it
        if params is None:
            return
        Cum_resid, intercept, slope = params
        

        # if we want to use the original linear regression result 
        # we should not output the residue. We should output the 
        # linear regression object
        

        ######################################################
        # 2. Compute spread and zscore
        zscore = self.compute_zscore(Cum_resid)
        self.record(zscores=zscore)

        ######################################################
        # 3. Place orders
        self.place_orders(data, zscore, slope)

    def compute_zscore(self, Cum_resid):
        """1. Compute the spread given slope and intercept.
           2. zscore the spread.
        """
        X_t = Cum_resid[1:len(Cum_resid)].copy() 
        X_t_one = sm.add_constant(Cum_resid[0:len(Cum_resid)-1],prepend=True).copy()
        lm_Result = sm.OLS(X_t.values, X_t_one.values).fit()
        intercept,slope = lm_Result.params
        kappa = -1*np.log(slope) * 252
        m = intercept/(1-slope)
        rvar = np.var(X_t-lm_Result.predict(X_t_one))
        sigma = np.sqrt(rvar*2*kappa/(1-np.square(slope)))
        sigma_eq = np.sqrt(rvar/(1-np.square(slope)))
        zscore = -1*m/sigma_eq
         
        #spread = (data['PEP'].price - (slope * data['KO'].price + intercept))
        #self.spreads.append(spread)
        #spread_wind = self.spreads[-self.window_length:]
        #zscore = (spread - np.mean(spread_wind)) / np.std(spread_wind)
        return zscore

    def place_orders(self, data, zscore, beta):
        """Buy spread if zscore is > 2, sell if zscore < .5.
        """
        if zscore >= 1.2 and not self.invested:
            self.order('PEP', int(10000))
            self.order('KO', -int(10000*beta))
            self.invested = True
        elif zscore <= -1.2 and not self.invested:
            self.order('PEP', -int(10000))
            self.order('KO', int(10000*beta))
            self.invested = True
        elif abs(zscore) < .5 and self.invested:
            self.sell_spread() # exit our position
            self.invested = False # indicating that we do not hold any shares of stocks
            
            # dynamic hedge
            
    def sell_spread(self):
        """
        decrease exposure, regardless of position long/short.
        buy for a short position, sell for a long.
        """
        ko_amount = self.portfolio.positions['KO'].amount
        self.order('KO', -1 * ko_amount)
        pep_amount = self.portfolio.positions['PEP'].amount
        self.order('PEP', -1 * pep_amount)

if __name__ == '__main__':
    start = datetime(2000, 1, 1, 0, 0, 0, 0, pytz.utc)
    end = datetime(2002, 1, 1, 0, 0, 0, 0, pytz.utc)
    data = load_from_yahoo(stocks=['PEP', 'KO'], indexes={},
                           start=start, end=end)

    pairtrade = Pairtrade()
    results = pairtrade.run(data)
    data['spreads'] = np.nan

    ax1 = plt.subplot(211)
    data[['PEP', 'KO']].plot(ax=ax1)
    plt.ylabel('price')
    plt.setp(ax1.get_xticklabels(), visible=False)

    ax2 = plt.subplot(212, sharex=ax1)
    results.zscores.plot(ax=ax2, color='r')
    plt.ylabel('zscored spread')

    plt.gcf().set_size_inches(18, 8)
    
    br = trading.environment.benchmark_returns
    bm_returns = br[(br.index >= start) & (br.index <= end)]
    results['benchmark_returns'] = (1 + bm_returns).cumprod().values
    results['algorithm_returns'] = (1 + results.returns).cumprod()
    fig = plt.figure()
    ax1 = fig.add_subplot(211, ylabel='cumulative returns')
    results[['algorithm_returns', 'benchmark_returns']].plot(ax=ax1,sharex=True)
    plt.show()

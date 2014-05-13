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

from sklearn.linear_model import OrthogonalMatchingPursuit
from sklearn.linear_model import OrthogonalMatchingPursuitCV
from sklearn.datasets import make_sparse_coded_signal

from zipline.algorithm import TradingAlgorithm
from zipline.finance import trading
from zipline.transforms import batch_transform
from zipline.utils.factory import load_from_yahoo
from zipline.finance.slippage import (
    VolumeShareSlippage,
    SlippageModel,
    FixedSlippage,
    transact_partial
)
from zipline.finance.commission import PerTrade, PerShare

securities = ['JPM']
factors = ['XLF', 'XLE', 'SMH', 'XLI', 'XLK', 'XLP']

def getReturn(Price_Data):
    """
    From the pd datafram price time series we can get to the Return Data Series.
    """
    # the data has been delete the last row but to get the return from price, we have to 
    # delete another row
    R_data = Price_Data[:-1].copy()
    for i in range(0,len(R_data.index)):
        for sid in Price_Data.columns.values:
            R_data[sid][i] = (Price_Data[sid][i+1] - Price_Data[sid][i])/Price_Data[sid][i]
    return R_data


def Linear_Regression(R_data):# return data
    """
    The R_data is with nXm matrix with n observations and m factors.
    Each column will be the time series for each ticker name
    """
    # even though we change the order of getting data
    #ticker_list = R_data.columns.values
    
    #Depend_sid = ticker_list[sid1]
    #Indep_sids = ticker_list[sid2]
    sid_list = []
    for i in range(0,len(factors)):
        sid_list.append(R_data[factors[i]])
    
    Y = R_data[securities[0]]
#     del R_data[securities[0]]
#     indep = R_data.ix[:,1:len(securities)]
    indep = pd.concat(sid_list, axis=1)
    
    omp = OrthogonalMatchingPursuit(n_nonzero_coefs=len(factors), fit_intercept= True)
    omp.fit(indep, Y)
#     coef = omp.coef_
#     idx_r, = coef.nonzero()
#     X = sm.add_constant(indep, prepend=True)
#     lm_Result = sm.OLS(Y, X).fit()
    return omp
    

def get_Predict_Resi(data, Liner_Object):
    """
    calculate the residuals  by using the linear regression parameter that we passed into
    That is dS/S = alpha + beta * dI/I + dX we calculate the dX for each time.
    """
    #ticker_list = data.columns.values
    
  

    #Depend_sid = ticker_list[sid1]
    #Indep_sids = ticker_list[sid2]
    
    sid_list = []
    for i in range(0,len(factors)):
            sid_list.append(data[factors[i]])
    
    Y = data[securities[0]]
    #X = data[sid2]
    indep = pd.concat(sid_list, axis=1)
        
    resid = Y - Liner_Object.predict(indep)
    return resid

@batch_transform
def ols_transform(data):

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
    resid = get_Predict_Resi(R_data, lm)
    Cum_resid = resid.copy()
    for i in range(1,len(resid)):
        Cum_resid[i] = resid[i]+Cum_resid[i-1]
    # the last one should really close to 0 since this is the residue of the linear regression
    
#     slope1 = lm.params[1]
#     slope = lm.params[2]
#     intercept = lm.params[0]
#    resid is a pd dataframe and slop and intercept are just double 
#     intercept, slope1, slope2 = lm.params
    return [Cum_resid, lm.coef_, lm.intercept_]


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

    def initialize(self, window_length=60):
        self.spreads = []
        self.invested = 0
        self.window_length = window_length
        self.commission = PerShare(cost= 0.0005)
        self.ols_transform = ols_transform(refresh_period=1,
                                           window_length=self.window_length)
        self.instant_fill = True
        self.slippage = FixedSlippage(spread = 0)
        self.sscore =[] # store those sscores which can be regarded as the hist
#         securities = ['JPM', 'XLF', 'XLE']
        #self.slippage = VolumeShareSlippage(volume_limit = 1,price_impact = 0)
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
        params = self.ols_transform.handle_data(data)# because we @batch_transform so ther eis handle_data function in it
        if params is None:
            return
        Cum_resid = params[0]
        regr_params = params[1]

        zscore = self.compute_zscore(Cum_resid)
        self.record(zscores=zscore)
#         sid1 = 'JPM'
#         sid2 = 'XLF'
        
        self.place_orders(data, zscore, regr_params)

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
        
        return zscore

    def order_exec(self, size, dir, params):
        """ Just execs order for all the securities and all the factors/ETFs, dir = +1 (Buy), -1 (Sell/Short)"""
        for i in range(0, len(securities)):
            self.order(securities[i], dir*int(size))
        for i in range(0, len(factors)):
            self.order(factors[i], -1*dir*int(size)*params[i+1]) #the beta
            
    def order_exec_omp(self, size, dir, params):
        """ Just execs order for all the securities and all the factors/ETFs, dir = +1 (Buy), -1 (Sell/Short)"""
        for i in range(0, len(securities)):
            self.order(securities[i], dir*int(size))
        for i in range(0, len(factors)):
            self.order(factors[i], -1*dir*int(size)*params[i]) #the beta
        
    def place_orders(self, data, zscore, regr_params):
        """Buy spread if zscore is > 2, sell if zscore < .5."""
        
        self.sscore.append(zscore)
        if zscore >= 1.25 :#and not self.invested:
            if len(self.sscore)< 5 or zscore < np.max(self.sscore[len(self.sscore)-5:len(self.sscore)-1]):  
#                 self.order(securities[0], -int(2000))
#                 self.order(factors[0], int(2000*regr_params[0]))
                self.order_exec_omp(2000, -1, regr_params)
                self.invested = True 

        elif zscore <= -1.25:# and not self.invested:
            if len(self.sscore)< 5 or zscore > np.max(self.sscore[len(self.sscore)-5:len(self.sscore)-1]):
#                 self.order(sid1, int(2000))
#                 self.order(sid2, -int(2000*beta))
                self.order_exec_omp(2000, 1, regr_params)
                self.invested = True

        elif abs(zscore) < .5 and self.invested:
            self.sell_spread() # exit our position
            self.invested = False # indicating that we do not hold any shares of stocks
            self.hit_before = False

            
            # dynamic hedge
            
    def sell_spread(self):
        """
        decrease exposure, regardless of position long/short.
        buy for a short position, sell for a long.
        """
#         sad1_amount = self.portfolio.positions[sid1].amount
#         self.order(sid1, -1 * sad1_amount)
        for i in range(0, len(securities)):
            self.order(securities[i], self.portfolio.positions[securities[i]].amount*-1)
        for i in range(0, len(factors)):
            self.order(factors[i], self.portfolio.positions[factors[i]].amount*-1)

#         sad2_amount = self.portfolio.positions[sid2].amount
#         self.order(sid2, -1 * sad2_amount)

if __name__ == '__main__':
    ###########################################################################################################
    start = datetime(2006, 1, 1, 0, 0, 0, 0, pytz.utc)
    end = datetime(2008, 1, 1, 0, 0, 0, 0, pytz.utc)
    
    data = load_from_yahoo(stocks= (securities + factors), indexes={},
                           start=start, end=end)
    ## heard to save in the csv file and reget it. because the time zone and the timestampe is different
    
    
    #data.to_csv("PEP_KO_2007-2008.csv",index = True ,cols=('PEP','KO'))
    # the cols = () is order sensitive. If we save them as PEP and KO in cols them they will change the order
    # If want to load another dataset then denote these lines.
    ############################################################################################################
    #read_data = pd.DataFrame.from_csv('PEP_KO_2007-2008.csv',infer_datetime_format = True)
    #read_data.index = pd.DatetimeIndex(read_data["Date"])

    pairtrade = Pairtrade()
    results = pairtrade.run(data)
    #pd.DataFrame.join(data, results.pnl, on = 'index')
    
    result = pd.concat([results.zscores,results.orders,results.positions,results.pnl],axis = 1)
    result.to_csv("zscores_orders_position_2.csv")

    data['spreads'] = np.nan

    ax1 = plt.subplot(211)
    data[securities + factors].plot(ax=ax1)
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
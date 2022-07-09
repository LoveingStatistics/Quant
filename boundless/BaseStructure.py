#!/usr/bin/env python
# -*- encoding: utf-8 -*-
'''
@File    :   BaseStructure.py
@Time    :   2022/05/08 14:51:22
@Author  :   Yuze Chang
             Liu Junli
@Version :   1.0
@Contact :   yuzechang@uchicago.com
'''

import json
import os
import time as tm
import warnings
from typing import List, Tuple, Union, Dict

import numbagg
import numpy as np
import pandas as pd
import pyecharts.options as opts
import statsmodels.api as sm
import xarray as xr
from pyecharts.charts import Bar, Line, Page, Tab

warnings.filterwarnings("ignore")


class BaseDataSet(object):
    def __init__(self, setting: dict):
        self.start_date: int = setting['start_date']
        self.end_date: int = setting['end_date']
        self.daily_lag: bool = setting['daily_lag']
        self.market: xr.DataArray = None
        self.Return: xr.DataArray = None
        self.flag: xr.DataArray = None
        self.price_ipo_st_flag: xr.DataArray = None
        self.risk_factor: xr.DataArray = None
        self.industry_dummies: xr.DataArray = None
        self.industry: xr.DataArray = None
        self.capital: xr.DataArray = None
        self.benchmark: xr.DataArray = None
        self.transaction: xr.DataArray = None
        self.index: xr.DataArray = None
        self.y: xr.DataArray = None
        self.prepare_data(setting)

    @staticmethod
    def get_tradingday_range(start_date: int, end_date: int, trade_calendar_path: Union[str, None] =  None, timeWindow: int = 1, delay: int = 0) -> List[int]:
        if trade_calendar_path is None:
            trade_calendar_path = setting['daily_data_path']
        tradeDate = pd.read_csv(os.path.join(trade_calendar_path, 'trade_calendar.csv'))['TRADE_DAYS']
        endIndex = tradeDate[np.array(tradeDate <= end_date)].index[-1]
        if start_date == end_date:
            startIndex = endIndex
        else:
            startIndex = tradeDate[np.array(tradeDate >= start_date)].index[0]
        if timeWindow > 1:
            startIndex -= (timeWindow - 1)
        elif timeWindow < 0:
            endIndex -= (timeWindow)
        if delay:
            startIndex -= delay
            endIndex -= delay
        date_range = tradeDate.loc[startIndex:endIndex]
        return list(date_range.values.flatten())

    @staticmethod
    def read_daily_data(data_path: str,
                        date_range: List[int],
                        yesterday_range: List[int],
                        file_name: str,
                        variable_name: str,
                        ticker_list=None,
                        time: int = 145900) -> xr.DataArray:
        if yesterday_range is None:
            date_range_str = ['S_INFO_WINDCODE']
            date_range_str.extend([str(date) for date in date_range])
            data = pd.read_csv(os.path.join(data_path, file_name), index_col=0, usecols=date_range_str)
            data.columns = date_range
        else:
            yesterday_range_str = ['S_INFO_WINDCODE']
            yesterday_range_str.extend([str(date) for date in yesterday_range])
            data = pd.read_csv(os.path.join(data_path, file_name), index_col=0, usecols=yesterday_range_str)
            data.columns = date_range
        data.columns.name = 'date'
        data.index.name = 'ticker'
        data = data.stack().to_xarray()
        data = data.expand_dims('variable').expand_dims('time')
        data['variable'] = [variable_name]
        data['time'] = [time]
        if ticker_list is not None:
            data = data.reindex({'ticker': ticker_list})
        data = data.transpose('ticker', 'date', 'time', 'variable')
        return data

    @staticmethod
    def read_minute_data(data_path: str,
                         date_range: List[int] = None,
                         ticker_list: List[int] = None,
                         fields: List[str] = None,
                         timelist: List[int] = None) -> xr.DataArray:
        """read_minute_data _summary_

        静态函数, 读取一定日期范围内DataArray的h5文件.

        Args:
            data_path (str): h5路径.
            date_range (List[int], optional): 若非空, 读取指定日期范围的文件, 空值则读取路径下的所有日期. Defaults to None.
            ticker_list (List[int], optional): 若非空, 则用ticker_list 'Reindex' DataArray的ticker维度. Defaults to None.
            fields (List[str], optional): 若非空,读取指定的variable. Defaults to None.
            timelist (List[int], optional): 若非空,对于存在time维度,且长度大于1的DataArray,对其time维度Reindex. Defaults to None.

        Returns:
            xr.DataArray: 按日期叠加在一起的DataArray.
        """

        Data = None
        if date_range is None:
            date_range = [int(file.split('.')[0]) for file in sorted(os.listdir(data_path))]
        for date in date_range:
            if fields is None:
                data = xr.open_dataarray(os.path.join(data_path, f'{date}.h5'), engine='netcdf4')
            else:
                data = xr.open_dataarray(os.path.join(data_path, f'{date}.h5'), engine='netcdf4').sel(variable=fields)
            if Data is None:
                Data = data
            else:
                Data = xr.concat([Data, data], dim='date')
        if ticker_list is not None:
            Data: xr.DataArray = Data.reindex({'ticker': ticker_list})
        if ('time' in Data.dims) and (len(Data['time']) > 1) and (timelist is not None):
            Data = Data.reindex({'time': timelist})
        return Data

    @staticmethod
    def prepare_style(data_path: str, riskList: List[int], date_range: List[int], yesterday_range, ticker_list: List[int]) -> xr.DataArray:
        risk_factor_path = os.path.join(data_path, f'risk_factors.h5')
        risk_factor = None
        for i in range(len(riskList)):
            if yesterday_range is None:
                data = pd.read_hdf(risk_factor_path, key=riskList[i])[date_range]
            else:
                yesterday_range_str = [str(date) for date in yesterday_range]
                data = pd.read_hdf(risk_factor_path, key=riskList[i])[yesterday_range_str]
                data.columns = date_range
            data.columns.name = 'date'
            data.index.name = 'ticker'
            data = data.stack().to_xarray()
            data = data.expand_dims('time').expand_dims('variable')
            data['time'] = [145900]
            data['variable'] = [riskList[i]]
            if risk_factor is None:
                risk_factor = data
            else:
                risk_factor = xr.concat([risk_factor, data], dim='variable')
        risk_factor = risk_factor.transpose('ticker', 'date', 'time', 'variable')
        risk_factor = risk_factor.reindex({'ticker': ticker_list})
        return risk_factor

    @staticmethod
    def prepare_industry_dummies(data_path: str, date_range: List[int], ticker_list: List[int]) -> xr.DataArray:
        industry_data_path = os.path.join(data_path, 'industry.csv')
        data = pd.read_csv(industry_data_path, index_col=0)
        data.columns.name = 'variable'
        data.index.name = 'ticker'
        data = data.stack().to_xarray()
        data = data.expand_dims('date').expand_dims('time')
        data['date'] = [date_range[0]]
        data['time'] = [145900]
        data = data.transpose('ticker', 'date', 'time', 'variable')
        data = data.reindex({'ticker': ticker_list}).reindex({'date': date_range}, method='ffill')
        return data

    @staticmethod
    def prepare_industry(data_path: str, date_range: List[int], ticker_list: List[int]) -> xr.DataArray:
        industry_data_path = os.path.join(data_path, 'industry.csv')
        industry_flag = pd.read_csv(industry_data_path, index_col=0)
        ticker, ind = np.where(industry_flag == 1)
        data = pd.DataFrame(ind + 1, industry_flag.index[ticker])
        data.columns = ['industry']
        data.index.name = 'ticker'
        data.columns.name = 'variable'
        data = data.stack().to_xarray()
        data = data.expand_dims('date').expand_dims('time')
        data['date'] = [date_range[0]]
        data['time'] = [145900]
        data = data.transpose('ticker', 'date', 'time', 'variable')
        data = data.reindex({'ticker': ticker_list}).reindex({'date': date_range}, method='ffill')
        return data

    def select_date(self, Return_path: str = None):
        # 默认Return_path里面的日期是连续的
        if Return_path is None:
            if ('BaseDataSet_path' in setting.keys()) and (setting['BaseDataSet_path'] is not None):
                Return_path = os.path.join(setting['BaseDataSet_path'], 'Return')
            else:
                Return_path = os.path.join(setting['dst_dir'], 'BaseDataSet', 'Return')
        read_st_ed = []
        if os.path.exists(Return_path):
            file_list = sorted(os.listdir(Return_path))
            if file_list != []:
                start, end = int(file_list[0].split('.')[0]), int(file_list[-1].split('.')[0])
                if (start <= self.start_date) & (end >= self.next_date):
                    read_st_ed = [self.start_date, self.next_date]
                elif (start <= self.start_date) & (end < self.next_date):
                    read_st_ed = [self.start_date, end]
                elif (start > self.start_date) & (end >= self.next_date):
                    read_st_ed = [start, self.next_date]
        if read_st_ed != []:
            read_range = BaseDataSet.get_tradingday_range(start_date=read_st_ed[0],end_date=read_st_ed[1])
        else:
            read_range = []
        remain_range = list(pd.Series(self.date_range).loc[~pd.Series(self.date_range).isin(pd.Series(read_range))])
        return read_range, remain_range

    @staticmethod
    def prepare_market(market_path: str,
                       date_range: List[int],
                       ticker_list: List[int],
                       fields,
                       market_processed_path: str = None,
                       timelist: List[int] = None) -> xr.DataArray:
        # 先读取trade_calendar_path: str = setting['trade_calendar_path']
        # 市场数据会默认多读一天
        # next_date = BaseDataSet.get_tradingday_range(trade_calendar_path, start_date = date_range[0], end_date = date_range[0], delay = -1)
        # new_date_range = date_range.append(next_date)
        market_data = BaseDataSet.read_minute_data(data_path=market_path,
                                                   date_range=date_range,
                                                   ticker_list=ticker_list,
                                                   fields=fields,
                                                   timelist=timelist)
        # 用新的ticker_list读前一天的数据
        # data_path0是上一段数据的存储路径
        previous_date = BaseDataSet.get_tradingday_range(start_date=date_range[0], end_date=date_range[0], delay=1)

        if market_processed_path is None:
            if ('market_processed_path' in setting.keys()) and (setting['market_processed_path'] is not None):
                market_processed_path = setting['market_processed_path']
            else:
                market_processed_path = os.path.join(setting['dst_dir'], 'BaseDataSet', 'market_processed')

        if os.path.exists(os.path.join(market_processed_path, f'{previous_date[0]}.h5')):
            market_data_previous = BaseDataSet.read_minute_data(data_path=market_processed_path,
                                                                date_range=previous_date,
                                                                ticker_list=ticker_list,
                                                                fields=fields,
                                                                timelist=timelist)
        else:
            market_data_previous = BaseDataSet.read_minute_data(data_path=market_path,
                                                                date_range=previous_date,
                                                                ticker_list=ticker_list,
                                                                fields=fields,
                                                                timelist=timelist)
        market_data = xr.concat([market_data, market_data_previous], dim='date')
        market_data = market_data.sortby('date', ascending=True)
        
        market_data = xr.where(market_data==0.0,np.nan,market_data)
        market_data.loc[:,:,:,'Close'] = market_data.loc[:,:,:,'Close'].stack(datetime = ('date','time')).ffill(dim = 'datetime').unstack()
        market_data.loc[:, :, 92500, 'preDayClose'] = market_data.loc[:, :, :, 'preDayClose'].stack(datetime=('date', 'time')).fillna(
            market_data.loc[:, :, :, 'Close'].stack(datetime=('date', 'time')).shift(datetime=1)).unstack().loc[:, :, 92500]
        market_data.loc[:, :, :, 'preDayClose'] = market_data.loc[:, :, :, 'preDayClose'].ffill(dim='time')
        market_data = market_data[:, 1:, :, :]
        # market_data = market_data.drop_isel(date = 0) # 填充完成后删除前一天的数据 #!服务器xarray的版本没有这个函数
        return market_data

    def prepare_return(self, market_data: xr.DataArray) -> xr.DataArray:
        # timelist 每天固定的几个时间点,计算这些时间点之间的Return.
        # 可以在后续改变时间点时重新调用计算Return
        Return = xr.zeros_like(market_data).reindex(variable=['MinuteReturn', 'AccumulatedReturn'])
        for itime in self.timelist:
            if itime == self.timelist[0]:
                Return.loc[:, :, itime, 'MinuteReturn'] = market_data.loc[:, :, itime, 'Close'] / market_data.loc[:, :, itime, 'preDayClose'] - 1
            else:
                Return.loc[:, :, itime, 'MinuteReturn'] = market_data.loc[:, :, itime, 'Close'] / market_data.loc[:, :, lasttime, 'Close'] - 1
            lasttime = itime
        # Return.loc[:,:,:,'AccumulatedReturn'] = (np.exp(np.log(1 + Return.loc[:,:,:,'MinuteReturn'].fillna(0).stack(datetime=('date', 'time'))).rolling(datetime=predict_period).sum()) - 1).shift(datetime=-(predict_period + lag_period)).unstack()
        Return.loc[:, :, :, 'AccumulatedReturn'] = market_data.loc[:, :, :, 'Close'] / market_data.loc[:, :, self.timelist[0], 'preDayClose']
        return Return

    @staticmethod
    def prepare_price_flag(market_data: xr.DataArray) -> xr.DataArray:
        close = market_data.loc[:, :, :, 'Close']
        preDayClose = market_data.loc[:, :, 145900, 'preDayClose']  # 填充market时,所有的preDayClose空值都被填充了
        change = close / preDayClose.expand_dims('variable')
        change['variable'] = ['price_flag']
        gem_star = [True if (i[0:3] == '300') | (i[0:3] == '688') else False for i in change['ticker'].values]
        cond1 = xr.DataArray(gem_star, dims=['ticker'])
        cond2 = xr.DataArray(~np.array(gem_star), dims=['ticker'])
        price_flag = xr.where((cond2 & (change < 1.1) & (change > 0.9)) | (cond1 & (change < 1.2) & (change > 0.8)), 1, 0)
        return price_flag

    @staticmethod
    def prepare_risk_flag(risk_factor: xr.DataArray) -> xr.DataArray:
        risk_flag = xr.where(risk_factor.isnull(), 0, 1)
        risk_flag = risk_flag.prod('variable').expand_dims('variable')
        risk_flag['variable'] = ['risk_flag']
        risk_flag = risk_flag.transpose('ticker', 'date', 'time', 'variable')
        return risk_flag

    @staticmethod
    def prepare_daily_flag(daily_data: xr.DataArray, variable_name: str) -> xr.DataArray:
        daily_flag = xr.where(daily_data.isnull(), 0, 1)
        daily_flag['variable'] = [variable_name]
        return daily_flag

    @staticmethod
    def save_DataArray(DataArrayDict: dict,
                       save_path,
                       byDate: bool = True,
                       separateFolder: bool = True,
                       override: bool = False,
                       datelist: List[int] = None):
        if not os.path.exists(save_path):
            os.makedirs(save_path)
        if byDate:
            for dataname, data in DataArrayDict.items():
                if (~separateFolder) & (len(DataArrayDict) == 1):
                    isave_path = save_path
                else:
                    isave_path = os.path.join(save_path, dataname)
                if not os.path.exists(isave_path):
                    os.makedirs(isave_path)
                if datelist is None:
                    datelist = data['date'].values
                for idate in datelist:
                    idate_path = os.path.join(isave_path, f'{idate}.h5')
                    if (not override) and os.path.exists(idate_path):
                        continue
                    data.sel(date=[idate]).to_netcdf(path=idate_path)
        else:
            for dataname, data in DataArrayDict.items():
                data.to_netcdf(path=os.path.join(save_path, f'{dataname}.h5'))

    @staticmethod
    def concat_on_dim(Array, array, dim):
        if Array is None:
            Array = array
        else:
            Array = xr.concat([Array, array], dim=dim)
        return Array

    def creat_dataarray(self, var: List[str]) -> xr.DataArray:
        return xr.DataArray(np.nan, coords=[self.ticker_list, self.date_range, self.timelist, var], dims=['ticker', 'date', 'time', 'variable'])

    def save_cache(self, varlist, save_path: str = None, datelist: List[int] = None):
        # save时需要写入一些东西
        if save_path is None:
            if ('BaseDataSet_path' in setting.keys()) and (setting['BaseDataSet_path'] is not None):
                save_path = setting['BaseDataSet_path']
            else:
                save_path = os.path.join(setting['dst_dir'], 'BaseDataSet')
        # 还不知道怎么更新
        # with open(os.path.join(save_path,'info.json'), 'w') as file:
        #     json.dump(setting, file)

        for var in varlist:
            BaseDataSet.save_DataArray({var: self.__dict__[var]}, save_path, byDate=True, datelist=datelist, separateFolder=True)

    def load_cache(self, read_range, varlist_read: str, read_path: str = None):
        if read_path is None:
            if ('BaseDataSet_path' in setting.keys()) and (setting['BaseDataSet_path'] is not None):
                read_path = setting['BaseDataSet_path']
            else:
                read_path = os.path.join(setting['dst_dir'], 'BaseDataSet')
        # 还没想好怎么改
        # with open(os.path.join(read_path, file, 'info.json')) as file:
        #     info = json.load(file)

        if os.path.exists(read_path):
            for var in varlist_read:
                self.__dict__[var] = BaseDataSet.concat_on_dim(self.__dict__[var],
                                                               BaseDataSet.read_minute_data(data_path=os.path.join(read_path, var),
                                                                                            date_range=read_range,
                                                                                            ticker_list=self.ticker_list,
                                                                                            timelist=self.timelist),
                                                               dim='date').sortby('date', ascending=True)

    def prepare_data(self, setting: dict):
        """prepare_data _summary_
        1. date_range, yeaterday_range, ticker_list
        2. 选择已经有数据的read_range, remain_range
        3. 读read_range
        4. 重新整理remain_range
        5. 将3和4读的数据concat在一起并save
        Args:
            setting (dict): 全局变量
        """
        # 获取时间范围
        # 根据起止日期，获取交易日列表
        self.next_date = BaseDataSet.get_tradingday_range(start_date=self.end_date,end_date=self.end_date,delay=-1)[0]
        self.date_range = BaseDataSet.get_tradingday_range(start_date=self.start_date,end_date=self.next_date)
        if self.daily_lag:
            # 由于日频数据需要滞后一天处理，获取前一个交易日列表
            self.yesterday_range = list(
                int(yesterday) for yesterday in BaseDataSet.get_tradingday_range(start_date=self.start_date, end_date=self.next_date, delay=1))
        else:
            self.yesterday_range = None
        # 获取股票范围
        # 先读取univ flag，并据此获取ticker_list
        self.univ_flag = BaseDataSet.read_daily_data(data_path=setting['daily_data_path'],
                                                     date_range=self.date_range,
                                                     yesterday_range=self.yesterday_range,
                                                     file_name=setting['univ_flag_file_name'],
                                                     variable_name='univ_flag',
                                                     ticker_list=None).fillna(0)
        self.ticker_list = list(self.univ_flag['ticker'].values)
        # 读取timelist
        if ('timelist' in setting.keys()) and (setting['timelist'] is not None):
            self.timelist = setting['timelist']
        else:
            self.timelist = pd.read_csv(os.path.join(setting['timelist_path'], setting['timelist_file_name']), header=None)[0].values.tolist()

        if ('varlist' in setting.keys()) and (setting['varlist'] is not None):
            varlist_read = setting['varlist'].copy()
        else:
            varlist_read = ['market', 'Return', 'risk_factor', 'industry_dummies', 'industry', 'capital', 'benchmark', 'flag', 'price_ipo_st_flag']
        # 直接读取的话，需要啥数据直接读取啥数据，需要计算的部分，再额外加
        read_range, remain_range = self.select_date()
        print('已经有的日期:%s' % read_range)
        print('需要加的日期:%s' % remain_range)
        if read_range != []:
            self.load_cache(read_range, varlist_read)
            print('已有日期已经读取')
        if remain_range != []:
            varlist_prepare: List[str] = varlist_read.copy()
            if 'Return' in varlist_read:
                varlist_prepare.append('market')
            if 'industry' in varlist_read:
                varlist_prepare.append('industry_dummies')
            if 'flag' in varlist_read:
                varlist_prepare.extend(['market', 'risk_factor', 'industry_dummies', 'capital', 'benchmark'])
            if 'price_ipo_st_flag' in varlist_read:
                varlist_prepare.extend(['market'])
            varlist_prepare = list(set(varlist_prepare))
            remainVar = {}
            if self.daily_lag:
                # 由于日频数据需要滞后一天处理，获取前一个交易日列表
                remain_yesterday_range = list(
                    int(yesterday) for yesterday in BaseDataSet.get_tradingday_range(start_date=remain_range[0], end_date=remain_range[-1], delay=1))
            else:
                remain_yesterday_range = None
            if 'market' in varlist_prepare:
                remainVar.update(market=BaseDataSet.prepare_market(market_path=setting['market_path'],
                                                                   date_range=remain_range,
                                                                   ticker_list=self.ticker_list,
                                                                   fields=setting['market_fields'],
                                                                   timelist=self.timelist))
                if 'Return' in varlist_prepare:
                    remainVar.update(Return=self.prepare_return(market_data=remainVar['market']))
            # 从h5文件中，读取需要的分钟线交易数据
            if 'transaction' in varlist_prepare:
                remainVar.update(transaction=BaseDataSet.read_minute_data(data_path=setting['transaction_path'],
                                                                          date_range=remain_range,
                                                                          ticker_list=self.ticker_list,
                                                                          fields=setting['transaction_fields'],
                                                                          timelist=self.timelist))
            # 从h5文件中，读取需要的分钟线index数据
            if 'index' in varlist_prepare:
                remainVar.update(index=BaseDataSet.read_minute_data(data_path=setting['index_path'],
                                                                    date_range=remain_range,
                                                                    ticker_list=self.ticker_list,
                                                                    fields=setting['index_fields'],
                                                                    timelist=self.timelist))
            # 读取风险因子并进行格式处理（日频）                   #路径可能单独存放
            if 'risk_factor' in varlist_prepare:
                remainVar.update(risk_factor=BaseDataSet.prepare_style(data_path=setting['risk_factor_path'],
                                                                       riskList=setting['risk_list'],
                                                                       date_range=remain_range,
                                                                       yesterday_range=remain_yesterday_range,
                                                                       ticker_list=self.ticker_list))
            # 读取股票行业哑变量并进行格式处理（之后会转为日频，目前为单一值）
            if 'industry_dummies' in varlist_prepare:
                remainVar.update(industry_dummies=BaseDataSet.prepare_industry_dummies(
                    data_path=setting['daily_data_path'], date_range=remain_range, ticker_list=self.ticker_list))
            # 读取股票行业并进行格式处理（之后会转为日频，目前为单一值
            if 'industry' in varlist_prepare:
                remainVar.update(industry=BaseDataSet.prepare_industry(
                    data_path=setting['daily_data_path'], date_range=remain_range, ticker_list=self.ticker_list))
            # 读取股票市值并进行格式处理（日频
            if 'capital' in varlist_prepare:
                remainVar.update(capital=BaseDataSet.read_daily_data(data_path=setting['daily_data_path'],
                                                                     date_range=remain_range,
                                                                     yesterday_range=remain_yesterday_range,
                                                                     file_name='weight.csv',
                                                                     variable_name='capital',
                                                                     ticker_list=self.ticker_list))
            # 读取benchmark并进行格式处理（日频
            if 'benchmark' in varlist_prepare:
                remainVar.update(benchmark=BaseDataSet.read_daily_data(data_path=setting['daily_data_path'],
                                                                       date_range=remain_range,
                                                                       yesterday_range=remain_yesterday_range,
                                                                       file_name='W_bm_zz_500_close.csv',
                                                                       variable_name='benchmark',
                                                                       ticker_list=self.ticker_list,
                                                                       time=92500))

            if ('flag' in varlist_prepare) or ('price_ipo_st_flag' in varlist_prepare):
                flag: xr.DataArray = self.univ_flag.sel(date=remain_range).reindex(time=self.timelist, method='bfill')
                flag['variable'] = ['flag']
                if 'price_flag' in setting['flag_list']:
                    price_flag = BaseDataSet.prepare_price_flag(market_data=remainVar['market'])
                    flag.values = flag.values * price_flag.values
                # 读取ipo_st flag（存在缺失值，需填充为0）
                if 'ipo_st_flag' in setting['flag_list']:
                    ipo_st_flag = BaseDataSet.read_daily_data(data_path=setting['daily_data_path'],
                                                              date_range=remain_range,
                                                              yesterday_range=remain_yesterday_range,
                                                              file_name=setting['ipo_st_flag_file_name'],
                                                              variable_name='ipo_st_flag',
                                                              ticker_list=self.ticker_list).fillna(0)
                    flag.values = flag.values * ipo_st_flag.values

                if 'price_ipo_st_flag' in varlist_prepare:
                    price_ipo_st_flag = flag.copy()
                    remainVar.update(price_ipo_st_flag=price_ipo_st_flag)
                if 'flag' in varlist_prepare:
                    # 计算风险因子flag（日频，任一因子缺失即flag=0）
                    if 'risk_flag' in setting['flag_list']:
                        risk_flag = BaseDataSet.prepare_risk_flag(risk_factor=remainVar['risk_factor'])
                        flag.values = flag.values * risk_flag.values
                    # 读取行业flag
                    if 'industry_flag' in setting['flag_list']:
                        industry_flag = BaseDataSet.prepare_daily_flag(daily_data=remainVar['industry'], variable_name='industry_flag')
                        flag.values = flag.values * industry_flag.values
                    # 读取市值flag
                    if 'capital_flag' in setting['flag_list']:
                        capital_flag = BaseDataSet.prepare_daily_flag(daily_data=remainVar['capital'], variable_name='capital_flag')
                        flag.values = flag.values * capital_flag.values
                    remainVar.update(flag=flag)

            print('需加日期已经准备')
            for var in varlist_read:
                self.__dict__[var] = BaseDataSet.concat_on_dim(self.__dict__[var], remainVar[var], dim='date')
            print('合并完成')
            self.save_cache(varlist=varlist_read, datelist=remain_range)
            print('保存新加日期')
        self.date_range = self.date_range[:-1]
        if 'market' in varlist_read:
            varlist_read.remove('market')
        for var in varlist_read:
            self.__dict__[var] = self.__dict__[var].reindex(date=self.date_range)

    def datetime_lag(self, array: xr.DataArray, lag: int = 2) -> xr.DataArray:

        time925_930 = [92600, 92700, 92800, 92900]
        time1456_1459 = [145700, 145800]
        time1129_ = [time * 100 for time in range(1130, 1130 + abs(lag))]
        time145900_ = [time * 100 for time in range(1500, 1500 + abs(lag))]
        timelist_pro: list = list(set(self.timelist + time925_930 + time1129_ + time1456_1459 + time145900_))
        timelist_pro.sort()  #升序排列

        array = array.reindex({'time': timelist_pro}).stack(datetime=['date', 'time'])
        if lag > 0:
            array = array.ffill(dim='datetime')
        else:
            array = array.bfill(dim='datetime')
        array_lag = array.shift(datetime=lag).unstack().reindex({'time': self.timelist}).transpose('ticker','date','time','variable')
        return array_lag.transpose('ticker','date','time','variable')

    def lookup_date(date_range: List[int], filepath: str):
        if (os.path.exists(filepath)) and (os.listdir(filepath) != []):
            datelist = sorted([int(ifile.split('.')[0]) for ifile in os.listdir(filepath)])
            date_load = list(set(date_range) & set(datelist))
            date_calc = list(set(date_range) - set(datelist))
        else:
            date_load = []
            date_calc = date_range

        return date_load, date_calc

    def calc_y(self, lag: int) -> xr.DataArray:
        """calc_y _summary_
        这个函数只需要完成y的计算即可.输入dataset,主要是用dataset.market来算y.输出y

        Args:
            lag (int): 滞后阶数,由上层函数传入

        Returns:
            _type_: y. xr.DataArray
        """
        mkd = self.market.sel(variable='Close')
        # 创建容器
        pre_return: xr.DataArray = self.creat_dataarray(var=['low', 'high'])
        low: xr.DataArray = self.creat_dataarray(var=['low_today', 'low_next'])
        # high
        pre_return.loc[:, :, :, 'high'] = mkd.shift(date=-1).dropna(dim='date', how='all').max(dim='time', skipna=True) / mkd - 1
        # low_next
        low.loc[:, :, :, 'low_next'] = mkd.shift(date=-1).dropna(dim='date', how='all').min(dim='time', skipna=True) / mkd - 1
        # low_today
        for itime in pre_return['time']:
            low.loc[:, :, itime, 'low_today'] = (mkd.loc[:, :, itime:].min(dim='time', skipna=True) / mkd.sel(time=itime) - 1)[:, :-1]
        pre_return.loc[:, :, :, 'low'] = low.min(dim='variable')
        # '滞后'处理
        pre_return = self.datetime_lag(pre_return, lag)

        return pre_return

    def prepare_y(self,
                  lag: int = 2,
                  y: Union[xr.DataArray, None] = None,
                  y_name: str = 'y',
                  y_path: Union[str, None] = None,
                  dim: Union[str, None] = None,
                  save_path: str = None):
        """calc_y 说明
        这个函数计算回归用的y值,并保存.是从market计算的,就是分钟先数据, Close, preDayClose之类的.
        这个函数往dataset中加一些新的变量(不仅仅是y),覆盖或者concat到已有的变量,按date读取.
    
        y: 要加到self上的变量值,默认不给出,从y_path读取;
        y_name: 要新增的变量的名字, 默认新增变量名为'y';
        y_path: 新增变量存放的路径,默认不给出,从setting里读取,若setting里也没有,从全局默认路径读取;
        dim: 给出该参数,则按该维度concatenate到已有变量上, 不给出该维度则覆盖原来的变量;
        """
        # 直接给定y,
        # y_path, 默认的path
        # 先读取, 后计算
        if save_path is None:
            if ('BaseDataSet_path' in setting.keys()) and (setting['BaseDataSet_path'] is not None):
                save_path = setting['BaseDataSet_path']
            else:
                save_path = os.path.join(setting['dst_dir'], 'BaseDataSet')
        
        if lag is None:
            lag = -setting['lag']  #lag应该为负数，'提前'而不是'滞后'

        if y is None:
            if y_path is None:
                if (y_name + '_path' in setting.keys()) and (setting[y_name + '_path'] is not None):
                    y_path = setting[y_name + '_path']
                elif ('BaseDataSet_path' in setting.keys()) and (setting['BaseDataSet_path'] is not None):
                    y_path = os.path.join(setting['BaseDataSet_path'], y_name)
                else:
                    y_path = os.path.join(setting['dst_dir'], 'BaseDataSet',y_name)
            date_load, date_calc = BaseDataSet.lookup_date(self.date_range, filepath=y_path)
            print('已经有的日期:%s' % date_load)
            print('需要加的日期:%s' % date_calc)
            y = None
            if date_load != []:
                y = BaseDataSet.read_minute_data(y_path, date_range=date_load)
            if date_calc != []:
                y = BaseDataSet.concat_on_dim(y, self.calc_y(lag=lag), dim='date')
            y = y.reindex(time=self.timelist, date=self.date_range, ticker=self.ticker_list)

        if dim is None:
            self.__dict__[y_name] = y
        else:
            self.__dict__[y_name] = BaseDataSet.concat_on_dim(self.__dict__[y_name], y, dim=dim)
        BaseDataSet.save_DataArray({y_name: y}, save_path, byDate=True, separateFolder=True)
        print('Saved here%s' % save_path)

    def export_variables(self):
        variables_map = {k: v['variable'].values.tolist() for k, v in self.__dict__.items() if isinstance(v, xr.DataArray)}
        return variables_map


class BaseAlphaSet(object):
    def __init__(self, setting: dict):
        self.start_date = setting['start_date']
        self.end_date = setting['end_date']
        self.alpha_names = []
        self.turnover = None
        self.sort = None
        self.alpha_autocorr = None
        self.alpha: xr.DataArray = None
        self.residual: dict = {}
        self.__prepare(setting)

    def __prepare(self, setting):
        self.date_range = BaseDataSet.get_tradingday_range(start_date=self.start_date,end_date=self.end_date)
        univ_flag = BaseDataSet.read_daily_data(data_path=setting['daily_data_path'],
                                                date_range=self.date_range,
                                                yesterday_range=None,
                                                file_name=setting['univ_flag_file_name'],
                                                variable_name='univ_flag',
                                                ticker_list=None).fillna(0)
        self.ticker_list = list(univ_flag['ticker'].values)
        # 读取timelist
        if ('timelist' in setting.keys()) and (setting['timelist'] is not None):
            self.timelist = setting['timelist']
        else:
            self.timelist = pd.read_csv(os.path.join(setting['timelist_path'], setting['timelist_file_name']), header=None)[0].values.tolist()

    def calc_alpha(self, dataset: BaseDataSet, period: int = 30):
        raw_data = dataset.transaction.fillna(0)
        Amount = (raw_data.sel(variable=['smallB', 'smallS', 'mediumB', 'mediumS', 'bigB', 'bigS', 'hugeB', 'hugeS'])
                          .sum(dim='variable').stack(datetime=['date','time'])
                          .rolling_exp(window={'datetime': period}).mean().unstack())
        raw_data = (raw_data.stack(datetime=('date', 'time')).rolling_exp(window={'datetime': period})
                            .mean().unstack().transpose('ticker', 'date', 'time', 'variable'))
        
        mainS1 = (raw_data.sel(variable='mainB') - raw_data.sel(variable='mainS')) / Amount
        nonmainS1 = (raw_data.sel(variable='nonmainB') - raw_data.sel(variable='nonmainS')) / Amount
        alpha = xr.concat([mainS1, nonmainS1], dim='variable').fillna(0)
        alpha['variable'] = ['mainS1', 'nonmainS1']

        Return = (np.exp(
            np.log(1 + dataset.Return.sel(variable='MinuteReturn').fillna(0).stack(datetime=('date', 'time'))).rolling(datetime=period).sum()) -
                  1).unstack().fillna(0)
        flag = dataset.flag.squeeze(dim='variable').fillna(0)

        def __ols(alpha, Return, flag):
            Return = sm.add_constant(Return)
            resid = sm.WLS(alpha, Return, weights=flag).fit().resid
            # resid = LinearRegressionFitter.OLS(alpha, Return, flag)['residual']
            return np.array(np.squeeze(resid))

        resid = xr.apply_ufunc(__ols, alpha, Return, flag,
                               input_core_dims=[['ticker'], ['ticker'], ['ticker']],
                               output_core_dims=[['ticker']],
                               vectorize=True,
                               dask="parallelized",
                               output_dtypes=[alpha.dtype])
        return resid.transpose('ticker', 'date', 'time', 'variable')

    def prepare_alpha(self,
                      dataset: Union[BaseDataSet, None] = None,
                      alpha: Union[xr.DataArray, None] = None,
                      alpha_path: Union[str, None] = None,
                      save_path: Union[str, None] = None,
                      alpha_names: Union[List[str], None] = None,
                      dim: Union[str, None] = None, #str: 'time', 'variable'
                      **kwargs):
        """prepare_alpha _summary_

        该函数读取alpha, 或者计算alpha. 也可以把新的alpha加到已经有的alphaset上. 加进来residual使用add_alpha

        Args:
            dataset (Union[BaseDataSet, None], optional): _description_. Defaults to None.
            alpha (Union[xr.DataArray, None], optional): _description_. Defaults to None.
            alpha_path (Union[str, None], optional): _description_. Defaults to None.
            save_path (Union[str, None], optional): _description_. Defaults to None.
            alpha_names (Union[List[str], None], optional): _description_. Defaults to None.
            dim (Union[str, None], optional): _description_. Defaults to None.
        """

        if save_path is None:
            if ('BaseAlphaSet_path' in setting.keys()) and (setting['BaseAlphaSet_path'] is not None):
                save_path = setting['BaseAlphaSet_path']
            else:
                save_path = os.path.join(setting['dst_dir'], 'BaseAlphaSet')

        if alpha is None:
            if alpha_path is None:
                if ('alpha_path' in setting.keys()) and (setting['alpha_path'] is not None):
                    alpha_path = setting['alpha_path']
                elif ('BaseAlphaSet_path' in setting.keys()) and (setting['BaseAlphaSet_path'] is not None):
                    alpha_path = os.path.join(setting['BaseAlphaSet_path'], 'alpha')
                else:
                    alpha_path = os.path.join(setting['dst_dir'], 'BaseAlphaSet', 'alpha')
            date_load, date_calc = BaseDataSet.lookup_date(self.date_range, filepath=alpha_path)
            alpha = None
            if date_load != []:
                alpha = BaseDataSet.read_minute_data(alpha_path, date_range=date_load, fields=alpha_names)
            if date_calc != []:
                if dataset is None:
                    print('需要dataset中的transaction计算因子值')
                alpha = self.calc_alpha(dataset, **kwargs)
        alpha = alpha.reindex(time=self.timelist, date=self.date_range, ticker=self.ticker_list)

        if dim is None:
            self.alpha = alpha
            self.alpha_names = list(alpha['variable'].values)
        else:
            self.alpha = BaseDataSet.concat_on_dim(self.alpha, alpha, dim=dim)
            self.alpha_names = list(set(alpha['variable'].values) & set(self.alpha_names))
        BaseDataSet.save_DataArray({'alpha': alpha}, save_path, byDate=True, separateFolder=True)
        print('Alpha Saved here%s' % save_path)

    def add_alpha(self,
                  alphas_array: Union[xr.DataArray, None] = None,
                  alpha_names: Union[List[str], None] = None,
                  alpha_dir: str = None,
                  residual_dir: dict = None,
                  residuals_array: Union[dict, None] = None,
                  dim: str = 'variable'):
        def concat_on_dim(Array, array, dim):
            if Array is None:
                Array = array
            else:
                Array = xr.concat([Array, array], dim=dim)
            return Array

        if residuals_array is not None:
            self.residual.update(residuals_array)
        if residual_dir is not None:
            for iFitter_name, iResidual_dir in residual_dir.items():
                iResiduals_array = BaseDataSet.read_minute_data(data_path=iResidual_dir,
                                                                date_range=self.date_range,
                                                                ticker_list=self.ticker_list,
                                                                fields=alpha_names,
                                                                timelist=self.timelist)
                self.residual.update({iFitter_name: iResiduals_array})
        if alphas_array is not None:
            self.alpha = concat_on_dim(self.alpha, alphas_array, dim=dim)
            self.alpha_names.extend(list(alphas_array['variable'].values))
        if alpha_dir is not None:
            alphas_array = BaseDataSet.read_minute_data(data_path=alpha_dir,
                                                        date_range=self.date_range,
                                                        ticker_list=self.ticker_list,
                                                        fields=alpha_names,
                                                        timelist=self.timelist)
            self.alpha = concat_on_dim(self.alpha, alphas_array, dim=dim)
            self.alpha_names.extend(list(alphas_array['variable'].values))

    def get_alpha(self, alpha_names: list):
        alpha_y = list(set(self.alpha['variable'].values) & set(alpha_names))
        alpha_n = list(set(self.alpha['variable'].values) - set(alpha_names))
        if len(alpha_n) != 0:
            print('这些Alpha因子不在对象中: %s' % alpha_n)
        return self.alpha.loc[:, :, :, alpha_y]

    def remove_alpha(self, alpha_names: list):
        alpha_y = list(set(self.alpha['variable'].values) & set(alpha_names))
        alpha_n = list(set(self.alpha['variable'].values) - set(alpha_names))
        self.alpha.drop_sel(variable=alpha_y)
        self.alpha_names = list(set(self.alpha_names) - set(alpha_y))
        if len(alpha_n) != 0:
            print('这些Alpha因子不在对象中: %s;%n 删除后剩下的因子为: %s' % (alpha_n, self.alpha_names))

    def save_residual(self, save_path: str = None):
        if save_path is None:
            if ('alpha_processed_path' in setting.keys()) and (setting['alpha_processed_path'] is not None):
                save_path = setting['alpha_processed_path']
            else:
                save_path = os.path.join(setting['dst_dir'], 'BaseAlphaSet', 'alpha_processed')
        if not os.path.exists(save_path):
            os.makedirs(save_path)

        for fitter_name, resid in self.residual:
            BaseDataSet.save_DataArray({fitter_name: resid}, save_path, separateFolder=True)

    def calculate(self, dataset, handle_data, timewindow: int = 1, data_require: dict = {}, add=False, **kwargs):
        data = xr.concat([getattr(dataset, member).sel(variable=field) for member, field in data_require.items()], dim='variable')

        full_univ = getattr(dataset, 'univ_flag')

        def __handle_data(date):
            tickerlist = full_univ['ticker'][full_univ.sel(date=date, time=145900, variable='univ_flag').astype(bool)].values
            datelist = BaseDataSet.get_tradingday_range(start_date=date,end_date=date,timeWindow=timewindow)
            daily_data = data.sel(ticker=tickerlist, date=datelist)
            return handle_data(daily_data, **kwargs)

        out = [__handle_data(date) for date in self.date_range]
        out = xr.concat(out, dim='variable')
        if add:
            self.add_alpha(alphas_array=out)
        else:
            return out


class BaseFitResult(object):
    def __init__(self, fitter):
        pass

    def predict(self, alphaset: BaseAlphaSet):
        pass

    def export_report(self):
        pass


class BaseFitter(object):
    def __init__(self, fitter_name: str):
        self.fitter_name = fitter_name

    def pre_processing(self, alpha: BaseAlphaSet, dataset: BaseDataSet) -> BaseAlphaSet:
        pass

    def fit(self, alpha: BaseAlphaSet, dataset: BaseDataSet) -> BaseFitResult:
        pass


class LinearRegressionFitter(BaseFitter):
    def __init__(self, fitter_name: str):
        self.fitter_name = fitter_name

    @staticmethod
    def OLS(Y, X, W=None, onlyResid=True, check_singular=True, get_rsquared=False):
        Y = np.c_[Y]  # type: np.ndarray # Y和W都是一维的
        if len(X.shape) ==1:
            X = X[:, np.newaxis]
        if W is None:
            W = np.ones(Y.shape) # type: np.ndarray # Y和W都是一维的
            wX, wY = X, Y
        else:
            if len(W.shape) ==1:
                W = W[:, np.newaxis]
            W = W / W.sum(axis=0)
            wX, wY = np.sqrt(W) * X, np.sqrt(W) * Y
        features = X.shape[1]
        if check_singular & (np.linalg.matrix_rank(wX) < features):
            corr = abs(np.tril(np.corrcoef(wX.T)))
            np.fill_diagonal(corr, 0)
            i, j = np.where(corr > 0.99)
            wX[:, list(set(list(i) + list(j)))] = 0

        flag = wX.any(axis=0)
        outBeta = np.zeros(flag.shape)
        outTValue = np.zeros(flag.shape)
        outBse = np.zeros(flag.shape)

        wX = wX[:, flag]
        invFctCov = np.linalg.pinv(wX.T.dot(wX))  #广义逆矩阵, sm.WLS采用pinv
        beta = invFctCov.dot(wX.T.dot(wY))
        residual = (Y - X[:, flag].dot(beta))
        outBeta[flag] = beta[:, 0]
        out = {
            'beta': outBeta,
            'residual': residual,
        }

        if not onlyResid:
            bse = np.expand_dims(np.sqrt(np.diag(invFctCov * (wY - wX.dot(beta)).T.dot(wY) / ((W[:, 0] != 0).sum() - flag.sum()))), axis=1)
            tValue = beta / bse
            outTValue[flag] = tValue[:, 0]
            outBse[flag] = bse[:, 0]
            out.update({
                't_value': outTValue,
                'bse': outBse,
            })
        if get_rsquared:
            W = W / W.sum()
            SSE = (W * np.square(residual)).sum()
            TSS = (W * np.square(Y - Y.mean())).sum()
            R2 = (1 - SSE / TSS)
            num_exog = flag.sum()
            num_sample = (W[:, 0] != 0).sum()
            adjR2 = R2 - (1 - R2) * num_exog / (num_sample - num_exog - 1)
            out.update({
                'r2': R2,
                'adj_r2': adjR2,
            })
        return out

    def pre_processing(self,
                       alphaset: BaseAlphaSet,
                       dataset: BaseDataSet,
                       save_path: str = None,
                       orth: bool = True,
                       method: str = 'indmean',
                       mode: str = 'mad',
    ):
        def __dropTicker(alpha: np.ndarray, ind_risk: np.ndarray, industry: np.ndarray, weight: np.ndarray, flag: np.ndarray) -> np.ndarray:
            alpha = alpha[flag == 1]
            ind_risk = ind_risk[flag == 1, :]
            industry = industry[flag == 1]
            weight = weight[flag == 1]
            return alpha, ind_risk, industry, weight

        def __processna(alpha: np.ndarray, industry: np.ndarray, method: str = 'indmean') -> np.ndarray:
            # NaN不影响这一步的处理
            alpha = pd.DataFrame(alpha)
            industry = pd.DataFrame(industry, columns=['industry'])
            if method == 'front':
                alpha = alpha.fillna(method='pad', axis=0)  #按时间填充的话,可以在DataArray上填充
            if method == 'mean':
                alpha = alpha.fillna(value=alpha.mean())
            if method == 'indmean':
                df = pd.merge(industry, alpha, right_index=True, left_index=True)
                alpha = df.fillna(df.groupby('industry').transform('mean')).iloc[:, 1:]
                #有可能整个行业都是NaN,所以按行业均值填充完还需要按全部均值再填充一次
                alpha = alpha.fillna(value=alpha.mean())
            return np.array(np.squeeze(alpha))

        def __dropExtremeValue(alpha: np.ndarray, mode: str = 'mad', multiple: int = 5) -> np.ndarray:
            alpha = pd.DataFrame(alpha)
            if mode == 'mad':
                med = alpha.median(axis=0)
                center = med
                abs_med = (alpha - med).abs().replace(np.inf, np.nan)
                diff = multiple * abs_med.median(axis=0)
                diff[diff == 0] = abs_med.mean(axis=0)[abs_med.mean(axis=0).index.isin(diff[diff == 0].index)]
            elif mode == 'std':
                x = alpha.mask(np.isinf(alpha))
                center = x.mean(axis=0)
                diff = multiple * x.std(ddof=0)
            alpha = alpha.clip(lower=center - diff, upper=center + diff, axis=1)
            return np.array(np.squeeze(alpha))

        def __ols(y: np.ndarray, X: np.ndarray, w: np.ndarray) -> np.ndarray:
            resid = LinearRegressionFitter.OLS(y, X, w)['residual']
            return np.array(np.squeeze(resid))

        def __ZscoreStandard(alpha: np.ndarray) -> np.ndarray:
            # nan值不影响这一步
            # 如果所有ticker的某个因子值都是一样的,则标准化只减均值,不除标准差.
            alpha = pd.DataFrame(alpha)
            alpha = (alpha - alpha.mean()) / alpha.std().replace(0, np.nan).fillna(1)
            return np.array(np.squeeze(alpha))

        def __reindex(alpha: np.ndarray, flag: np.ndarray) -> np.ndarray:
            alpha_reindex = np.full_like(flag, np.nan, dtype=np.double)
            alpha_reindex[flag == 1] = alpha
            return alpha_reindex

        def __main_process(alpha: np.ndarray,
                           ind_risk: np.ndarray,
                           industry: np.ndarray,
                           weight: np.ndarray,
                           flag: np.ndarray,
                           method: str = 'indmean',
                           mode: str = 'mad',
                           orth: bool = True) -> np.ndarray:
            # 用flag筛选ticker_list
            alpha, ind_risk, industry, weight = __dropTicker(alpha, ind_risk, industry, weight, flag)
            alpha = __processna(alpha, industry, method=method)
            alpha = __dropExtremeValue(alpha, mode=mode)
            if orth:
                alpha = __ols(alpha, ind_risk, weight)
            alpha = __ZscoreStandard(alpha)
            alpha = __reindex(alpha, flag)
            return np.array(np.squeeze(alpha))

        def __xr_process(alpha: xr.DataArray,
                         ind_risk: xr.DataArray,
                         industry: xr.DataArray,
                         weight: xr.DataArray,
                         flag: xr.DataArray,
                         method: str = 'indmean',
                         mode: str = 'mad',
                         orth: bool = True) -> xr.DataArray:
            alpha = xr.apply_ufunc(
                __main_process,alpha,ind_risk,industry,weight,flag,
                input_core_dims=[['ticker'], ['ticker', 'variable'], ['ticker'], ['ticker'], ['ticker']],  # 每个参数只有一个条目的列表
                output_core_dims=[['ticker']],  # 计算beta:'variable',计算resid:'ticker'
                # exclude_dims=set(('ticker',)),        # 结果变量中要删除的变量
                kwargs={
                    'method': method,
                    'mode': mode,
                    'orth': orth
                },  # !必须以关键字的形式传进去
                vectorize=True,
                dask="parallelized",
                output_dtypes=[alpha.dtype]  # 每个输出一个； 也可以是float或np.dtype（“ float64”）
            )
            return alpha

        def __process_part(var: List[str], date_range: List[int], save_path: str) -> xr.DataArray:
            """__process_part _summary_

            输入变量名和日期范围，负责读取计算并保存.

            Args:
                var (List[str]): 需要预处理的变量
                date_range (List[int]): 需要预处理的日期
                save_path (_type_, optional): 读取cache的路径. Defaults to save_path.

            Returns:
                xr.DataArray: 预处理后的alpha_resid
            """
            alpha_resid = None
            for ivar in var:
                ifilepath = os.path.join(save_path, ivar)
                read_range, remain_range = BaseDataSet.lookup_date(date_range, filepath=ifilepath)
                ialpha_resid = None
                if read_range != []:
                    ialpha_resid = BaseDataSet.read_minute_data(ifilepath, date_range=read_range)
                    print('%s已有日期已经读取'%ivar)
                if remain_range != []:
                    # alpha的time是dataset的time的子集.
                    alpha = alphaset.alpha.sel(variable=[ivar], date=remain_range).reindex(time=dataset.timelist).rename({'variable': 'alpha'})
                    ind_risk = xr.concat([dataset.industry_dummies.sel(date=remain_range),
                                          dataset.risk_factor.sel(date=remain_range)],
                                         dim='variable').squeeze(dim='time')
                    industry = dataset.industry.sel(date=remain_range).squeeze(dim=['variable', 'time'])
                    # weight = (1/np.sqrt(dataset.capital)).squeeze(dim = ['variable','time'])
                    flag = dataset.flag.sel(date=remain_range).squeeze(dim='variable')
                    weight = xr.ones_like(flag)
                    ialpha_resid_ = __xr_process(alpha, ind_risk, industry, weight, flag, orth=orth, method=method,
                                                 mode=mode).rename(alpha='variable').transpose('ticker', 'date', 'time', 'variable')
                    ialpha_resid_ = xr.where(
                        (dataset.price_ipo_st_flag.sel(date=remain_range).squeeze() == 1) & (dataset.flag.sel(date=remain_range).squeeze() == 0), 0,
                        ialpha_resid_)
                    ialpha_resid = BaseDataSet.concat_on_dim(ialpha_resid, ialpha_resid_, dim='date').reindex(date=date_range)
                alpha_resid = BaseDataSet.concat_on_dim(alpha_resid, ialpha_resid, dim='variable')
            return alpha_resid

        start_time = tm.time()
        if save_path is None:
            if ('BaseAlphaSet_path' in setting.keys()) and (setting['BaseAlphaSet_path'] is not None):
                save_path = os.path.join(setting['BaseAlphaSet_path'], 'alpha_processed', self.fitter_name)
            else:
                save_path = os.path.join(setting['dst_dir'], 'BaseAlphaSet', 'alpha_processed', self.fitter_name)
        if not os.path.exists(save_path):
            os.makedirs(save_path)
        """ 
        先检查alphaset上已经预处理过的alpha有哪些, 需要重新预处理的有哪些.
            对于已经存在的alpha, 检查其尚未预处理的日期有哪些, 如果存在则进行预处理.
            对于尚未存在的alpha, 进行预处理.
        """
        if alphaset.residual is None:
            alphaset.residual = {}
        if self.fitter_name in alphaset.residual.keys():
            alpha_e = list(alphaset.residual[self.fitter_name]['variable'].values)
        else:
            alphaset.residual.update({self.fitter_name: None})
            alpha_e = []
        alpha_add = list(set(alphaset.alpha_names) - set(alpha_e))
        if alpha_e != []:
            # 对已经存在的因子检查日期是不是够的
            date_e = list(alphaset.residual[self.fitter_name]['date'].values)
            date_add = list(set(alphaset.date_range) - set(date_e))
            if date_add != []:
                alpha_resid = __process_part(alpha_e, date_add, save_path)
                alphaset.residual[self.fitter_name] = BaseDataSet.concat_on_dim(alphaset.residual[self.fitter_name], alpha_resid,
                                                                                dim='date').reindex(date=dataset.date_range)
        if alpha_add != []:
            alpha_resid = __process_part(alpha_add, alphaset.date_range, save_path)
            alphaset.residual[self.fitter_name] = BaseDataSet.concat_on_dim(alphaset.residual[self.fitter_name], alpha_resid, dim='variable')

        def __to_dict(beta: xr.DataArray):
            newBeta = {}
            for var in beta['variable'].values:
                newBeta.update({var: beta.sel(variable=[var])})
            return newBeta

        BaseDataSet.save_DataArray(__to_dict(alphaset.residual[self.fitter_name]), save_path, byDate=True)
        print('共耗时%f s' % (tm.time() - start_time))

    @staticmethod
    def alpha_autocorr(alpha: xr.DataArray,
                       dataset: BaseDataSet,
                       date_range: List[int],
                       method: str = 'pearson',
                       lag: int = 10) -> xr.DataArray:
        # NaN值不影响自相关的计算.
        # * method 1 最快, method 3 次之, method 2 最慢
        #! method 1
        def main_corr(alpha, lag_coords, method: str = 'pearson'):
            alpha = pd.DataFrame(alpha)
            alpha_corr = np.full((alpha.shape[1], len(lag_coords)), np.nan)
            for i in lag_coords:
                alpha_corr[:, i - 1] = alpha.corrwith(alpha.shift(-i, axis=1), method=method)
            return alpha_corr

        def xr_corr(alpha, lag_coords, method: str = 'pearson'):
            alpha = xr.apply_ufunc(
                main_corr,
                alpha,
                lag_coords,
                input_core_dims=[['ticker', 'time'], ['lag']],  # 每个参数只有一个条目的列表
                output_core_dims=[['time', 'lag']],  # 计算beta:'variable',计算resid:'ticker'
                exclude_dims=set(('ticker', )),  # 结果变量中要删除的变量
                kwargs={'method': method},  # !必须以关键字的形式传进去
                vectorize=True,
                dask="parallelized",
                output_dtypes=[alpha.dtype]  # 每个输出一个； 也可以是float或np.dtype（“ float64”）
            )
            alpha['lag'] = lag_coords
            return alpha

        if date_range is None:
            date_range = dataset.date_range

        alpha = alpha.sel(date=date_range).reindex(time=dataset.timelist,
                                                   fill_value=np.nan).where(dataset.flag.sel(date=date_range).sel(variable='flag') == 1)
        alpha_plus = alpha.shift(date=-1)[:, :, :lag, :]
        alpha_plus['time'] = ['next' + str(i) for i in range(1, lag + 1)]
        alpha = xr.concat([alpha, alpha_plus], dim='time')
        lag_coords = xr.DataArray(np.array(range(1, (lag + 1))), coords=[np.array(range(1, (lag + 1)))], dims=['lag'])
        alpha_corr = xr_corr(alpha, lag_coords=lag_coords, method=method).transpose('date', 'time', 'variable', 'lag')
        alpha_corr = alpha_corr[:, :-lag, :, :]
        return alpha_corr

    @staticmethod
    def turnover(alpha: xr.DataArray, dataset: BaseDataSet, date_range: List[int]) -> xr.DataArray:
        # 每期的票不一样,该期存在,上一期不存在的票,上一期的因子值填为0
        alpha = alpha.sel(date=date_range).reindex(time=dataset.timelist,
                                                   fill_value=np.nan).where(dataset.flag.sel(date=date_range).loc[:, :, :, 'flag'] == 1, 0).fillna(0)
        turnover = (np.abs(alpha.stack(datetime=('date', 'time')) - alpha.stack(datetime=('date', 'time')).shift(datetime=1)).sum(dim='ticker') /
                    np.abs(alpha.stack(datetime=('date', 'time')).shift(datetime=1)).sum(dim='ticker')).unstack()
        turnover = xr.where(turnover == np.inf, 1, turnover)
        turnover = turnover.transpose('date', 'time', 'variable')
        return turnover

    @staticmethod
    def get_beta_t(alpha: xr.DataArray,
                   dataset: BaseDataSet,
                   date_range: Union[List[int], None] = None,
                   var: Union[str, None] = None,
                   multi_factor: bool = True,
                   add_ind_risk: bool = True) -> xr.DataArray:
        #! method 1
        def main_ols(Return, alpha, ind_risk, weight, flag, add_ind_risk: bool = True):
            Y = Return[flag == 1]
            if add_ind_risk:
                X = np.c_[alpha, ind_risk][flag == 1, :]
            else:
                X = np.c_[np.ones(alpha.shape[0]), alpha][flag == 1, ]
            W = weight[flag == 1]
            alpha_num = np.expand_dims(alpha, axis=-1).shape[1]
            out = LinearRegressionFitter.OLS(Y, X, W, onlyResid=False)

            if add_ind_risk:
                beta, t_value = out['beta'][:alpha_num], out['t_value'][:alpha_num]
            else:
                beta, t_value = out['beta'][1:], out['t_value'][1:]
            # out = LinearRegressionFitter.OLS(Y, X, W, onlyResid = False)
            # beta, t_value = out['beta'][:alpha_num], out['t_value'][:alpha_num]
            return np.array(np.squeeze(beta)), np.array(np.squeeze(t_value))

        def xr_ols(Return, alpha, ind_risk, weight, flag, multi_factor: bool = True, add_ind_risk: bool = True):
            if multi_factor:
                beta, t_value = xr.apply_ufunc(
                    main_ols,Return,alpha,ind_risk,weight,flag,
                    input_core_dims=[['ticker'], ['ticker', 'variable'], ['ticker', 'ind_risk'], ['ticker'], ['ticker']],  # 每个参数只有一个条目的列表
                    output_core_dims=[['variable'], ['variable']],  # 计算beta:'variable',计算resid:'ticker'
                    exclude_dims=set(('ticker', )),  # 结果变量中要删除的变量
                    kwargs={'add_ind_risk': add_ind_risk},  # !必须以关键字的形式传进去
                    vectorize=True,
                    dask="parallelized",
                    output_dtypes=[alpha.dtype, alpha.dtype]  # 每个输出一个； 也可以是float或np.dtype（“ float64”）
                )
            else:
                beta, t_value = xr.apply_ufunc(
                    main_ols,Return,alpha,ind_risk,weight,flag,
                    input_core_dims=[['ticker'], ['ticker'], ['ticker', 'ind_risk'], ['ticker'], ['ticker']],  # 每个参数只有一个条目的列表
                    output_core_dims=[[], []],  # 计算beta:'variable',计算resid:'ticker'
                    exclude_dims=set(('ticker', )),  # 结果变量中要删除的变量
                    kwargs={'add_ind_risk': add_ind_risk},  # !必须以关键字的形式传进去
                    vectorize=True,
                    dask="parallelized",
                    output_dtypes=[alpha.dtype, alpha.dtype]  # 每个输出一个； 也可以是float或np.dtype（“ float64”）
                )
            return beta, t_value

        if date_range is None:
            date_range = dataset.date_range
        if var is not None:
            Return = dataset.y.sel(variable=var, date=date_range).fillna(0)
        else:
            Return = dataset.y.sel(date=date_range).fillna(0).rename({'variable': 'y'})

        alpha = alpha.reindex(time=dataset.timelist).sel(date=date_range).fillna(0)
        ind_risk = xr.concat([dataset.industry_dummies, dataset.risk_factor],
                             dim='variable').squeeze(dim='time').sel(date=date_range).rename(variable='ind_risk')
        weight = dataset.capital.squeeze(dim=['variable', 'time']).sel(date=date_range)
        flag = dataset.flag.squeeze(dim='variable').sel(date=date_range)
        if alpha['variable'].size == 1: 
            multi_factor = False  # 单因子的话,两个结果是一样,但是True那部分单因子不符合输入要求.
        #! apply会将输入的长度为1的维度给弄没了，因子判断这里需要判断一下给补上.
        beta, t_value = xr_ols(Return, alpha, ind_risk, weight, flag, multi_factor=multi_factor, add_ind_risk=add_ind_risk)

        return beta, t_value

    @staticmethod
    def IC(alpha: xr.DataArray, dataset: BaseDataSet, beta: xr.DataArray, var: str, date_range, method: str = 'pearson') -> Tuple[xr.DataArray]:
        # *method 1最快, method 2次之, method 3 最慢.
        #! method 1
        def main_IC(alpha, Return, beta, method: str = 'pearson'):
            # np.corrcoef不能接受NaN, pd.DataFrame().corrwith可以忽略NaN,并且可以选择相关系数的类型
            # flag拿过来剔除一下空值就行了,但是太麻烦了
            alpha, Return = pd.DataFrame(alpha), pd.DataFrame(Return)
            IC = np.array(alpha.corrwith(Return, method=method))
            alpha_return = alpha * beta  # type: pd.DataFrame
            cond1 = alpha_return > alpha_return.quantile(q=2 / 3, axis=0)
            IC_long = np.array(alpha_return[cond1].corrwith(Return[cond1], method=method))
            cond2 = alpha_return < alpha_return.quantile(q=1 / 3, axis=0)
            IC_short = np.array(alpha_return[cond2].corrwith(Return[cond2], method=method))
            return IC, IC_long, IC_short

        def xr_IC(alpha, Return, beta, method: str = 'pearson'):
            IC, IC_long, IC_short = xr.apply_ufunc(
                main_IC, alpha, Return, beta,
                input_core_dims=[['ticker', 'time'], ['ticker', 'time'], ['time']],  # 每个参数只有一个条目的列表
                output_core_dims=[['time'], ['time'], ['time']],  # 计算beta:'variable',计算resid:'ticker'
                exclude_dims=set(('ticker', )),  # 结果变量中要删除的变量
                kwargs={'method': method},  # !必须以关键字的形式传进去
                vectorize=True,
                dask="parallelized",
                output_dtypes=[alpha.dtype, alpha.dtype, alpha.dtype]  # 每个输出一个； 也可以是float或np.dtype（“ float64”）
            )
            return (IC.transpose('date', 'time', 'variable'), IC_long.transpose('date', 'time', 'variable'), IC_short.transpose('date', 'time', 'variable'))

        alpha = alpha.sel(date=date_range).reindex(time=dataset.timelist, fill_value=np.nan).where(dataset.flag.sel(variable='flag') == 1)
        Return = dataset.y.sel(variable=var, date=date_range).where(dataset.flag.sel(variable='flag') == 1).fillna(0)
        IC, IC_long, IC_short = xr_IC(alpha, Return, beta.sel(date=date_range), method=method)

        return IC, IC_long, IC_short

    @staticmethod
    def sort(alpha: xr.DataArray, dataset: BaseDataSet, N: int = 5, neutralizing: bool = False, cost_rate: int = 0.0008) -> dict:
        alpha = alpha.reindex({'time': dataset.timelist}).fillna(0).where(dataset.flag.sel(variable='flag') == 1, np.nan)
        #有可能因子值在某几个时间点没有# alpha为np.nan都是被剔除掉的票
        groups = ['portfolio_' + str(i) for i in range(1, N + 1)]
        groups.append('portfolio_long_short')

        def xr_rank(arr: xr.DataArray) -> xr.DataArray:
            arr = arr.stack(datetime=('date', 'time')).transpose('ticker', 'datetime', 'variable')
            arr_rank = xr.full_like(arr, np.nan)
            for factor in arr['variable'].values:
                arr_rank.loc[:, :, factor] = pd.DataFrame(arr.loc[:, :, factor].values, index=arr['ticker'],
                                                          columns=arr['datetime']).rank(axis=0, method='first', pct=True).values
            return arr_rank.unstack().transpose('ticker', 'date', 'time', 'variable')

        def xr_str(arr0, arr1) -> xr.DataArray:
            def add_str(obj0, obj1):
                return str(obj0) + str(obj1)

            out = xr.apply_ufunc(add_str, arr0, arr1, input_core_dims=[[], []], output_core_dims=[[]], vectorize=True, dask="parallelized")
            return out

        if neutralizing:
            capital = dataset.capital
            industry = dataset.industry.astype(int)
            labels_cap = xr.where((capital >= capital.quantile(0, dim='ticker')) & (capital < capital.quantile(1 / 3, dim='ticker')), 'small',
                                  np.nan)  #type:xr.DataArray
            labels_cap = xr.where((capital >= capital.quantile(1 / 3, dim='ticker')) & (capital < capital.quantile(2 / 3, dim='ticker')), 'medium',
                                  labels_cap)
            labels_cap = xr.where((capital >= capital.quantile(2 / 3, dim='ticker')) & (capital <= capital.quantile(1, dim='ticker')), 'big',
                                  labels_cap)
            labels = (xr_str(labels_cap.squeeze(dim='variable'), industry.squeeze(dim='variable')).reindex({
                'time': dataset.timelist
            }, method='bfill').where(dataset.flag.sel(variable='flag') == 1).transpose('ticker', 'date', 'time', 'variable'))
            label_names = [cap + str(ind) for ind in range(1, industry.max().values + 1) for cap in ['small', 'medium', 'big']]
        else:
            labels = xr.full_like(alpha, 'n', dtype=np.str_)
            labels = labels.where(dataset.flag.sel(variable='flag') == 1, np.nan)  # 剔除未标flag的票
            label_names = ['n']

        portfolio_return = (xr.DataArray(0,coords=[dataset.date_range, dataset.timelist, alpha['variable'].values],dims=['date', 'time', 'variable'])
                           .expand_dims(dim={'group': groups}).transpose('date', 'time', 'variable', 'group'))
        count = xr.full_like(portfolio_return, 0)
        turnover = xr.full_like(portfolio_return, 0)
        sort_flag = xr.full_like(alpha, 0)

        for ilabel in label_names:
            ialpha_pct = xr_rank(xr.where(labels == ilabel, alpha, np.nan))
            #da.rank(dim = 'ticker', pct=True) # ialpha_pct:ticker,date,time,variable # xr.DataArray.rank函数会忽略NaN,结果返回为NaN
            for i in range(N):
                iflag = xr.where((ialpha_pct > (i / N)) & (ialpha_pct <= ((i + 1) / N)), 1,
                                 np.nan).fillna(0).stack(datetime=('date', 'time'))  #type: xr.DataArray
                portfolio_return.loc[:, :, :, 'portfolio_' + str(i + 1)] += dataset.Return.sel(variable='MinuteReturn').where(iflag == 1).sum(dim='ticker')
                count.loc[:, :, :, 'portfolio_' + str(i + 1)] += (iflag == 1).sum()
                sort_flag = xr.where(iflag==1,i,sort_flag)
        portfolio_return = portfolio_return / count
        for i in range(N):
            temp = sort_flag.where(sort_flag==i)/sort_flag.where(sort_flag==i).sum(dim='tocker')
            # 这里的换手还需要return再scale一下吗？
            turnover.loc[:,:,:,'portfolio_' + str(i + 1)] = np.abs(temp-temp.stack(datetime=['date','time']).shift(datetime=1).unstack().fillna(0))
        portfolio_return.loc[:, :,
                             'portfolio_long_short'] = portfolio_return.loc[:, :, 'portfolio_' + str(N)] - portfolio_return.loc[:, :, 'portfolio_1']
        turnover.loc[:, :, 'portfolio_long_short'] = turnover.loc[:, :, 'portfolio_' + str(N)] + turnover.loc[:, :, 'portfolio_1']
        portfolio_return_cost = portfolio_return - turnover * cost_rate

        return portfolio_return_cost

    @staticmethod
    def plot(data: dict, save_path, width: str = '1500px', height: str = '800px'):
        # 有什么数据就画什么图, beta一定要有
        save_path = os.path.join(save_path, 'single_factor_result')
        if not os.path.exists(save_path):
            os.makedirs(save_path)

        def set_global_opts(chart: Line, title: str):
            chart = chart.set_global_opts(
                title_opts=opts.TitleOpts(title=title),
                legend_opts=opts.LegendOpts(type_="scroll", pos_left="right", orient="vertical"),
                datazoom_opts=opts.DataZoomOpts(range_start=0, range_end=100),  # 水平线
                xaxis_opts=opts.AxisOpts(interval=0),
                yaxis_opts=opts.AxisOpts(min_='dataMin',
                                         splitline_opts=opts.SplitLineOpts(is_show=True))).set_series_opts(label_opts=opts.LabelOpts(is_show=False))
            return chart

        Xaxis = [str(idate) for idate in data['beta']['date'].values]
        Xaxis.insert(0, 'preDay')
        if 'alpha_autocorr' in data.keys(): Xaxis1 = [str(lag) for lag in data['alpha_autocorr']['lag'].values]
        newDate = np.r_[0, data['beta']['date'].values]
        for dataname, datavalue in data.items():
            data[dataname] = datavalue.reindex(date=newDate, method=None).fillna(0)

        for ifactor in data['beta']['variable'].values:
            # 一个因子一张图
            tab = Tab()
            # beta: beta必须有
            beta = (Line(init_opts=opts.InitOpts(width=width, height=height)).add_xaxis(Xaxis).add_yaxis(
                'beta', (data['beta'].sel(variable=ifactor).fillna(0) + 1).prod(dim='time').cumprod(dim='date').values.tolist()))
            beta = set_global_opts(chart=beta, title=ifactor + '_beta')
            tab = tab.add(beta, 'beta')

            # turnover
            if 'turnover' in data.keys():
                turnover = (Line(init_opts=opts.InitOpts(width=width, height=height)).add_xaxis(Xaxis).add_yaxis(
                    'turnover', data['turnover'].sel(variable=ifactor).mean(dim='time').values.tolist()))
                turnover = set_global_opts(chart=turnover, title=ifactor + '_turnover')
                tab = tab.add(turnover, 'turnover')

            # IC
            if 'IC' in data.keys():
                IC = (Line(init_opts=opts.InitOpts(width=width, height=height)).add_xaxis(Xaxis).add_yaxis(
                    'IC', data['IC'].sel(variable=ifactor).mean(dim='time').values.tolist()).add_yaxis(
                        'IC_long', data['IC_long'].sel(variable=ifactor).mean(dim='time').values.tolist()).add_yaxis(
                            'IC_short', data['IC_short'].sel(variable=ifactor).mean(dim='time').values.tolist()))
                IC = set_global_opts(chart=IC, title=ifactor + '_IC')
                tab = tab.add(IC, 'IC')

            # auto_corr
            if 'autocorr' in data.keys():
                autocorr = (Line(init_opts=opts.InitOpts(width=width, height=height)).add_xaxis(Xaxis1).add_yaxis(
                    'autocorr', data['alpha_autocorr'].sel(variable=ifactor).mean(dim=['date', 'time']).values.tolist()))
                autocorr = set_global_opts(chart=autocorr, title=ifactor + '_autocorr')
                tab = tab.add(autocorr, 'alpha_autocorr')

            # sort
            if 'sort' in data.keys():
                sort = Line(init_opts=opts.InitOpts(width=width, height=height)).add_xaxis(Xaxis)
                for igroup in data['sort']['group'].values:
                    sort = sort.add_yaxis(igroup, (data['sort'].sel(variable=ifactor, group=igroup).fillna(0) +
                                                   1).prod(dim='time').cumprod(dim='date').values.tolist())
                sort = set_global_opts(chart=sort, title=ifactor + '_portfolio')
                tab = tab.add(sort, 'sort')

            tab.render(os.path.join(save_path, f'{ifactor}_single_test.html'))
            print(f'{ifactor} Garph saved')

    def fit(self,
            alphaset: BaseAlphaSet,
            dataset: BaseDataSet,
            y_name: Union[str, None] = None,
            alpha_name: Union[List[str], None] = None,
            save_path: Union[List[str], None] = None,
            fit_result: Union[dict, 'LinearRegressionResult'] = None,
            multi_factor: bool = True,
            add_ind_risk: bool = True,
            BetaOnly: bool = True,
            single_factor_test_list: Union[List[str], None] = None,
            byDate: bool = True,
            isPlot: bool = False,
    ) -> 'LinearRegressionResult':
        def __concat_on_dim(Array, array, dim):
            if Array is None:
                Array = array
            else:
                Array = xr.concat([Array, array], dim=dim)
            return Array

        def __to_dict(beta: xr.DataArray):
            newBeta = {}
            for var in beta['variable'].values:
                newBeta.update({var: beta.sel(variable=[var])})
            return newBeta

        def __load_cache(obj, attr, var, read_date: List[int], file_path: str):
            Data = None
            for ivar in var:
                data = BaseDataSet.read_minute_data(os.path.join(file_path,ivar), date_range=read_date)
                Data = BaseDataSet.concat_on_dim(Data, data, dim='variable')
            obj.__dict__[attr] = __concat_on_dim(obj.__dict__[attr], Data, dim='date')
            obj.__dict__[attr] = obj.__dict__[attr].reindex(date=np.sort(obj.__dict__[attr]['date'].values))

        start_time = tm.time()

        if fit_result is None:
            fit_result = LinearRegressionResult(self, y_name, alpha_name)
        else:
            y_name = fit_result.y_name
            alpha_name = fit_result.alpha_name
        if alpha_name is None:
            alpha_name = alphaset.residual[self.fitter_name]['variable'].values
        # 保存路径
        if save_path is None:
            if ('fit_result_path' in setting.keys()) and (setting['fit_result_path'] is not None):
                save_path = setting['fit_result_path']
            else:
                save_path = os.path.join(setting['dst_dir'], 'fit_result')
        filename = [y_name]
        filename.extend(sorted(alpha_name))
        filename = '_'.join(filename)
        save_path = os.path.join(save_path, filename)
        if not os.path.exists(save_path):
            os.makedirs(save_path)
        alpha = alphaset.residual[self.fitter_name].sel(variable=alpha_name)
        # fit的几个β应该都是相同日期的.
        read_range, remain_range = BaseDataSet.lookup_date(dataset.date_range, os.path.join(save_path, 'beta', alpha_name[0]))
        print('已经有的日期:%s' % read_range)
        print('需要加的日期:%s' % remain_range)
        if read_range != []:
            __load_cache(fit_result, 'beta', alpha_name, read_range, os.path.join(save_path, 'beta'))
            __load_cache(fit_result, 't_values', alpha_name, read_range, os.path.join(save_path, 't_values'))
            print('已有日期已经读取')
        if remain_range != []:
            beta_, t_values_ = LinearRegressionFitter.get_beta_t(alpha,
                                                                 dataset,
                                                                 date_range=remain_range,
                                                                 var=y_name,
                                                                 multi_factor=multi_factor,
                                                                 add_ind_risk=add_ind_risk)
            fit_result.beta, = __concat_on_dim(fit_result.beta, beta_, dim='date'),
            fit_result.t_values = __concat_on_dim(fit_result.t_values, t_values_, dim='date')
            print('需要计算beta的日期已计算完成')
            BaseDataSet.save_DataArray(__to_dict(beta_), os.path.join(save_path, 'beta'), byDate=byDate, separateFolder=True)
            BaseDataSet.save_DataArray(__to_dict(t_values_), os.path.join(save_path, 't_values'), byDate=byDate, separateFolder=True)
            print('beta,tvalues已保存在%s!' % save_path)
        fit_data = dict(beta=fit_result.beta)

        if not BetaOnly:
            # 计算IC
            for ifactor in alpha_name:
                iIC_path = os.path.join(save_path, 'IC', ifactor)
                iIC_long_path = os.path.join(save_path, 'IC_long', ifactor)
                iIC_short_path = os.path.join(save_path, 'IC_short', ifactor)
                read_range, remain_range = BaseDataSet.lookup_date(dataset.date_range, iIC_path)
                ialpha_IC = ialpha_IC_long = ialpha_IC_short = None
                if read_range != []:
                    ialpha_IC = BaseDataSet.read_minute_data(iIC_path, read_range)
                    ialpha_IC_long = BaseDataSet.read_minute_data(iIC_long_path, read_range)
                    ialpha_IC_short = BaseDataSet.read_minute_data(iIC_short_path, read_range)
                if remain_range != []:
                    ialpha_IC_, ialpha_IC_long_, ialpha_IC_short_ = LinearRegressionFitter.IC(
                        alphaset.residual[self.fitter_name].sel(variable=[ifactor]), dataset, fit_result.beta, var=y_name, date_range=remain_range)
                    ialpha_IC = __concat_on_dim(ialpha_IC, ialpha_IC_, dim='date')
                    ialpha_IC_long = __concat_on_dim(ialpha_IC_long, ialpha_IC_long_, dim='date')
                    ialpha_IC_short = __concat_on_dim(ialpha_IC_short, ialpha_IC_short_, dim='date')
                fit_result.IC = __concat_on_dim(fit_result.IC, ialpha_IC, dim='variable')
                fit_result.IC_long = __concat_on_dim(fit_result.IC_long, ialpha_IC_long, dim='variable')
                fit_result.IC_short = __concat_on_dim(fit_result.IC_short, ialpha_IC_short, dim='variable')
            fit_data.update(dict(IC=fit_result.IC, IC_long=fit_result.IC_long, IC_short=fit_result.IC_short))
            print('IC计算完成')

            if single_factor_test_list is not None:
                if alpha_path is None:
                    if ('alpha_path' in setting.keys()) and (setting['alpha_path'] is not None):
                        alpha_path = setting['fit_result_path']
                    else:
                        alpha_path = os.path.join(setting['dst_dir'], 'BaseAlphaSet')

                for ifactortest in single_factor_test_list:
                    if alphaset.__dict__[ifactortest] is not None:
                        alpha_calc = list(set(alpha_name) - set(alphaset.__dict__[ifactortest]['variable'].values))
                    else:
                        alpha_calc = alpha_name
                    if alpha_calc != []:
                        for ifactor in alpha_calc:
                            ialpha_path = os.path.join(alpha_path, ifactortest, ifactor)
                            ialpha = None
                            read_range, remain_range = BaseDataSet.lookup_date(dataset.date_range, ialpha_path)
                            if read_range != []:
                                ialpha = BaseDataSet.read_minute_data(ialpha_path, read_range)
                            if remain_range != []:
                                ialpha_ = LinearRegressionFitter.__dict__[ifactortest](alphaset.residual[self.fitter_name].sel(variable=[ifactor]),
                                                                                       dataset, remain_range)
                                ialpha = __concat_on_dim(ialpha, ialpha_, dim='date')
                            alphaset.__dict__[ifactortest] = __concat_on_dim(alphaset.__dict__[ifactortest], ialpha, dim='variable')
                    fit_result.__dict__[ifactortest] = __concat_on_dim(fit_result.__dict__[ifactortest],
                                                                       alphaset.__dict__[ifactortest].sel(variable=alpha_name),
                                                                       dim='variable')
                    fit_data.update({ifactortest: fit_result.__dict__[ifactortest]})
                    print('%f计算完成' % ifactortest)

            BaseDataSet.save_DataArray(fit_data, save_path, byDate=True)
        if isPlot:
            LinearRegressionFitter.plot(fit_data, os.path.join(save_path, 'single_factor_test'))
        print('共耗时%f s' % (tm.time() - start_time))
        return fit_result


class LinearRegressionResult(BaseFitResult):
    def __init__(self, fitter: LinearRegressionFitter, y_name: str, alpha_name: List[str] = ['None']):
        self.fitter_name = fitter.fitter_name
        self.y_name: str = y_name
        self.alpha_name: str = alpha_name
        self.beta: xr.DataArray = None
        self.t_values: xr.DataArray = None
        self.predicted_beta: xr.DataArray = None
        self.predicted_return: xr.DataArray = None
        self.weight: xr.DataArray = None
        self.IC: xr.DataArray = None
        self.IC_long: xr.DataArray = None
        self.IC_short: xr.DataArray = None
        self.turnover: xr.DataArray = None
        self.alpha_autocorr: xr.DataArray = None
        self.sort: xr.DataArray = None
        self.score: pd.DataFrame = None
        self.factor_name_picked = {}
        self.factor_name_dropped = {}

    def calculate_attr(self):
        self.score.loc[:, 'factor_return_mean'] = pd.Series(self.beta.mean(dim=['date', 'time']) * 240, index=self.beta['variable'])
        self.score.loc[:, 'factor_return_std'] = pd.Series(self.beta.std(dim=['date', 'time'], ddof=1) * np.sqrt(240), index=self.beta['variable'])
        self.score.loc[:, 'factor_return_IR'] = self.score.loc[:, 'factor_return_mean'] / self.score.loc[:, 'factor_return_std']
        self.score.loc[:, 'factor_return_IR1'] = (
            pd.Series(self.beta[:, int(len(self.beta['time']) * 1 / 2):, :].mean(dim=['date', 'time']) * 240, index=self.beta['variable']) /
            pd.Series(self.beta[:, int(len(self.beta['time']) * 1 / 2):, :].std(dim=['date', 'time'], ddof=1) * np.sqrt(240),
                      index=self.beta['variable']))
        self.score.loc[:, 'factor_return_IR2'] = (
            pd.Series(self.beta[:, :int(len(self.beta['time']) * 1 / 2), :].mean(dim=['date', 'time']) * 240, index=self.beta['variable']) /
            pd.Series(self.beta[:, :int(len(self.beta['time']) * 1 / 2), :].std(dim=['date', 'time'], ddof=1) * np.sqrt(240),
                      index=self.beta['variable']))
        self.score.loc[:, 'factor_return_IR3'] = (
            pd.Series(self.beta[:, int(len(self.beta['time']) * 1 / 4):, :].mean(dim=['date', 'time']) * 240, index=self.beta['variable']) /
            pd.Series(self.beta[:, int(len(self.beta['time']) * 1 / 4):, :].std(dim=['date', 'time'], ddof=1) * np.sqrt(240),
                      index=self.beta['variable']))
        self.score.loc[:, 't_mean'] = pd.Series(self.t_values.mean(dim=['date', 'time']), index=self.t_values['variable'])
        self.score.loc[:, 't_mean_abs'] = np.abs(self.score.loc[:, 't_mean'])
        # !需要改t2_ratio,t2_ratio_positive
        self.score.loc[:, 't2_ratio'] = pd.Series((self.t_values > 2).sum(dim=['date', 'time']) / self.t_values.count(dim=['date', 'time']))
        self.score.loc[:, 't2_ratio_positive'] = pd.Series(
            (self.t_values > 2).sum(dim=['date', 'time']) / (self.t_values > 0).sum(dim=['date', 'time']))
        self.score.loc[:, 'IC_mean'] = pd.Series(self.IC.mean(dim=['date', 'time']), index=self.IC['variable'])
        self.score.loc[:, 'ICIR'] = self.score.loc[:, 'IC_mean'] / pd.Series(self.IC.std(dim=['date', 'time'], ddof=1) * np.sqrt(240),
                                                                             index=self.IC['variable'])
        self.score.loc[:, 'IC_long_mean'] = pd.Series(self.IC_long.mean(dim=['date', 'time']), index=self.IC_long['variable'])
        self.score.loc[:, 'IC_short_mean'] = pd.Series(self.IC_short.mean(dim=['date', 'time']), index=self.IC_short['variable'])
        self.score.loc[:, 'turnver_mean'] = pd.Series(self.turnover.mean(dim=['date', 'time']), index=self.turnover['variable'])

    def decorr(self, score_name: str, corr_thres: List[int], get_detail: bool = False) -> pd.DataFrame:
        coef = self.beta
        score = self.score[score_name]
        coef = coef.stack(datetime=('date', 'time'))
        coef = pd.DataFrame(np.squeeze(coef.values.T), index=coef['datetime'], columns=coef['variable'])
        if len(corr_thres) == 1:
            corr_thres = pd.Series(float(*corr_thres), index=score.index)
        print(f"start separate alpha by correlation lower than {corr_thres}")
        coef = coef.replace(0, np.nan)
        pick = []
        score = score.sort_values(ascending=False)
        max_per_iter = 5000

        if get_detail:
            separateDetail = pd.DataFrame(columns=['score', 'num_remaining'])

        def _corr(df1: pd.DataFrame, df2: pd.DataFrame) -> pd.DataFrame:
            # calculate correlation between df1.columns and df2.columns
            df1_flag = df1.notnull().astype(int)
            df2_flag = df2.notnull().astype(int)
            df1 = df1.fillna(0)
            df2 = df2.fillna(0)
            df1_sum = df1.T.dot(df2_flag)
            df2_sum = df2.T.dot(df1_flag).T
            n_sample = df1_flag.T.dot(df2_flag)

            corr = ((df1.T.dot(df2) - (df1_sum * df2_sum) / n_sample) / np.sqrt(
                ((df1**2).T.dot(df2_flag) - (df1_sum**2) / n_sample) * ((df2**2).T.dot(df1_flag).T - (df2_sum**2) / n_sample)))
            return corr  # DataFrame with df1.columns as rows and df2.columns as cols

        iter_times = 0
        while len(score):
            iter_start = tm.time()  # print time used in current iter
            score_iter, score = score[:max_per_iter], score[max_per_iter:]
            remaining = score_iter.index
            # DataFrame method corr() can only calculate correlation of all columns in one df
            # use _corr() to accelerate and save memory resources
            corr = _corr(coef.loc[:, pick + remaining.tolist()], coef.loc[:, remaining]).fillna(1)
            remaining = remaining[~(corr.loc[pick, remaining].abs().max() > corr_thres.loc[remaining])]
            score_iter = score_iter.loc[remaining]
            corr = corr.loc[remaining, remaining]
            while len(remaining):
                add_alp = score_iter.idxmax()
                add_alp_score = score_iter.loc[add_alp]
                pick.append(add_alp)
                remaining = remaining[corr.loc[add_alp].abs() <= corr_thres.loc[remaining]]
                score_iter = score_iter.loc[remaining]
                corr = corr.loc[remaining, remaining]
                if get_detail:
                    separateDetail.loc[add_alp] = [add_alp_score, len(remaining) + len(score)]
            iter_times += 1
            iter_end = tm.time()  # print time used in current iter
            print(f"iter{iter_times}({(iter_end - iter_start):.2f}sec), now {len(pick)} picked alphas and {len(score)} remaining")
        print(f"total {iter_times} iters, picked {len(pick)} alphas")
        drop = list(set(coef.columns) - set(pick))
        self.factor_name_picked.update({score_name: pick})
        self.factor_name_dropped.update({score_name: drop})
        if get_detail:
            return separateDetail

    def predict_beta(self,
                     window,
                     date_range: Union[List[int], None] = None,
                     beta: xr.DataArray = None,
                     beta_path: str = None,
                     isSave: bool = True,
                     save_path: str = None):
        # 滚动三十天计算日度的beta
        if save_path is None:
            if ('fit_result_path' in setting.keys()) and (setting['fit_result_path'] is not None):
                save_path = setting['fit_result_path']
            else:
                save_path = os.path.join(setting['dst_dir'], 'fit_result')
        filename = [self.y_name]
        filename.extend(sorted(self.alpha_name))
        filename = '_'.join(filename)
        save_path = os.path.join(save_path, filename, 'predicted_beta')
        if not os.path.exists(save_path):
            os.makedirs(save_path)
        if beta is None:
            if self.beta is not None:
                beta = self.beta
            else:
                if beta_path is None:
                    if ('beta_path' in setting.keys()) and (setting['beta_path'] is not None):
                        beta_path = setting['beta_path']
                    elif ('fit_result_path' in setting.keys()) and (setting['fit_result_path'] is not None):
                        beta_path = os.path.join(setting['fit_result_path'], filename, 'beta')
                    else:
                        beta_path = os.path.join(setting['dst_dir'], 'fit_result', filename, 'beta')
                # date_range = BaseDataSet.get_tradingday_range(trade_calendar_path=setting['trade_calendar_path'], start_date=setting['start_date'], end_date=setting['end_date'], timeWindow=1, delay=0)
                beta = None
                factor_list = os.listdir(beta_path)
                for ifactor in factor_list:
                    ibeta_path = os.path.join(beta_path, ifactor)
                    ibeta = BaseDataSet.read_minute_data(ibeta_path, date_range=date_range).sortby('date', ascending=True)
                    if beta is None:
                        beta = ibeta
                    else:
                        beta = xr.concat([beta, ibeta], dim='variable')
        # self.predicted_beta = beta.mean(dim = 'time').rolling_exp(window ={'date': window}).mean().shift(date = 1)
        self.predicted_beta = beta.rolling(date=window).mean().shift(date=1)

        if isSave:

            def __to_dict(arr: xr.DataArray):
                newArr = {}
                for var in arr['variable'].values:
                    newArr.update({var: arr.sel(variable=[var])})
                return newArr

            BaseDataSet.save_DataArray(__to_dict(self.predicted_beta), save_path, byDate=True, separateFolder=True)

    def predict_return(self,
                       dataset: BaseDataSet,
                       alphaset: BaseAlphaSet,
                       date_range: Union[List[int], None] = None,
                       predicted_beta: xr.DataArray = None,
                       save_path: str = None,
                       predicted_beta_path: str = None):
        if save_path is None:
            if ('fit_result_path' in setting.keys()) and (setting['fit_result_path'] is not None):
                save_path = setting['fit_result_path']
            else:
                save_path = os.path.join(setting['dst_dir'], 'fit_result')
        filename = [self.y_name]
        filename.extend(sorted(self.alpha_name))
        filename = '_'.join(filename)
        save_path = os.path.join(save_path, filename, 'predicted_beta')
        if not os.path.exists(save_path):
            os.makedirs(save_path)
        if predicted_beta is None:
            if self.predicted_beta is not None:
                predicted_beta = self.predicted_beta
            else:
                if predicted_beta_path is None:
                    if ('predicted_beta_path' in setting.keys()) and (setting['predicted_beta_path'] is not None):
                        predicted_beta_path = setting['predicted_beta_path']
                    elif ('fit_result_path' in setting.keys()) and (setting['fit_result_path'] is not None):
                        predicted_beta_path = os.path.join(setting['fit_result'], filename, 'predicted_beta')
                    else:
                        predicted_beta_path = os.path.join(setting['dst_dir'], 'fit_result', filename, 'predicted_beta')

                date_range = BaseDataSet.get_tradingday_range(start_date=setting['start_date'],end_date=setting['end_date'])
                predicted_beta = None
                factor_list = os.listdir(predicted_beta_path)
                for ifactor in factor_list:
                    ipath = os.path.join(predicted_beta_path, ifactor)
                    ipredicted_beta = BaseDataSet.read_minute_data(ipath, date_range=date_range).sortby('date', ascending=True)
                    if predicted_beta is None:
                        predicted_beta = ipredicted_beta
                    else:
                        predicted_beta = xr.concat([predicted_beta, ipredicted_beta], dim='variable')

        #! 计算 PredictedReturn (具体问题具体分析)
        if self.alpha_name is None:
            alpha = alphaset.residual[self.fitter_name]
        else:
            alpha = alphaset.residual[self.fitter_name].sel(variable=self.alpha_name)
        self.predicted_return = dataset.datetime_lag(alpha * predicted_beta, lag=setting['lag'])
        self.predicted_return['variable'] = ['PredictedReturn']
        BaseDataSet.save_DataArray(dict(predicted_return=self.predicted_return), save_path, byDate=True, separateFolder=True)

    def getWeight(self,
                  dataset: BaseDataSet,
                  PredictedReturn: xr.DataArray = None,
                  predicted_return_path: str = None,
                  save_path: str = None,
                  pre_weight: xr.DataArray = None,
                  MaxTotalPosition: float = 1,  # 总持仓上限.总仓位（1-总持仓上限） #若总仓位存在上限，如80%，可将初始值设为0.2
                  MaxStockPosition: float = 0.03,  # 个股持仓上限
                  SwapTurnover: float = 0.025,  # 每分钟最大换仓 
                  AddTurnover: float = 1,  # 每次加仓上限, 针对绝对收益. 相对收益默认值即可.
                  BuyBenchmark: float = 0.0015,  # 加仓基准
                  SellBenchmark: float = 0,  # 减仓基准
                  SwapBenchmark: float = 0.0015,  # 换仓基准
                  Relative: bool = True,  # 相对收益or绝对收益
                  Delay: int = 0,  # 推迟XX天才开开始调仓
                  isReturn: bool = False,
    ):
        def process_weight(Weight, TradableWeight, date, time):
            # 计算本分钟内因股价波动而带来的仓位被动变化
            TotalPosition_before = Weight.loc[:, date, time, 'Weight'].sum()
            Weight.loc[:, date, time, 'Weight'] = Weight.loc[:, date, time, 'Weight'] * Return.loc[:, date, time, 'MinuteReturn']
            ScaleWeight = (MaxTotalPosition - TotalPosition_before + Weight.loc[:, date, time, 'Weight'].sum())
            Weight.loc[:, date, time, 'Weight'] = Weight.loc[:, date, time, 'Weight'] / ScaleWeight
            TradableWeight.loc[:, date, time,
                               'SellableWeight'] = Return.loc[:, date, time, 'MinuteReturn'] * TradableWeight.loc[:, date, time,
                                                                                                                  'SellableWeight'] / ScaleWeight
            TradableWeight.loc[:, date, time, 'BuyableWeight'] = np.maximum(MaxStockPosition - Weight.loc[:, date, time, 'Weight'], 0)

            return Weight, TradableWeight

        def DecIncrease(Weight, TradableWeight, date, time):
            iPR = PredictedReturn.loc[:, date, time, 'PredictedReturn'].dropna(dim='ticker').to_pandas().rename('PR')
            BuyPR = iPR[iPR > BuyBenchmark]
            SellPR = iPR[iPR < SellBenchmark]
            iTradableWeight = TradableWeight.loc[:, date, time, :].to_pandas()
            BuyWeight = iTradableWeight['BuyableWeight'][iTradableWeight['BuyableWeight'] > 0].rename('Weight')
            SellWeight = iTradableWeight['SellableWeight'][iTradableWeight['SellableWeight'] > 0].rename('Weight')
            toBuy = pd.concat([BuyWeight, BuyPR], axis=1, join='inner')
            toSell = pd.concat([SellWeight, SellPR], axis=1, join='inner')

            if not toSell.empty:
                Weight.loc[toSell.index, date, time, 'Weight'] -= toSell['Weight']
                TradableWeight.loc[toSell.index, date, time, 'SellableWeight'] -= toSell['Weight']
                print('全行业纯减仓%f' % (Weight.loc[:, date, time, 'Weight'].sum()))
            remain = MaxTotalPosition - Weight.loc[:, date, time, 'Weight'].sum().values
            if (remain > 0) & (not toBuy.empty):  #总仓位仍有可加的空间,并且有可加的票
                toBuy = toBuy.sort_values(by='PR', ascending=False)
                toBuy['AccumWeight'] = np.min([remain, AddTurnover]) - toBuy['Weight'].cumsum()
                tickers = toBuy.index[toBuy['AccumWeight'] > 0]
                Weight.loc[tickers, date, time, 'Weight'] = MaxStockPosition
                ticker = toBuy.index[toBuy['AccumWeight'] <= 0]
                if not ticker.empty:
                    Weight.loc[ticker[0], date, time, 'Weight'] += toBuy.loc[ticker[0], 'Weight'] + toBuy['AccumWeight'][ticker[0]]
                print('全行业纯加仓%f' % (Weight.loc[:, date, time, 'Weight'].sum()))
            TradableWeight.loc[:, date, time, 'BuyableWeight'] = np.maximum(MaxStockPosition - Weight.loc[:, date, time, 'Weight'], 0)
            return Weight, TradableWeight

        def MatchBM(Weight, TradableWeight, date, time):
            # 符合加仓条件的股票：1、预测收益率大于绝对基准
            iPR = PredictedReturn.loc[:, date, time, 'PredictedReturn'].dropna(dim='ticker').to_pandas().rename('PR')
            BuyPR = iPR[iPR > BuyBenchmark]
            SellPR = iPR[iPR < SellBenchmark]
            iTradableWeight = TradableWeight.loc[:, date, time, :].to_pandas()
            BuyWeight = iTradableWeight['BuyableWeight'][iTradableWeight['BuyableWeight'] > 0].rename('Weight')
            SellWeight = iTradableWeight['SellableWeight'][iTradableWeight['SellableWeight'] > 0].rename('Weight')
            toBuy = pd.concat([BuyWeight, BuyPR], axis=1, join='inner')
            toSell = pd.concat([SellWeight, SellPR], axis=1, join='inner')

            # 有可加仓的股票
            if (not toBuy.empty) or (not toSell.empty):
                # 符合加仓条件的股票行业：
                for i in range(len(indlist)):
                    sumi = Weight.loc[Weight['ticker'].isin(indlist[i]), date, time, :].sum().values
                    diff = dailyBMlimit[i] - sumi
                    remain = MaxTotalPosition - Weight.loc[:, date, time, :].sum().values
                    if ((diff > 0) & (remain > 0)):  #行业i仍有可加的仓位    且          总仓位仍有可加的空间
                        itoBuy = toBuy.loc[toBuy.index.isin(indlist[i]), :]
                        if not itoBuy.empty:  #行业i存在符合加仓条件的股票
                            print('\t加仓:行业%d:%f' % (i + 1, min(itoBuy['Weight'].sum(), diff, remain, AddTurnover)))
                            itoBuy = itoBuy.sort_values(by='PR', ascending=False)
                            itoBuy['AccumWeight'] = np.min([diff, remain, AddTurnover]) - itoBuy['Weight'].cumsum()
                            tickers = itoBuy.index[itoBuy['AccumWeight'] > 0]
                            Weight.loc[tickers, date, time, 'Weight'] = MaxStockPosition
                            ticker = itoBuy.index[itoBuy['AccumWeight'] <= 0]
                            if not ticker.empty:
                                Weight.loc[ticker[0], date, time, 'Weight'] += itoBuy.loc[ticker[0], 'Weight'] + itoBuy.loc[ticker[0], 'AccumWeight']

                    elif (diff < 0):
                        itoSell = toSell.loc[toSell.index.isin(indlist[i]), :]
                        if not itoSell.empty:
                            print('\t减仓:行业%d:%f' % (i + 1, min(itoSell['Weight'].sum(), -diff)))
                            itoSell = itoSell.sort_values(by='PR', ascending=True)
                            itoSell['AccumWeight'] = -diff - itoSell['Weight'].cumsum()
                            tickers = itoSell.index[itoSell['AccumWeight'] > 0]
                            Weight.loc[tickers, date, time, 'Weight'] -= itoSell.loc[tickers, 'Weight']
                            TradableWeight.loc[tickers, date, time, 'SellableWeight'] = 0
                            ticker = itoSell.index[itoSell['AccumWeight'] <= 0]
                            if not ticker.empty:
                                Weight.loc[ticker[0], date, time,
                                           'Weight'] -= (itoSell.loc[ticker[0], 'Weight'] + itoSell.loc[ticker[0], 'AccumWeight'])
                                TradableWeight.loc[ticker[0], date, time, 'SellableWeight'] = -itoSell.loc[ticker[0], 'AccumWeight']
                TradableWeight.loc[:, date, time, 'BuyableWeight'] = np.maximum(MaxStockPosition - Weight.loc[:, date, time, 'Weight'], 0)
            return Weight, TradableWeight

        def Swap(Weight, TradableWeight, date, time):
            Diff = pd.DataFrame()
            iPR = PredictedReturn.loc[:, date, time, 'PredictedReturn'].dropna(dim='ticker').to_pandas().rename('PR')  # !PR没有值和等于0是不一样的
            if not iPR.empty:
                for i in range(indNum):
                    iTradableWeight = TradableWeight.loc[TradableWeight['ticker'].isin(indlist[i]), date, time, :].to_pandas()
                    SellWeight = iTradableWeight['SellableWeight'][iTradableWeight['SellableWeight'] > 0].rename('Weight')
                    BuyWeight = iTradableWeight['BuyableWeight'][iTradableWeight['BuyableWeight'] > 0].rename('Weight')
                    Sell = pd.concat([SellWeight, iPR], axis=1, join='inner').dropna(how='any').reset_index().rename(columns={
                        'PR': 'PR-',
                        'ticker': 'ticker-'
                    }).sort_values(by='PR-', ascending=True)
                    Buy = pd.concat([BuyWeight, iPR], axis=1, join='inner').dropna(how='any').reset_index().rename(columns={
                        'PR': 'PR+',
                        'ticker': 'ticker+'
                    }).sort_values(by='PR+', ascending=False)
                    Sell['AccumWeight'], Buy['AccumWeight'] = Sell['Weight'].cumsum(), Buy['Weight'].cumsum()
                    diff = pd.concat([Sell, Buy], axis=0, join='outer').sort_values(by='AccumWeight').bfill()
                    diff['diff'] = diff['PR+'] - diff['PR-']
                    diff['weight_diff'] = diff['AccumWeight'] - diff['AccumWeight'].shift(1).fillna(0)
                    Diff = pd.concat([Diff, diff], axis=0)

            if not Diff.empty:
                Diff = Diff.reset_index(drop=True)
                Diff = Diff.sort_values(by='diff', ascending=False).loc[Diff['diff'] > SwapBenchmark, :]
                if not Diff.empty:
                    print('\t换仓:%f' % (np.min([SwapTurnover, Diff['weight_diff'].sum()])))
                    Diff['allIndAW'] = Diff['weight_diff'].cumsum()
                    # 单边限制
                    Change = Diff.loc[Diff['allIndAW'] < SwapTurnover, ['ticker+', 'ticker-', 'weight_diff']]
                    if (Diff['allIndAW'] >= SwapTurnover).any():
                        ticker = Diff.index[Diff['allIndAW'] >= SwapTurnover][0]
                        Change.loc[ticker, :] = Diff.loc[ticker, :]
                        Change.loc[ticker, 'weight_diff'] = Diff.loc[ticker, 'weight_diff'] - (Diff.loc[ticker, 'allIndAW'] - SwapTurnover)

                    Weight.loc[Change['ticker+'].unique(), date, time,
                               'Weight'] += Change.groupby('ticker+', sort=False)['weight_diff'].sum()[Change['ticker+'].unique()]
                    Weight.loc[Change['ticker-'].unique(), date, time,
                               'Weight'] -= Change.groupby('ticker-', sort=False)['weight_diff'].sum()[Change['ticker-'].unique()]
                    # TradableWeight.loc[Change['ticker+'].unique(),date,time,'BuyableWeight'] -= Change.groupby('ticker+')['weight_diff'].sum().values
                    TradableWeight.loc[Change['ticker-'].unique(), date, time,
                                       'SellableWeight'] -= Change.groupby('ticker-', sort=False)['weight_diff'].sum()[Change['ticker-'].unique()]
                    TradableWeight.loc[:, date, time, 'BuyableWeight'] = np.maximum(MaxStockPosition - Weight.loc[:, date, time, 'Weight'], 0)

            return Weight, TradableWeight

        def get_dailyBMlimit(BM, idate):
            dailyBMlimit = []
            for indstocks in indlist:
                indweight = BM[BM['ticker'].isin(indstocks)].sel(date=idate).sum().values
                dailyBMlimit.append(indweight)
            dailyBMlimit = dailyBMlimit / sum(dailyBMlimit)
            return dailyBMlimit

        # weight保存路径
        if save_path is None:
            if ('fit_result_path' in setting.keys()) and (setting['fit_result_path'] is not None):
                save_path = setting['fit_result_path']
            else:
                save_path = os.path.join(setting['dst_dir'], 'fit_result')
        filename = [self.y_name]
        filename.extend(sorted(self.alpha_name))
        filename = '_'.join(filename)
        save_path = os.path.join(save_path, filename)

        weight_save_path = os.path.join(save_path, 'weight')
        turnover_save_path = os.path.join(save_path, 'turnover')
        if not os.path.exists(weight_save_path):
            os.makedirs(weight_save_path)
        if not os.path.exists(turnover_save_path):
            os.makedirs(turnover_save_path)

        Return = (dataset.Return.fillna(0) + 1)  #!函数中为全局变量
        industry = dataset.industry[:, -1, -1, -1].astype(int).to_pandas().rename('industry')
        indlist = []  #!函数中为全局变量
        indNum = np.array(industry).max()  #!函数中为全局变量
        for i in range(indNum):
            indlist.append(list(industry[industry == (i + 1)].index))
        # BM已经shift过了
        BM = dataset.benchmark.squeeze(dim=['time', 'variable']).transpose('ticker', 'date')
        datelist_delay = dataset.date_range[Delay:]
        if PredictedReturn is None:  #!函数中为全局变量
            if self.predicted_return is not None:
                PredictedReturn = self.predicted_return.reindex(date=dataset.date_range)
            else:
                if predicted_return_path is None:
                    if ('predicted_return_path' in setting.keys()) and (setting['predicted_return_path'] is not None):
                        predicted_return_path = setting['predicted_return_path']
                    elif ('fit_result_path' in setting.keys()) and (setting['fit_result_path'] is not None):
                        predicted_return_path = os.path.join(setting['fit_result_path'], filename, 'predicted_return')
                    else:
                        predicted_return_path = os.path.join(setting['dst_dir'], 'fit_result', filename, 'predicted_return')
                PredictedReturn = BaseDataSet.read_minute_data(data_path=os.path.join(predicted_return_path),
                                                               date_range=datelist_delay,
                                                               ticker_list=dataset.ticker_list)
        if pre_weight is None:
            pre_weight = xr.DataArray(coords=[dataset.ticker_list], dims=['ticker']).fillna(0)
        Weight = dataset.creat_dataarray(var=['Weight']).reindex(date=datelist_delay).fillna(0)
        turnover: xr.DataArray = dataset.creat_dataarray(var=['TotalTO', 'SwapTO', 'MatchTO']).reindex(date=datelist_delay).sum(
            dim='ticker').fillna(0)
        TradableWeight = dataset.creat_dataarray(var=['BuyableWeight', 'SellableWeight']).reindex(date=datelist_delay).fillna(0)
        TradableWeight.loc[:, :, :, 'BuyableWeight'] = MaxStockPosition

        DayStartFlag = dataset.timelist[0]
        print('开始循环')

        for date in datelist_delay:
            start_time = tm.time()
            dailyBMlimit = get_dailyBMlimit(BM, date)
            for time in dataset.timelist:
                Weight.loc[:, date, time, 'Weight'] = pre_weight
                if time == DayStartFlag:
                    TradableWeight.loc[:, date, time, 'SellableWeight'] = pre_weight
                else:
                    TradableWeight.loc[:, date, time, 'SellableWeight'] = pre_SellableWeight
                Weight, TradableWeight = process_weight(Weight, TradableWeight, date, time)
                weight_scaled = Weight.loc[:, date, time, 'Weight'].copy()
                if Relative:
                    Weight, TradableWeight = MatchBM(Weight, TradableWeight, date, time)
                    turnover.loc[date, time, 'MatchTO'] = np.abs(Weight.loc[:, date, time, 'Weight'] - weight_scaled).sum(dim='ticker')
                    Weight, TradableWeight = Swap(Weight, TradableWeight, date, time)
                    turnover.loc[date, time,
                                 'SwapTO'] = np.abs(Weight.loc[:, date, time, 'Weight'] - weight_scaled).sum(dim='ticker') - turnover.loc[date, time,
                                                                                                                                       'MatchTO']
                else:
                    Weight, TradableWeight = DecIncrease(Weight, TradableWeight, date, time)
                turnover.loc[date, time, 'TotalTO'] = np.abs(Weight.loc[:, date, time, 'Weight'] - weight_scaled).sum(dim='ticker')
                print('日期:%s,时间:%s,仓位:%f' % (date, time, Weight.loc[:, date, time, :].sum()))
                pre_weight = Weight.loc[:, date, time, 'Weight'].copy()
                pre_SellableWeight = TradableWeight.loc[:, date, time, 'SellableWeight']

            # 算换手，算组合收益，BM收益
            turnover.sel(date=[date]).transpose('date', 'time', 'variable').to_netcdf(path=os.path.join(turnover_save_path, f'{date}.h5'))
            Weight.sel(date=[date]).transpose('ticker', 'date', 'time', 'variable').to_netcdf(path=os.path.join(weight_save_path, f'{date}.h5'))
            print('耗时%fs' % (tm.time() - start_time))

        self.weight = Weight
        self.turnover = turnover
        if isReturn:
            return Weight, turnover, pre_weight

    def getPortReturn(self,
                      dataset: BaseDataSet,
                      weight: xr.DataArray = None,
                      turnover: xr.DataArray = None,
                      weight_path: str = None,
                      load_date: Union[List[int], None] = None,
                      isSave: bool = True,
                      save_path: str = None,
                      isReturn: bool = False,
                      isLoadTO: bool = True,
                      TO_path: str = None,
                      costRate: int = 0.0015,
                      Delay: int = 0,
    ):
        """getPortReturn _summary_

            1. 给出weight: xr.DataArray
            2. 给出self.weight
            3. 给出weight的路径. 
            4. 若1,2,3都不给出, 则从默认路径读取weight.
            注: 读取日期默认同dataset.date_range, 与dataset的日期偏离由Delay控制. 若load_date=None and Delay=None, 则读取所有日期.
        Args:
            turnover (xr.DataArray, optional): 1:只有date和time两个维度.2:三个维度date,time,variable.variable长度为1.
            load_date (List(int), optional): load_date读取的日期范围.若为空,Delay.
            Delay (int, optional): Delay=None表示读路径下的所有日期,Delay=0表示同dataset.date_range. Defaults to 0.

        Returns:
            html: 保存在默认路径下: fit_result/portReturn/netValue.html
        """

        # 如果weight的time为每天的所有time
        def timescale(Return: np.ndarray, weight: np.ndarray):
            weight.flags.writeable = True
            weight[np.isnan(weight)] = 0
            totalposition_before = weight.sum()
            scale_weight = weight * Return
            scale_weight[np.isnan(scale_weight)] = 0
            scale_weight = scale_weight / (1 - totalposition_before + scale_weight.sum())
            return scale_weight, weight

        # 如果weight的time为每天的几个固定点
        def datescale(Return: np.ndarray, weight: np.ndarray):
            previous_scale_weight = 0  #比如每天只给两个时间点,shift滞后，第一天的第一分钟weight.sum()==0
            weight.flags.writeable = True
            scale_weight = np.zeros_like(weight)
            for i in range(weight.shape[1]):
                if (np.isnan(weight[:, i]).sum() == weight.shape[0]) | (weight[:, i].sum() == 0):
                    weight[:, i] = previous_scale_weight
                totalposition_before = weight[:, i].sum()
                temp = weight[:, i] * Return[:, i]
                temp[np.isnan(temp)] = 0
                scale_weight[:, i] = previous_scale_weight = temp / (1 - totalposition_before + temp.sum())
            return scale_weight, weight

        def xr_scaleweight(Return: xr.DataArray, weight: xr.DataArray, isTimeScale: bool = True) -> Tuple[xr.DataArray, xr.DataArray]:
            if isTimeScale:
                scale_weight, weight = xr.apply_ufunc(timescale,
                                                      Return,
                                                      weight,
                                                      input_core_dims=[['ticker'], ['ticker']],
                                                      output_core_dims=[['ticker'], ['ticker']],
                                                      vectorize=True,
                                                      dask="parallelized",
                                                      output_dtypes=[weight.dtype, weight.dtype])
            else:
                scale_weight, weight = xr.apply_ufunc(
                    datescale,
                    Return,
                    weight,
                    input_core_dims=[['ticker', 'time'], ['ticker', 'time']],
                    output_core_dims=[['ticker', 'time'], ['ticker', 'time']],  # 计算beta:'variable',计算resid:'ticker'
                    vectorize=True,
                    dask="parallelized",
                    output_dtypes=[weight.dtype, weight.dtype])
            return scale_weight.fillna(0), weight.fillna(0)

        def calcStatistics(weight: xr.DataArray,
                           turnover_: xr.DataArray,
                           BMweight: xr.DataArray,
                           Return: xr.DataArray,
                           save_path: str,
                           costRate: int = 0.0015):
            """calcStatistics _summary_

            传入weight,BMweight,turnover,Return算一些指标, 画图的数据.
            注: weight没有滞后,BMweight是滞后一期的.

            Args:
                weight (xr.DataArray): 未滞后.
                turnover (xr.DataArray): _description_
                BMweight (xr.DataArray): 滞后一期.
                Return (xr.DataArray): _description_
                save_path (str): _description_
                costRate (int, optional): _description_. Defaults to 0.0015.

            Returns:
                _type_: _description_
            """
            if ('variable' in turnover_.dims) and (len(turnover_['variable']) > 1):
                turnover = turnover_.sel(variable='TotalTO')
            else:
                turnover = turnover_.squeeze()

            portReturn = (weight.stack(datetime=['date', 'time']).shift(datetime=1).unstack().fillna(0) * (Return - 1)).sum(dim='ticker') + 1
            portReturnNet = (portReturn - turnover * costRate / 2)
            BMReturn = (BMweight * (Return - 1)).sum(dim='ticker') + 1
            portReturn_BM = portReturn - BMReturn + 1
            portReturnNet_BM = portReturnNet - BMReturn + 1

            # *Date
            # 条形图
            totalWeightDate = weight.isel(time=-1).sum(dim='ticker')
            # turnoverDate = turnover.sum(dim = 'time')
            turnoverDate = turnover_.sum(dim='time')
            # 折线图
            portReturnDate = portReturn.prod(dim='time').cumprod(dim='date')
            portReturnNetDate = portReturnNet.prod(dim='time').cumprod(dim='date')
            BMReturnDate = BMReturn.prod(dim='time').cumprod(dim='date')
            portReturn_BMDate = portReturn_BM.prod(dim='time').cumprod(dim='date')
            portReturnNet_BMDate = portReturnNet_BM.prod(dim='time').cumprod(dim='date')

            # Time
            turnoverTime = turnover_.mean(dim='date')
            # 折线图
            portReturnTime = portReturn.mean(dim='date').cumprod(dim='time')
            portReturnNetTime = portReturnNet.mean(dim='date').cumprod(dim='time')
            BMReturnTime = BMReturn.mean(dim='date').cumprod(dim='time')
            portReturn_BMTime = portReturn_BM.mean(dim='date').cumprod(dim='time')
            portReturnNet_BMTime = portReturnNet_BM.mean(dim='date').cumprod(dim='time')

            plot_data = {
                'totalWeightDate': totalWeightDate,
                'turnoverDate': turnoverDate,
                'portReturnDate': portReturnDate,
                'portReturnNetDate': portReturnNetDate,
                'BMReturnDate': BMReturnDate,
                'portReturn_BMDate': portReturn_BMDate,
                'portReturnNet_BMDate': portReturnNet_BMDate,
                'turnoverTime': turnoverTime,
                'portReturnTime': portReturnTime,
                'portReturnNetTime': portReturnNetTime,
                'BMReturnTime': BMReturnTime,
                'portReturn_BMTime': portReturn_BMTime,
                'portReturnNet_BMTime': portReturnNet_BMTime,
            }
            port_value = pd.Series(portReturnDate.values)
            annual_return = pd.Series(port_value.iloc[-1]**(252 / len(port_value)) - 1)
            tracking_error = pd.Series((portReturn.prod(dim='time') - 1).std(dim='date').values * np.sqrt(252))
            sharp_ratio = annual_return.div(tracking_error.replace(0, np.nan))
            portValueMax = port_value.expanding().max()
            portValueMin = port_value.iloc[::-1].expanding().min()[::-1]
            maximum_drawdown = pd.Series(((portValueMax - portValueMin) / portValueMax).max())
            meanTurnover = pd.Series(turnover.sum(dim='time').mean(dim='date').values)

            stats = pd.concat([
                annual_return.rename('年化收益率'),
                tracking_error.rename('跟踪误差(年化波动率)'),
                sharp_ratio.rename('夏普比率'),
                maximum_drawdown.rename('最大回撤'),
                meanTurnover.rename('平均换手率')
            ],
                              axis=1).T
            with pd.ExcelWriter(os.path.join(save_path, 'stats.xlsx')) as writer:
                stats.to_excel(writer, sheet_name='Stats_Indicators')
                pd.Series(portReturnTime.values, index=portReturnTime['time'].values).to_excel(writer, sheet_name='Date_Return')
                pd.Series(portReturnDate.values, index=portReturnDate['date'].values).to_excel(writer, sheet_name='Time_Return')
                pd.DataFrame(turnoverDate.values, index=turnoverDate['date'].values).to_excel(writer, sheet_name='Date_turnover')
                pd.DataFrame(turnoverTime.values, index=turnoverTime['time'].values).to_excel(writer, sheet_name='Time_turnover')
            print('Stats_excel Saved! Port_net_value_excel Saved!')
            return portReturn, portReturnNet, plot_data, stats

        def plot_netValue(plot_data, save_path: str):
            def BarDate(lastweightDate, turnoverDate_, portReturnDate, portReturnNetDate, BMReturnDate, portReturn_BMDate,
                        portReturnNet_BMDate) -> Bar:
                date_axis = [str(idate) for idate in list(portReturnDate['date'].values)]
                lastweightDate = list(lastweightDate.values)
                portReturnDate = list(portReturnDate.values)
                portReturnNetDate = list(portReturnNetDate.values)
                BMReturnDate = list(BMReturnDate.values)
                portReturn_BMDate = list(portReturn_BMDate.values)
                portReturnNet_BMDate = list(portReturnNet_BMDate.values)

                date_axis.insert(0, 'preday_ret')
                lastweightDate.insert(0, 0.0)
                portReturnDate.insert(0, 1.0)
                portReturnNetDate.insert(0, 1.0)
                BMReturnDate.insert(0, 1.0)
                portReturn_BMDate.insert(0, 1.0)
                portReturnNet_BMDate.insert(0, 1.0)

                if ('variable' in turnoverDate_.dims) and (len(turnoverDate_['variable']) > 1):
                    turnoverDate = list(turnoverDate_.sel(variable='TotalTO').values)
                    turnoverSwapDate = list(turnoverDate_.sel(variable='SwapTO').values)
                    turnoverMatchDate = list(turnoverDate_.sel(variable='MatchTO').values)
                    turnoverDate.insert(0, 0.0)
                    turnoverSwapDate.insert(0, 0.0)
                    turnoverMatchDate.insert(0, 0.0)
                else:
                    turnoverDate = list(turnoverDate_.squeeze().values)
                    turnoverDate.insert(0, 0.0)

                bar = (Bar(init_opts=opts.InitOpts(width='1500px', height='900px'))
                       .add_xaxis(date_axis)
                       .add_yaxis("dailyTotalWeight", lastweightDate, z=0, label_opts=opts.LabelOpts(is_show=False))
                       .add_yaxis("dailyTurnover", turnoverDate, z=0, label_opts=opts.LabelOpts(is_show=False))
                       .extend_axis(yaxis=opts.AxisOpts(type_="value",name="收益率",max_='dataMax',min_='dataMin',position="left",
                                    axisline_opts=opts.AxisLineOpts(linestyle_opts=opts.LineStyleOpts(color="#215EFF")),
                                    axislabel_opts=opts.LabelOpts(formatter="{value}"),
                                    splitline_opts=opts.SplitLineOpts(is_show=True, linestyle_opts=opts.LineStyleOpts(opacity=1)),))
                       .set_global_opts(yaxis_opts=opts.AxisOpts(name="换手率",max_='dataMax',min_='dataMin',position="right",
                                        axisline_opts=opts.AxisLineOpts(linestyle_opts=opts.LineStyleOpts(color="#d14a61")),),
                                        title_opts=opts.TitleOpts(title="组合收益及换手率--date"),
                                        tooltip_opts=opts.TooltipOpts(trigger="axis", axis_pointer_type="cross"),
                                        xaxis_opts=opts.AxisOpts(min_='dataMin'),
                                        legend_opts=opts.LegendOpts(type_="scroll", pos_left="right", orient="vertical")))
                if 'turnoverSwapDate' in locals():
                    (bar.add_yaxis('dailyTurnoverSwap', turnoverSwapDate, z=0, stack='stack0', label_opts=opts.LabelOpts(is_show=False))
                        .add_yaxis('dailyTurnoverMatch', turnoverMatchDate, z=0, stack='stack0', label_opts=opts.LabelOpts(is_show=False)))

                line = (Line().add_xaxis(date_axis)
                             .add_yaxis("return", portReturnDate, yaxis_index=1, label_opts=opts.LabelOpts(is_show=False),
                                        itemstyle_opts=opts.ItemStyleOpts(color='#5793f3'))
                             .add_yaxis("return-cost", portReturnNetDate, yaxis_index=1, label_opts=opts.LabelOpts(is_show=False),
                                        itemstyle_opts=opts.ItemStyleOpts(color='#A86C0C'))
                             .add_yaxis("bm", BMReturnDate, yaxis_index=1, label_opts=opts.LabelOpts(is_show=False),
                                        itemstyle_opts=opts.ItemStyleOpts(color='#d14a61'))
                             .add_yaxis("return-bm", portReturn_BMDate, yaxis_index=1, label_opts=opts.LabelOpts(is_show=False),
                                        itemstyle_opts=opts.ItemStyleOpts(color='#FF9A5B'))
                             .add_yaxis("return-bm-cost", portReturnNet_BMDate, yaxis_index=1, label_opts=opts.LabelOpts(is_show=False),
                                        itemstyle_opts=opts.ItemStyleOpts(color='#FF9A5B')))
                bar.overlap(line)
                return bar

            def BarTime(turnoverTime_, portReturnTime, portReturnNetTime, BMReturnTime, portReturn_BMTime, portReturnNet_BMTime) -> Bar:
                time_axis = [str(itime) for itime in list(portReturnTime['time'].values)]
                portReturnTime = list(portReturnTime.values)
                portReturnNetTime = list(portReturnNetTime.values)
                BMReturnTime = list(BMReturnTime.values)
                portReturn_BMTime = list(portReturn_BMTime.values)
                portReturnNet_BMTime = list(portReturnNet_BMTime.values)

                time_axis.insert(0, 'preday_ret')
                portReturnTime.insert(0, 1.0)
                portReturnNetTime.insert(0, 1.0)
                BMReturnTime.insert(0, 1.0)
                portReturn_BMTime.insert(0, 1.0)
                portReturnNet_BMTime.insert(0, 1.0)

                if ('variable' in turnoverTime_.dims) and (len(turnoverTime_['variable']) > 1):
                    turnoverTime = list(turnoverTime_.sel(variable='TotalTO').values)
                    turnoverSwapTime = list(turnoverTime_.sel(variable='SwapTO').values)
                    turnoverMatchTime = list(turnoverTime_.sel(variable='MatchTO').values)
                    turnoverTime.insert(0, 0.0)
                    turnoverSwapTime.insert(0, 0.0)
                    turnoverMatchTime.insert(0, 0.0)
                else:
                    turnoverTime = list(turnoverTime_.squeeze().values)
                    turnoverTime.insert(0, 0.0)

                bar = (Bar(init_opts=opts.InitOpts(width='1500px', height='900px'))
                       .add_xaxis(time_axis)
                       .add_yaxis("turnover", turnoverTime, z=0, label_opts=opts.LabelOpts(is_show=False))
                       .extend_axis(yaxis=opts.AxisOpts(type_="value",name="收益率",max_='dataMax',min_='dataMin',position="left",
                                    axisline_opts=opts.AxisLineOpts(linestyle_opts=opts.LineStyleOpts(color="#215EFF")),
                                    axislabel_opts=opts.LabelOpts(formatter="{value}"),
                                    splitline_opts=opts.SplitLineOpts(is_show=True, linestyle_opts=opts.LineStyleOpts(opacity=1)),))
                       .set_global_opts(yaxis_opts=opts.AxisOpts(name="换手率",max_='dataMax',min_='dataMin',position="right",
                                        axisline_opts=opts.AxisLineOpts(linestyle_opts=opts.LineStyleOpts(color="#d14a61")),),
                                        datazoom_opts=opts.DataZoomOpts(),
                                        title_opts=opts.TitleOpts(title="组合收益及换手率--date"),
                                        tooltip_opts=opts.TooltipOpts(trigger="axis", axis_pointer_type="cross"),
                                        xaxis_opts=opts.AxisOpts(min_='dataMin'),
                                        legend_opts=opts.LegendOpts(type_="scroll", pos_left="right", orient="vertical")))

                if 'turnoverSwapDate' in locals():
                    (bar.add_yaxis('turnoverSwap', turnoverSwapTime, z=0, stack='stack0',label_opts=opts.LabelOpts(is_show=False))
                        .add_yaxis('turnoverMatch',turnoverMatchTime,z=0,stack='stack0',label_opts=opts.LabelOpts(is_show=False)))

                line = (Line().add_xaxis(time_axis)
                              .add_yaxis("return",portReturnTime,yaxis_index=1,label_opts=opts.LabelOpts(is_show=False),
                                         itemstyle_opts=opts.ItemStyleOpts(color='#5793f3'))
                              .add_yaxis("return-cost",portReturnNetTime,yaxis_index=1,label_opts=opts.LabelOpts(is_show=False),
                                         itemstyle_opts=opts.ItemStyleOpts(color='#A86C0C'))
                              .add_yaxis("bm",BMReturnTime,yaxis_index=1,label_opts=opts.LabelOpts(is_show=False),
                                         itemstyle_opts=opts.ItemStyleOpts(color='#d14a61'))
                              .add_yaxis("return-bm",portReturn_BMTime,yaxis_index=1,label_opts=opts.LabelOpts(is_show=False),
                                         itemstyle_opts=opts.ItemStyleOpts(color='#FF9A5B'))
                              .add_yaxis("return-bm-cost",portReturnNet_BMTime,yaxis_index=1,label_opts=opts.LabelOpts(is_show=False),
                                         itemstyle_opts=opts.ItemStyleOpts(color='#FF9A5B')))
                bar.overlap(line)
                return bar

            (Tab().add(
                BarDate(plot_data['totalWeightDate'], plot_data['turnoverDate'], plot_data['portReturnDate'], plot_data['portReturnNetDate'],
                        plot_data['BMReturnDate'], plot_data['portReturn_BMDate'], plot_data['portReturnNet_BMDate']), "Date")
                  .add(
                BarTime(plot_data['turnoverTime'], plot_data['portReturnTime'], plot_data['portReturnNetTime'], plot_data['BMReturnTime'],
                        plot_data['portReturn_BMTime'], plot_data['portReturnNet_BMTime']), "Time").render(os.path.join(save_path,'netValue.html')))
            print("Net Value Graph saved")

        if save_path is None:
            if ('fit_result_path' in setting.keys()) and (setting['fit_result_path'] is not None):
                save_path = setting['fit_result_path']
            else:
                save_path = os.path.join(setting['dst_dir'], 'fit_result')
        filename = [self.y_name]
        filename.extend(sorted(self.alpha_name))
        filename = '_'.join(filename)
        save_path = os.path.join(save_path, filename, 'portValue')

        if not os.path.exists(save_path):
            os.makedirs(save_path)

        if (load_date is None) and (Delay is not None): load_date = dataset.date_range[Delay:]

        if weight is None:
            if self.weight is not None:
                weight = self.weight
            else:
                if ('weight_path' in setting.keys()) and (setting['weight_path'] is not None):
                    weight_path = setting['weight_path']
                elif ('fit_result_path' in setting.keys()) and (setting['fit_result_path'] is not None):
                    weight_path = os.path.join(setting['fit_result_path'], filename, 'weight')
                else:
                    weight_path = os.path.join(setting['dst_dir'], 'fit_result', filename, 'weight')
                weight = BaseDataSet.read_minute_data(weight_path, date_range=load_date, ticker_list=dataset.ticker_list, timelist=dataset.timelist).fillna(0)
        # weight滞后一个time, weight四个维度，variable长度必须为1
        weight = weight.reindex(time=dataset.timelist, ticker=dataset.ticker_list, method=None).squeeze(dim='variable')
        datelist = weight['date'].values.tolist()
        BM = dataset.benchmark.squeeze(dim='variable').reindex(time=dataset.timelist, date=datelist).fillna(0)  #BM读进来时已经滞后一期了
        Return = dataset.Return.loc[:, :, :, 'MinuteReturn'].reindex(date=datelist).fillna(0) + 1
        _, BMweight = xr_scaleweight(Return, BM, isTimeScale=False)

        if turnover is None:
            if isLoadTO:
                if TO_path is None:
                    if ('TO_path' in setting.keys()) and (setting['TO_path'] is not None):
                        TO_path = setting['TO_path']
                    elif ('fit_result_path' in setting.keys()) and (setting['fit_result_path'] is not None):
                        TO_path = os.path.join(setting['fit_result_path'], 'turnover')
                    else:
                        TO_path = os.path.join(setting['dst_dir'], 'fit_result', 'turnover')
                turnover = BaseDataSet.read_minute_data(TO_path, date_range=load_date, timelist=dataset.timelist).fillna(0)
            else:
                if self.turnover is not None: #! 这个逻辑有问题
                    turnover = self.turnover
                else:
                    weight_ = weight.stack(datetime=['date', 'time']).shift(datetime=1).unstack().fillna(0)
                    scale_weight, _ = xr_scaleweight(Return, weight_, isTimeScale=True)
                    turnover = np.abs(weight - scale_weight).sum(dim='ticker').transpose('date', 'time')
        turnover = turnover.reindex(time=dataset.timelist, method=None).fillna(0)

        #! calcStatistics传入的weight没有滞后,BMweight滞后一期
        portReturn, portReturnNet, plot_data, stats = calcStatistics(weight, turnover, BMweight, Return, save_path, costRate)
        if isSave:
            if isLoadTO:
                BaseDataSet.save_DataArray(dict(portReturn=portReturn, portReturnNet=portReturnNet), save_path)
            else:
                BaseDataSet.save_DataArray(dict(portReturn=portReturn, portReturnNet=portReturnNet, turnover=turnover), save_path)

        plot_netValue(plot_data, os.path.join(save_path, 'netValue.html'))
        if isReturn:
            if isLoadTO:
                return portReturn, portReturnNet, stats
            else:
                return portReturn, turnover, portReturnNet, stats

    # 需要把getPortReturn拆开. 对一段较长的时间, 分段跑完weight, 画图仍然需要较大的内存, 因为还需要读dataset和weight, 这两个变量都是 ticker X date X time X variable
    def calc_netValue(self,
                      dataset: BaseDataSet,
                      weight: xr.DataArray = None,
                      turnover: xr.DataArray = None,
                      weight_path: str = None,
                      load_date: Union[List[int], None] = None,
                      isSave: bool = True,
                      save_path: str = None,
                      isReturn: bool = False,
                      isLoadTO: bool = True,
                      TO_path: str = None,
                      costRate: int = 0.0015,
                      Delay: Union[int, None] = None,):
        """getPortReturn _summary_

            1. 给出weight: xr.DataArray
            2. 给出self.weight
            3. 给出weight的路径. 
            4. 若1,2,3都不给出, 则从默认路径读取weight.
            注: 读取日期默认同dataset.date_range, 与dataset的日期偏离由Delay控制. 若load_date=None and Delay=None, 则读取所有日期.
        Args:
            turnover (xr.DataArray, optional): 1:只有date和time两个维度.2:三个维度date,time,variable.variable长度为1.
            load_date (List(int), optional): load_date读取的日期范围.若为空,Delay.
            Delay (int, optional): Delay=None表示读路径下的所有日期,Delay=0表示同dataset.date_range. Defaults to 0.

        Returns:
            html: 保存在默认路径下: fit_result/portReturn/netValue.html
        """

        # 如果weight的time为每天的所有time
        def timescale(Return: np.ndarray, weight: np.ndarray):
            weight.flags.writeable = True
            weight[np.isnan(weight)] = 0
            totalposition_before = weight.sum()
            scale_weight = weight * Return
            scale_weight[np.isnan(scale_weight)] = 0
            scale_weight = scale_weight / (1 - totalposition_before + scale_weight.sum())
            return scale_weight, weight

        # 如果weight的time为每天的几个固定点
        def datescale(Return: np.ndarray, weight: np.ndarray):
            previous_scale_weight = 0  #比如每天只给两个时间点,shift滞后，第一天的第一分钟weight.sum()==0
            weight.flags.writeable = True
            scale_weight = np.zeros_like(weight)
            for i in range(weight.shape[1]):
                if (np.isnan(weight[:, i]).sum() == weight.shape[0]) | (weight[:, i].sum() == 0):
                    weight[:, i] = previous_scale_weight
                totalposition_before = weight[:, i].sum()
                temp = weight[:, i] * Return[:, i]
                temp[np.isnan(temp)] = 0
                scale_weight[:, i] = previous_scale_weight = temp / (1 - totalposition_before + temp.sum())
            return scale_weight, weight

        def xr_scaleweight(Return: xr.DataArray, weight: xr.DataArray, isTimeScale: bool = True) -> Tuple[xr.DataArray, xr.DataArray]:
            if isTimeScale:
                scale_weight, weight = xr.apply_ufunc(timescale, Return, weight,
                                                      input_core_dims=[['ticker'], ['ticker']],
                                                      output_core_dims=[['ticker'], ['ticker']],
                                                      vectorize=True,
                                                      dask="parallelized",
                                                      output_dtypes=[weight.dtype, weight.dtype])
            else:
                scale_weight, weight = xr.apply_ufunc(datescale, Return, weight,
                                                      input_core_dims=[['ticker', 'time'], ['ticker', 'time']],
                                                      output_core_dims=[['ticker', 'time'], ['ticker', 'time']],  # 计算beta:'variable',计算resid:'ticker'
                                                      vectorize=True,
                                                      dask="parallelized",
                                                      output_dtypes=[weight.dtype, weight.dtype])
            return scale_weight.fillna(0), weight.fillna(0)

        def calc(weight: xr.DataArray, turnover_: xr.DataArray, BMweight: xr.DataArray, Return: xr.DataArray, costRate: int = 0.0015):
            if ('variable' in turnover_.dims) and (len(turnover_['variable']) > 1):
                turnover = turnover_.sel(variable='TotalTO')
            else:
                turnover = turnover_.squeeze()
            portReturn = (((weight.stack(datetime=['date', 'time']).shift(datetime=1).unstack().fillna(0) * (Return - 1)).sum(dim='ticker') + 1)
                           .expand_dims(dim = dict(variable=['portReturn'])))
            portReturnNet = (portReturn - turnover * costRate / 2)
            BMReturn = (BMweight * (Return - 1)).sum(dim='ticker') + 1
            totalWeight = weight.sum(dim='ticker')
            net_value = xr.concat([portReturn,portReturnNet,BMReturn,totalWeight], dim='variable')
            net_value['variable'] = ['portReturn','portReturnNet','BMReturn','totalWeight']
            return net_value

        if save_path is None:
            if ('fit_result_path' in setting.keys()) and (setting['fit_result_path'] is not None):
                save_path = setting['fit_result_path']
            else:
                save_path = os.path.join(setting['dst_dir'], 'fit_result')
        filename = [self.y_name]
        filename.extend(sorted(self.alpha_name))
        filename = '_'.join(filename)
        save_path = os.path.join(save_path, filename, 'net_value')

        if not os.path.exists(save_path):
            os.makedirs(save_path)

        if (load_date is None) and (Delay is not None):
            load_date = dataset.date_range[Delay:]

        if weight is None:
            if self.weight is not None:
                weight = self.weight
            else:
                if ('weight_path' in setting.keys()) and (setting['weight_path'] is not None):
                    weight_path = setting['weight_path']
                elif ('fit_result_path' in setting.keys()) and (setting['fit_result_path'] is not None):
                    weight_path = os.path.join(setting['fit_result_path'], filename, 'weight')
                else:
                    weight_path = os.path.join(setting['dst_dir'], 'fit_result', filename, 'weight')
                weight = BaseDataSet.read_minute_data(weight_path, date_range=load_date, ticker_list=dataset.ticker_list, timelist=dataset.timelist).fillna(0)
        # weight滞后一个time, weight四个维度，variable长度必须为1
        weight = weight.reindex(time=dataset.timelist, ticker=dataset.ticker_list, method=None).squeeze(dim='variable')
        datelist = weight['date'].values.tolist()
        BM = dataset.benchmark.squeeze(dim='variable').reindex(time=dataset.timelist, date=datelist).fillna(0)  #BM读进来时已经滞后一期了
        Return = dataset.Return.loc[:, :, :, 'MinuteReturn'].reindex(date=datelist).fillna(0) + 1
        _, BMweight = xr_scaleweight(Return, BM, isTimeScale=False)

        if turnover is None:
            if isLoadTO:
                if TO_path is None:
                    if ('TO_path' in setting.keys()) and (setting['TO_path'] is not None):
                        TO_path = setting['TO_path']
                    elif ('fit_result_path' in setting.keys()) and (setting['fit_result_path'] is not None):
                        TO_path = os.path.join(setting['fit_result_path'], 'turnover')
                    else:
                        TO_path = os.path.join(setting['dst_dir'], 'fit_result', 'turnover')
                turnover = BaseDataSet.read_minute_data(TO_path, date_range=load_date, timelist=dataset.timelist).fillna(0)
            else:
                if self.turnover is not None: #! 这个逻辑有问题
                    turnover = self.turnover
                else:
                    weight_ = weight.stack(datetime=['date', 'time']).shift(datetime=1).unstack().fillna(0)
                    scale_weight, _ = xr_scaleweight(Return, weight_, isTimeScale=True)
                    turnover = np.abs(weight - scale_weight).sum(dim='ticker').transpose('date', 'time')
        turnover = turnover.reindex(time=dataset.timelist, method=None).fillna(0)

        #! calc传入的weight没有滞后,BMweight滞后一期
        net_value = calc(weight, turnover, BMweight, Return, costRate)
        if isSave:
            BaseDataSet.save_DataArray(dict(net_value=net_value, turnover=turnover), save_path, byDate=True, separateFolder=True)

        if isReturn:
            return net_value, turnover

    def plot_netValue(self,
                      plot_data: Dict[str, xr.DataArray] = None, # str: 'net_velua', 'turnover'
                      load_path: Union[str, None] = None,
                      save_path: Union[str, None] = None,
                      load_date: Union[List[int], None] = None, ):

        # 从文件中plot_data, 然后画图.
        # 先读plot_data和turnover

        def calc(net_value: xr.DataArray, turnover_: xr.DataArray, save_path: str):
            portReturn = net_value.sel(variable='portReturn')
            portReturnNet = net_value.sel(variable='portReturnNet')
            BMReturn = net_value.sel(variable='BMReturn')
            totalWeight = net_value.sel(variable='totalWeight')
            portReturn_BM = portReturn - BMReturn + 1
            portReturnNet_BM = portReturnNet - BMReturn + 1

            if ('variable' in turnover_.dims) and (len(turnover_['variable']) > 1):
                turnover = turnover_.sel(variable='TotalTO')
            else:
                turnover = turnover_.squeeze()

            # *Date
            # 条形图
            totalWeightDate = totalWeight.isel(time=-1)
            # turnoverDate = turnover.sum(dim = 'time')
            turnoverDate = turnover_.sum(dim='time')
            # 折线图
            portReturnDate = portReturn.prod(dim='time').cumprod(dim='date')
            portReturnNetDate = portReturnNet.prod(dim='time').cumprod(dim='date')
            BMReturnDate = BMReturn.prod(dim='time').cumprod(dim='date')
            portReturn_BMDate = portReturn_BM.prod(dim='time').cumprod(dim='date')
            portReturnNet_BMDate = portReturnNet_BM.prod(dim='time').cumprod(dim='date')

            # Time
            turnoverTime = turnover_.mean(dim='date')
            # 折线图
            portReturnTime = portReturn.mean(dim='date').cumprod(dim='time')
            portReturnNetTime = portReturnNet.mean(dim='date').cumprod(dim='time')
            BMReturnTime = BMReturn.mean(dim='date').cumprod(dim='time')
            portReturn_BMTime = portReturn_BM.mean(dim='date').cumprod(dim='time')
            portReturnNet_BMTime = portReturnNet_BM.mean(dim='date').cumprod(dim='time')

            plot_data = {
                        'totalWeightDate': totalWeightDate,
                        'turnoverDate': turnoverDate,
                        'portReturnDate': portReturnDate,
                        'portReturnNetDate': portReturnNetDate,
                        'BMReturnDate': BMReturnDate,
                        'portReturn_BMDate': portReturn_BMDate,
                        'portReturnNet_BMDate': portReturnNet_BMDate,
                        'turnoverTime': turnoverTime,
                        'portReturnTime': portReturnTime,
                        'portReturnNetTime': portReturnNetTime,
                        'BMReturnTime': BMReturnTime,
                        'portReturn_BMTime': portReturn_BMTime,
                        'portReturnNet_BMTime': portReturnNet_BMTime,
                         }
            port_value = pd.Series(portReturnDate.values)
            annual_return = pd.Series(port_value.iloc[-1]**(252 / len(port_value)) - 1)
            tracking_error = pd.Series((portReturn.prod(dim='time') - 1).std(dim='date').values * np.sqrt(252))
            sharp_ratio = annual_return.div(tracking_error.replace(0, np.nan))
            portValueMax = port_value.expanding().max()
            portValueMin = port_value.iloc[::-1].expanding().min()[::-1]
            maximum_drawdown = pd.Series(((portValueMax - portValueMin) / portValueMax).max())
            meanTurnover = pd.Series(turnover.sum(dim='time').mean(dim='date').values)

            stats = pd.concat([
                annual_return.rename('年化收益率'),
                tracking_error.rename('跟踪误差(年化波动率)'),
                sharp_ratio.rename('夏普比率'),
                maximum_drawdown.rename('最大回撤'),
                meanTurnover.rename('平均换手率')
            ],axis=1).T
            with pd.ExcelWriter(os.path.join(save_path, 'stats.xlsx')) as writer:
                stats.to_excel(writer, sheet_name='Stats_Indicators')
                pd.Series(portReturnTime.values, index=portReturnTime['time'].values).to_excel(writer, sheet_name='Date_Return')
                pd.Series(portReturnDate.values, index=portReturnDate['date'].values).to_excel(writer, sheet_name='Time_Return')
                pd.DataFrame(turnoverDate.values, index=turnoverDate['date'].values).to_excel(writer, sheet_name='Date_turnover')
                pd.DataFrame(turnoverTime.values, index=turnoverTime['time'].values).to_excel(writer, sheet_name='Time_turnover')
            print('Stats_excel Saved! Port_net_value_excel Saved!')
            return plot_data, stats

        def plot(plot_data, save_path: str):
            def BarDate(lastweightDate, turnoverDate_, portReturnDate, portReturnNetDate, BMReturnDate, portReturn_BMDate,
                        portReturnNet_BMDate) -> Bar:
                date_axis = [str(idate) for idate in list(portReturnDate['date'].values)]
                lastweightDate = list(lastweightDate.values)
                portReturnDate = list(portReturnDate.values)
                portReturnNetDate = list(portReturnNetDate.values)
                BMReturnDate = list(BMReturnDate.values)
                portReturn_BMDate = list(portReturn_BMDate.values)
                portReturnNet_BMDate = list(portReturnNet_BMDate.values)

                date_axis.insert(0, 'preday_ret')
                lastweightDate.insert(0, 0.0)
                portReturnDate.insert(0, 1.0)
                portReturnNetDate.insert(0, 1.0)
                BMReturnDate.insert(0, 1.0)
                portReturn_BMDate.insert(0, 1.0)
                portReturnNet_BMDate.insert(0, 1.0)

                if ('variable' in turnoverDate_.dims) and (len(turnoverDate_['variable']) > 1):
                    turnoverDate = list(turnoverDate_.sel(variable='TotalTO').values)
                    turnoverSwapDate = list(turnoverDate_.sel(variable='SwapTO').values)
                    turnoverMatchDate = list(turnoverDate_.sel(variable='MatchTO').values)
                    turnoverDate.insert(0, 0.0)
                    turnoverSwapDate.insert(0, 0.0)
                    turnoverMatchDate.insert(0, 0.0)
                else:
                    turnoverDate = list(turnoverDate_.squeeze().values)
                    turnoverDate.insert(0, 0.0)

                bar = (Bar(init_opts=opts.InitOpts(width='1500px', height='900px'))
                       .add_xaxis(date_axis)
                       .add_yaxis("dailyTotalWeight", lastweightDate, z=0, label_opts=opts.LabelOpts(is_show=False))
                       .add_yaxis("dailyTurnover", turnoverDate, z=0, label_opts=opts.LabelOpts(is_show=False))
                       .extend_axis(yaxis=opts.AxisOpts(type_="value",name="收益率",max_='dataMax',min_='dataMin',position="left",
                                    axisline_opts=opts.AxisLineOpts(linestyle_opts=opts.LineStyleOpts(color="#215EFF")),
                                    axislabel_opts=opts.LabelOpts(formatter="{value}"),
                                    splitline_opts=opts.SplitLineOpts(is_show=True, linestyle_opts=opts.LineStyleOpts(opacity=1)),))
                       .set_global_opts(yaxis_opts=opts.AxisOpts(name="换手率",max_='dataMax',min_='dataMin',position="right",
                                        axisline_opts=opts.AxisLineOpts(linestyle_opts=opts.LineStyleOpts(color="#d14a61")),),
                                        title_opts=opts.TitleOpts(title="组合收益及换手率--date"),
                                        tooltip_opts=opts.TooltipOpts(trigger="axis", axis_pointer_type="cross"),
                                        xaxis_opts=opts.AxisOpts(min_='dataMin'),
                                        legend_opts=opts.LegendOpts(type_="scroll", pos_left="right", orient="vertical")))
                if 'turnoverSwapDate' in locals():
                    (bar.add_yaxis('dailyTurnoverSwap', turnoverSwapDate, z=0, stack='stack0', label_opts=opts.LabelOpts(is_show=False))
                        .add_yaxis('dailyTurnoverMatch', turnoverMatchDate, z=0, stack='stack0', label_opts=opts.LabelOpts(is_show=False)))

                line = (Line().add_xaxis(date_axis)
                             .add_yaxis("return", portReturnDate, yaxis_index=1, label_opts=opts.LabelOpts(is_show=False),
                                        itemstyle_opts=opts.ItemStyleOpts(color='#5793f3'))
                             .add_yaxis("return-cost", portReturnNetDate, yaxis_index=1, label_opts=opts.LabelOpts(is_show=False),
                                        itemstyle_opts=opts.ItemStyleOpts(color='#A86C0C'))
                             .add_yaxis("bm", BMReturnDate, yaxis_index=1, label_opts=opts.LabelOpts(is_show=False),
                                        itemstyle_opts=opts.ItemStyleOpts(color='#d14a61'))
                             .add_yaxis("return-bm", portReturn_BMDate, yaxis_index=1, label_opts=opts.LabelOpts(is_show=False),
                                        itemstyle_opts=opts.ItemStyleOpts(color='#FF9A5B'))
                             .add_yaxis("return-bm-cost", portReturnNet_BMDate, yaxis_index=1, label_opts=opts.LabelOpts(is_show=False),
                                        itemstyle_opts=opts.ItemStyleOpts(color='#FF9A5B')))
                bar.overlap(line)
                return bar

            def BarTime(turnoverTime_, portReturnTime, portReturnNetTime, BMReturnTime, portReturn_BMTime, portReturnNet_BMTime) -> Bar:
                time_axis = [str(itime) for itime in list(portReturnTime['time'].values)]
                portReturnTime = list(portReturnTime.values)
                portReturnNetTime = list(portReturnNetTime.values)
                BMReturnTime = list(BMReturnTime.values)
                portReturn_BMTime = list(portReturn_BMTime.values)
                portReturnNet_BMTime = list(portReturnNet_BMTime.values)

                time_axis.insert(0, 'preday_ret')
                portReturnTime.insert(0, 1.0)
                portReturnNetTime.insert(0, 1.0)
                BMReturnTime.insert(0, 1.0)
                portReturn_BMTime.insert(0, 1.0)
                portReturnNet_BMTime.insert(0, 1.0)

                if ('variable' in turnoverTime_.dims) and (len(turnoverTime_['variable']) > 1):
                    turnoverTime = list(turnoverTime_.sel(variable='TotalTO').values)
                    turnoverSwapTime = list(turnoverTime_.sel(variable='SwapTO').values)
                    turnoverMatchTime = list(turnoverTime_.sel(variable='MatchTO').values)
                    turnoverTime.insert(0, 0.0)
                    turnoverSwapTime.insert(0, 0.0)
                    turnoverMatchTime.insert(0, 0.0)
                else:
                    turnoverTime = list(turnoverTime_.squeeze().values)
                    turnoverTime.insert(0, 0.0)

                bar = (Bar(init_opts=opts.InitOpts(width='1500px', height='900px'))
                       .add_xaxis(time_axis)
                       .add_yaxis("turnover", turnoverTime, z=0, label_opts=opts.LabelOpts(is_show=False))
                       .extend_axis(yaxis=opts.AxisOpts(type_="value",name="收益率",max_='dataMax',min_='dataMin',position="left",
                                    axisline_opts=opts.AxisLineOpts(linestyle_opts=opts.LineStyleOpts(color="#215EFF")),
                                    axislabel_opts=opts.LabelOpts(formatter="{value}"),
                                    splitline_opts=opts.SplitLineOpts(is_show=True, linestyle_opts=opts.LineStyleOpts(opacity=1)),))
                       .set_global_opts(yaxis_opts=opts.AxisOpts(name="换手率",max_='dataMax',min_='dataMin',position="right",
                                        axisline_opts=opts.AxisLineOpts(linestyle_opts=opts.LineStyleOpts(color="#d14a61")),),
                                        datazoom_opts=opts.DataZoomOpts(),
                                        title_opts=opts.TitleOpts(title="组合收益及换手率--date"),
                                        tooltip_opts=opts.TooltipOpts(trigger="axis", axis_pointer_type="cross"),
                                        xaxis_opts=opts.AxisOpts(min_='dataMin'),
                                        legend_opts=opts.LegendOpts(type_="scroll", pos_left="right", orient="vertical")))

                if 'turnoverSwapDate' in locals():
                    (bar.add_yaxis('turnoverSwap', turnoverSwapTime, z=0, stack='stack0',label_opts=opts.LabelOpts(is_show=False))
                        .add_yaxis('turnoverMatch',turnoverMatchTime,z=0,stack='stack0',label_opts=opts.LabelOpts(is_show=False)))

                line = (Line().add_xaxis(time_axis)
                              .add_yaxis("return",portReturnTime,yaxis_index=1,label_opts=opts.LabelOpts(is_show=False),
                                         itemstyle_opts=opts.ItemStyleOpts(color='#5793f3'))
                              .add_yaxis("return-cost",portReturnNetTime,yaxis_index=1,label_opts=opts.LabelOpts(is_show=False),
                                         itemstyle_opts=opts.ItemStyleOpts(color='#A86C0C'))
                              .add_yaxis("bm",BMReturnTime,yaxis_index=1,label_opts=opts.LabelOpts(is_show=False),
                                         itemstyle_opts=opts.ItemStyleOpts(color='#d14a61'))
                              .add_yaxis("return-bm",portReturn_BMTime,yaxis_index=1,label_opts=opts.LabelOpts(is_show=False),
                                         itemstyle_opts=opts.ItemStyleOpts(color='#FF9A5B'))
                              .add_yaxis("return-bm-cost",portReturnNet_BMTime,yaxis_index=1,label_opts=opts.LabelOpts(is_show=False),
                                         itemstyle_opts=opts.ItemStyleOpts(color='#FF9A5B')))
                bar.overlap(line)
                return bar

            (Tab().add(
                BarDate(plot_data['totalWeightDate'], plot_data['turnoverDate'], plot_data['portReturnDate'], plot_data['portReturnNetDate'],
                        plot_data['BMReturnDate'], plot_data['portReturn_BMDate'], plot_data['portReturnNet_BMDate']), "Date")
                  .add(
                BarTime(plot_data['turnoverTime'], plot_data['portReturnTime'], plot_data['portReturnNetTime'], plot_data['BMReturnTime'],
                        plot_data['portReturn_BMTime'], plot_data['portReturnNet_BMTime']), "Time").render(os.path.join(save_path,'netValue.html')))
            print("Net Value Graph saved")

        if save_path is None:
            if ('fit_result_path' in setting.keys()) and (setting['fit_result_path'] is not None):
                save_path = setting['fit_result_path']
            else:
                save_path = os.path.join(setting['dst_dir'], 'fit_result')
        filename = [self.y_name]
        filename.extend(sorted(self.alpha_name))
        filename = '_'.join(filename)
        save_path = os.path.join(save_path, filename, 'net_value_plot')
        if not os.path.exists(save_path):
            os.makedirs(save_path)

        if plot_data is not None:
            net_value = plot_data['net_value']
            turnover = plot_data['turnover']
        else:
            if load_path is None:
                if ('net_value_path' in setting.keys()) and (setting['net_value_path'] is not None):
                    load_path = setting['net_value_path']
                elif ('fit_result_path' in setting.keys()) and (setting['fit_result_path'] is not None):
                    load_path = os.path.join(setting['fit_result'], filename, 'net_value')
                else:
                    load_path = os.path.join(setting['dst_dir'], 'fit_result', filename, 'net_value')
            net_value = BaseDataSet.read_minute_data(os.path.join(load_path,'net_value'), date_range=load_date)
            turnover = BaseDataSet.read_minute_data(os.path.join(load_path,'turnover'), date_range=load_date)

        plot_data, _ = calc(net_value, turnover, save_path=save_path)
        plot(plot_data, save_path=save_path)

    def returnAttri(self,
                    dataset: BaseDataSet,
                    Return: xr.DataArray = None,
                    weight: xr.DataArray = None,
                    weight_path: str = None,
                    save_path: str = None,
                    BM: bool = True,
                    isAdjust: bool = False):
        def return_adjust(Return: xr.DataArray, method='carino') -> xr.DataArray:
            newRet = xr.full_like(Return, np.nan).stack(datetime=('date', 'time')).copy()
            df = Return.squeeze(dim='variable').stack(datetime=('date',
                                                                'time')).transpose('datetime',
                                                                                   'variable').to_dataframe(name='values').pivot(index='datetime',
                                                                                                                                 columns='variable',
                                                                                                                                 values='values')
            R = (df.loc[:, 'portfolio_return'] + 1).cumprod()[-1] - 1
            R_T = df.loc[:, 'portfolio_return']
            T = len(R_T)
            if method == 'carino':
                c_t = (np.log(1 + R_T) / R_T) / (np.log(1 + R) / R).fillna(1)
            elif method == 'menchero':
                c_t = (R / (pow(1 + R, 1 / T) - 1)) / T + R_T * (R - ((R / pow(1 + R, 1 / T)) - 1) / T * R_T.sum()) / (pow(R_T, 2).sum())
            df['adjust'] = c_t
            df[df.columns.difference(['portfolio_return', 'adjust'])] = df[df.columns.difference(['portfolio_return',
                                                                                                  'adjust'])].multiply(df.loc[:, 'adjust'],
                                                                                                                       axis='index')
            newRet.loc[:, :] = df[df.columns.difference(['adjust'])].loc[:, Return['variable'].values].values
            return newRet.unstack().transpose('date', 'time', 'variable')

        def get_beta(dataset: BaseDataSet, Return: xr.DataArray) -> xr.DataArray:
            def ols(y: np.array, X: np.array, w: np.array) -> np.array:
                beta = LinearRegressionFitter.OLS(y, X, W=w)['beta']
                return beta

            def xr_ols(y: xr.DataArray, X: xr.DataArray, w: xr.DataArray) -> xr.DataArray:
                beta = xr.apply_ufunc(
                    ols, y, X, w,
                    input_core_dims=[['ticker'], ['ticker', 'variable'], ["ticker"]],  # 每个参数只有一个条目的列表
                    output_core_dims=[['variable']],  # 计算beta:'variable',计算resid:'ticker'
                    exclude_dims=set(('ticker', )),  # 结果变量中要删除的变量
                    vectorize=True,
                    dask="parallelized",
                    output_dtypes=[y.dtype]  # 每个输出一个； 也可以是float或np.dtype（“ float64”）
                )
                return beta

            y = Return.squeeze(dim='variable').where(dataset.flag.squeeze() == 1).fillna(0)
            X = xr.concat([dataset.industry_dummies, dataset.risk_factor],
                          dim='variable').squeeze(dim='time').where(dataset.flag.squeeze() == 1).fillna(0)
            w = (1 / np.sqrt(dataset.capital)).fillna(0).squeeze(dim=['variable', 'time']).where(dataset.flag.squeeze() == 1).fillna(0)
            beta = xr_ols(y, X, w)
            return beta

        def return_dec(dataset: BaseDataSet,
                       Return: xr.DataArray,
                       weight: xr.DataArray,
                       beta: xr.DataArray,
                       adjust: bool = False) -> Tuple[xr.DataArray]:
            portfolio_return = (weight.squeeze() * Return.squeeze()).sum(dim='ticker').expand_dims(dim={'variable': 'portfolio_return'})
            portfolio_factor = weight.squeeze(dim='variable') * xr.concat([dataset.industry_dummies, dataset.risk_factor],
                                                                          dim='variable').sum(dim='ticker')
            bechmark_return = beta * portfolio_factor
            alpha_return = (portfolio_return.squeeze() - bechmark_return.sum(dim='variable').squeeze()).expand_dims(dim={'variable': 'alpha_return'})
            total_return = xr.concat([portfolio_return, bechmark_return, alpha_return], dim='variable')
            if adjust:
                total_return = return_adjust(total_return, method='')
            accu_total_return = (total_return + 1).stack(datetime=('date', 'time')).prod(dim='datetime')
            return total_return, accu_total_return

        if save_path == None:
            if 'returnAttri_save_path' in setting.keys():
                if setting['returnAttri_save_path'] != None:
                    save_path = setting['returnAttri_save_path']
                else:
                    save_path = os.path.join(setting['dst_dir'], 'returnAttri')
            else:
                save_path = os.path.join(setting['dst_dir'], 'returnAttri')
        if not os.path.exists(save_path):
            os.makedirs(save_path)

        if weight == None:
            if self.weight != None:
                weight = self.weight
            elif weight_path == None:
                if 'weight_path' in setting.keys():
                    if setting['weight_path'] != None:
                        weight_path = setting['weight_path']
                else:
                    weight_path = os.path.join(setting['dst_dir'], 'weight')
                weight = BaseDataSet.read_minute_data(weight_path, dataset.date_range, dataset.ticker_list)
        weight = weight.squeeze().fillna(0).stack(datetime=('date',
                                                            'time')).shift(datetime=1).unstack().reindex(ticker=dataset.ticker_list).transpose(
                                                                'ticker', 'date', 'time')

        if BM:
            weight = ((weight - dataset.benchmark.fillna(0).squeeze(dim=['time', 'variable'])).squeeze(dim='variable').transpose(
                'ticker', 'date', 'time'))

        if Return == None:
            Return = dataset.Return.loc[:, :, :, 'MiunteReturn']

        beta = get_beta(dataset, Return)
        total_return, accu_total_return = return_dec(dataset, Return, weight, beta, isAdjust)
        return total_return, accu_total_return

    def save_statistics(self, dst_dir: str):
        pass

    def load_statistics(self, dst_dir: str):
        pass


setting = {
    "start_date": 20211103,
    "end_date": 20211214,
    "daily_lag": True,
    "timelist": None,  #不给出的话,读取timelist_path->timelist_file中的timelist
    "lag": 2,  #滞后的分钟
    "varlist": ['market','Return','transaction',
                'risk_factor','industry_dummies','industry',
                'capital','benchmark','flag','price_ipo_st_flag'],
    # 全部可加的varlist ['market','Return',,'index',
    #                   'risk_factor','industry_dummies','industry',
    #                   'capital','benchmark','flag','price_ipo_st_flag']
    "dst_dir": "/usr/intern/test_liujl",  # 根目录: 所有数据默认的保存和读取路径, 
    "BaseDataSet_path": None,  # 二级目录: 想要新做一个BaseDataSet, 可以重设该路径.
    "y_path": None,  # 三级目录: 想要读取另外已经准备好的y,需要重设该目录.
    "fit_result_path": None,  # 二级目录: 每次不同的fit_result, 可以只修改该路径.
    "beta_path": None,  # 三级目录:
    "predicted_beta_path": None,  # 三级目录:
    "predicted_return_path": None,  # 三级目录:
    "weight_path": None,  # 三级目录:
    "daily_data_path": "/usr/datashare/risk_factors_190711",
    "timelist_path": "/usr/intern/",
    "market_path": "/usr/datashare/level2/Market/1m",
    "transaction_path": "/usr/datashare/level2/Transaction/1m/",
    "index_path": None,
    "risk_factor_path": "/usr/datashare/risk_factors_200206",
    "alpha_path": None, #"/usr/intern/Alpha/cashflow_factor/"
    "univ_flag_file_name": "flag_return.csv",
    "ipo_st_flag_file_name": "flag_ipo_st.csv",
    "timelist_file_name": "minute_timelist.csv",
    "market_fields": ["Close", "preDayClose"],  # Close和PreDayClose必须要有, 可以新加其他字段, 用来算y.
    "transaction_fields": ['bigB', 'bigS', 'hugeB', 'hugeS', 'mediumB', 'mediumS', 'smallB', 'smallS', 'mainB', 'mainS', 'nonmainB', 'nonmainS'],
    "risk_list": ["beta", "momentum", "size", "earning_yield", "value", "volatility", "liquidity", "leverage", "growth"],
    "flag_list": ["univ_flag", "price_flag", "risk_flag", "ipo_st_flag", "industry_flag", "capital_flag"]
}

if __name__ == '__main__':
    # 一段短的时间
    dataset = BaseDataSet(setting)  
    dataset.prepare_y(lag=-2)

    alphaset = BaseAlphaSet(setting)
    alphaset.prepare_alpha(dataset, period=30)
    # del dataset.transaction
    
    fitter = LinearRegressionFitter(fitter_name='orth')
    fitter.pre_processing(alphaset, dataset, orth=True)  

    fit_result = LinearRegressionResult(fitter, y_name='high', alpha_name=['mainS1'])
    self = fit_result

    fit_result = fitter.fit(alphaset,
                            dataset,
                            fit_result=fit_result,
                            multi_factor=True,
                            add_ind_risk=True,
                            BetaOnly=True,
                            single_factor_test_list=['sort', 'turnover', 'alpha_autocorr'],  # 'sort'运行很慢，占用内存很大，不要轻易尝试
                            isPlot=False)
    fit_result.predict_beta(window=6)

    fit_result.predict_return(dataset,alphaset)
    fit_result.getWeight(dataset, Delay=6)
    fit_result.calc_netValue(dataset, Delay=None)
    fit_result.plot_netValue(load_date=dataset.date_range, Delay=None)

#!/usr/bin/env python
# -*- encoding: utf-8 -*-
'''
@File    :   Untitled-1
@Time    :   2022/05/09 09:14:37
@Author  :   Liu Junli
@Version :   1.0
@Contact :   liujunli.xmu@foxmail.com
'''

import warnings
import numpy as np
import xarray as xr
import BaseStructure as bs

warnings.filterwarnings("ignore")


def concat_pr(pr1, pr2):
    pred_return = xr.where(pr1 >= 0, pr1, np.nan)
    pred_return = xr.where(pr2 <= 0, pr2, pred_return)
    predicted_return = (xr.where((pr1 > 0) & (pr2 < 0), pr1 + pr2,
                                 pred_return).squeeze().expand_dims('variable').transpose('ticker', 'date', 'time', 'variable'))
    predicted_return['variable'] = ['PredictedReturn']
    return predicted_return


class mydataset(bs.BaseDataSet):
    def __init__(self, setting: dict):
        super().__init__(setting)

    # def calc_y(self, lag: int):
    #     # 重新定义y的计算
    #     y = self.market
    #     return y


class myalphaset(bs.BaseAlphaSet):
    def __init__(self, setting: dict):
        super().__init__(setting)

    # def calc_alpha(
    #     self,
    #     dataset,
    # ):
    #     pass


start_date: int = 20211103
end_date: int = 20220101
cycleDate = 20 #每个循环跑多少天
window = 30 #beta的滚动期
bs.setting['dst_dir'] = "/usr/intern/test_liujl"
# bs.setting['varlist'] = None

date_range = mydataset.get_tradingday_range(start_date=start_date,end_date=end_date)
cycleNum = np.ceil(len(date_range) / cycleDate).astype(int)

fitter = bs.LinearRegressionFitter(fitter_name='orth')
fit_result_high = bs.LinearRegressionResult(fitter, y_name='high', alpha_name=['mainS1'])
fit_result_low = bs.LinearRegressionResult(fitter, y_name='low', alpha_name=['nonmainS1'])


for i in range(cycleNum):
    bs.setting['start_date'] = date_range[cycleDate * i]
    bs.setting['end_date'] = date_range[min(cycleDate * i + cycleDate - 1, date_range.index(date_range[-1]))]

    dataset = mydataset(bs.setting)  #需要用到的基本的数据
    dataset.prepare_y(lag=-2)  #lag应该为负数, '提前'而不是'滞后'

    alphaset = myalphaset(bs.setting)
    alphaset.prepare_alpha(dataset)  #会默认取以下路径读取

    fitter.pre_processing(alphaset, dataset, orth=True)  #alpha预处理, 处理后的alpha附在alphaset的residual里,同时保存在BaseAlphaSet文件夹下
    # 这条命令同时会把已经处理过的alpha读进来,避免重复计算.

    fit_result_high = fitter.fit(alphaset,
                                dataset,
                                fit_result=fit_result_high,
                                multi_factor=False,
                                add_ind_risk=True,
                                BetaOnly=True,
                                single_factor_test_list=['sort', 'turnover', 'alpha_autocorr'],  # 'sort'运行很慢，占用内存很大，不要轻易尝试
                                isPlot=False)

    fit_result_low = fitter.fit(alphaset,
                                dataset,
                                fit_result=fit_result_low,
                                multi_factor=False,
                                add_ind_risk=True,
                                BetaOnly=True,
                                single_factor_test_list=['sort', 'turnover', 'alpha_autocorr'],
                                isPlot=False)

fit_result_high.predict_beta(window=window)
fit_result_low.predict_beta(window=window)

myfit = bs.LinearRegressionResult(fitter, y_name='high_low')
pre_weight = None
date_range_ = date_range[window:]
cycleNum_ = np.ceil(len(date_range_) / cycleDate).astype(int)

for i in range(cycleNum_):
    i=0
    bs.setting['start_date'] = date_range_[cycleDate * i]
    bs.setting['end_date'] = date_range_[min(cycleDate * i + cycleDate - 1, date_range_.index(date_range_[-1]))]

    dataset = mydataset(bs.setting)
    dataset.prepare_y(lag=-2)  #lag应该为负数, '提前'而不是'滞后'

    alphaset = myalphaset(bs.setting)
    alphaset.prepare_alpha(dataset)
    fitter.pre_processing(alphaset, dataset, orth=True)

    fit_result_high.predict_return(dataset,alphaset)
    fit_result_low.predict_return(dataset,alphaset)

    myfit.predicted_return = concat_pr(fit_result_high.predicted_return, fit_result_low.predicted_return)
    _,_,pre_weight = myfit.getWeight(dataset, Delay=0, pre_weight=pre_weight, isReturn=True)
    myfit.calc_netValue(dataset, Delay=0, isLoadTO=False)

myfit.plot_netValue(load_date=None)

import uqer
import yaml


f = open('.\\uqer_factor\\factor_config.yml', 'r')
config = yaml.load(f, Loader=yaml.FullLoader)
uqer_account = config['uqer_account']
uqer.Client(token=uqer_account['token'])


# def read_uqer_data(codes, from_date, to_date, var=None):
#     if var is None:
#         var = list(uqer_factor.keys())
#     call_dict = {}
#     for v in var:
#         call_func = uqer_factor.get(v, None)['func']
#         if call_func is None:
#             raise ValueError(f'{v} is not in factor_config.yml, please update the config file.')
#         call_dict.setdefault(call_func, []).append(v)
#     for func, var in call_dict.items():
#         dt = eval(f"{func}({codes}, '{from_date}', '{to_date}', {var})")
#         return dt


def read_uqer_factor_oneday(codes, from_date, to_date, var):
    dt = uqer.DataAPI.MktStockFactorsDateRangeProGet(secID=codes, ticker=u"", beginDate=from_date, endDate=to_date,
                                                     field=["ticker", "tradeDate", ",".join(var)], pandas="1")
    return dt


def get_trade_calendar(from_date, to_date):
    calendar = uqer.DataAPI.TradeCalGet(exchangeCD=u"XSHG", beginDate=from_date, endDate=to_date,
                                        isOpen=u"1", field=u"calendarDate", pandas="1")['calendarDate'].to_list()
    return calendar


def get_stock_a(date):
    stock_a = uqer.DataAPI.MktEqudGet(secID=u"", ticker=u"", tradeDate=date, beginDate=u"", endDate=u"", isOpen="",
                                      field=u"secID", pandas="1")['secID'].to_list()
    return stock_a


def get_uqer_goldstockinfo(from_date, to_date):
  dt = uqer.DataAPI.ResRrGoldStockInfoGet(beginDate=from_date,endDate=to_date,infoID=u"",orgName=u"",freq=u"",field="",pandas="1")
  return dt


def get_uqer_goldstockfore(from_date, to_date):
  dt = uqer.DataAPI.ResRrGoldForeGet(beginDate=from_date,endDate=to_date,infoID=u"",tickerSymbol=u"",secShortName=u"",foreYear=u"",field="",pandas="1")
  return dt


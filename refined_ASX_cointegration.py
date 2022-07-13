from statsmodels.tsa.stattools import coint
import yfinance as yf
from matplotlib import pyplot as plt
import pandas as pd
from time import sleep
from bs4 import BeautifulSoup
from selenium import webdriver

def get_ASX300():
    options = webdriver.ChromeOptions()
    options.add_argument('--ignore-certificate-errors')
    options.add_argument('--incognito')
    #options.add_argument('--headless')
    options.add_experimental_option("excludeSwitches", ["enable-logging"])
    driver = webdriver.Chrome('I:\\CodingProjects\\AlgoTrading\\windowsdriver\\chromedriver', options=options) #windows chrome driver location
    url = 'https://www.asx300list.com/'
    driver.get(url)
    sleep(5)
    html = driver.execute_script('return document.body.innerHTML;')
    soup = BeautifulSoup(html, 'lxml')
    driver.close()
    stocktable = soup.find('table', {'class':'tableizer-table sortable'})

    stocks = list(stocktable.find_all('td'))
    ASX300 = []
    for i, stock in enumerate(stocks):
        if i % 2 == 0: # every even index is the ticker, and every odd index is the company name.
            ASX300.append(stock.text)
    return ASX300

def data_to_CSV():
    ASXdata = pd.DataFrame()
    for stock in get_ASX300():
        stock = stock+'.AX'
        if stock not in ASXdata:
            try:
                data = yf.download(stock, period='1y')['Adj Close']
                if len(data) > 0:
                    ASXdata[stock] = data
            except:
                pass

    ASXdata.to_csv('I:\\CodingProjects\\AlgoTrading\\newASXdata.csv')


def get_coint_list():
    data = pd.read_csv('I:\\CodingProjects\\AlgoTrading\\newASXdata.csv')
    stocks = list(data.columns[1:])
    pval_list = []
    one_percent = []
    five_percent = []
    for i,stock1 in enumerate(stocks):
        #print(len(stocks)-i-1,'to go')
        if i != len(stocks)-1:
            for stock2 in stocks[i+1:]: 
                #print(stock1,stock2)
                if stock1 != stock2:
                    try:
                        T_stat, P_val, crit_vals = coint(data[stock1],data[stock2])
                        #print(P_val)
                        if P_val < 0.05 and P_val > 0:
                            #print(T_stat, crit_vals)
                            info = [stock1, stock2, P_val]
                            pval_list.append(info)
                            if T_stat > crit_vals[2]:
                                print(stock1,stock2)
                                one_percent.append(info)
                            elif T_stat > crit_vals[1]:
                                print(stock1,stock2)
                                five_percent.append(info)
                    except:
                        pass
                else:
                    pass
    print(len(pval_list), len(five_percent), len(one_percent))

    return five_percent

def get_stockpairs(coint_list):
    stockpairs = []
    for stockpair in coint_list:
        stockpairs.append(stockpair[0:2])
    return stockpairs

def save_pairs_to_txt():
    stockpairs = get_stockpairs(get_coint_list())

    file = open('I:\\CodingProjects\\AlgoTrading\\one_year_refined_ASX_significant_pairs.txt', 'w')
    for stockpair in stockpairs:
        string = ' '.join(stockpair)
        file.write(string + '\n')
    file.close()

save_pairs_to_txt()
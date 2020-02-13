import datetime
import requests
import urllib.request
from bs4 import BeautifulSoup
import ssl

base_url = "https://sisweb.tesouro.gov.br/apex/"
query = "f?p=2031:2"
url = base_url + query
response = requests.get(url, verify=False)

soup = BeautifulSoup(response.text, "html.parser")
div = soup.find("div", {"class": "bl-body"})
#ass = div.find_all('a')[0]

#for a in ass:
#  path = a['href']
#  name = a.string
#  download_url = base_url + path
#  ssl._create_default_https_context = ssl._create_unverified_context
#  urllib.request.urlretrieve(download_url, "C:/Users/ajzan/Documents/GitHub/Tesouro-Direto/DataBase/Historic/{}.xls".format(name))

#Fazer um a um para adicionar na pasta correta
#data de ontem
x = (datetime.datetime.today()-datetime.timedelta(days = 1)).year


a = div.find_all('a')[0]
path = a['href']
name = a.string
download_url = base_url + path
ssl._create_default_https_context = ssl._create_unverified_context
<<<<<<< HEAD
urllib.request.urlretrieve(download_url, "C:/Users/ajzan/Documents/GitHub/Tesouro-Direto/DataBase/Historic/LFT/{}_%s.xls".format(name) %(x))

a = div.find_all('a')[1]
path = a['href']
name = a.string
download_url = base_url + path
ssl._create_default_https_context = ssl._create_unverified_context
urllib.request.urlretrieve(download_url, "C:/Users/ajzan/Documents/GitHub/Tesouro-Direto/DataBase/Historic/LTN/{}_%s.xls".format(name) %(x))

a = div.find_all('a')[2]
path = a['href']
name = a.string
download_url = base_url + path
ssl._create_default_https_context = ssl._create_unverified_context
urllib.request.urlretrieve(download_url, "C:/Users/ajzan/Documents/GitHub/Tesouro-Direto/DataBase/Historic/NTN-C/{}_%s.xls".format(name) %(x))

a = div.find_all('a')[3]
path = a['href']
name = a.string
download_url = base_url + path
ssl._create_default_https_context = ssl._create_unverified_context
urllib.request.urlretrieve(download_url, "C:/Users/ajzan/Documents/GitHub/Tesouro-Direto/DataBase/Historic/NTN-B/{}_%s.xls".format(name) %(x))

a = div.find_all('a')[4]
path = a['href']
name = a.string
download_url = base_url + path
ssl._create_default_https_context = ssl._create_unverified_context
urllib.request.urlretrieve(download_url, "C:/Users/ajzan/Documents/GitHub/Tesouro-Direto/DataBase/Historic/NTN-BPrincipal/{}_%s.xls".format(name) %(x))

a = div.find_all('a')[5]
path = a['href']
name = a.string
download_url = base_url + path
ssl._create_default_https_context = ssl._create_unverified_context
urllib.request.urlretrieve(download_url, "C:/Users/ajzan/Documents/GitHub/Tesouro-Direto/DataBase/Historic/NTN-F/{}_%s.xls".format(name) %(x))





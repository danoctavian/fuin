import httplib
import requests
import sys
import httplib
import scipy

pyserver = "www.python.org" 
address = "127.0.0.1"
pyaddress = "http://www.python.org/index.html"
port = 3128 

conn = httplib.HTTPConnection(address, port)
#conn = httplib.HTTPConnection(pyserver)

conn.request("GET", pyaddress)
r1 = conn.getresponse()
print r1.status, r1.read()

sys.exit()
web = httplib.HTTPConnection(pyaddress)
#headers = {"Content-type": "application/x-www-form-urlencoded", "Accept": "text/plain"}
#web.request("HEAD","/index.html")
#response = web.getresponse()
#print response.read()

http_proxy  = address + ":3128"
https_proxy = "10.10.1.11:1080"
ftp_proxy   = "10.10.1.10:3128"

proxyDict = { 
              "http"  : http_proxy,
              "https"  : http_proxy,
              "ftp"  : http_proxy
            }

r = requests.get("http://bing.com", proxies=proxyDict)
print r.content



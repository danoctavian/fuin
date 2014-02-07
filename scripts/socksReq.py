import socks

sockAddr = "127.0.0.1"
s = socks.socksocket()
s.setproxy(socks.PROXY_TYPE_SOCKS5,sockAddr)
s.connect(("www.example.com",80))

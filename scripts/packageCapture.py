from scapy.all import *

a = sniff(filter="tcp",count=1000)
for packet in a:
  packet.show()
#    if HTTPResponse in packet:

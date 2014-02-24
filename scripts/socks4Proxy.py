from twisted.internet import reactor
from twisted.protocols import socks
import datetime
import time
import sys

import socksMod

from twisted.internet.protocol import Factory

factory = Factory()

#class Foo():
#  def wtf(self):
#    print "this is wtf"
#
#class Bar(Foo):
#  def wtf(self):
#    print "this is Bar"
#    Foo.wtf(self)
#
#b = Bar()
#b.wtf()
#    

#sys.exit()

#class Socks4Mod(socksMod.SOCKSv4):

#  def dataReceived(self, data):
#    print datetime.datetime.fromtimestamp(time.time()).strftime('%Y-%m-%d %H:%M:%S')
#    print data
#    return socksMod.SOCKSv4.dataReceived(self, data)

factory.protocol = socksMod.SOCKSv4#socks.SOCKSv4

reactor.listenTCP(9999, factory)
reactor.run()

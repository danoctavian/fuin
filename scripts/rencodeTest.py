import rencode
import struct
import hmac_sha256 as hs
'''
enc = rencode.dumps({'a':0, 'b':[1,2], 'c':99})


#print len(struct.pack('!l', 22))
#print rencode.dumps(bigInt)

testStr = "wtfisthisshit"
packedStr = rencode.dumps(testStr)

#print len(packedStr)
#print ord(packedStr[0])

listR = [bigInt, "mue", 32.0]

encList = rencode.dumps(listR)
print len(encList)

#print enc
#print len(enc)
'''

int8 = 127
int16 = 32767
int32 = 2147483647
int64 = 9223372036854775807
hugeInt = 922337203685477580700
float32 = 45.5

smallString = "wtf"
bigString = hs.compl("bigString", 100)

bigDict = {}

for i in range(0, 100):
  bigDict[str(i)] = i
bigList = [13] * 100

nestedList = [bigList, 1, 2] + bigList
testCases = [12, -11, int8, int16, int32, int64, hugeInt,
            float32, smallString, bigString, [1, 2, 3, True, False], bigList,
            {'x': 1, 'y': 2}, bigDict, nestedList]

i = 0
for t in testCases:
  t = rencode.dumps(t)
  print t
  print open('rencodeSamples/' + (str(i)),'w').write(t)
  i += 1





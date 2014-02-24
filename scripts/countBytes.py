
i = 0
filename = "packCap"

bs = [False] * 256
with open(filename) as f:
  while True:
    c = f.read(1)
    if not c or i > 10:
      print "End of file"
      break
#    i += 1
#    print "Read a character: ", ord(c)
    bs[ord(c)] = True

print bs
print reduce(lambda x, y: x and y, bs)
  

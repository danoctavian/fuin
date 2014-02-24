var http = require('http')
var os=require('os')

ips = {}
var ifaces=os.networkInterfaces()
for (var dev in ifaces) {
  var alias=0
  ifaces[dev].forEach(function(details){
    if (details.family=='IPv4') {
      console.log(dev+(alias?':'+alias:''),details.address)
      ++alias
      ips[dev] = details.address
    }
  });
}

console.log(JSON.stringify(ips))

//process.exit()
var localhost = '127.0.0.1'
var ip = ips.ppp0
var port = 4000
http.createServer(function (req, res) {
  console.log("got request")
  res.writeHead(200, {'Content-Type': 'text/plain'})
  res.end('Hello World\n')
}).listen(port, localhost);
console.log('Server running at http://' + ip + ':' + port);

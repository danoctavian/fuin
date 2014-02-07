var http = require('http');
var localhost = '127.0.0.1'
var port = 4000
http.createServer(function (req, res) {
  res.writeHead(200, {'Content-Type': 'text/plain'});
  res.end('Hello World\n');
}).listen(port, localhost);
console.log('Server running at http://' + localhost + ':' + port);

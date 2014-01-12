var http = require('http');
http.createServer(function (req, res) {
  res.writeHead(200, {'Content-Type': 'text/plain'});
  res.end('Hello World\n');
}).listen(6881, '192.168.0.109');
console.log('Server running at http://192.168.0.109:50003/');

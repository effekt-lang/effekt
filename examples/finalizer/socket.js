const net = require("net");
const http = require("http");

const createHttpServer = (port) => {
    const server = http.createServer((req, res) => {
        res.writeHead(200, { "Content-Type": "text/html" });
        res.write("<b>hello!</b>");
        res.end();
    });
    server.listen(port);
    return server;
};

async function sleepImpl(ms) {
    await new Promise((resolve) => setTimeout(resolve, ms));
    return;
}

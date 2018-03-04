var compress = require('compression');
module.exports = {
    port: 9503,
    files: ["dist/bench/all.js"],
    server: {
        baseDir: "dist/bench/",
        middleware: [compress()]
    }
};

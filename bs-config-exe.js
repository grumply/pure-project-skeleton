var compress = require('compression');
module.exports = {
    port: 9501,
    files: ["dist/exe/all.js"],
    server: {
        baseDir: "dist/exe/",
        middleware: [compress()]
    }
};

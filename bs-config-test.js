var compress = require('compression');
module.exports = {
    port: 9502,
    files: ["dist/test/all.js"],
    server: {
        baseDir: "dist/test/",
        middleware: [compress()]
    }
};

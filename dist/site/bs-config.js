module.exports = {
    ui: { port: 3000 },
    port: 8080,
    files: ["*.js"],
    server: {
        baseDir: "./",
        middleware: {
		        1: require('connect-history-api-fallback')({index: '/index.html', verbose: true})
	      }
    },
    logPrefix: "",
    logFileChanges: true,
    notify: false
};

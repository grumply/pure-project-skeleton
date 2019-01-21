module.exports = {
    ui: { port: 8080 },
    port: 3000,
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

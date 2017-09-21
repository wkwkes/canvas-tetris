const path = require("path");

module.exports = {
  entry: "./lib/js/src/app.js",
  output: {
    filename: "bundle.js",
    path: path.join(__dirname, "lib/js")
  }
}
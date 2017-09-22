const path = require("path");

module.exports = {
  entry: "./lib/js/src/tetris.js",
  output: {
    filename: "bundle.js",
    path: path.join(__dirname, "lib/js")
  }
}
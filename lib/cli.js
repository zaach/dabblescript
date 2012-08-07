#!/usr/bin/env node

var path = require("path");
var dabble = require("./dabble").dabble;
var escodegen = require("escodegen");

var source = read(process.argv[2]);
var target = dabble.parse(source, true);

console.log(escodegen.generate(target));

function read (fpath) {
    return require("fs").readFileSync(path.normalize(fpath), "utf8");
}

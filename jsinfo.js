#! /usr/bin/env node

var FS = require("fs");
var U2 = require("uglify-js");
var SYS = require("util")

var ARGS = process.argv.splice(2);
var POS = parseInt(ARGS[0], 10) - 1;
var FILE = ARGS[1] || "/dev/stdin";

var code = FS.readFileSync(FILE, "utf8").replace(/^#!/, "//"), ast;
try {
    ast = U2.parse(code, { filename: FILE });
} catch(ex) {
    if (ex instanceof U2.JS_Parse_Error) {
        SYS.puts(JSON.stringify({
            "parse-error": {
                message : ex.message,
                line    : ex.line,
                col     : ex.col,
                pos     : ex.pos + 1
            }
        }));
        process.exit(0);
    }
}

var best_node = null;
var path = [];
var stat = [];
var the_func = null;
var OUT = {};
try {
    ast.walk(new U2.TreeWalker(function (node, descend){
        if (node instanceof U2.AST_Lambda)
            the_func = node;
        if (node.start.pos > POS)
            throw OUT;
        if (node.start.pos <= POS && node.end.endpos >= POS) {
            best_node = node;
            path.push(node);
            if (node instanceof U2.AST_Statement &&
                !(node instanceof U2.AST_Function)) {
                stat.push(node);
            }
        }
    }));
} catch(ex) {
    if (ex !== OUT) throw ex;
}

var result = null;
if (best_node) {
    if (best_node instanceof U2.AST_Constant) {
        var a = [];
        ast.walk(new U2.TreeWalker(function(node, descend){
            if (node.TYPE == best_node.TYPE &&
                node.getValue().toString() == best_node.getValue().toString()) {
                a.push(node);
            }
        }));
        result = {
            name       : best_node.name,
            origin     : make_pos(best_node),
            definition : [],
            references : a.map(make_pos)
        }
    }
    else if (best_node instanceof U2.AST_This) {
        var a = [];
        var top = (the_func || ast);
        top.walk(new U2.TreeWalker(function(node, descend){
            if (node instanceof U2.AST_This)
                a.push(node);
            if (node !== top && node instanceof U2.AST_Lambda)
                return true;
        }));
        result = {
            name       : best_node.name,
            origin     : make_pos(best_node),
            definition : [],
            references : a.map(make_pos)
        };
    }
    else if (best_node instanceof U2.AST_Symbol) {
        ast.figure_out_scope();
        result = {
            name       : best_node.name,
            origin     : make_pos(best_node),
            definition : best_node.definition().orig.map(make_pos),
            references : best_node.definition().references.map(make_pos)
        };
    }
    else {
        result = {
            origin: make_pos(best_node)
        };
    }
}

if (result) {
    result.path = path.map(make_pos);
    result.stat = stat.map(make_pos);
}

SYS.puts(JSON.stringify(result));



function make_pos(node) {
    return node ? {
        //file  : node.start.file,
        type  : node.TYPE,
        begin : node.start.pos + 1,
        end   : node.end.endpos + 1
    } : null;
}
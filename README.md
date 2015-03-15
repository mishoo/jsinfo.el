jsinfo.el — an Emacs minor mode for helping with JS
===================================================

**Hint:** try [Tern](http://ternjs.net/doc/manual.html#emacs).  If that works for you, it's probably much better than this little mode.  Tern does a lot of magic and when it works it's really impressive.

`jsinfo.el` is a lot more limited, being able to deal only with the current buffer rather than scanning an entire directory tree.  It doesn't help with object methods or properties; instead, it only deals with variables (scope).  Some of the best code I wrote is not so much about objects, but functions (heavily nested functions, sometimes) and I find it useful to be able to quickly locate where a variable is defined, what are the free variables in a function (hint: if there are no free vars, or if all the free vars are global, then you can lift that one closure to upper levels), or quickly rename a variable.

Since I've been using this for quite a while and I find it useful I decided to publish it and hope it'll help others too.

**Note:** since it's using a rigorous JS parser, for using any of the functionality described below the buffer must contain syntactically valid JS code.  I might try to change the parser to [Acorn's loose parser](http://marijnhaverbeke.nl/blog/parse-dammit.html) but personally I don't mind this restriction.

**Install:**

- you need NodeJS.  I have 0.10.29, should work with later versions too.
- npm install uglify-js
- clone this repository
- load jsinfo.el in your `.emacs`

`jsinfo-mode` will activate automatically when you load a JavaScript file (the hook is added on `js-mode-hook`; if you use something else, like `js2-mode`, then you should add the hook yourself).

**Features:**

- `M-?` (`jsinfo-highlight-symbol`) — highlights all occurrences of the symbol at point
- `M-.` (`jsinfo-goto-definition`) — go to definition of the variable at point
- `M-,` (`jsinfo-undo-goto-definition`) — after a `M-.`, this goes back to the previous location
- `C-c f` or `C-c C-f` (`jsinfo-highlight-free-vars`) — highlight the free variables in the current function
- `C-c v` or `C-c C-v` (`jsinfo-highlight-local-vars`) — highlight local variable/function definitions in the current scope
- `C-c r` (`jsinfo-highlight-return-points`) — highlight return statements in the current function
- `C-c <left>` or `C-c C-<left>` (`jsinfo-extend-region-node`) — select the current JS expression; press repeatedly to extend it to the parent node
- `C-c <up>` or `C-c C-<up>` (`jsinfo-extend-region-statement`) — select the current statement; press repeatedly to extend to the parent node
- `C-c <down>` or `C-c C-<down>` (`jsinfo-extend-region-undo`) — undo last region extension by one of the previous two functions

**Highlight mode:**

After highlighting something via one of the `jsinfo-highlight-*` functions the following additional bindings are available:

- `C-<up>` (`jsinfo-goto-prev-symbol`) — move to the previous occurrence
- `C-<down>` (`jsinfo-goto-next-symbol`) — move to the next occurrence
- `C-<return>` (`jsinfo-rename-symbol`) — rename the symbol at point (only makes sense when point is on a variable name)
- `ESC` or `C-g` (`jsinfo-forgetit`) — quit the highlight minor mode; removes any highlighting


Kkmarkdown
======

Small & safe markdown engine

* Only a subset of markdown syntax is supported.
* Legit HTML code generation & no XSS attack is the goal.

Supported syntax are listed here:  
<https://github.com/kkeundotnet/kkmarkdown/blob/master/syntax.md>

Build & run
---

```
$ make
$ make test
```

It will make

* one executable `_build/defualt/src/bin/kkmarkdown.exe` (samely
  `_build/install/default/bin/kkmarkdown`)
* one javascript `_build/default/src/js/kkmarkdown-js.js`

In shell (using stdin):

```
$ kkmarkdown
*abc* (then control+D)
<p><em>abc</em></p>
```

or (using file)

```
$ echo "*abc*" > a
$ kkmarkdown a
<p><em>abc</em></p>
```

In html:

```
<script src='https://kkeun.net/kkmarkdown-js.js'></script>
<script>result = kkmarkdown.trans("*abc*");</script>
```

### Unsafe mode

There is *unsafe mode* that can be used when the markdown source is
trustworthy.  See below for supported syntax in the unsafe mode:  
<https://github.com/kkeundotnet/kkmarkdown/blob/master/syntax.md>

In shell:

```
$ kkmarkdown --unsafe [FILE]
```

In html:

```
<script>result = kkmarkdown.unsafe("*abc*");</script>
```

Install
---

```
$ opam pin add kkmarkdown .
```

or

```
$ make install
$ make uninstall
```

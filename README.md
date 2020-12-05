Kkmarkdown
======

Small & safe markdown engine

* Only a subset of markdown syntax is supported.
* Legit HTML code generation & no XSS attack is the goal.

Supported syntax are listed here:  
<https://github.com/kkeundotnet/kkmarkdown/blob/master/syntax.md>

Build & run
---

Install dependencies:

```
$ opam install dune js_of_ocaml js_of_ocaml-ppx alcotest
```

Build:

```
$ make
$ make test
```

It will make

* one executable `_build/install/default/bin/kkmarkdown`
* one javascript `_build/install/default/share/kkmarkdown/kkmarkdown.js`

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

### RSS mode

*RSS mode* suppresses `class`es and inline HTMLs in the unsafe mode.
HTML `class`es or inline HTMLs including javscripts may not work
properly in external feed readers.

```
$ kkmarkdown --rss --unsafe [FILE]
```

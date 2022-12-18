kkmarkdown
======

Safe markdown engine

Goal:

* To avoid XSS attack, by supporting [limited markdown
  syntax](https://github.com/kkeundotnet/kkmarkdown/blob/master/syntax.md).

* To run the same markdown engine in both server and client sides, thanks to
  [js_of_ocaml](https://github.com/ocsigen/js_of_ocaml).

Not goal:

* Generating legit HTML is not a goal. Garbage in, garbage out at the moment.

![build by push](https://github.com/kkeundotnet/kkmarkdown/actions/workflows/build.yml/badge.svg?event=push)

Build & run
---

Install dependencies:

```
$ opam install --deps-only . [--with-test]
```

Build:

```
$ make [|test|doc|clean|fmt]
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
<script src='https://kkeun.net/kkmarkdown.js'></script>
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

Kkmarkdown syntax
=====

It supports a subset of original markdown syntax
(<https://daringfireball.net/projects/markdown/syntax>), in order to
avoid generating broken html code or XSS attacks.

## Unicode

`&#xhhhhh;` or `&#nnnnnn;` can be written in the text.

## HTML special character & escape

HTML special characters, e.g. `&`, `<`, etc., are translated to
`&amp;`, `&lt;`, etc.  The following characters can be escaped by
backslash in the text.

```
\ ` * _ { } [ ] ( ) # + - . !
```

## Line break

The lines that terminate with two or more spaces are translated to
`<br>`.

## Emphasis & strong

```
*word*
**word**
***word***
```

Nested forms of emphasis and strong are NOT supported since I don't
want to think about ambiguity from there.  For example,

```
***word*word**
```

will NOT be translated to as we can imagine, i.e.,
`<strong><em>word</em>word</strong>`.

## Strike

```
~~word~~
```

## Code

```
`code`
```

## Automatic link

Only the next form of link syntax, so called "automatic links", is supported.

```
<http://kkeun.net/>
```

is translated to

```
<a href="http://kkeun.net/">http://kkeun.net/</a>
```

For simplicity, `https://` is omitted, so

```
<https://kkeun.net/>
```

is translated to

```
<a href="https://kkeun.net/">kkeun.net/</a>
```

The link must start with `https://` and `http://`, not the others.

## Paragraph

```
paragraph1

paragraph2
```

## Header

```
# H1

## H2

...

###### H6

H1
===

H2
---
```

## Blockquote

```
> blockquote
```

## List

```
* abc
* def

1. abc
2. def
```

Also, each list elements can have multiple paragraphs by adding some line spaces.

```
1.  line1
    line2

    * line3
    * line4

2.  line5
```

is translated to

```
<ol><li><p>line1
line2</p>
<ul><li>line3</li>
<li>line4</li></ul></li>
<li><p>line5</p></li></ol>
```

However, the nested list of non-pragraph elements is NOT supported.

```
*   abc
    *   def
    *   ghi
```

will be translated to

```
<ul><li>abc
<em>   def
</em>   ghi</li></ul>
```

rather than

```
<ul><li>abc
<ul><li>def</li>
<li>ghi</li></ul></ul>
```

Because I am too lazy to support that kinds of ambiguous syntax and
supporting all of the ambiguous syntax will make the markdown engine
not only complicated, but also vulnerable.

## Code block

`````
```
abc
```

~~~
abc
~~~
`````

## Horizontal line

```
***
```

## Unsafe mode: more supports on trustworthy source

Basically, the following syntax are not supported.

* Inline HTML
* Image
* Link (general one `[...](...)`)

BUT, there is *unsafe mode* that can be used when the markdown
source is trustworthy.

### [unsafe mode only] Image with/without class

```
![text](link)
```

or

```
![text](link) {.c1 .c2}
```

is translated to

```
<img alt="text" src="link" class="c1 c2">
```

### [unsafe mode only] Code block with class

~~~~~
``` {.class1 .class2}
x=1
```

~~~ {.class1 .class2}
x=1
~~~
~~~~~

is translated to

```
<pre><code class="class1 class2">x=1</code></pre>
```

### [unsafe mode only] Inline HTML

```
<div>
...
</div>
```

* `<div>` (or `<div ...`) and `</div>` must be prefixes of the line.
* Supported tags: `div`, `script`

### [unsafe mode only] Link on text

```
[text](link)
```

* The link phrase must be in the same one line.
* `text` is addressed as markdown.

That's it.  Enjoy Kkmarkdown!

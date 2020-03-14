module F = Format
module Kkmarkdown = Kkmarkdown_lib.Kkmarkdown

let trans input = F.asprintf "%a" Kkmarkdown.pp (Kkmarkdown.trans input)

let check msg expecting input =
  Alcotest.(check string) msg expecting (trans input)

let test_br () =
  check "br" {|<p>hello<br>
hi</p>|} {|hello  
hi|} ;
  check "br" {|<p>hello<br>
hi</p>|} {|hello     
hi|}

let test_code () =
  check "code" {|<p>abc<code>&lt;javascript&gt;abc</code>def</p>|}
    {|abc`<javascript>abc`def|} ;
  check "code" {|<p>abc<code>&lt;javascript&gt;a
bc</code>def</p>|}
    {|abc`<javascript>a
bc`def|}

let test_code_block () =
  check "code block" {|<pre><code>int x;
int y;</code></pre>|}
    {|```
int x;
int y;
```|} ;
  check "code block" {|<pre><code>int x;
int y;</code></pre>|}
    {|~~~
int x;
int y;
~~~|} ;
  check "code block" {|<pre><code>int x;
int y;</code></pre>|}
    {|    int x;
    int y;|}

let test_empty () =
  check "empty" "" "" ; check "empty" "" "\n" ; check "empty" "" "\n\n"

let test_em () =
  check "em" {|<p><em>ab
c</em></p>|} {|*ab
c*|} ;
  check "em" {|<p><em>abc</em></p>|} {|*abc|}

let test_em_strong () =
  check "em_strong" {|<p><em><strong>abc</strong></em></p>|} {|***abc***|} ;
  check "em_strong" {|<p><em><strong>abc</strong></em></p>|} {|***abc|}

let test_escape () =
  check "escape" "<p>&amp;&lt;&gt;&quot;&apos;</p>" {|&<>"'|} ;
  check "escape" {|<p>\`*_{}[]()#+-.!</p>|} {|\\\`\*\_\{\}\[\]\(\)\#\+\-\.\!|}

let test_header () =
  check "header" "<h1>abc</h1>" "# abc" ;
  check "header" "<h3>abc</h3>" "### abc" ;
  check "header" "<h1>abc</h1>" "abc\n===" ;
  check "header" "<h2>abc</h2>" "abc\n---"

let test_hr () = check "hr" "<hr>" "***" ; check "hr" "<hr>" "******"

let test_p () =
  check "p" {|<p>abc</p>
<p>def</p>|} {|abc

def|} ;
  check "p" {|<p>ab
c</p>
<p>def</p>|} {|ab
c

def|}

let test_quote () =
  check "quote" {|<blockquote><p>abc</p>
<p>def</p></blockquote>|}
    {|> abc
>
> def|} ;
  check "quote"
    {|<blockquote><p>abc</p></blockquote>
<blockquote><p>def</p></blockquote>|}
    {|> abc

> def|} ;
  check "quote"
    {|<blockquote><p>abc</p>
<blockquote><p>hi</p></blockquote>
<p>def</p></blockquote>|}
    {|> abc
>
> > hi
>
> def|} ;
  check "quote"
    {|<blockquote><h1>header</h1>
<p>sample code:</p>
<pre><code>def</code></pre></blockquote>|}
    {|> # header
> 
> sample code:
>
>     def|}

let test_strong () =
  check "strong" {|<p><strong>abc</strong></p>|} {|**abc**|} ;
  check "strong" {|<p><strong>abc</strong></p>|} {|**abc|}

let test_unicode () = check "unicode" {|<p>&#x1F602;</p>|} {|&#x1F602;|}

let tests =
  [ ("br", `Quick, test_br)
  ; ("code", `Quick, test_code)
  ; ("code block", `Quick, test_code_block)
  ; ("em", `Quick, test_em)
  ; ("em_strong", `Quick, test_em_strong)
  ; ("empty", `Quick, test_empty)
  ; ("escape", `Quick, test_escape)
  ; ("header", `Quick, test_header)
  ; ("hr", `Quick, test_hr)
  ; ("p", `Quick, test_p)
  ; ("quote", `Quick, test_quote)
  ; ("strong", `Quick, test_strong)
  ; ("unicode", `Quick, test_unicode) ]

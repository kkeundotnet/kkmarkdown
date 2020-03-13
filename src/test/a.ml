module F = Format
module Kkmarkdown = Kkmarkdown_lib.Kkmarkdown

let trans input = F.asprintf "%a" Kkmarkdown.pp (Kkmarkdown.trans input)

let check msg expecting input =
  Alcotest.(check string) msg expecting (trans input)

let test_code_block () =
  check "code block" {|<pre><code>int x;
int y;
</code></pre>|}
    {|```
int x;
int y;
```|} ;
  check "code block" {|<pre><code>int x;
int y;
</code></pre>|}
    {|~~~
int x;
int y;
~~~|} ;
  check "code block" {|<pre><code>int x;
int y;
</code></pre>|}
    {|    int x;
    int y;|}

let test_empty () =
  check "empty" "" "" ; check "empty" "" "\n" ; check "empty" "" "\n\n"

let test_escape () =
  check "escape" "<p>&amp;&lt;&gt;&quot;&apos;</p>" {|&<>"'|} ;
  check "escape" {|<p>\`*_{}[]()#+-.!</p>|} {|\\\`\*\_\{\}\[\]\(\)\#\+\-\.\!|}

let test_header () =
  check "header" "<h1>abc</h1>" "# abc" ;
  check "header" "<h3>abc</h3>" "### abc" ;
  check "header" "<h1>abc</h1>" "abc\n===" ;
  check "header" "<h2>abc</h2>" "abc\n---"

let test_hr () = check "hr" "<hr>" "***" ; check "hr" "<hr>" "******"

let test_p () = check "p" {|<p>abc</p><p>def</p>|} {|abc

def|}

let tests =
  [ ("code block", `Quick, test_code_block)
  ; ("empty", `Quick, test_empty)
  ; ("escape", `Quick, test_escape)
  ; ("header", `Quick, test_header)
  ; ("hr", `Quick, test_hr)
  ; ("p", `Quick, test_p) ]

(** Run all test suites *)

module F = Format
module Kkmarkdown = Lib.Kkmarkdown
module Typ = Lib.Typ

let trans ?unsafe input =
  F.asprintf "%a" (Typ.pp ~rss:false) (Kkmarkdown.trans ?unsafe input)

let trans_rss input =
  F.asprintf "%a" (Typ.pp ~rss:true) (Kkmarkdown.trans ~unsafe:true input)

let check msg expecting input =
  Alcotest.(check string) msg expecting (trans input)

let unsafe_check msg expecting input =
  Alcotest.(check string) msg expecting (trans ~unsafe:true input)

let rss_check msg expecting input =
  Alcotest.(check string) msg expecting (trans_rss input)

let test_a () =
  check "a" {|<p><a href="https://kkeun.net">kkeun.net</a></p>|}
    {|<https://kkeun.net>|};
  check "a" {|<p>woo <a href="https://kkeun.net">kkeun.net</a> woo</p>|}
    {|woo <https://kkeun.net> woo|};
  check "a" {|<p><a href="http://kkeun.net">http://kkeun.net</a></p>|}
    {|<http://kkeun.net>|};
  check "a" {|<p>woo <a href="http://kkeun.net">http://kkeun.net</a> woo</p>|}
    {|woo <http://kkeun.net> woo|};
  check "a" {|<p>&lt;javascript:void(0)&gt;</p>|} {|<javascript:void(0)>|};
  check "a" {|<p>&lt;&quot; onclick=&quot;myattack&gt;</p>|}
    {|<" onclick="myattack>|};
  check "a"
    {|<p><a href="https://kkeun.net">kkeun.net</a><a href="https://kkeun.net">kkeun.net</a></p>|}
    {|<https://kkeun.net><https://kkeun.net>|}

let test_br () =
  check "br" {|<p>hello<br>hi</p>|} {|hello  
hi|};
  check "br" {|<p>hello<br>hi</p>|} {|hello     
hi|}

let test_code () =
  check "code" {|<p>abc<code>&lt;javascript&gt;abc</code>def</p>|}
    {|abc`<javascript>abc`def|};
  check "code" {|<p>abc<code>&lt;javascript&gt;a bc</code>def</p>|}
    {|abc`<javascript>a
bc`def|}

let test_code_block () =
  check "code block" {|<pre><code>int x;
int y;</code></pre>|}
    {|```
int x;
int y;
```|};
  check "code block" {|<pre><code>int x;
int y;</code></pre>|}
    {|~~~
int x;
int y;
~~~|};
  check "code block" {|<pre><code>int x;
int y;</code></pre>|}
    {|    int x;
    int y;|};
  check "code block" {|<pre><code>int x;
int y;</code></pre>|}
    {|    int x;
    int y;
|}

let test_deep_block () =
  check "quote"
    {|<blockquote><blockquote><blockquote><blockquote><blockquote><p>&gt; no quote here</p></blockquote></blockquote></blockquote></blockquote></blockquote>|}
    {|> > > > > > no quote here|};
  check "ol"
    {|<ol><li><p>a</p></li>
<li><ol><li><p>b</p></li>
<li><ol><li><p>c</p></li>
<li><ol><li><p>d</p></li>
<li><ol><li><p>ok</p></li>
<li><p>1.  no ol here</p>
<p>2.  no ol here</p></li></ol></li></ol></li></ol></li></ol></li></ol>|}
    {|1.  a

2.  1.  b

    2.  1.  c

        2.  1.  d

            2.  1.  ok

                2.  1.  no ol here

                    2.  no ol here|};
  check "ul"
    {|<ul><li><p>a</p></li>
<li><ul><li><p>b</p></li>
<li><ul><li><p>c</p></li>
<li><ul><li><p>d</p></li>
<li><ul><li><p>ok</p></li>
<li><p><em>   no ul here</em></p></li>
<li><p><em>   no ul here</em></p></li></ul></li></ul></li></ul></li></ul></li></ul>|}
    {|*   a

*   *   b

    *   *   c

        *   *   d

            *   *   ok

                *   *   no ul here

                *   *   no ul here|};
  check "mixed"
    {|<ul><li><p>a</p></li>
<li><blockquote><p>b</p>
<ol><li><p>c</p></li>
<li><ul><li><p>d</p></li>
<li><blockquote><p>ok</p>
<p>1.  no ol here</p>
<p>1.  no ol here</p>
<p><em>   no ul here</em></p>
<p><em>   no ul here</em></p>
<p>&gt; no quote here</p></blockquote></li></ul></li></ol></blockquote></li></ul>|}
    {|*   a

*   > b
    >
    > 1.  c
    >
    > 2.  *  d
    >
    >     *  > ok
    >        >
    >        > 1.  no ol here
    >        >
    >        > 1.  no ol here
    >        >
    >        > *   no ul here
    >        >
    >        > *   no ul here
    >        >
    >        > > no quote here|}

let test_empty () =
  check "empty" "" "";
  check "empty" "" "\n";
  check "empty" "" "\n\n"

let test_em () =
  check "em" {|<p><em>ab c</em></p>|} {|*ab
c*|};
  check "em" {|<p><em>abc</em></p>|} {|*abc|}

let test_em_strong () =
  check "em_strong" {|<p><em><strong>abc</strong></em></p>|} {|***abc***|};
  check "em_strong" {|<p><em><strong>abc</strong></em></p>|} {|***abc|}

let test_escape () =
  check "escape" "<p>&amp;&lt;&gt;&quot;&apos;</p>" {|&<>"'|};
  check "escape" {|<p>\`*_{}[]()#+-.!</p>|} {|\\\`\*\_\{\}\[\]\(\)\#\+\-\.\!|}

let test_header () =
  check "header" "<h1>abc</h1>" "# abc";
  check "header" "<h3>abc</h3>" "### abc";
  check "header" "<h1>abc</h1>" "abc\n===";
  check "header" "<h2>abc</h2>" "abc\n---"

let test_hr () =
  check "hr" "<hr>" "***";
  check "hr" "<hr>" "******"

let test_ol () =
  check "ol" {|<ol><li>hi</li>
<li>bye</li></ol>|} {|1. hi
2. bye|};
  check "ol" {|<ol><li><p>hi</p></li>
<li><p>bye</p></li></ol>|}
    {|1. hi

2. bye|};
  check "ol"
    {|<ol><li><p>hi</p>
<ul><li>good?</li></ul></li>
<li><p>bye</p></li></ol>
<ul><li>hey</li></ul>|}
    {|1. hi

    * good?

2. bye

* hey|};
  check "ol" {|<ol><li><p>hi</p></li>
<li><p>bye</p></li></ol>|}
    {|1.  hi

2.  bye|}

let test_p () =
  check "p" {|<p>abc</p>
<p>def</p>|} {|abc

def|};
  check "p" {|<p>ab c</p>
<p>def</p>|} {|ab
c

def|}

let test_quote () =
  check "quote" {|<blockquote><p>abc</p>
<p>def</p></blockquote>|}
    {|> abc
>
> def|};
  check "quote"
    {|<blockquote><p>abc</p></blockquote>
<blockquote><p>def</p></blockquote>|}
    {|> abc

> def|};
  check "quote"
    {|<blockquote><p>abc</p>
<blockquote><p>hi</p></blockquote>
<p>def</p></blockquote>|}
    {|> abc
>
> > hi
>
> def|};
  check "quote"
    {|<blockquote><h1>header</h1>
<p>sample code:</p>
<pre><code>def</code></pre></blockquote>|}
    {|> # header
>
> sample code:
>
>     def|}

let test_strike () = check "strike" {|<p>a<s>b</s>c</p>|} {|a~~b~~c|}

let test_strong () =
  check "strong" {|<p><strong>abc</strong></p>|} {|**abc**|};
  check "strong" {|<p><strong>abc</strong></p>|} {|**abc|}

let test_unicode () = check "unicode" {|<p>&#x1F602;</p>|} {|&#x1F602;|}

let test_ul () =
  check "ul" {|<ul><li>hi</li>
<li>bye</li></ul>|} {|* hi
* bye|};
  check "ul" {|<ul><li><p>hi</p></li>
<li><p>bye</p></li></ul>|} {|* hi

* bye|};
  check "ul"
    {|<ul><li><p>hi</p>
<ul><li>good?</li></ul></li>
<li><p>bye</p></li></ul>|}
    {|* hi

    * good?

* bye|}

let test_no_unsafe_img () =
  check "no unsafe img" {|<p>![  alt  ](  link  ) {  .c1  .c2  }</p>|}
    {|![  alt  ](  link  ) {  .c1  .c2  }|}

let test_unsafe_img () =
  unsafe_check "unsafe img" {|<p><img alt="alt" src="link" class="c1 c2"></p>|}
    {|![  alt  ](  link  ) {  .c1  .c2  }|};
  unsafe_check "unsafe img" {|<p><img alt="alt" src="link" class=""></p>|}
    {|![  alt  ](  link  ) {   }|}

let test_rss_img () =
  rss_check "rss img" {|<p><img alt="alt" src="link"></p>|}
    {|![  alt  ](  link  ) {  .c1  .c2  }|};
  rss_check "rss img" {|<p><img alt="alt" src="link"></p>|}
    {|![  alt  ](  link  ) {   }|}

let test_no_unsafe_code_block () =
  check "no unsafe code block"
    {|<p><code></code><code> {.c1 .c2} code </code><code></code></p>|}
    {|``` {.c1 .c2}
code
```|};
  check "no unsafe code block"
    {|<p><code></code><code> {  .c1 .c2  } code </code><code></code></p>|}
    {|``` {  .c1 .c2  }
code
```|}

let test_unsafe_code_block () =
  unsafe_check "unsafe code block"
    {|<pre><code class="c1 c2">code</code></pre>|} {|``` {.c1 .c2}
code
```|};
  unsafe_check "unsafe code block"
    {|<pre><code class="c1 c2">code</code></pre>|}
    {|``` {  .c1 .c2  }
code
```|};
  unsafe_check "unsafe code block" {|<pre><code class="c">code</code></pre>|}
    {|~~~ {.c}
code
~~~|}

let test_rss_code_block () =
  rss_check "rss code block" {|<pre><code>code</code></pre>|}
    {|``` {.c1 .c2}
code
```|};
  rss_check "rss code block" {|<pre><code>code</code></pre>|}
    {|``` {  .c1 .c2  }
code
```|}

let test_no_unsafe_div () =
  check "no unsafe div" {|<p>&lt;div&gt; contents &lt;/div&gt;</p>|}
    {|<div>
contents
</div>|};
  check "no unsafe div"
    {|<p>&lt;div class=&quot;a&quot;&gt; contents &lt;/div&gt;</p>|}
    {|<div class="a">
contents
</div>|}

let test_unsafe_div () =
  unsafe_check "unsafe div" {|<div>
contents
</div>|} {|<div>
contents
</div>|};
  unsafe_check "unsafe div" {|<div class="a">
contents
</div>|}
    {|<div class="a">
contents
</div>|};
  unsafe_check "unsafe div" {|<div class="a">
contents
</div>
<p>Hello.</p>|}
    {|<div class="a">
contents
</div>

Hello.|}

let test_rss_div () =
  rss_check "rss div" {||} {|<div>
contents
</div>|};
  rss_check "rss div" {||} {|<div class="a">
contents
</div>|}

let test_no_unsafe_a () =
  check "no unsafe a" {|<p>[a](b)</p>|} {|[a](b)|};
  check "no unsafe a" {|<p>Hello [a](b).</p>|} {|Hello [a](b).|}

let test_unsafe_a () =
  unsafe_check "unsafe a" {|<p><a href="b">a</a></p>|} {|[a](b)|};
  unsafe_check "unsafe a" {|<p>Hello <a href="b">a</a>.</p>|} {|Hello [a](b).|};
  unsafe_check "unsafe a" {|<p><a href="b">a</a> hello <a href="d">c</a>.</p>|}
    {|[a](b) hello [c](d).|};
  unsafe_check "unsafe a" {|<p><a href="b">a</a> hello (d).</p>|}
    {|[a](b) hello (d).|}

let test_no_unsafe_script () =
  check "no unsafe script" {|<p>&lt;script&gt; contents &lt;/script&gt;</p>|}
    {|<script>
contents
</script>|};
  check "no unsafe script"
    {|<p>&lt;script class=&quot;a&quot;&gt; contents &lt;/script&gt;</p>|}
    {|<script class="a">
contents
</script>|}

let test_unsafe_script () =
  unsafe_check "unsafe script" {|<script>
contents
</script>|}
    {|<script>
contents
</script>|};
  unsafe_check "unsafe script" {|<script class="a">
contents
</script>|}
    {|<script class="a">
contents
</script>|}

let test_rss_script () =
  rss_check "rss script" {||} {|<script>
contents
</script>|};
  rss_check "rss script" {||} {|<script class="a">
contents
</script>|}

let tests =
  [
    ("a", `Quick, test_a);
    ("br", `Quick, test_br);
    ("code", `Quick, test_code);
    ("code block", `Quick, test_code_block);
    ("deep block", `Quick, test_deep_block);
    ("em", `Quick, test_em);
    ("em_strong", `Quick, test_em_strong);
    ("empty", `Quick, test_empty);
    ("escape", `Quick, test_escape);
    ("header", `Quick, test_header);
    ("hr", `Quick, test_hr);
    ("no_unsafe_a", `Quick, test_no_unsafe_a);
    ("no_unsafe_code_block", `Quick, test_no_unsafe_code_block);
    ("no_unsafe_div", `Quick, test_no_unsafe_div);
    ("no_unsafe_img", `Quick, test_no_unsafe_img);
    ("no_unsafe_script", `Quick, test_no_unsafe_script);
    ("ol", `Quick, test_ol);
    ("p", `Quick, test_p);
    ("quote", `Quick, test_quote);
    ("rss_code_block", `Quick, test_rss_code_block);
    ("rss_div", `Quick, test_rss_div);
    ("rss_img", `Quick, test_rss_img);
    ("rss_script", `Quick, test_rss_script);
    ("strike", `Quick, test_strike);
    ("strong", `Quick, test_strong);
    ("ul", `Quick, test_ul);
    ("unicode", `Quick, test_unicode);
    ("unsafe_a", `Quick, test_unsafe_a);
    ("unsafe_code_block", `Quick, test_unsafe_code_block);
    ("unsafe_div", `Quick, test_unsafe_div);
    ("unsafe_img", `Quick, test_unsafe_img);
    ("unsafe_script", `Quick, test_unsafe_script);
  ]

let () = Alcotest.run "kkmarkdown" [ ("end-to-end", tests) ]

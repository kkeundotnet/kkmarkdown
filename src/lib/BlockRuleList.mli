(** List *)

(* SYNTAX: START *)

module Ul : BlockRule.S
(** Unordered list

{[
* a
* b
* c
]}

A list element can have block-level elements by adding line spaces.

{[
* line1

  * line2
  * line3

* line4
]}

is translated to

{[
<ul>
  <li>
    <p>line1</p>
    <ul>
      <li>line2</li>
      <li>line3</li>
    </ul>
  </li>
  <li>
    <p>line4</p>
  </li>
</ul>
]}

However, without a line space, a list element CANNOT include another list.

{[
* line1
  * line2
  * line3
]}

is translated to

{[
<ul>
  <li>
    <p>line1 <em> line2 </em> line3</p>
  </li>
</ul>
]}

rather than something like

{[
<ul>
  <li>
    line1
    <ul>
      <li>line2</li>
      <li>line3</li>
    </ul>
  </li>
</ul>
]}

This is beacause, I am too {i lazy} to think about such ambiguous corner cases. /o\ *)

module Ol : BlockRule.S
(** Ordered list

{[
1. a
2. b
3. c
]} *)

(* SYNTAX: END *)

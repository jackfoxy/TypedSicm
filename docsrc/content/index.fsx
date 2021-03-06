(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/TypedSicm"

(**
TypedSicm
======================

Documentation

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The TypedSicm library can be <a href="https://nuget.org/packages/TypedSicm">installed from NuGet</a>:
      <pre>PM> Install-Package TypedSicm</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

[Structure and Interpretation of Classical Mechanics](https://mitpress.mit.edu/sites/default/files/titles/content/sicm_edition_2/toc.html)

second edition

Gerald Jay Sussman and Jack Wisdom

Use Fable Femto to manage npm packages
https://fable.io/blog/Introducing-Femto.html

Example
-------

This example demonstrates using a function defined in this sample library.

*)
#r "TypedSicm.dll"
open TypedSicm

printfn "hello = %i" <| Library.hello 0

(**
Some more info

Samples & documentation
-----------------------

The library comes with comprehensible documentation. 
It can include tutorials automatically generated from `*.fsx` files in [the content folder][content]. 
The API reference is automatically generated from Markdown comments in the library implementation.

 * [Tutorial](tutorial.html) contains a further explanation of this sample library.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/fsprojects/TypedSicm/tree/master/docs/content
  [gh]: https://github.com/fsprojects/TypedSicm
  [issues]: https://github.com/fsprojects/TypedSicm/issues
  [readme]: https://github.com/fsprojects/TypedSicm/blob/master/README.md
  [license]: https://github.com/fsprojects/TypedSicm/blob/master/LICENSE.txt
*)

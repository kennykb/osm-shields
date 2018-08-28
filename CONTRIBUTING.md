Thanks so much for contributing to this project!

The [project administrator](mailto:Kevin.B.Kenny+github@gmail.com)
has a very simple-minded approach toward the legalities of contributing
to the project: it is intended to be in the Public Domain in common-law
countries, and Creative Commons Zero elsewhere. That is, the code and
data contributed here are dedicated to the public good and to the detriment
of the contributors, their heirs, successors and assigns. In Civil Law
countries, the contributors covenant, insofar as the local law will allow,
to refrain from asserting any _droits moraux_ or _droits d'auteur_ that
may attach to their contributions.

```text
Anything free
Comes with no guarantee
So do as you please
And please don't sue me!
```

Tcl code contributed to this project should conform, as far as is
practicable, with the [Tcl Style Guide](https://core.tcl-lang.org/tips/doc/trunk/tip/352.md).
The author of the existing Tcl code concedes that what is already there
has far to go to bring it into conformance!

SQL code should:
* Prefer subqueries in WITH clauses to embedded SELECT statements
* Render SQL keywords in ALL CAPS, PostGIS function names as ST_MixedCase, and 
  user variables in lowercase.
* Indent queries so that terms at the same syntactic level align if possible: 
  for instance, when continuing the list of columns in a SELECT, align the 
  first column name on the second line with the first column name on the first line.
  
In all source files, TAB characters, if used, MUST correspond to columns that are
eight character positions wide. Use of TAB characters is discouraged but
tolerated.
  
Any graphic templates that match the Manual of Uniform Traffic Control
Devices (MUTCD) in the United States should use the Roadgeek fonts.
If you're introducing signs from a new jurisdiction that requires a
specific font, please tell us in your pull request where to find the
font that's needed - or rather, an open-source font that looks sufficiently
like it that we can use for rendering.

Respect one another, and have fun.

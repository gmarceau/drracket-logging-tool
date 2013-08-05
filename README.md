drracket-logging-tool
=====================

Gathering and analysis of data for learning science experiments

A good place to start reading is the function `load-log` in the
file `drracket-logging-tool/analysis/private/loading.rkt`, which
implements the parsing and loading of the file format used by the
server to store its data, and `load-compiles` in
`drracket-logging-tool/analysis/private/load-compiles.rkt`, which
implements reading all of the log files in a directory while
interpreting the data stored in the file names.

* Our paper "Do Values Grow on Trees?: Expression Integrity in
Functional Programming" is implemented in `expression integrity
metric*.rkt`. 

* Our paper "Measuring the Effectiveness of Error
Messages Designed for Novice Programmers" is implemented in
`coding_summary_-_how_many_fixes.rkt"

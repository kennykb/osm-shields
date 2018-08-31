#!/usr/bin/env tclsh8.6

# Configuration

set here [file dirname [file normalize [info script]]]

source [file join $here config.tcl]

set init 0
if {"--init" in $::argv} {
    set init 1
}

package require tdbc::postgres
tdbc::postgres::connection create db -db $dbname


# Make the tables that keep track of signed routes

if {$init} {
    puts stderr "Execute: graphictables.sql.in"
    exec sed s/@PREFIX@/$prefix/g [file join $here graphictables.sql.in] \
	| psql -d $dbname >@stdout 2>@stderr
}

foreach fn {
    osmfuncs.sql.in
    shieldtables.sql.in
    queryprocs.sql.in
} {
    puts stderr "Execute: $fn"
    exec sed s/@PREFIX@/$prefix/g [file join $here $fn] \
	| psql -d $dbname >@stdout 2>@stderr
}

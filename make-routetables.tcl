#!/usr/bin/env tclsh8.6

package require tdbc::postgres

# Configuration

set here [file dirname [file normalize [info script]]]
source [file join $here config.tcl]

set init 0
for {set i 0} {$i < [llength $argv]} {incr i} {
    set key [lindex $argv $i]
    switch -exact -- $key {
	--init {
	    set init 1
	}
	--prefix - -p {
	    incr i
	    if {$i >= [llength $argv]} {
		error "--prefix requires a value"
	    }
	    set prefix [lindex $argv [incr i]]
	}
    }
}

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
    shieldindices.sql.in
    queryprocs.sql.in
} {
    puts stderr "Execute: $fn"
    exec sed s/@PREFIX@/$prefix/g [file join $here $fn] \
	| psql -q -d $dbname -f - >@stdout 2>@stderr
}

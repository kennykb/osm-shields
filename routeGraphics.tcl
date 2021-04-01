#!/usr/bin/env tclsh8.6

# Configuration

set here [file normalize [file dirname [info script]]]
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
	    set prefix [lindex $argv $i]
	}
    }
}

set templateDir [file join $here templates]

# Connect to the database

package require tdbc::postgres
tdbc::postgres::connection create db -db $dbname

namespace eval routeGraphics {

    variable templateCache {}

    proc init {} {
	variable ::pngDir
	variable ::tmpDir
	variable ::templateDir
	variable sawUnknownNetwork
	variable didBanner
	variable didRoute
	variable templateExists

	file mkdir $pngDir

	set sawUnknownNetwork {}
	set didBanner {}
	set didRoute {}

	file mkdir $tmpDir

	foreach template [glob -directory $templateDir -types f -tails *.svg] {
	    dict set templateExists $template {}
	}
    }

    # Colors for simplified 'flower pot' shields for counties in Ontario
    # Any county not listed is black-on-white

    variable CA_ON_county_shieldcolors {
	Grey				white_blue
	Halton				yellow_blue
	Hamilton			white_blue
	Niagara				white_blue
	Peel				yellow_black
	Simcoe				blue_white
    }

    # Haldimand, Huron, Lennox and Addington, Middlesex,
    # Norfolk, Prescott and Russell are unverified

    # Perth needs special handling for PERTH LINE and PERTH ROAD

    # Don't attempt these labels at the moment - they are not
    # really useful for markers on maps.

    variable CA_ON_county_labels {
	"Brant"				{BRANT 		COUNTY}
	"Bruce"				{BRUCE		COUNTY}
	"Chatham-Kent"			{CHATHAM	KENT}
	"Dufferin"			{DUFFERIN	COUNTY}
	"Durham"			{DURHAM		REGION}
	"Elgin"				{ELGIN		COUNTY}
	"Essex"				{ESSEX		COUNTY}
	"Frontenac"			{FRONTENAC	COUNTY}
	"Haldimand"			{HALDIMAND	COUNTY}
	"Haliburton"			{HALIBURTON	COUNTY}
	"Hastings"			{HASTINGS	COUNTY}
	"Huron"				{HURON		COUNTY}
	"Kawartha Lakes"		{KAWARTHA	LAKES}
	"Greater Sudbury"		{GREATER	SUDBURY}
	"Lambton"			{LAMBTON	COUNTY}
	"Lanark"			{LANARK		COUNTY}
	"Leeds and Grenville"		{"LEEDS AND\nGRENVILLE" COUNTIES}
	"Lennox and Addington"		{"LENNOX AND\nADDINGTON" COUNTY}
	"Middlesex"			{MIDDLESEX	COUNTY}
	"Muskoka"			{MUSKOKA	{}}
	"Norfolk"			{NORFOLK	COUNTY}
	"Northumberland"		{NORTHUMBERLAND	COUNTY}
	"Ottawa"			{OTTAWA		{}}
	"Oxford"			{OXFORD		COUNTY}
	"Peterborough"			{PETERBOROUGH	COUNTY}
	"Prescott and Russell"		{"PRESCOTT AND\nRUSSELL" COUNTIES}
	"Prince Edward"			{"PRINCE\nEDWARD" COUNTY}
	"Renfrew"			{RENFREW	COUNTY}
	"Stormont, Dundas and Glengarry" {S.D.G.	{}}
	"Waterloo"			{WATERLOO	REGION}
	"Wellington"			{WELLINGTON	COUNTY}
	"York"				{REGION		YORK}
    }


    # New York has a great many roads that have unique shields.
    # List them here

    variable US_NY_parkways {
	"BMP"					US:NY-BMP.svg
	"Bear Mountain State Parkway"		US:NY-BMP.svg
	"BP"					US:NY-BP.svg
	"Belt Parkway"				US:NY-BP.svg
	"B"					US:NY-BSP.svg
	"Bethpage State Parkway"		US:NY-BSP.svg
	"BRP"					US:NY-BRP.svg
	"Bronx River Parkway"			US:NY-BRP.svg
	"CCP" 					US:NY-CCP.svg
	"Cross Country Parkway"			US:NY-CCP.svg
	"CIP"					US:NY-CIP.svg
	"Cross Island Parkway"			US:NY-CIP.svg
	"FDR"					US:NY-FDR.svg
	"FDRD"					US:NY-FDR.svg
	"FDR Drive"				US:NY-FDR.svg
	"Franklin D. Roosevelt East River Drive" US:NY-FDR.svg
	"GCP"					US:NY-GCP.svg
	"Grand Central Parkway"			US:NY-GCP.svg
	"HRD"					US:NY-HRD.svg
	"Harlem River Drive"			US:NY-HRD.svg
	"H"					US:NY-HSP.svg
	"Heckscher State Parkway"		US:NY-HSP.svg
	"HHP"					US:NY-HHP.svg
	"Henry Hudson Parkway"			US:NY-HHP.svg
	"HRP"					US:NY-HRP.svg
	"Hutchinson River Parkway"		US:NY-HRP.svg
	"JRP"					US:NY-JRP.svg
	"Jackie Robinson Parkway"		US:NY-JRP.svg
	"KWVP"					US:NY-KWVP.svg
	"Korean War Veterans Parkway"		US:NY-KWVP.svg
	"LOSP" 					US:NY-LOSP.svg
	"Lake Ontario State Parkway"		US:NY-LOSP.svg
	"LE"					US:NY-LE.svg
	"LaSalle Expressway"			US:NY-LE.svg
	"LWP"					US:NY-LWP.svg
	"Lake Welch Parkway"			US:NY-LWP.svg
	"L"					US:NY-LP.svg
	"Loop Parkway"				US:NY-LP.svg
	"M"					US:NY-MSP.svg
	"Meadowbrook State Parkway"		US:NY-MSP.svg
	"MP"					US:NY-MP.svg
	"Mosholu Parkway"			US:NY-MP.svg
	"N"					US:NY-NSP.svg
	"NSP"					US:NY-NSP.svg
	"Northern State Parkway"		US:NY-NSP.svg
	"O"					US:NY-OP.svg
	"Ocean Parkway"				US:NY-OP.svg
	"PIP"					US:NY-PIP.svg
	"Palisades Interstate Parkway"		US:NY-PIP.svg
	"PP"					US:NY-PP.svg
	"Pelham Parkway"			US:NY-PP.svg
	"Bronx and Pelham Parkway"		US:NY-PP.svg
	"RM"					US:NY-RMC.svg
	"Robert Moses Causeway"			US:NY-RMC.svg
	"RMSP"					US:NY-RMSP.svg
	"Robert Moses State Parkway"		US:NY-RMSP.svg
	"SA"					US:NY-SASP.svg
	"Sagtikos State Parkway"		US:NY-SASP.svg
	"SMP"					US:NY-SMP.svg
	"Saw Mill River Parkway"		US:NY-SMP.svg
	"SLD"					US:NY-SLD.svg
	"Seven Lakes Drive"			US:NY-SLD.svg
	"SM"					US:NY-SMSP.svg
	"Sunken Meadow State Parkway"		US:NY-SMSP.svg
	"SO"					US:NY-SOSP.svg
	"Southern State Parkway"		US:NY-SOSP.svg
	"SBP"					US:NY-SBP.svg
	"Sprain Brook Parkway"			US:NY-SBP.svg
	"TSP"					US:NY-TSP.svg
	"Taconic State Parkway"			US:NY-TSP.svg
	"W"					US:NY-WSP.svg
	"Wantagh State Parkway"			US:NY-WSP.svg
    }

    # Ohio abbreviates its county names - expand them here

    variable US_OH_county_abbr {
	ATH ATHENS
	AUG AUGLAIZE
 	BEL BELMONT
	BUT BUTLER
	CAR CARROLL
	COL COLUMBIANA
	COS COSHOCTON
	FAI FAIRFIELD
	FUL FULTON
	GAL GALLIA
	GUE GUERNSEY
	HAR HARDIN
	HAS HARRISON
	HEN HENRY
	HOC HOCKING
	HOL HOLMES
	KNO KNOX
	JEF JEFFERSON
	LAW LAWRENCE
	LIC LICKING
	LOG LOGAN
	LUC LUCAS
	MAH MAHONING
	MED MEDINA
	MOE MONROE
	MRW MORROW
	NOB NOBLE
	OTT OTTAWA
	PAU PAULDING
	PER PERRY
	SCI SCIOTO
	SEN SENECA
	STA STARK
	SUM SUMMIT
	TUS TUSCARAWAS
	UNI UNION
	VIN VINTON
	WAY WAYNE
	WIL WILLIAMS
	WYA WYANDOT
    }

    proc finish {} {
	variable ::tmpDir
	file delete -force $tmpDir
    }

    init
}
    
proc routeGraphics::launchInkscape {} {
    variable inkscapeChan
    variable inkscapeCollect
    variable inkscapeReady

    puts "Launch inkscape"
    set inkscapeChan [open "|inkscape --shell" w+]
    chan configure $inkscapeChan -blocking 0 -buffering line
    set inkscapeCollect {}
    set inkscapeReady 0
    fileevent $inkscapeChan readable [namespace current]::replyFromInkscape
    waitForInkscape
    puts "Inkscape ready"
}

proc routeGraphics::waitForInkscape {} {
    variable inkscapeReady
    while {!$inkscapeReady} {
	vwait [namespace current]::inkscapeReady
    }
}    

proc routeGraphics::runInkscape {svg size png} {
    variable inkscapeChan
    variable inkscapeReady
    waitForInkscape
    set inkscapeReady 0
    # puts "Is $svg readable? [file readable $svg]"
    # puts "Send to Inkscape:"
    # puts               "\"$svg\" --export-area-snap --export-height=$size\
    #                              --export-png=\"$png\""
    puts $inkscapeChan "\"$svg\" --export-area-snap --export-height=$size\
                                 --export-png=\"$png\""
}

proc routeGraphics::closeInkscape {} {
    variable inkscapeChan
    close $inkscapeChan
}

proc routeGraphics::replyFromInkscape {} {
    variable inkscapeChan
    variable inkscapeCollect
    variable inkscapeReady
    set status [catch {read $inkscapeChan} data]
    if {$status != 0} {
	puts stderr "Inkscape error: $data"
	catch {close $inkscapeChan}
    } elseif {[string length $data] > 0} {
	append inkscapeCollect $data
	if {[string range $inkscapeCollect end-1 end] eq "\n>"} {
	    #puts stderr [string range $inkscapeCollect 0 end-1]
	    set inkscapeReady 1
	    set inkscapeCollect {}
	}
    } elseif {[eof $inkscapeChan]} {
	puts stderr $inkscapeCollect
	puts stderr "Unexpected EOF from inkscape"
	catch {close $inkscapeChan}
    }
}

namespace eval routeGraphics {
    variable modifierKeys {
	Alternate	Belt
	Business	Bypass
	City		Connector	Downtown
	Emergency	Future		Link
	Loop		Scenic		Spur		
	Toll		Truck
    }
}

proc routeGraphics::strip_modifiers {network modifiersVar} {
    upvar 1 $modifiersVar modifiers
    variable modifierKeys
    set parts [split $network :]
    set mods {}
    for {set i [expr {[llength $parts] - 1}]} {$i > 0} {incr i -1} {
	set part [lindex $parts $i]
	if {$part in $modifierKeys} {
	    lappend mods $part
	} else {
	    break
	}
    }
    set modifiers [lreverse $mods]
    return [join [lrange $parts 0 $i] :]
}

proc routeGraphics::readTemplate {template} {
    variable ::templateDir
    variable templateCache
    if {![dict exists $templateCache $template]} {
	set f [open [file join $templateDir $template] r]
	dict set templateCache $template [read $f]
	close $f
    }
    return [dict get $templateCache $template]
}

proc routeGraphics::getSVGName {network ref} {
    variable ::tmpDir
    # Sanitize the file name
    set ref [string map {/ :} $ref]
    return [file join $tmpDir $network $ref.svg]
}

proc routeGraphics::getPNGName {network ref size} {
    variable ::pngDir
    set dir [file join $pngDir default $size $network]
    file mkdir $dir
    # Sanitize the file name
    set ref [string map {/ :} $ref]
    if {$ref eq ""} { set ref "_" }
    return [file join $dir $ref.png]
}

proc routeGraphics::getBannerSVGName {network banner} {
    variable ::tmpDir
    return [file join $tmpDir $network-$banner.svg]
}

proc routeGraphics::getBannerPNGName {network banner size} {
    variable ::pngDir
    set bannerDir [file join $pngDir default $size BANNER:$network]
    file mkdir $bannerDir
    return [file join $bannerDir $banner.png]
    
}

proc routeGraphics::makeSVG {network ref templateFile keys values} {
    variable ::tmpDir

    set svgname [getSVGName $network $ref]
    if {![file exists $svgname]} {
	file mkdir [file dirname $svgname]
	set template [readTemplate $templateFile]
	set map {}
	foreach k $keys v $values {
	    if {$k != {}} {
		lappend map %$k% $v
	    }
	}
	set data [string map $map $template]
	set f [open $svgname w]
	puts -nonewline $f $data
	close $f
    }
    return $svgname
}

proc routeGraphics::makePNGs {network ref {scale 1.0}} {

    variable ::sizes
    variable makeshield
    variable removeshield

    db transaction {
	set svgName [getSVGName $network $ref]
	
	foreach size $sizes {
	    
	    set pngName [getPNGName $network $ref $size]
	    set svgHeight [expr {$size * $scale}]
	    runInkscape $svgName $svgHeight $pngName
	    $removeshield allrows [list \
				       network $network \
				       ref $ref \
				       size $size]
	    $makeshield allrows [list \
				     network $network \
				     ref $ref \
				     size $size \
				     filename $pngName]
	}
    }
}

namespace eval routeGraphics {
    variable  bannerFilesDefault {
	ALTERNATE	banner:alternate.svg
	ALT		banner:alt.svg
	BELT		banner:belt.svg
	BUSINESS	banner:bus.svg
	BYPASS		banner:byp.svg
	CITY		banner:city.svg
	CONNECTOR	banner:connector.svg
	EMERGENCY	banner:emergency.svg
	LOOP		banner:loop.svg
	SCENIC		banner:scenic.svg
	SPUR		banner:spur.svg
	TOLL		banner:toll.svg
	TRUCK		banner:truck.svg
    }
    variable bannerFilesI {
	FUTURE		banner:i:future.svg
	SPUR		banner:i:spur.svg
    }
    variable bannerFilesCR {
	ALTERNATE	banner:cr:alt.svg
	SPUR		banner:cr:spur.svg
	TRUCK		banner:cr:truck.svg
    }
}

proc routeGraphics::makeBannerSVG {network modifier} {

    variable bannerFilesDefault
    variable bannerFilesI
    variable bannerFilesCR

    variable ::templateDir

    if {$network eq "US:I"} {
	set bannerFiles $bannerFilesI
    } elseif {[regexp "US:..:.*" $network]} {
	set bannerFiles $bannerFilesCR
    } else {
	set bannerFiles $bannerFilesDefault
    }
    if {![dict exists $bannerFiles $modifier]} {
	puts stderr "OOPS: No file for modifier $modifier in network $network"
    }

    set svgname [getBannerSVGName $network $modifier]
    set templateFile [file join $templateDir [dict get $bannerFiles $modifier]]
    
    if {![file exists $svgname]} {
	set template [readTemplate $templateFile]
	set map [list %num% $modifier]
	set data [string map $map $template]
	set f [open $svgname w]
	puts -nonewline $f $data
	close $f
    }

}

proc routeGraphics::makeBannerPNG {network banner size {scale 0.5}} {

    set svgName [getBannerSVGName $network $banner]
    set pngName [getBannerPNGName $network $banner $size]
    
    if {![file exists $pngName]} {
	#puts "Make PNG $pngName from $svgName at scale $scale and size $size"
	runInkscape $svgName [expr {$scale * $size}] $pngName
    }
}

proc routeGraphics::stackModifiers {network rootNetwork ref
				    modifiers {modnw {}}} {

    variable didBanner
    variable makeshield
    variable removeshield
    variable ::sizes

    if {$modnw eq {}} {
	set modnw $rootNetwork
    }

    set modifiers [string toupper $modifiers]

    foreach modifier $modifiers {
	if {![dict exists $didBanner $modnw $modifier]} {
	    makeBannerSVG $modnw $modifier
	}
    }

    db transaction {
	foreach size $sizes {
	    set pngs {}
	    if {![dict exists $didBanner $modnw $modifiers]} {
		foreach modifier $modifiers {
		    makeBannerPNG $modnw $modifier $size
		}
	    }
	    foreach modifier $modifiers {
		lappend pngs [getBannerPNGName $modnw $modifier $size]
	    }
	    lappend pngs [getPNGName $rootNetwork $ref $size]
	    set stackName [getPNGName $network $ref $size]
	    waitForInkscape
	    set status [catch {
		exec convert -background transparent -gravity Center \
		    {*}$pngs -append $stackName
	    } result]
	    if {$status != 0} {
		puts "ERROR: $result"
	    }
	    $removeshield allrows [list \
				       network $network \
				       ref $ref \
				       size $size]
	    $makeshield allrows [list \
				     network $network \
				     ref $ref \
				     size $size \
				     filename $stackName]
	}
    }
 
    foreach modifier $modifiers {
	dict set didBanner $network $modifier
    }

}

proc routeGraphics::findGenericTemplate {pattern ref} {

    variable templateExists

    # This procedure is for numbered routes. If there is no number supplied,
    # give up at this point so as not to generate blank shields.

    if {$ref eq {}} {
	return {}
    }

    switch -regexp -- $ref {
	{^[^1]$}		{ set needwid 1 }
	{^.$}			{ set needwid 0.5 }
	{^[^1][^1]$}		{ set needwid 2 }
	{^..$}			{ set needwid 1.5 }
	{^[^1][^1][^1]$}	{ set needwid 3 }
	{^...$}			{ set needwid 2.5 }
	{^[^1][^1][^1][^1]$}	{ set needwid 4 }
	{^....$}		{ set needwid 3.5 }
	{^.....$}		{ set needwid 5 }
	default			{
	    set needwid 6
	}
    }

    set widest {}
    for {set wid 0.5} {$wid <= 6} {set wid [format %g [expr {0.5 + $wid}]]} {
	set trial ${pattern}_${wid}.svg
	if {[dict exists $templateExists $trial]} {
	    set widest $trial
	    if {$wid >= $needwid} {
		return $widest
	    }
	}
    }

    return $widest

}

proc routeGraphics::make_pngs {network ref} {

    variable didRoute
    variable sawUnknownNetwork

    if {[dict exists $didRoute $network $ref]} {
	return
    }
    dict set didRoute $network $ref {}

    set ok 0

    set rootnetwork [strip_modifiers $network modifiers]

    set pat ""
    switch -regexp -matchvar nwparts $rootnetwork {

	lcn - rcn - ncn -
	lwn - rwn - nwn {
	    # Don't process shields for cycleways and footways yet.
	    return 0
	}

	{^US:NHS High Priority Corridors} -
	{^US:NY:Reference} {
	    # These should be unsigned_ref!
	    return 0
	}

	^CA:AB$ {
	    if {$ref in {1 16}} {
		set mod {ALBERTA}
		set pat CA:TCH_1.5.svg
		set scale 1.33333
	    } elseif {[regexp {^[5678]\d\d$} $ref]} {
		set mod {}
		set pat [findGenericTemplate CA:AB_local $ref]
		set num $ref
		set scale 1.33333
	    } else {
		set mod {}
		set pat [findGenericTemplate CA:AB $ref]
		set num $ref
		set scale 1.33333
	    }
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {num mod} [list $num $mod]
		makePNGs $rootnetwork $ref $scale
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	    
	}

	^CA:AB:primary$ {
	    set mod {}
	    set pat [findGenericTemplate CA:AB $ref]
	    set num $ref
	    set scale 1.33333
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {num mod} [list $num $mod]
		makePNGs $rootnetwork $ref $scale
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^CA:AB:secondary$ {
	    set mod {}
	    set pat [findGenericTemplate CA:AB_local $ref]
	    set num $ref
	    set scale 1.33333
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {num mod} [list $num $mod]
		makePNGs $rootnetwork $ref $scale
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}
	
	^CA:BC$ -
	^CA:BC:primary$ -
	^CA:BC:trunk$ {
	    if {$ref in {1 16}} {
		set mod {BRITISH COLUMBIA}
		set pat CA:TCH_1.5.svg
		set num $ref
		set suf {}
		set scale 1.3
	    } elseif {[regexp {^([0-9]+)([A-Z])$} $ref -> num suf]} {
		set mod {}
		set pat [findGenericTemplate CA:BC_suf $num]
		set scale 1.2
	    } elseif {[regexp {^[0-9]+$} $ref num]} {
		set mod {}
		set pat [findGenericTemplate CA:BC $num]
		set suf {}
		set scale 1.2
	    } else {
		puts "No pattern matches $network $ref"
	    }
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {num suf mod} [list $num $suf $mod]
		makePNGs $rootnetwork $ref $scale
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^CA:MB$ -
	^CA:MB:PTH$ {
	    if {$ref in {1 16 100}} {
		set mod {MANITOBA}
		set pat [findGenericTemplate CA:TCH $ref]
		set num $ref
		set suf {}
		set scale 1.3
	    } elseif {[regexp {^([0-9]+)([A-Z])$} $ref -> num suf]} {
		set mod {}
		set pat [findGenericTemplate CA:MB_suf $num]
		set scale 1.25
	    } elseif {[regexp {^[0-9]+$} $ref num]} {
		set mod {}
		if {$num >= 200} {
		    set pat [findGenericTemplate CA:MB_secondary $num]
		    set scale 1.25
		} else {
		    set pat [findGenericTemplate CA:MB $num]
		    set scale 1.25
		}
		set suf {}
	    } else {
		puts "No pattern matches $network $ref"
	    }
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {num suf mod} [list $num $suf $mod]
		makePNGs $rootnetwork $ref $scale
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^CA:MB:PR$ {
	    set pat [findGenericTemplate CA:MB_secondary $ref]
	    set scale 1.25
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num} $ref
		makePNGs $rootnetwork $ref $scale
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^CA:NB:primary$ {
	    set pat [findGenericTemplate CA:NB_green $ref]
	    set scale 1.0
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num} $ref
		makePNGs $rootnetwork $ref $scale
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}
	
	^CA:NB:secondary$ {
	    set pat [findGenericTemplate CA:NB_blue $ref]
	    set scale 1.0
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num} $ref
		makePNGs $rootnetwork $ref $scale
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}
	
	^CA:NB:tertiary$ {
	    set pat [findGenericTemplate CA:NB_black $ref]
	    set scale 1.0
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num} $ref
		makePNGs $rootnetwork $ref $scale
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}
	
	^CA:NL$ -
	^CA:NL:R$ {
	    if {$ref in {1}} {
		set pat CA:TCH
		set scale 1.2
	    } else {
		# Replace dashes with centred dots for compactness
		regsub -all -- {-} $ref \u00b7 ref
		set pat CA:NL
		set scale 1.0
	    }
	    set pat [findGenericTemplate $pat $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num} $ref
		makePNGs $rootnetwork $ref $scale
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^CA:NS:T {
	    if {![string is integer -strict $ref]} {
		set ok 0
	    } else {
		set pat [findGenericTemplate US:US $ref]
		makeSVG $rootnetwork $ref $pat {num} [list $ref]
		makePNGs $rootnetwork $ref 1.0
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^CA:NS:H {
	    if {![string is integer -strict $ref]} {
		set ok 0
	    } else {
		set pat CA:NS_arterial_2.5.svg
		if {$ref >= 104 && $ref <= 106} {
		    set pat [findGenericTemplate CA:TCH $ref]
		}
		if {$pat ne ""} {
		    makeSVG $rootnetwork $ref $pat {num} [list $ref]
		    makePNGs $rootnetwork $ref 1.2
		    set ok 1
		}
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^CA:NS:R {
	    if {![string is integer -strict $ref]} {
		set ok 0
	    } else {
		set pat CA:NS_collector_3.svg
		makeSVG $rootnetwork $ref $pat {num} [list $ref]
		makePNGs $rootnetwork $ref 0.5
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^CA:NS$ {
	    set scale 1.0
	    if {![string is integer -strict $ref]} {
		set ok 0
	    } elseif {$ref < 100} {
		# Nova Scotia trunk roads look just like US Highways
		set pat [findGenericTemplate US:US $ref]
	    } elseif {$ref >= 104 && $ref <= 106} {
		set pat [findGenericTemplate CA:TCH $ref]
	    } elseif {$ref < 200} {
		set pat [findGenericTemplate CA:NS_arterial $ref]
		set scale 1.2
	    } else {
		set pat [findGenericTemplate CA:NS_collector $ref]
		set scale 0.5
	    }
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {num} [list $ref]
		makePNGs $rootnetwork $ref $scale
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}
	    
	^CA:NT$ {
	    set pat [findGenericTemplate CA:NT $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num} $ref
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}
	    
	^CA:ON:primary$ {
	    set tpos [lsearch -nocase -exact Toll $modifiers]
	    if {$tpos >= 0} {
		set modifiers [lreplace $modifiers $tpos $tpos]
		append rootnetwork :Toll
		set num $ref
		set suf {}
		set pat [findGenericTemplate CA:ON:primary:Toll $num]
		set scale 2.0
	    } else {
		switch -regexp -matchvar refparts $ref {
		    QEW {
			set pat CA:ON:primary-QEW.svg
			set num QEW
			set suf {}
		    }
		    {(\d+)([[:alpha:]])} {
			lassign $refparts - num suf
			set pat [findGenericTemplate CA:ON:primary_suf $num]
		    }
		    {\d+} {
			set num $ref
			set suf {}
			set pat [findGenericTemplate CA:ON:primary $num]
		    }
		    default {
			puts "$network $ref not handled"
		    }
		}
		set scale 1.6
	    }
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {num suf} [list $num $suf]
		makePNGs $rootnetwork $ref $scale
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^CA:ON:primary:ETR$ {
	    regsub -- { ETR$} $ref {} num
	    set pat [findGenericTemplate CA:ON:primary:ETR $num]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {num} [list $num]
		makePNGs $rootnetwork $ref 1.0
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^CA:ON:secondary$ {
	    switch -regexp -matchvar refparts $ref {
		{(\d+)([[:alpha:]])} {
		    lassign $refparts - num suf
		    set pat [findGenericTemplate CA:ON:secondary_suf $num]
		}
		{\d+} {
		    set num $ref
		    set suf {}
		    set pat [findGenericTemplate CA:ON:secondary $num]
		}
		default {
		    puts "$network $ref not handled"
		}
	    }
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {num suf} [list $num $suf]
		makePNGs $rootnetwork $ref 1.0
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^CA:ON:tertiary$ {
	    set pat [findGenericTemplate CA:ON:tertiary $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {num} [list $ref]
		makePNGs $rootnetwork $ref 1.0
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	{^CA:ON:([A-Z][a-z][A-Za-z ,-]*)$} {

	    variable CA_ON_county_shieldcolors

	    lassign $nwparts - county

	    # County roads in ontario

	    set generic CA:ON:county
	    if {[dict exists $CA_ON_county_shieldcolors $county]} {
		append generic :[dict get $CA_ON_county_shieldcolors $county]
	    }
	    set pat [findGenericTemplate $generic $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {num} [list $ref]
		makePNGs $rootnetwork $ref 1.0
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^CA:PE$ {
	    if {$ref eq {1}} {
		set pat [findGenericTemplate CA:TCH $ref]
		set num $ref
		set suf {}
	    } elseif {[regexp {^(\d+)([A-Z]?)$} $ref -> num suf]} {
		if {$suf ne {}} {
		    set b CA:PEI_suf
		} else {
		    set b CA:PEI
		}
		set pat [findGenericTemplate $b $num]
	    }
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {num suf} [list $num $suf]
		makePNGs $rootnetwork $ref 1.0
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^CA:QC:A$ {
	    set pat [findGenericTemplate CA:QC_AR $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {num} [list $ref]
		makePNGs $rootnetwork $ref 1.3
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}
	
	^CA:QC:R$ {
	    set pat CA:QC_R.svg
	    makeSVG $rootnetwork $ref $pat \
		{num} [list $ref]
	    makePNGs $rootnetwork $ref 1.3
	    set ok 1
	    stackModifiers $network $rootnetwork $ref $modifiers
	}

	^CA:SK:primary$ -
	^CA:SK:tertiary$ {
	    set pat [findGenericTemplate CA:SK:primary $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {num} [list $ref]
		makePNGs $rootnetwork $ref 1.0
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^CA:SK:secondary$ {
	    set pat [findGenericTemplate CA:SK:rural $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {num} [list $ref]
		makePNGs $rootnetwork $ref 1.6
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}
	
	^CA:YT$ {
	    if {$ref in {1 2 3 4 5 6 7 8 9 10 11 37}} {
		set pat CA:YT:$ref.svg
	    } else {
		set pat [findGenericTemplate rectangle_thick $ref]
	    }
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num suf} [list $ref {}]
		makePNGs $rootnetwork $ref 1.0
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}
	
	^CA:transcanada$ -
	^CA:yellowhead$ {
	    # Yellowhead Route is signed TCH for most of its length.
	    # Also bears a custom shield in many places, but let TCH
	    # dominate for now.

	    if {$ref eq {}} {
		set pat CA:TCH_1.5.svg
	    } else {
		set pat [findGenericTemplate CA:TCH $ref]
	    }
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num suf} [list $ref {}]
		makePNGs $rootnetwork $ref 1.0
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^MEX$ -
	^MX$ -
	^MX:MEX$ -
	^MX:MX$ {
	    set scale 1.375
	    set pat [findGenericTemplate MX:MX $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num suf} [list $ref {}]
		makePNGs $rootnetwork $ref $scale
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^MX:QRO$ -
	^QRO$ {
	    regsub {^QRO } $ref {} ref
	    set pat [findGenericTemplate MX:QRO $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num suf} [list $ref {}]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^CA:MB:Winnipeg$ -
	^US:AK$ -
	^US:AL$ -
	^US:AZ$ -
	^US:CA$ -
	^US:CO$ -
	^US:DC$ -
	^US:HI$ -
	^US:ID$ -
	^US:IL$ -
	^US:KS$ -
	^US:MD$ -
	^US:MI$ -
	^US:MN$ -
	^US:MO$ -
	^US:NC$ -
	^US:ND$ -
	^US:NM$ -
	^US:NV$ -
	^US:OH$ -
	^US:OR$ -
	^US:RI$ -
	^US:SC$ -
	^US:SD$ -
	^US:TN$ -
	^US:TN:Secondary$ -
	^US:UT$ -
	^US:VA$ -
	^US:WA$ -
	^US:WI$ -
	^US:WV$ -
	^US:WY$ {
	    set pat [findGenericTemplate $rootnetwork $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num} $ref
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^US:AR$ {
	    switch -regexp -matchvar refparts $ref {
		{(\d+)([[:alpha:]])} {
		    lassign $refparts - num suf
		}
		{\d+} {
		    set num $ref
		    set suf {}
		}
		{917-1} {
		    set num $ref
		    set suf {}
		}
	    }
	    if {$ref eq {75S}} {
		lappend modifiers Truck
	    }
	    switch -exact $ref {
		980 {
		    set pat US:AR_airport.svg
		}
		917-1 {
		    set pat US:AR_917-1.svg
		}
		default {
		    set pat [findGenericTemplate US:AR $num]
		}
	    }
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num suf} [list $num $suf]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^US:CT$ -
	^US:MO:Supplemental$ {
	    set pat [findGenericTemplate rectangle_thick $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num} $ref
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^US:DE$ -
	^US:MS$ {
	    if {[regexp {^..?$} $ref]} {
		set pat circle_2.svg
	    } else {
		set pat [findGenericTemplate oval $ref]
	    }
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num} $ref
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^US:FL$ {
	    set scale 1.0
	    set mod {}
	    if {[regexp {Turnpike$} $ref]} {
		set pat US:FL-Turnpike.svg
		set scale 1.2
	    } elseif {[set pos [lsearch -nocase -exact Toll $modifiers]] >= 0} {
		set mod Toll
		set modifiers [lreplace $modifiers $pos $pos]
		set pat US:FL:Toll.svg
		set scale 1.25
	    } else {
		set mod {}
		set pat [findGenericTemplate US:FL $ref]
		set scale 1.0
	    }
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num} $ref
		makePNGs $rootnetwork $ref $scale
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^US:GA$ {
	    set vals [list $ref]
	    switch -regexp $ref {
		515 {
		    set pat US:GA:ADHS:A_3.svg
		}
		520 {
		    set pat US:GA:ADHS:Z_3.svg
		}
		.* {
		    set pat [findGenericTemplate US:GA $ref]
		}
	    }
	    if {$pat ne ""} {
		if {[llength $modifiers] == 0} {
		    lappend vals {}
		} else {
		    switch -exact -- [lindex $modifiers 0] {
			Alternate { lappend vals ALT }
			Business  { lappend vals BUS }
			Bypass    { lappend vals BYP }
			Connector { lappend vals CONN }
			default   {
			    lappend vals \
				[string toupper [lindex $modifiers 0]]
			}
		    }
		}
		if {[llength $vals] != 2} {
		    error "Got here with $ref $vals - wtf"
		}
		makeSVG $network $ref $pat {num mod} $vals
		makePNGs $network $ref
		set ok 1
	    }
	}

	^US:I$ {
	    set mod {}
	    set modnw US:US
	    set pat [findGenericTemplate US:I $ref]
	    set type US:I
	    switch -regexp -matchvar modfields -- $modifiers {
		{(Business|Downtown) (Loop|Spur)} {
		    set mod [string toupper [lindex $modfields 2]]
		    set type US:I:[lindex $modfields 1]_banner
		    set rootnetwork US:I:[join $modifiers :]
		    set pat [findGenericTemplate $type $ref]
		    set modifiers {}
		}
		{Future} -
		{Spur} {
		    set modnw US:I
		}
	    }
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num mod} [list $ref $mod]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers $modnw
	    }
	}

	^US:IA$ -
	^US:VA:[Ss]econdary$ -
	^US:VA:Fairfax$ {
	    set pat [findGenericTemplate circle $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num} $ref
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^US:IN$ {
	    if {$ref eq "Indiana Toll Road"} {
		set pat US:IN-Toll_Road.svg
	    } else {
		set pat [findGenericTemplate $rootnetwork $ref]
	    }
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num} $ref
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}
	
	^US:KY$ {
	    if {[set pos [lsearch -exact Parkway $modifiers]] >= 0} {
		set modifiers [lreplace $modifiers $pos $pos]
		switch -regexp -- $ref {
		    {Audubon} {
			set pat US:KY:Parkway-Audubon.svg
		    }
		    {Bluegrass} -
		    {Blue Grass} {
			set pat US:KY:Parkway-Bluegrass.svg
		    }
		    {Cumberland} {
			set pat US:KY:Parkway-Cumberland.svg
		    }
		    {Hal Rogers} {
			set pat US:KY:Parkway-Hal_Rogers.svg
		    }
		    {Mountain} {
			set pat US:KY:Parkway-Mountain.svg
		    }
		    {Natcher} {
			set pat US:KY:Parkway-Natcher.svg
		    }
		    {Pennyrile} {
			set pat US:KY:Parkway-Pennyrile.svg
		    }
		    {Purchase} {
			set pat US:KY:Parkway-Purchase.svg
		    }
		    {Western Kentucky} {
			set pat US:KY:Parkway-Western_KY.svg
		    }
		}
		
	    } else {
		
		if {$ref eq {AA}} {
		    set pat US:KY-AA.svg
		} elseif {[regexp {^..?$} $ref]} {
		    set pat circle_2.svg
		} else {
		    set pat [findGenericTemplate oval $ref]
		}
		if {$pat ne ""} {
		    makeSVG $rootnetwork $ref $pat {num} $ref
		    makePNGs $rootnetwork $ref
		    set ok 1
		}
		if {$ok} {
		    stackModifiers $network $rootnetwork $ref $modifiers
		}
	    }
	}

	^US:LA$ {
	    set pat US:LA.svg
	    makeSVG $rootnetwork $ref $pat {num} $ref
	    makePNGs $rootnetwork $ref 1.2
	    stackModifiers $network $rootnetwork $ref $modifiers
	    set ok 1
	}
	   
	
	^US:MA$  -
	^US:ME$ {
	    set pat [findGenericTemplate rectangle $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num} $ref
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^US:MT$ {
	    if {[string is digit $ref]
		&& $ref>200 && $ref ni {200 287}} {
		set type US:MT:Secondary
	    } else {
		set type US:MT
	    }
	    set pat [findGenericTemplate $type $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num} $ref
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^US:MT:secondary$ {
	    set pat [findGenericTemplate US:MT:Secondary $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num} $ref
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^US:NE:Recreation$ {
	    if {![regexp {^(.*)([[:alpha:]])$} $ref -> num suf]} {
		set num $ref
		set suf {}
	    }
	    set pat [findGenericTemplate US:NE_recreation $num]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num suf} [list $num $suf]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^US:NH$ {
	    switch -regexp -matchvar refparts -- $ref {
		{^..$} {
		    set type US:NH
		    set num $ref
		    set suf {}
		}
		{^(\d+)([[:alpha:]])$} {
		    lassign $refparts - num suf
		    set type US:NH_suffix
		}
		{^\d+$} {
		    set num $ref
		    set suf {}
		    set type US:NH
		}
		default {
		    puts "$network $ref not handled"
		    return 0
		}
	    }
	    set pat [findGenericTemplate $type $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num suf} [list $num $suf]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^US:NJ$ {
	    switch -regexp -- $ref {
		{^GSP$} -
		{^Garden State Parkway$} {
		    set pat US:NJ-GSP.svg
		}
		{^PIP$} -
		{^Palisades Interstate Parkway$} {
		    set pat US:NJ-PIP.svg
		}
		{^ACE$} -
		{^ACX$} -
		{^Atlantic City Expressway$} {
		    set pat US:NJ-ACX.svg
		}
		{^NJTP$} -
		{^New Jersey Turnpike$} {
		    set pat US:NJ-NJTP.svg
		}
		{^ACBC$} -
		{^Atlantic City Brigantine Connector$} {
		    set pat US:NJ-ACBC.svg
		}
		{^..?$} {
		    set pat circle_2.svg
		}
		default {
		    set pat [findGenericTemplate oval $ref]
		}
	    }
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num} $ref
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^US:NY$ {

	    variable US_NY_parkways
	    if {[regexp {^([0-9]+)([A-Z])$} $ref -> num suf]} {
		set pat [findGenericTemplate US:NY_suf $num]
	    } elseif {[regexp {^[0-9]+$} $ref num]} {
		set pat [findGenericTemplate US:NY $num]
		set suf {}
	    } elseif {[dict exists $US_NY_parkways $ref]} {
		set pat [dict get $US_NY_parkways $ref]
		set num $ref
		set suf {}
	    } else {
		puts stderr "$network $ref not handled"
	    }
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num suf} [list $num $suf]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^US:NY:Thruway$ {
	    set pat US:NY-Thruway.svg
	    makeSVG $rootnetwork $ref $pat {} {}
	    makePNGs $rootnetwork $ref
	    set ok 1
	    stackModifiers $network $rootnetwork $ref $modifiers
	}

	^US:NE$ {
	    if {[llength $modifiers] > 0} {
		set modifiers [lassign $modifiers mod]
		set mod [string toupper $mod]
		if {[regexp {^([0-9]+)([A-Z])$} $ref -> num suf]} {
		    set pat [findGenericTemplate US:NE_banner $num]
		} elseif {[regexp {^[0-9]+$} $ref num]} {
		    set pat [findGenericTemplate US:NE_banner $num]
		    set suf {}
		} else {
		    puts "US:NE $mod $ref not handled"
		}
	    } else {
		set mod {}
		if {[regexp {^([0-9]+)([A-Z])$} $ref -> num suf]} {
		    set pat [findGenericTemplate US:NE_suf $num]
		} elseif {[regexp {^[0-9]+$} $ref num]} {
		    set pat [findGenericTemplate US:NE $num]
		    set suf {}
		} else {
		    puts "US:NE $ref not handled"
		}
	    }
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {mod num suf} [list $mod $num $suf]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^US:OK(:Turnpike)?$ {
	    set tpk [lindex $nwparts 1]
	    if {$tpk eq ":Turnpike"} {
		set pat US:OK-Turnpike.svg
		set num {}
		set suf {}
	    } elseif {[regexp {^([0-9]+)([A-Z])$} $ref -> num suf]} {
		set pat [findGenericTemplate US:OK_suf $num]
	    } elseif {[regexp {^[0-9]+$} $ref num]} {
		set pat [findGenericTemplate US:OK $num]
		set suf {}
	    } elseif {[regexp {^(.*) TURNPIKE} $ref -> num suf]} {
		set pat US:OK-Turnpike.svg
		set num {}
		set suf {}
	    } else {
		puts stderr "$network $ref not handled"
	    }
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num suf} [list $num $suf]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^US:PA$ {
	    if {[set idx [lsearch -exact $modifiers Belt]] >= 0} {
		set modifiers [lreplace $modifiers $idx $idx]
		if {[regexp -nocase {Blue|Green|Orange|Purple|Red|Yellow} \
			 $ref colour]} {
		    set pat US:PA:Belt-[string totitle ${colour}].svg
		}
	    } else {
		set pat [findGenericTemplate US:PA $ref]
	    }
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num} [list $ref]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^US:PA:BELT {
	    if {[regexp -nocase {Blue|Green|Orange|Purple|Red|Yellow} \
		     $ref colour]} {
		set pat US:PA:Belt-[string totitle ${colour}].svg
	    }
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num} [list $ref]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^US:PA:Turnpike {
	    if {$ref eq ""} {
		set pat US:PA:Turnpike.svg
	    } else {
		set pat [findGenericTemplate US:PA:Turnpike $ref]
	    }
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num} [list $ref]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }

	}

	^US:TX$ {
	    if {[llength $modifiers] == 0} {
		set num $ref
		set suf {}
		set pat [findGenericTemplate US:TX $ref]
		set mod {}
	    } else {
		set modifiers [lassign $modifiers mod1]
		append rootnetwork : $mod1
		if {[regexp {^(.*)-([[:alpha:]])} $ref -> num suf]} {
		    set sfx _suffix
		} else {
		    set sfx ""
		    set num $ref
		    set suf {}
		}
		switch -exact $mod1 {
		    "Toll" {
			set mod [string toupper $mod1]
			set num $ref
			set suf {}
			set pat [findGenericTemplate US:TX:Toll $num]
		    }
		    "Business" -
		    "Loop" -
		    "Spur" -
		    default {
			set mod [string toupper $mod1]
			set pat [findGenericTemplate US:TX_banner${sfx} $num]
		    }
		}
	    }
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {num suf mod} [list $num $suf $mod]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }

	}

	^US:TX:(Beltway)$ {
	    set mod [string toupper [lindex $nwparts 1]]
	    set pat [findGenericTemplate US:TX_banner $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {mod num suf} [list $mod $ref {}]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^US:TX:BUS$ {
	    set mod BUS
	    if {[regexp {^(.*)-([[:alpha:]])} $ref -> num suf]} {
		set sfx _suffix
	    } else {
		set sfx ""
		set num $ref
		set suf {}
	    }
	    set pat [findGenericTemplate US:TX_banner${sfx} $num]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {num suf mod} [list $num $suf $mod]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^US:TX:FM$ {
	    set scale 1.2
	    if {[llength $modifiers] > 0} {
		set modifiers [lassign $modifiers mod1]
		set num $ref
		set mod [string toupper $mod1]
		set suf {}
		append rootnetwork : $mod1
		set pat US:TX:FM_banner_suffix.svg
	    } else {
		set pat US:TX:FM.svg
		set num $ref
		set suf {}
		set mod {}
	    }
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {mod num suf} [list $mod $num $suf]
		makePNGs $rootnetwork $ref $scale
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^US:TX:(RE|RM)$ {
	    set scale 1.2
	    set pat $rootnetwork.svg
	    makeSVG $rootnetwork $ref $pat {num} [list $ref]
	    makePNGs $rootnetwork $ref $scale
	    set ok 1
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}
	    
	^US:TX:(NASA)$ -
	^US:TX:(Park)$ -
	^US:TX:(PA)$ -
	^US:TX:(PR)$ -
	^US:TX:(Recreational) {
	    set mod [lindex $nwparts 1]
	    if {$mod eq {PR}} { set mod PARK }
	    set mod [string toupper $mod]
	    set pat [findGenericTemplate US:TX_road_banner $ref]
	    set suf {}
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num mod} [list $ref $mod]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}
	

	^US:US$ {
	    switch -regexp -matchvar refparts $ref {
		{^Historic (?:Route )?66$} {
		    makeSVG $rootnetwork $ref US:US-66.svg {num} $refparts
		    makePNGs $rootnetwork $ref
		    set ok 1
		}
		.* {
		    set pat [findGenericTemplate US:US [lindex $refparts 0]]
		    if {$pat ne ""} {
			makeSVG $rootnetwork $ref $pat {num} $refparts
			makePNGs $rootnetwork $ref
			set ok 1
		    }
		}
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^US:US:Historic$ {
	    if {$ref eq "66"} {
		makeSVG $rootnetwork $ref US:US-66.svg {num} [list $ref]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	^US:VT$ {

	    if {$ref in {19 23 35 53 68}} {
		set num $ref
		set suf {}
		set pat circle_2.svg
	    } elseif {[regexp {(8)(A)} $ref -> num suf]} {
		set pat US:VT:city_suf_1.svg
	    } elseif {$ref in {
		121 127 139 143 144 153
		215 225 235
		315 F-5
	    }} {
		set num $ref
		set suf {}
		set pat [findGenericTemplate oval $ref]
	    } elseif {[regexp {^([0-9]+)([A-Z])$} $ref -> num suf]} {
		set pat [findGenericTemplate US:VT_suf $num]
	    } elseif {[regexp {^[0-9]+$} $ref num]} {
		set pat [findGenericTemplate US:VT $num]
		set suf {}
	    } else {
		puts "US:VT $ref not handled"
	    }
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num suf} [list $num $suf]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	{^US:WI:Rustic$} {
	    set pat US:WI:Rustic.svg
	    set ok 1
	    makeSVG $rootnetwork $ref $pat {num} [list $ref]
	    makePNGs $rootnetwork $ref 1.5
	    stackModifiers $network $rootnetwork $ref $modifiers
	}

	{^US:CA:CR()$} -

	{^US:FL:CR:([^:]+)$} -

	{^US:IA:()[Cc]ounty$} -
	{^US:IA:(Adams|Boone|Bremer|Buena Vista|Cedar|Chickasaw|Clayton)$} -
	{^US:IA:(Dallas|Delaware|Des Moines|Floyd|Franklin|Hardin|Harrison)$} -
	{^US:IA:(Henry|Ida|Jasper|Keokuk|Lee|Lucas|Marshall|Monona)$} -
	{^US:IA:(Plymouth|Polk|Pottawattamie|Sac|Shelby|Story|Van Buren)$} -
	{^US:IA:(Wayne|Woodbury|Worth)$} -


	{^US:IL:(Champaign)$} -
	{^US:IL:(Cook)$} -
	{^US:IL:(Du[Pp]age)$} -
	{^US:IL:(Kane)$} -
	{^US:IL:(Lake)$} -
	{^US:IL:(Livingston)$} -
	{^US:IL:(Macon)$} -
	{^US:IL:(McDonough)$} -
	{^US:IL:(McHenry)$} -
	{^US:IL:(Peoria)$} -
	{^US:IL:(Shelby)$} -

	{^US:MI:(Leelanau)$} -
	{^US:MI:(Marquette)$} -

	{^US:NJ:(Atlantic|Burlington|Camden)$} -
	{^US:NJ:(Cape May|Cape_May)$} -
	{^US:NJ:(Cumberland)$} -
	{^US:NJ:(Essex|Gloucester|Hudson|Mercer|Middlesex|Monmouth)$} -
	{^US:NJ:(Morris|Ocean|Passaic|Somerset|Sussex)$} -
	{^US:NJ:(Union|Warren)$} -
	{^US:NJ:CR$} -

	{^US:NY:(Albany)$} -
	{^US:NY:(Allegany)$} -
	{^US:NY:(Broome)$} -
	{^US:NY:(Cattaraugus)$} -
	{^US:NY:(Chautauqua)$} -
	{^US:NY:(Chemung)$} -
	{^US:NY:(Chenango)$} -
	{^US:NY:(Columbia)$} -
	{^US:NY:(Delaware)$} -
	{^US:NY:(Dutchess)$} -
	{^US:NY:(Essex)$} -
	{^US:NY:(Franklin)$} -
	{^US:NY:(Fulton)$} -
	{^US:NY:(Greene)$} -
	{^US:NY:(Hamilton)$} -
	{^US:NY:(Herkimer)$} -
	{^US:NY:(Jefferson)$} -
	{^US:NY:(Lewis)$} -
	{^US:NY:(Livingston)$} -
	{^US:NY:(Madison)$} -
	{^US:NY:(Montgomery)$} -
	{^US:NY:(Oneida)$} -
	{^US:NY:(Onondaga)$} -
	{^US:NY:(Orange)$} -
	{^US:NY:(Oswego)$} -
	{^US:NY:(Otsego)$} -
	{^US:NY:(Putnam)$} -
	{^US:NY:(Rensselaer)$} -
	{^US:NY:(Rockland)$} -
	{^US:NY:(Saratoga)$} -
	{^US:NY:(Saint Lawrence)$} -
	{^US:NY:(Schenectady)$} -
	{^US:NY:(Schoharie)$} -
	{^US:NY:(Schuyler)$} -
	{^US:NY:(Steuben)$} -
	{^US:NY:(Suffolk)$} -
	{^US:NY:(Sullivan)$} -
	{^US:NY:(Ulster)$} -
	{^US:NY:(Tioga)$} -
	{^US:NY:(Tompkins)$} -
	{^US:NY:(Warren)$} -
	{^US:NY:(Washington)$} -
	{^US:NY:(Westchester)$} -
	{^US:NY:(Yates)$} -
	{^US:NY:CR:Suffolk$} -
	{^US:OH:(COL|JEF|LUC|MAH|OTT|SEN|STA|SUM|TUS)$} {

	    # MUTCD county road shield

	    # Expand Ohio abbreviations

	    variable US_OH_county_abbr
	    if {[regexp {^US:OH:} $network]} {
		set mod [dict get $US_OH_county_abbr [lindex $nwparts 1]]
	    } else {
		set mod [string map {_ { }} \
			     [string toupper \
				  [lindex $nwparts 1]]]
	    }
	    set pat [findGenericTemplate US:county $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num mod} [list $ref $mod]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }

	}

	{^US:ID:(Boundary)$} -

	{^US:MN:(Aitkin|Alger|Anoka|Becker|Beltrami|Benton)$} -
	{^US:MN:(Big Stone|Blue Earth|Brown|Carlton|Carver)$} -
	{^US:MN:(Cass|Chippewa|Chisago|Clay|Clearwater)$} -
	{^US:MN:(Cook|Cottonwood|Crow Wing|Dakota|Dodge)$} -
	{^US:MN:(Douglas|Faribault|Fillmore|Freeborn|Goodhue)$} -
	{^US:MN:(Grant|Hennepin|Houston|Hubbard|Isanti)$} -
	{^US:MN:(Itasca|Jackson|Kanabec|Kandiyohi|Kittson)$} -
	{^US:MN:(Koochiching|Lac qui parle|Lake|Lake of the Woods)$} -
	{^US:MN:(Le Sueur|Lincoln|Lyon|Martin|McLeod)$} -
	{^US:MN:(Meeker|Mille Lacs|Murray|Nicollet|Nobles)$} -
	{^US:MN:(Olmsted|Pine|Pipestone|Pope|Ramsey)$} -
	{^US:MN:(Redwood|Renville|Rice|Sibley|Stearns)$} -
	{^US:MN:(Steele|Stevens|Swift|Traverse|Waseca|Washington)$} -
	{^US:MN:(Watonwan|Wilkin|Winona|Wright|Yellow Medicine)$} -

	{^US:NJ:(Bergen)$} -

	{^US:OH:(AUG|CAR|COS|FAI|FUL|GAL|HAS|HOC)$} -
	{^US:OH:(HOL|KNO|LAW|LIC|LOG|MRW)$} -
	{^US:OH:(PER|SHE|UNI|WAY|WYA)$}  {

	    # Square county road shield

	    variable US_OH_county_abbr
	    if {[regexp {^US:OH:} $network]} {
		set mod [dict get $US_OH_county_abbr [lindex $nwparts 1]]
	    } else {
		set mod [string toupper [lindex $nwparts 1]]
	    }
	    set pat [findGenericTemplate US:county:square $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num mod} [list $ref $mod]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }

	}

	{^US:NY:(Erie)$} {

	    set pat US:NY:Erie.svg
	    makeSVG $rootnetwork $ref $pat {num} $ref
	    makePNGs $rootnetwork $ref
	    set ok 1
	    stackModifiers $network $rootnetwork $ref $modifiers

	}
	
	{^US:OH:(ATH|BEL|GUE|HAR|HEN|WIL)$} {

	    # Green square county road shield

	    variable US_OH_county_abbr
	    if {[regexp {^US:OH:} $network]} {
		set mod [dict get $US_OH_county_abbr [lindex $nwparts 1]]
	    } else {
		set mod [string toupper [lindex $nwparts 1]]
	    }
	    set pat [findGenericTemplate US:county:square:green $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num mod} [list $ref $mod]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }

	}

	{^US:OH:(MED|NOB)$} {

	    # Blue square county road shield

	    variable US_OH_county_abbr
	    if {[regexp {^US:OH:} $network]} {
		set mod [dict get $US_OH_county_abbr [lindex $nwparts 1]]
	    } else {
		set mod [string toupper [lindex $nwparts 1]]
	    }
	    set pat [findGenericTemplate US:county:square:blue $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num mod} [list $ref $mod]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }

	}

	{^US:OH:(VIN)$} {

	    # Yellow square county road shield

	    variable US_OH_county_abbr
	    if {[regexp {^US:OH:} $network]} {
		set mod [dict get $US_OH_county_abbr [lindex $nwparts 1]]
	    } else {
		set mod [string toupper [lindex $nwparts 1]]
	    }
	    set pat [findGenericTemplate US:county:square:yellow $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num mod} [list $ref $mod]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }

	}

	{^US:OH:(ASD)$} -
	{^US:OH:(ASD):(TWP)$} {

	    # Custom county road shield - Ashland County, Ohio

	    lassign $nwparts -> county mod
	    if {$mod == ""} { set mod CO }
	    set pat [findGenericTemplate US:OH:ASD $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num mod} [list $ref $mod]
		makePNGs $rootnetwork $ref 1.15
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }

	}
	
	{^US:OH:(MOE)$} {

	    # Custom county road shield - Monroe County, Ohio

	    variable US_OH_county_abbr
	    if {[regexp {^US:OH:} $network]} {
		set mod [dict get $US_OH_county_abbr [lindex $nwparts 1]]
	    } else {
		set mod [string toupper [lindex $nwparts 1]]
	    }
	    set pat US:OH:MOE.svg
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num mod} [list $ref $mod]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }

	}
	
	{^US:OH:(PAU)$} {

	    # Custom county road shield - Paulding County, Ohio

	    variable US_OH_county_abbr
	    if {[regexp {^US:OH:} $network]} {
		set mod [dict get $US_OH_county_abbr [lindex $nwparts 1]]
	    } else {
		set mod [string toupper [lindex $nwparts 1]]
	    }
	    set pat [findGenericTemplate US:OH:PAU $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num mod} [list $ref $mod]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }

	}

	{^US:OH:(SCI)$} {

	    # Custom county road shield - Scioto County, Ohio

	    variable US_OH_county_abbr
	    if {[regexp {^US:OH:} $network]} {
		set mod [dict get $US_OH_county_abbr [lindex $nwparts 1]]
	    } else {
		set mod [string toupper [lindex $nwparts 1]]
	    }
	    set pat [findGenericTemplate US:OH:SCI $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num mod} [list $ref $mod]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }

	}

	{^US:OH:HOL:(Paint)$} -
	{^US:OH:LOG:(Jefferson)$} -
	{^US:OH:LOG:(Liberty)$} -
	{^US:OH:LOG:(Monroe)$} -
	{^US:OH:MED:(Harrisville)$} -
	{^US:OH:MED:(Wadsworth)$} -
	{^US:OH:MRW:(Harmony)$} -
	{^US:OH:MRW:(South Bloomfield)$} {
	    set mod [string toupper [lindex $nwparts 1]]
	    set num $ref
	    set pat [findGenericTemplate US:township:square $ref]
	    set suf {}
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {mod num suf}  [list $mod $num $suf]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	{^US:OH:MED:(Sharon)} -
	{^US:OH:COS:(Jackson)} {
	    set mod [string toupper [lindex $nwparts 1]]
	    set num $ref
	    set pat [findGenericTemplate US:township:square:green $ref]
	    set suf {}
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {mod num suf}  [list $mod $num $suf]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	{^US:OH:LOG:(Pleasant)$} {
	    set mod [string toupper [lindex $nwparts 1]]
	    set pat [findGenericTemplate US:OH:LOG:Pleasant $ref]
	    set suf {}
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {mod num suf}  [list $mod $ref $suf]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	{^US:OH:SAN:Fremont$} {
	    set pat [findGenericTemplate US:OH:SAN:Fremont $ref]
	    set suf {}
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {num}  [list $ref]
		makePNGs $rootnetwork $ref 1.25
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	{^US:WI:(Adams|Ashland|Barron)$} -
	{^US:WI:(Bayfield|Brown|Buffalo|Burnett|Calumet)} -
	{^US:WI:(Chippewa|Clark|Columbia|Crawford)} -
	{^US:WI:(Dane|Dodge|Door|Douglas|Dunn|Eau[_ ]Claire)$} -
	{^US:WI:(Florence|Fond_du_Lac|Fond du Lac|Forest)$} -
	{^US:WI:(Grant|Green|Green_Lake|Iowa|Iron)$} -
	{^US:WI:(Jackson|Jefferson|Juneau|Kenosha|Kewaunee)$} -
	{^US:WI:(Lafayette|Langlade|Lincoln)$} -
	{^US:WI:(Manitowoc|Marathon|Marinette|Marquette|Menominee|Milwaukee)$} -
	{^US:WI:(Monroe|Oconto|Oneida|Outagamie|Ozaukee)$} -
	{^US:WI:(Pepin|Pierce|Polk|Portage|Price)$} -
	{^US:WI:(Racine|Richland|Rock|Rusk)$} -
	{^US:WI:(Saint Croix|Sauk|Sawyer|Shawano|Sheboygan)$} -
	{^US:WI:(Taylor|Trempealeau|Vernon|Vilas)$} -
	{^US:WI:(Walworth|Washburn|Washington|Waukesha|Waupaca)$} -
	{^US:WI:(Waushara|Winnebago|Wood)$} {

	    # Wisconsin county road shield is a simple square

	    set mod COUNTY
	    set pat [findGenericTemplate US:WI:county $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat {num mod} [list $ref $mod]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }

	}

	{^US:WV:(Barbour|Berkeley|Braxton|Carbon|Calhoun|Clay|Doddridge)$} -
	{^US:WV:(Fairfield|Gilmer|Greenbrier|Hancock|Harrison|Lewis)$} -
	{^US:WV:(Marion|Mason|McDowell|Mercer|Mineral|Monroe)$} -
	{^US:WV:(Monongalia|Nicholas|Pendleton)$} -
	{^US:WV:(Pheasants|Pleasants|Pocahontas|Preston)$} -
	{^US:WV:(Randolph|Ritchie|Summers|Tucker|Tyler|Upshur|Webster|Wood)$} {

	    # West Virginia county roads have circular shields that may be
	    # divided.

	    # In some counties, mappers have left a spurious 'CR ' on the
	    # ref. Get rid of it.

	    set mod [string toupper [lindex $nwparts 1]]
	    if {[regexp {(?:CR )?(.*)/(.*)} $ref -> num suf]} {
		set pat circle:fraction.svg
	    } else {
		regsub "^CR " $ref {} num
		set pat [findGenericTemplate circle $num]
		set suf {}
	    }
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {mod num suf}  [list $mod $num $suf]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	    
	}

	{^US:WV:HARP$} {

	    # West Virginia Home Access Road Program

	    # In some counties, mappers have left a spurious 'HARP' on the
	    # ref. Get rid of it.

	    set mod [string toupper [lindex $nwparts 1]]
	    if {[regexp {(?:HARP )?(.*)/(.*)} $ref -> num suf]} {
		set pat house:fraction.svg
	    } else {
		regsub "^HARP " $ref {} num
		set pat [findGenericTemplate house $num]
		set suf {}
	    }
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {mod num suf}  [list $mod $num $suf]
		makePNGs $rootnetwork $ref
		set ok 1
	    }
	    if {$ok} {
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	    
	}

	{^US:BIA$} {
	    set pat [findGenericTemplate US:BIA $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {num}  [list $ref]
		makePNGs $rootnetwork $ref
		set ok 1
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}

	{^USFS()$} -
	{^US:NF$} -
	{^US:NFSR:(.*):NF[HR]} {

	    # Some mappers have left 'NF' on the route number. Remove it.
	    
	    regsub {^NF } $ref {} ref
	    # Don't try to fit the name of the forest on the shield!
	    set pat [findGenericTemplate US:NFSR $ref]
	    if {$pat ne ""} {
		makeSVG $rootnetwork $ref $pat \
		    {num}  [list $ref]
		makePNGs $rootnetwork $ref
		set ok 1
		stackModifiers $network $rootnetwork $ref $modifiers
	    }
	}
	
	default {
	    if {![dict exists $sawUnknownNetwork $network]} {
		puts "Unknown network: $network (sample ref $ref)"
	    }
	    dict incr sawUnknownNetwork $network
	}

    }
    if {!$ok && ![dict exists $sawUnknownNetwork $network]} {
	puts "No pattern matched [list network $network ref $ref]"
    }

    return $ok
}

namespace eval routeGraphics {
    variable makeshield [::db prepare {
        INSERT INTO osm_shield_graphics(route, network, "ref", "size", filename)
        VALUES('road', :network, :ref, :size, :filename)
    }]
    variable removeshield [::db prepare {
        DELETE FROM osm_shield_graphics
	WHERE route = 'road'
	AND network = :network
	AND "ref" = :ref
	AND "size" = :size
    }]
}

routeGraphics::launchInkscape

if {$init} {
    # Begin by making a set of generic shields for refs that belong
    # to unknown or unrecognized networks or cannot be rendered in their
    # networks

    set f [open [file join $here generic-shield.svg.in] r]
    set template [read $f]
    close $f
    
    set clean_marker [db prepare {
	DELETE FROM osm_shield_graphics
	WHERE NETWORK = 'generic-' || :highway
    }]

    file mkdir [file join $tmpDir generic]
    file mkdir [file join $pngDir generic]
    dict for {highway colour} $markerColours {
	$clean_marker allrows [list highway $highway]
	scan $colour "#%02x%02x%02x" r g b
	set stroke [format "#%02x%02x%02x" \
			[expr {$r/2}] [expr {$g/2}] [expr {$b/2}]]
	for {set cwidth 1} {$cwidth <= 10} {incr cwidth} {
	    set rectwidth [expr {$charWidth * $cwidth + 6}]
	    set canvwidth [expr {$rectwidth + 2}]
	    for {set cheight 1} {$cheight <= 4} {incr cheight} {
		set rectheight [expr {$charHeight*$cheight + 4}]
		set canvheight [expr {$rectheight + 2}]
		set froot $highway-${cheight}x${cwidth}
		set svgname [file join $tmpDir generic $froot.svg]
		set pngname [file join $pngDir generic $froot.png]
		set f [open $svgname w]
		puts $f [string map [list \
					 @CANVWIDTH@ $canvwidth \
					 @CANVHEIGHT@ $canvheight \
					 @RECTWIDTH@ $rectwidth \
					 @RECTHEIGHT@ $rectheight \
					 @FILLCOLOUR@ $colour \
					 @STROKECOLOUR@ $stroke]   $template]
		close $f
		routeGraphics::runInkscape $svgname $canvheight $pngname
		routeGraphics::waitForInkscape
		$routeGraphics::makeshield allrows \
		    [dict create network generic-$highway ref \
			 ${cheight}x${cwidth} \
			 size 0 filename $pngname]
	    }
	}
    }
}

puts "Find changed routes in ${prefix}_shieldroute"

set seenNetwork {}
set need {}
set processedRoute {}
set reported {}
set n 0
db foreach row [string map [list @PREFIX@ $prefix] {
    SELECT DISTINCT s.route, s.network, s.ref
    FROM @PREFIX@_shieldroute s
    WHERE s.changed
    AND s.route = 'road'
    AND s.network IS NOT NULL
    AND NOT EXISTS(SELECT 1
		   FROM osm_shield_graphics g
		   WHERE g.route = s.route
		   AND g.network = s.network
		   AND (g.ref = s.ref
			OR g.ref = '' AND s.ref IS NULL))
}] {

    if {$n == 0} {
	puts "Start the loop over needed shields"
    }

    if {![dict exists $row ref]} {
	dict set $row ref {}
    }

    set route {}
    set network {}
    set ref {}
    set highway {}
    set wayid {}
    dict with row {
	if {![dict exists $processedRoute $network $ref]} {
	    routeGraphics::make_pngs $network $ref
	    dict set processedRoute $network $ref {}
	}
    }
    if {[incr n] % 1000 == 0} {
	puts -nonewline $n...; flush stdout
    }
}
puts {}


routeGraphics::make_pngs US:OH:COS:Jackson 58
routeGraphics::make_pngs US:WV:HARP 908
routeGraphics::make_pngs US:WV:HARP 901/8

puts "Close inkscape - we're done with making shield graphics"
routeGraphics::closeInkscape
puts "Inkscape closed"

foreach {nw count} [lsort -stride 2 -integer -index 1 -decreasing \
			$routeGraphics::sawUnknownNetwork] {
    # How do I love thee? Let me count the ways."
    puts "$nw: $count ways"
}

db allrows [string map [list @PREFIX@ $prefix] {
    UPDATE @PREFIX@_shieldroute
    SET changed = FALSE
    WHERE changed
}]

file delete -force $tmpDir


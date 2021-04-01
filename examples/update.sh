#!/bin/bash

# This script runs periodically to keep the database in sync.
# It is safe to run it frequently from crontab, since it checks
# and will not allow two or more copies to run at the same time.

SCRIPT=$(readlink "$0" || echo "$0")
echo "Running $SCRIPT"
SCRIPTDIR=$(dirname "$SCRIPT")
echo "Look for scripts in $SCRIPTDIR"
pushd $SCRIPTDIR > /dev/null ; SCRIPTDIR=$(pwd -L); popd > /dev/null
if [ -f config.sh ]; then
    . config.sh
else
    . "$SCRIPTDIR/config.sh"
fi
LOCKFILE=/var/lock/.osm_shields.${PREFIX}.lock

# Launch a subprocess under exclusive lock

(
    echo "Make sure that nothing has a lock on $PREFIX tables"
    flock -x -w 1 200 || exit 1
    echo $$ >"$LOCKFILE"

    # Apply the database table prefix to the Lua script

    echo "Preparing scripts to work with $PREFIX tables"
    ln -s -f "$PROJDIR/shieldtables.lua" ./
    sed "s/@PREFIX@/$PREFIX/g" \
	< "$SCRIPTDIR/compatshields.lua.in" \
	> compatshields.lua

    # Retrieve and apply changes

    echo "Retrieving, extracting and applying changes"
    osmosis --rri \
	    --simplify-change \
	    --write-xml-change - \
	| "$TRIMOSC" \
	  -d "$DATABASE" \
	  -P "$PREFIX" \
	  -p "$POLYFILENAME" \
	  - - \
	| tee changes.xml \
	| osm2pgsql --append \
		    --prefix="$PREFIX" \
		    --slim \
		    --cache=$CACHESIZE \
		    --database="$DATABASE" \
		    --number-processes $PROCESSES \
		    --input-reader=xml \
		    --output=flex \
		    --style=compatshields.lua \
		    -

    # Make any needed updates on the shield graphics

    tclsh8.6 "$PROJDIR/routeGraphics.tcl" \
	     --init \
	     --prefix "$PREFIX"

    exit 0
    
) 200>>"$LOCKFILE"
FLOCKEXIT=$?

if [ $FLOCKEXIT == 0 ]; then
    echo "Clean up the lock $LOCKFILE"
    rm "$LOCKFILE"
fi
  
exit $FLOCKEXIT
    



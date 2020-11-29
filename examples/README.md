# Sample scripts for osm-shields

This directory contains an example setup that populates a database
that is suitable for rendering of ways with numbered shields from
extracts at [GeoFabrik](https://download.geofabrik.de/).

To set it up, you have to have the following:

   * A working PostGIS installation, with the `hstore` extension present.
   
   * A working installation of
     the [Tcl programming language](https://www.tcl.tk/)
   
   * A working build of [osm2pgsql](https://osm2pgsql.org), at least
     version 1.3.
	 
   * The command line tools [curl](https://curl.se/), 
     [osmosis](https://wiki.openstreetmap.org/wiki/Osmosis),
	 and [osmium](https://osmcode.org/osmium-tool/).
	 
   * The [osm2pgsql](https://osm2pgsql.org/) tool for creating and updating
     the tables in a rendering database.
	 
   * The `trim_osc.py` script from Ilya Zverev's
     [regiona](https://github.com/Zverik/regional/) project. dated
	 on or after 2020-11-27.
	 
## Setting up for your configuration

To set up osm-shields for your configuration, you need to edit the file,
config.sh`, with the values needed for your particular setup, or else
set environment variables of the same names.

   * **DATABASE** Name of the PostgreSQL database that you are using.
     Default is `gis`.
	 
   * **PREFIX** String to prepend to the names of tables in the database.
     Default is `planet_osm`.
	 
   * **POLYURL** URL from which the polygon describing the region of
     interest is to be downloaded. The example file is for the state
	 of Connecticut in the United States of America, If you go to
	 [the page for a region](https://download.geofabrik.de/north-america/us/connecticut.html)
	 on [GeoFabrik](https://download.geofabrik.de/), you'll usually
	 find a link to the `,poly' file under the heading, 
	 _Other Formats and Auxiliary FIles_.
	 
   * **EXTURL** URL from which the OpenStreetMap extract (in PBF
     format) is to be downloaded.  This URL must correspond to the one
     from the polygon file, and would ordinarily be obtained from
	 [the same place](https://download.geofabrik.de/north-america/us/connecticut.html).
	 
   * **SEQURL** URL for the service that, given a timestamp, determines
     the serial number of the OpenStreetMap minutely extract that
	 corresponds to the given time. Default is 
	 [https://replicate-sequences.osm.mazdermind.de/].
	 
   * ** TRIMOSC** Path name in the filesystem where the
     `trim_osc.py` script is found.
	 
## The Lua script for the flex backend

The Lua script for loading the database, `compatshields.lua.in` is
derived from
the
[compatible.lua](https://github.com/openstreetmap/osm2pgsql/blob/master/flex-config/compatible.lua) script
on the `osm2pgsql` project. It differs only in the following:

   * The `shieldtables` module is loaded at initialization time.

   * The table name prefix is replaced with the prefix in `config.sh` at run time.
   
   * The `hstore` and `multi_geometry` options are enabled.
   
   * Calls are added to `process_way`, `process_relation` and
     `selection_members` in the 'shieldtables` at the appropriate points
	 in the code.
	 
   * Obviously, this is only a starting point, and must be replaced with
     whatever style that your application requires.
	 
## The initial database load

Once everything above is in place, the initial database may be loaded
with the script `inital-load.sh`. It accepts no parameters (everything is
in the configuration file), and should be executed in a new, empty directory
that will hold the working files.

It will do the following:

   * Fetch the `,poly` file and the OSM data extract.
   * Use `osm2pgsql` to load the database from the extract.
   * Use the `route_graphics.tcl` script in the project to create
     the initial shield images.
   * Set up for replication from `planet.osm.org`.
   * Use the replication state server at `$SEQURL` to find the
     initial replication state, and set up for minutely replication.
	 
## Database update

The `update.sh` script must be executed from the same directory in which
`initial-load.sh` was run. It is save to run it as often as once a minute,
since it will check that it is the only copy running and exit immediately
if it finds another copy. It is intended to be executed from `crontab`
periodically to bring the database up to date with respect to OSM.

It performs the following functions:

   * Run `osmosis` to retrieve a tranche of changes to the database,
     and simplify the change file.
   * Pass the changes through the `trim_osc.py` script to confine them
     to the region of interest.
   * Run `osm2pgsql` to apply the changes to the database.
   * Run the `routeGraphics.tcl` script to create images for any new
     shields that are required.

It does _not_ yet manage a tile expiry, although this is planned eventually.

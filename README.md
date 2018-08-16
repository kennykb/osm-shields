# Custom Highway Shields for OpenStreetMap

This project is yet another attempt at a custom highway shield
rendering, suitable for US-style maps, for OpenStreetMap. 

## Background

This project is largely derived from an 
[earlier effort led by Phil! Gold](https://launchpad.net/osm-shields).
It attempts to layer atop a completely standard import of
OpenStreetMap data resulting from a run of <tt>osm2pgsql</tt>, and to
work with an unmodified Mapnik. These constraints are relatively
severe. They imply that that nothing can modify the 'slim tables' and
import tables even to the extent of creating foreign key constraints
that refer to them (doing so causes deadlocks when running
<tt>osm2pgsql</tt> to update the database). Moreover, they imply that
code executed at rendering time (that is, in response to a database
query from Mapnik) cannot modify the database in any way, because
Mapnik has a read-only connection. Rendering queries therefore may not
use temporary tables, much less cache results in the database. (This
restriction rules out the technique that the earlier project used for creating
shield clusters.)

## Installation

### Prerequisites

As a prerequisite, the user is expected to have a PostgreSQL database
already set up, with 'slim' tables available. The 
[workflow](https://switch2osm.org/manually-building-a-tile-server-18-04-lts/)
described at the 'switch2osm' web site is fairly typical of what is
required. Most people who develop any sort of rendering chain based on
OpenStreetMap have already performed the necessary steps.

Several of the main scripts that maintain the project's database
information are written in the [Tcl](https://core.tcl.tk) programming
language, and therefore its interpreter and PostgreSQL interface must
also be present on the target system. On a Debian-based system such as
Ubuntu, they may be installed with

```shell
sudo apt-get install tcl8.6 tcl8.6-tdbc tcl8.6-tdbc-postgres
```
In addition, Inkscape is required to process SVG files, and
ImageMagick is required in some cases to compose graphics that comprise
multiple signs. It is believed that the following command should
suffice on Debian systems:

```shell
sudo apt-get install imagemagick inkscape
```

Users of other operating systems should consult the local system's
package manager for where to find the required third-party software.

### Configuration

The file, _config.tcl_ must be edited to reflect the local
installation. The comments in the file should be self-explanatory.
Note that the path to the PNG images, _pngDir_, must be accessible to
the Mapnik instance at rendering time, since that is where the
renderer will obtain the shield graphics.

### Populating the database

Once the configuration is done, the system may be set up by typing,

```shell
./make-routetables.tcl --init
```

This command will initialize all the database tables required for
shield rendering at run time, deleting any existing content. (See
**Database Tables** below for an overview of the tables.) Leaving off
the _--init_ flag will reinitialize the tables describing route
relations, but leave any precomputed shield graphics intact.

Once there are tables describing the routes, then the shield graphics
corresponding to the routes have to be created so that the tile
renderer can find them. This can be done with

```shell
./routeGraphics.tcl
```

On the initial run, this might take an hour or two to pass through the
thousands of entries in the database and generate the graphics. On
subsequent runs, it should be quite fast, no more than a few minutes,
because most graphics will already be in place.

Expect that there will be a cascade of messages to the standard error
about networks not being found or references not matching their
networks. Some of these messages are caused by networks for which
properly formatted graphics are not yet available, while others are
simply typos in the OpenStreetMap data. At the end of the run, the
networks that could not be recognized are listed, with the ones having
the most associated routes being listed first.

Once this is done, the database should be set up and ready to go for
rendering.

## Rendering from the database

The installation will create several stored procedures in the database
to aid in identifying where to place highway shields. These procedures
will return ways and multiple columns containing the file names where
corresponding graphics are to be found, suitable for use with a
GroupSymbolizer in Mapnik.

A suitable style for placing shield custers in Mapnik XML might be:

```xml
  <Style name="road-shield">
    <Rule>
      <GroupSymbolizer start-column="1" num-columns="8"
		       placement="line" spacing="200"
		       clip="false">
	<PairLayout/>
	<GroupRule>
	  <Filter>[picture_%] != ''</Filter>
	  <ShieldSymbolizer file="[picture_%]"
	                    fontset-name="book-fonts" size="10" fill="white"
			    >' '</ShieldSymbolizer>
	</GroupRule>
      </GroupSymbolizer>
    </Rule>
  </Style>
```

(The procedures are set up to return eight columns because this is the
maximum number of concurrent signed routes that has yet been
observed. The count is easily changed, since internally there is no
hard limit.)

This style would then be driven from a layer whose definition might
look like:

```xml
   <Layer name="road-shields-16-up"
	   status="on"
	   srs="+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +units=m +k=1.0 +no_defs"
	   maximum-scale-denominator="12500" >
      <StyleName>road-shield</StyleName>
      <Datasource>
    <Parameter name="type">postgis</Parameter>
    <Parameter name="dbname">gis</Parameter>
	<Parameter name="estimate_extent">false</Parameter>
	<Parameter name="extent">
		-9462156.716111112, 3895303.962851664, 
		-7347086.391333333, 6106854.83403499
	</Parameter>
	<Parameter name="srid">3857</Parameter>
	<Parameter name="geometry_field">way</Parameter>
	<Parameter name="geometry_table">planet_osm_line</Parameter>
	<Parameter name="table">
	  (SELECT way, id, picture_1, picture_2, picture_3, picture_4,
	          picture_5, picture_6, picture_7, picture_8
           FROM planet_osm_query_shields_line_all(!bbox!,28))
	   AS pictures
	</Parameter>
      </Datasource>
    </Layer>
```

Here, most of the Parameter objects are just the usual mess required
for database connectivity and geographic projection. Only the _table_
parameter is any different from any other PostGIS layer. In it, a
stored procedure is called to calculate route concurrences and return
a set of ways (with mapping to sets of graphics) for routes in the
current bounding box. Multiple procedures are provided:

 * **PREFIX_query_shields_line_all**
 
   Returns all signed, numbered routes in the **PREFIX_line** table.
   This is suitable only for the highest zoom levels.
   
 * **PREFIX_query_shields_road_all**
 
   Returns all signed, numbered routes in the **PREFIX_road** table.
   This is suitable for medium zoom levels; the **road** table
   attempts to filter out the least significant highways.
   
 * **PREFIX_query_shields_road_major**
 
   Returns all signed, numbered routes in the **PREFIX_road** table
   that are labeled _highway=primary_, _highway=trunk_, or 
   _highway=motorway_. This is suitable for zoom levels out to
   views a few hundred kilometres across
   
 * **PREFIX_query_shields_road_motorway**
 
   Returns shield clusters only for motorways. This would be a
   suitable procedure for rendering at the scale of an entire country.
   
## Keeping up to date

At present, the workflow is set up to update the tables describing
routes all at once as a batch, by rerunning **make-routetables.tcl**
without the _--init_ option. Since this, and a rerun of
**routeGraphics.tcl**, take only a few minutes, this can easily be
slotted in after a daily run of a tool such as **osmosis**. This
workflow is reasonably well suited for users who, for example, run
from extracts posted at GeoFabrik.

Obviously, this process will not scale to a server that is processing
minutely updates of the entire planet. Incremental update of the
tables is, of course, possible. In fact, the developer of the current
tables attempted to automate incremental updates based on SQL triggers
on the **PREFIX_rels** and **PREFIX_ways** tables. This attempt proved
to be unsuccessful because of the way that **osm2pgsql** locks the
database. The first attempt by osm2pgsql to delete a relation
deadlocked with itself.

## Database tables

There are only a few tables needed to support shield clusters. They
record the location of graphics in the file system, and provide the
information needed to find out what shields are associated with a
relation

### PREFIX_shieldroute

The **PREFIX_shieldroute** table has a row for each route relation
that might have a highway shield (hence excluding bus and rail routes,
but including things like hiking and snowmobile routes so that they
can be labeled with OSMC:symbol later).

Column | Meaning
-------|---------
relid  | OSM ID of the route relation
route  | _route=*_ value, such as \'road\', \'hiking\', \'snowmobile\'
network | _network=*_ value, such as **US:I**
ref    | _ref=*_ value

### PREFIX_shieldway

The **PREFIX_shieldway** table has a row for each way that is a member
of a route relation that might have a highway shield. 

Column | Meaning
-------+----------
relid  | OSM ID of the route relation
idx    | Position of the way among the member ways of the route relation
wayid  | OSM ID of the member way
role   | Role of the member way in the relation

A foreign key constraint makes sure that 'relid' of the member pairs
with 'relid' of the relation.

### osm_shield_graphics

This table is not named with the import prefix, because it may be
shared among imports of several regions. It keeps track of the
generated graphics for highway shields

Column   | Meaning
---------|----------
id       | Arbitrary integer to serve as a primary key
route    | Type of route ('road', 'hiking', etc.) being represented
network  | Network (e.g. **US:I**) being represented.
ref      | Reference being represented
size     | Nominal size of the graphic in pixels.
filename | Path name in the file system where the grapnic may be found.

The stored procedures discussed in the previous section function by
joining one of the line tables (**PREFIX_roads** or **PREFIX_ways**)
to **PREFIX_shieldway**, then to **PREFIX_shieldroute** and finally
to **osm_shield_graphics**. This join may well yield multiple results.
The first eight non-NULL ones are used for labeling. (Ordinarily,
there will only be one or two.)


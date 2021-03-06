-- shieldtables.lua
--
-- This module provides the functionality for a 'flex' style in
-- 'osm2pgsql' to produce the tables needed for highway shield rendering.

local shieldtables = {}

-- shieldtables.routes is a table whose keys are the values of the 'route'
-- tag on a relation with 'type=route', and whose values are true if a route
-- of that type might get labeled with a highway shield

shieldtables.routes = {}
for _, r in ipairs({
    'bicycle',
    'foot',
    'hiking',
    'horse',
    'mtb',
    'piste',
    'road',
    'ski',
    'snowmobile',
}) do
   shieldtables.routes[r] = true
end

-- shieldtables.exclude_roles is a pattern that matches the roles of
-- members of route relations. The matching members should never get
-- highway shields

shieldtables.exclude_roles = 'guidepost|link|platform|stop'

local ShieldTablesMethods = {}

-- The 'is_shielded_route' method accepts a route relation and returns
-- true if the relation may require a highway shield, false otherwise.

function ShieldTablesMethods:is_shielded_route(object)
    return object.tags.type == 'route' and
           object.tags.route and
	   object.tags.network and
	   object.tags.ref and
	   shieldtables.routes[object.tags.route]
end

-- The 'is_route_member method accepts a dictionary with keys 'type',
-- 'ref' and 'role', corresponding to a member of a route relation.
-- It returns a true value if and only if the member may need a highway
-- shield.

function ShieldTablesMethods:is_route_member(member)
    return member.type == 'w' and
           ((not member.role) or
            (not string.match(member.role,
                              shieldtables.exclude_roles)))
end

-- The 'process_way' method should be called once for every OSM way
-- encountered. If it is called in stage 2, it populates the 'shieldway'
-- table with the way in question.

function ShieldTablesMethods:process_way(object)

    if osm2pgsql.stage ~= 2 then
        return
    end

    self.waysdone = self.waysdone + 1
    
    if object.tags.highway then
        local d = self.w2routes[object.id]
        if d then

	    -- push the way into the table

	    for relid, e in pairs(d) do
	        for idx, row in pairs(e) do
		    self.shieldway_tbl:add_row({
		        relid = relid,
		        idx = idx,
		        role = row.role,
		    })
	        end
	    end

	    -- report progress
	    
            local now = os.time()
            if self.tick == nil then
                self.tick = now
	        self.starttime = now
            elseif now >= self.tick + 1 then
	        local rate = (0.001 * self.waysdone / (now - self.starttime))
	        io.write(string.format("\rReprocessing: Way(%d %.2fk/s)",
	                 self.waysdone, rate))
	        io.flush()
		self.tick = now
	    end
	end
    end
end

-- The 'process_relation' method should be called once for every OSM
-- relation encountered.  If the relation is a route relation that might
-- have highway shields, then a row is created in the 'shieldroute' table,
-- and 'w2routes' is updated with the information about the relation's
-- members.

function ShieldTablesMethods:process_relation(object)

    if self:is_shielded_route(object) then

        -- create the row in shieldroute

        self.shieldroute_tbl:add_row({
            relid = object.id,
	    route = object.tags.route,
	    network = object.tags.network,
	    ref = object.tags.ref,
	    changed = true,
	})

        for idx, member in ipairs(object.members) do

	    -- go through the route members for ones with appropriate roles

            if self:is_route_member(member) then

		-- record a route member in self.w2routes

		local d = self.w2routes[member.ref]
		if not d then
		    d = {}
		end
		local d2 = d[object.id]
		if not d2 then
		    d2 = {}
		end
                d2[idx] = member.role
		d[object.id] = d2
		self.w2routes[member.ref] = d

            end
        end
    end
end

-- The 'select_relation_members' method, given a relation, returns
-- the ID's of member ways that may require highway shields.
-- It returns nil if the relation is not a route or the set of ways
-- is empty

function ShieldTablesMethods:select_relation_members(object)
    if self:is_shielded_route(object) then
        ways = {}
	for _, m in ipairs(object.members) do
	    if self:is_route_member(m) then
	        ways[#ways + 1] = m.ref
	    end
	end
	return {ways = ways}
    end
end

local shieldtablesMt = { __index = ShieldTablesMethods }

-- The constructor of the 'shieldtables' object accepts the
-- parameter 'prefix', which is a string that should be prepended to
-- each table name. Default is 'planet_osm'.

shieldtables.new = function(prefix)

    prefix = prefix or 'planet_osm'

    local values = {

        osm2pgsql = osm2pgsql,
	-- print = print,

        -- The 'shieldroute' table has one row per route that may be marked
	-- with a shield. The columns are:
	--     relid   - OSM ID of the route relation (primary key)
	--     route   - Value of the 'route' key of the relation
	--     network - Value of the 'network' key of the relation
	--     ref     - Value of the 'ref' key of the relation
	--     changed - 1 if the relation (not its members) has changed
	--               since last being examined. Used to trigger generation
	--               of graphichs files for shields.

        shieldroute_tbl = osm2pgsql.define_table{
	    name =  prefix .. '_shieldroute',
	    ids = { type = 'relation', id_column = 'relid' },
	    columns = {
	        { column = 'route', type = 'text' },
		{ column = 'network', type = 'text' },
		{ column = 'ref', type = 'text' },
		{ column = 'changed', type = 'boolean' },
	    },
	},

	-- The 'shieldway' table has one row per way that is a member of
	-- a route that might be marked with a shield. The columns are:
	--     wayid - OSM ID of the way
	--     relid - OSM ID of the relation
	--     idx   - Ordinal position of the way within the relation
	--     role  - Role of the way within the relation

	shieldway_tbl = osm2pgsql.define_table{
	    name = prefix .. '_shieldway',
	    ids = { type = 'way', id_column = 'wayid' },
	    columns = {
	        { column = 'relid', type = 'int8' },
		{ column = 'idx', type = 'int4' },
	        { column = 'role', type = 'text'},
	    },
	},

	-- w2routes[wayID][relationID][idx] contains the role
	-- for a way 'wayID' appearing in relation 'relationID'
	-- at position 'idx'

	w2routes = {},

        -- waysdone is the number of ways processed in phase 2

	waysdone = 0

    }

    retval = setmetatable(values, shieldtablesMt)
    return retval

end

return shieldtables

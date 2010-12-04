-define(ZIP_FORMATS, [ "#####", "#####-####" ]).

-define(STATES,
    [ "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
    "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho",
    "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
    "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
    "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",
    "New Hampshire", "New Jersey", "New Mexico", "New York",
    "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon",
    "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
    "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
    "West Virginia", "Wisconsin", "Wyoming" ]).

-define(STATES_ABBR,
    [ "AL", "AK", "AS", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FM", "FL",
    "GA", "GU", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MH",
    "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM",
    "NY", "NC", "ND", "MP", "OH", "OK", "OR", "PW", "PA", "PR", "RI", "SC",
    "SD", "TN", "TX", "UT", "VT", "VI", "VA", "WA", "WV", "WI", "WY", "AE",
    "AA", "AP" ]).

-define(COMPASS_DIRECTIONS, [ "North", "East",  "West", "South" ]).

-define(CITY_PREFIXES, ?COMPASS_DIRECTIONS ++ [ "New", "Lake", "Port" ]).

-define(CITY_SUFFIXES,
    [ "town", "ton", "land", "ville", "berg", "burgh", "borough", "bury",
    "view", "port", "mouth", "stad", "furt", "chester", "mouth", "fort",
    "haven", "side", "shire" ]).

-define(STREET_SUFFIX,
    [ "Avenue", "Court", "Lane", "Drive", "Parkway", "Road", "Street", "Way" ]).

-define(STREET_SUFFIX_EX,
    [ "Alley", "Branch", "Bridge", "Brook", "Brooks", "Burg", "Burgs",
    "Bypass", "Camp", "Canyon", "Cape", "Causeway", "Center", "Centers",
    "Circle", "Circles", "Cliff", "Cliffs", "Club", "Common", "Corner",
    "Corners", "Course", "Courts", "Cove", "Coves", "Creek", "Crescent",
    "Crest", "Crossing", "Crossroad", "Curve", "Dale", "Dam", "Divide",
    "Drives", "Estate", "Estates", "Expressway", "Extension", "Extensions",
    "Fall", "Falls", "Ferry", "Field", "Fields", "Flat", "Flats", "Ford",
    "Fords", "Forest", "Forge", "Forges", "Fork", "Forks", "Fort", "Freeway",
    "Garden", "Gardens", "Gateway", "Glen", "Glens", "Green", "Greens",
    "Grove", "Groves", "Harbor", "Harbors", "Haven", "Heights", "Highway",
    "Hill", "Hills", "Hollow", "Inlet", "Island", "Islands", "Isle",
    "Junction", "Junctions", "Key", "Keys", "Knoll", "Knolls", "Lake",
    "Lakes", "Land", "Landing", "Light", "Lights", "Loaf", "Lock", "Locks",
    "Lodge", "Loop", "Mall", "Manor", "Manors", "Meadow", "Meadows", "Mews",
    "Mill", "Mills", "Mission", "Motorway", "Mount", "Mountain", "Mountains",
    "Neck", "Orchard", "Oval", "Overpass", "Park", "Parks", "Parkways", "Pass",
    "Passage", "Path", "Pike", "Pine", "Pines", "Place", "Plain", "Plains",
    "Plaza", "Point", "Points", "Port", "Ports", "Prairie", "Radial", "Ramp",
    "Ranch", "Rapid", "Rapids", "Rest", "Ridge", "Ridges", "River", "Roads",
    "Route", "Row", "Rue", "Run", "Shoal", "Shoals", "Shore", "Shores",
    "Skyway", "Spring", "Springs", "Spur", "Spurs", "Square", "Squares",
    "Station", "Stravenue", "Stream", "Streets", "Summit", "Terrace",
    "Throughway", "Trace", "Track", "Trafficway", "Trail", "Tunnel",
    "Turnpike", "Underpass", "Union", "Unions", "Valley", "Valleys", "Via",
    "Viaduct", "View", "Views", "Village", "Villages", "Ville", "Vista",
    "Walk", "Walks", "Wall", "Ways", "Well", "Wells" ]).

-define(SUITE, [ "Apt. ###", "Suite ###" ]).

-define(UK_COUNTY,
    [ "Avon", "Bedfordshire", "Berkshire", "Borders", "Buckinghamshire",
    "Cambridgeshire", "Central", "Cheshire", "Cleveland", "Clwyd", "Cornwall",
    "County Antrim", "County Armagh", "County Down", "County Fermanagh",
    "County Londonderry", "County Tyrone", "Cumbria", "Derbyshire", "Devon",
    "Dorset", "Dumfries and Galloway", "Durham", "Dyfed", "East Sussex",
    "Essex", "Fife", "Gloucestershire", "Grampian", "Greater Manchester",
    "Gwent", "Gwynedd County", "Hampshire", "Herefordshire", "Hertfordshire",
    "Highlands and Islands", "Humberside", "Isle of Wight", "Kent", "Lancashire",
    "Leicestershire", "Lincolnshire", "Lothian", "Merseyside", "Mid Glamorgan",
    "Norfolk", "North Yorkshire", "Northamptonshire", "Northumberland",
    "Nottinghamshire", "Oxfordshire", "Powys", "Rutland", "Shropshire",
    "Somerset", "South Glamorgan", "South Yorkshire", "Staffordshire",
    "Strathclyde", "Suffolk", "Surrey", "Tayside", "Tyne and Wear",
    "Warwickshire", "West Glamorgan", "West Midlands", "West Sussex",
    "West Yorkshire", "Wiltshire", "Worcestershire" ]).

-define(UK_COUNTRY, [ "England", "Scotland", "Wales", "Northern Ireland" ]).

-define(UK_POSTCODE, [ "??# #??", "??## #??" ]).

-define(NEIGHBORHOOD, 
    [ "East of Telegraph Road", "North Norridge",
    "Northwest Midlothian/Midlothian Country Club",
    "Mott Haven/Port Morris", "Kingsbridge Heights", "Bronxdale", "Pennypack",
    "Bridesburg", "Allegheny West", "Bushwick South", "Dyker Heights",
    "Ocean Parkway South", "Summerlin North", "Seven Hills Area",
    "Greater Las Vegas National", "Phoenix", "Central Chandler",
    "South of Bell Road", "River Heights", "White Plains Central",
    "Mount Kisco West", "Pound Ridge East", "Babylon Bayside",
    "Sagaponack Seaside", "South of Lake Ave", "Far Rockaway/Bayswater",
    "Jamaica Estates/Holliswood", "Murray Hill", "East Renton", "Renton West",
    "Auburn North", "Northwoods West", "Florissant West", "Ladue South",
    "Candlewood Country Club", "West Covina East", "North East Irwindale",
    "Sunshine-Gardens", "Cipriani", "Brentwood Central", "Jupiter South/Abacoa",
    "Sea Ranch Lakes", "Schall Circle/Lakeside Green", "Olmsted Falls Central",
    "South of Lake Shore Blvd", "Gates Mills North",
    "White Oak South of Columbia Pike", "Rockville East of Hungerford Dr",
    "Cleveland Park" ]).


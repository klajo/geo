-module(geo_wgs84).

-define(GREAT_CIRCLE_EARTH_RADIUS, 6372.8). % km

-export([great_circle_distance/2]).

%% http://en.wikipedia.org/wiki/Great-circle_distance
great_circle_distance({DegLat0, DegLon0}, {DegLat1, DegLon1}) ->
    Lat0 = deg_to_rad(DegLat0),
    Lon0 = deg_to_rad(DegLon0),
    Lat1 = deg_to_rad(DegLat1),
    Lon1 = deg_to_rad(DegLon1),
    DeltaSigma = math:acos(
                   math:sin(Lat0) * math:sin(Lat1)
                   + math:cos(Lat0) * math:cos(Lat1) * math:cos(Lon1 - Lon0)),
    ?GREAT_CIRCLE_EARTH_RADIUS * DeltaSigma * 1000. % meters

deg_to_rad(A) ->
    A * math:pi() / 180.

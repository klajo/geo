-ifndef(GEO_HRL).
-define(GEO_HRL, true).
-record(trkpt,
        {lat,    % latitude in decimal degrees
         lon,    % longitude in decimal degrees
         time,   % UTC timestamp
         ele     % elevation in meters
        }). 
-endif.

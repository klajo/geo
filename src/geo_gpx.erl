-module(geo_gpx).

-export([parse_file/1]).

-include_lib("xmerl/include/xmerl.hrl").
-include("../include/geo.hrl").

parse_file(Filename) ->
    {Xml, _Rest} = xmerl_scan:file(Filename),
    TrkPts = xmerl_xpath:string("/gpx/trk/trkseg/trkpt", Xml),
    lists:map(fun parse_trkpt/1, TrkPts).

parse_trkpt(TrkPt) ->
    [#xmlAttribute{value=Lat}] = xmerl_xpath:string("//@lat", TrkPt),
    [#xmlAttribute{value=Lon}] = xmerl_xpath:string("//@lon", TrkPt),
    [#xmlText{value=Time}]     = xmerl_xpath:string("//time/text()", TrkPt),
    [#xmlText{value=Elev}]     = xmerl_xpath:string("//ele/text()", TrkPt),
    #trkpt{lat=list_to_float(Lat),
           lon=list_to_float(Lon),
           time=time_to_internal(Time),
           ele=list_to_float(Elev)}.

time_to_internal(TimeStr) ->
    %% "2011-07-06T09:25:58Z" ==> 
    case io_lib:fread("~4d-~2d-~2dT~2d:~2d:~2dZ", TimeStr) of
        {ok, [YYYY, MM, DD, H, M, S], _Rest} ->
            {{YYYY, MM, DD}, {H, M, S}};
        _Other ->
            erlang:error({unknown_time_stamp, TimeStr})
    end.
    

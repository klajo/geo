-module(geo_wgs84_tests).

-include_lib("eunit/include/eunit.hrl").

-define(assert_is_approx(Float, Tolerance, Expr),
        (fun() ->
                case begin Expr end of
                    __Val when __Val >= Float - Tolerance,
                               __Val =< Float + Tolerance ->
                        ok;
                    __Val ->
                        erlang:error({value_outside_tolerance,
                                      {expected, Float, plus_minus, Tolerance},
                                      {got, __Val}})
                end
        end)()).

computes_great_circle_distance_between_nashville_and_los_angeles_test() ->
    ?assert_is_approx(2887260, 10,   % within 10 meters
                      geo_wgs84:great_circle_distance({36.12, -86.67},
                                                      {33.94, -118.40})).

computes_great_circle_distance_between_heathrow_and_arlanda_test() ->
    ?assert_is_approx(1462000, 1000, % within 1km
                      geo_wgs84:great_circle_distance({51.477222, -0.461389},
                                                      {59.651667, 17.918333})).

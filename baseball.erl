-module(baseball).
-compile(export_all).

%% Analzye battings
start() ->
    analyze:battings("battings.csv").

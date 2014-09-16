-module(db).
-export([start/0]).

start() ->
    crypto:start(),
    application:start(emysql),
    emysql:add_pool(batmachine_pool, [{size,1},
				 {user, "batter"},
				 {password, "batmachine"},
				 {database, "batmachine"},
				 {encoding, utf8}]).
    

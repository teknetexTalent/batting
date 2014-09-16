-module(import).
-export([create_tables/0]).

create_tables() ->
    %% Open the MySQL database
    db:start(),
    create_players(),
    create_battings(),
    create_temp().

create_temp() ->
    emysql:execute(batmachine_pool, <<"Drop table IF EXISTS temp">>),
    %% Create table for Players
    emysql:execute(batmachine_pool, <<"Create table temp (player_id varchar(40), avg float)">>).

create_players() ->
    io:format("Reading Players ... Stand by"),
    %% Open CSV file with all the players
    {ok, File} = file:open("mastersmall.csv", [read]),
    %% Read over field definitions
    {ok, _Skip} = file:read_line(File),
    %% Drop table for fresh import
    emysql:execute(batmachine_pool, <<"Drop table IF EXISTS players">>),
    %% Create table for Players
    emysql:execute(batmachine_pool, <<"Create table players (player_id varchar(40), birth_year integer, name_first varchar(20), name_last varchar(20), index(player_id))">>),
    %% Read file into database table Players
    read_players(File),
    %% Close CSV file
    file:close(File),
    io:format(" Done reading players.~n").

read_players(File) ->
    case file:read_line(File) of
	{ok, Data} ->
	    %% Remove \n from it
	    New = string:left(Data, string:len(Data) -1),
	    [PlayerId, BirthYear, Lastname, Firstname] = string:tokens(New, ","),
	    emysql:prepare(insert_stmt, <<"Insert into players values (?,?,?,?)">>),
	    emysql:execute(batmachine_pool, insert_stmt, [PlayerId, BirthYear, Lastname, Firstname]),
	    read_players(File);
	{error, Error} ->
	    Error;
	eof ->
	    ok
    end.

create_battings() ->
    io:format("Raeding battings ... Stand by"),
    %% Open CSV file with all the players
    {ok, File} = file:open("battings.csv", [read]),
    %% Read over field definitions
    {ok, _Skip} = file:read_line(File),
    %% Drop table for fresh import
    emysql:execute(batmachine_pool, <<"Drop table IF EXISTS battings">>),
    %% Create table for Players
    emysql:execute(batmachine_pool,
		   <<"Create table battings (
                       player_id varchar(40),
                       year integer,
                       league varchar(40),
		       team varchar(40),
		       g integer,
		       ab integer,
		       r integer,
		       h integer,
		       b2 integer,
		       b3 integer,
		       hr integer,
		       rbi integer,
		       sb integer,
		       cs integer,
		       index(player_id))">>),
    %% Read file into database table Players
    read_battings(File),
    %% Close CSV file
    file:close(File),
    io:format(" Done reading battings.~n").

read_battings(File) ->
    case file:read_line(File) of
	{ok, Data} ->
	    %% Strip \n
	    New = string:left(Data, string:len(Data) - 1),
	    [PlayerId, Year, League, Team, G, AB, R, H, B2, B3, HR, RBI, SB, CS ] = string:tokens(New, ","),

	    emysql:prepare(insert_stmt, <<"Insert into battings values (?,?,?,?,?,?,?,?,?,?,?,?,?,?)">>),
	    emysql:execute(batmachine_pool, insert_stmt, [PlayerId, Year, League,
						     Team, G, AB, R, H, B2,
						     B3, HR, RBI, SB, CS]),
	    read_battings(File);
	{error, Error} ->
	    Error;
	eof ->
	    ok
    end.

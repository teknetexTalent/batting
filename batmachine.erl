%%% This is the Developers Exercise
%%% Date  : Sep 15, 2014
%%% Author: Kai Janson
-module(batmachine).
-compile([export_all]).

start() ->
    %% Start the MySQL database
    db:start(),
    %% Find out if we have all needed tables
    {_,_,_, Result, _} = emysql:execute(batmachine_pool, <<"show tables">>),
    case Result of
	[[<<"battings">>], [<<"players">>],[<<"temp">>]] ->
	    io:format("Needed tables found.~n");
	_Anything ->
	    io:format("Needed tables NOT found, creating them now...~n"),
	    import:create_tables()
    end,
    %% Find the most improved batting average
    most_improved(),
    %% Slugging percentage for all players on Oakland A's for 2007
    slugging(),
    %% Triple crown
    [triple_crown(TheLeague) || TheLeague <- ["AL","NL"]].

%% Computing the batting average
batting_avg(0,0) ->
    0;
batting_avg(_H, 0) ->
    0;
batting_avg(0,_H) ->
    0;
batting_avg(H, Ab) ->
    H / Ab.

%% Year to Year comparison
year_to_year([PlayerId]) ->
    emysql:prepare(sql1, "SELECT * from battings where player_id=? and year=?"),
    emysql:prepare(sql2, "SELECT * from battings where player_id=? and year=?"),
    {_,_,_,Y2009,_} = emysql:execute(batmachine_pool, sql1, [binary_to_list(PlayerId), 2009]),
    {_,_,_,Y2010,_} = emysql:execute(batmachine_pool, sql2, [binary_to_list(PlayerId), 2010]),
    Ratio1 = case Y2009 of
		 [[_,_,_,_,_,AB,_,H,_,_,_,_,_,_]] ->
		     batting_avg(H,AB);
		 _ ->
		     0
	     end,
    Ratio2 = case Y2010 of
		 [[_,_,_,_,_,AB0,_,H0,_,_,_,_,_,_]] ->
		     batting_avg(H0, AB0);
		 _ ->
		     0
	     end,
    case Ratio1 /= 0 andalso Ratio2 /= 0 of
	true ->
	    %% Player has stats in 2009 and 2010, thus store it in temp table
	    emysql:prepare(ratio_stmt, <<"Insert into temp values (?,?)">>),
	    emysql:execute(batmachine_pool, ratio_stmt, [PlayerId, Ratio2 - Ratio1]);
	false ->
	    0
    end.

%% Extracts the player's firat and lastname and creates a "full" name for better display
player_name(PlayerId) ->
    %% Find the player
    emysql:prepare(player_name_stmt, <<"Select name_first, name_last from players where player_id=?">>),
    {_,_,_,[[First, Last]],_} = emysql:execute(batmachine_pool, player_name_stmt, [PlayerId]),
    %% Create the "full" name
    binary_to_list(First) ++ " " ++ binary_to_list(Last).

%% Find the most improved player
most_improved() ->
    io:format("~nMost improved batting average~n"),
    %% Empty temp table
    emysql:execute(batmachine_pool, "Delete from temp"),
    %% Get a list of all players in the battings table
    {_,_,_,All,_} = emysql:execute(batmachine_pool, "Select distinct player_id from battings where ab > 200"),
    %% Then compare them Year to Year
    [year_to_year(X) || X <- All],
    %% Find highest scoring player in temp table...
    {_,_,_,[[PlayerId, Result]],_} = emysql:execute(batmachine_pool, "Select player_id, avg from temp order by avg desc limit 1"),
    %% ... and display
    io:format("Highest: ~p by ~p~n", [Result, player_name(PlayerId)]).

%% Print the slugging average
slugging_avg(PlayerId, Hits, Doubles, Triples, Homeruns, Atbats) ->
    io:format("~.20s: ~.2.2f%~n", [player_name(PlayerId), (((Hits - Doubles - Triples - Homeruns) + (2 * Doubles) + (3 * Triples) + (4 * Homeruns)) / Atbats) *  100]).

%% Compute the slugging
slugging() ->
    io:format("~n~nSlugging:~n"),
    {_,_,_,Slugs,_} = emysql:execute(batmachine_pool, "Select player_id,h,b2,b3,hr,ab from battings where team='OAK' and year=2007"),
    [slugging_avg(Player, Hits, Doubles, Triples, Homeruns, Atbats) || [Player, Hits, Doubles, Triples, Homeruns, Atbats] <- Slugs, Atbats > 0].

%% Find triple crown players
triple_crown(League) ->
    io:format("~n~nTriple Crown:~n"),
    emysql:prepare(topavg_stmt, <<"Select max(h/ab) from battings where league = ? and ab > 400">>),
    emysql:prepare(tophme_stmt, <<"Select max(h)    from battings where league = ? and ab > 400">>),
    emysql:prepare(toprbi_stmt, <<"Select max(rbi)  from battings where league = ? and ab > 400">>),
    emysql:prepare(triple_crown_stmt, <<"Select *, (h/ab) as avg from battings having ab > 400 and avg=? and h=? and rbi=?">>),

    {_,_,_,[[TopBattAvg]],_} = emysql:execute(batmachine_pool, topavg_stmt, [League] ),
    {_,_,_,[[TopHomeRuns]],_} = emysql:execute(batmachine_pool,tophme_stmt, [League] ),
    {_,_,_,[[TopRbi]],_} = emysql:execute(batmachine_pool,     toprbi_stmt, [League] ),

    {_,_,_,Winner,_} = emysql:execute(batmachine_pool, triple_crown_stmt, [TopBattAvg, TopHomeRuns, TopRbi]),

    Result = case Winner of
		 [] ->
		     "no winner";
		 Winner ->
		     Winner
	     end,
    io:format("~p~n", [Result]).

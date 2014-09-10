%%% This module takes care of the players and battings lists
%%% Autor: Kai Janson
%%% Date: 09/10/2014
%%% Time-stamp: <2014-09-10 15:44:36 kai.janson>
-module(analyze).
-export([battings/1]).

%% Thi function reads all players into a list for ease of use
players(Filename) ->
    %% Open the file
    {ok, Players} = file:open(Filename, [read]),
    %% Skip over the first line
    {ok, _Skip} = file:read_line(Players),
    %% Read players
    List = read_players(Players, []),
    file:close(Players),
    %% Return the sorted list of players
    lists:sort(List).
    
%% This function reads all battings in a list for ease of use
battings(Filename) ->
    %% Open the file
    {ok, Battings} = file:open(Filename, [read]),
    %% Skip over the first line
    {ok, _Skip} = file:read_line(Battings),
    %% Read battings
    List = read_battings(Battings, []),
    file:close(Battings),
    %% Return filtered results
    lists:sort(filter_battings(List)).

%% File operation on the players
read_players(FileHd, Acc) ->
    %% Read line by line
    case file:read_line(FileHd) of
        %% Read was good, we have data
        {ok, RawLine} ->
            %% Remove \n
            Line = string:left(RawLine, string:len(RawLine) -1),
            %% Split the CSV into variables
            [PlayerId, Year, First,Last] = string:tokens(Line, ","),
            %% Tail recursion through the file with the possible next line,
            %% plus storing the just created list item in the list
            read_players(FileHd, [[PlayerId, Year, First, Last]|Acc]);
        {error, Error} ->
            %% If we get an error, let it crash so we can fix it
            Error;
        eof ->
            Acc
    end.

read_battings(FileHd, Acc) ->
    case file:read_line(FileHd) of
        {ok, RawLine} ->
            Line = string:left(RawLine, string:len(RawLine) - 1),
            [PlayerId,Year,League,Team,G,Ab,R,H,B2,B3,Hr,Rbi,Sb,Cs] = string:tokens(Line, ","),
            read_battings(FileHd, [[PlayerId,Year,League,Team,G,Ab,R,H,B2,B3,Hr,Rbi,Sb,Cs]|Acc]);
        {error, Error} ->
            Error;
        eof ->
            %% We are done building the list, return the list
            Acc
    end.

%% This function checks if there is an entry in the list with a specific year
has(List, Year) ->
    has(List, Year, []).

%% Helperfuntion to create the list
has([], _InYear, Acc) ->
    %% return the list in the correct order
    lists:reverse(Acc);
has([Head|Tail], InYear, Acc) ->
    %% Look for the year only, the rest we ignore here
    [_PlayerId,Year,_League,_Team,_G,_Ab,_R,_H,_B2,_B3,_Hr,_Rbi,_Sb,_Cs] = Head,
    case InYear == Year of
        true ->
            %% Year matches, lets store the entire record as we need it later,
            %% and tail recursion through the list again
            has(Tail, InYear, [Head|Acc]);
        false ->
            %% Year did not match, tail recursion through the list again
            has(Tail, InYear, Acc)
    end.

%% Checks if a given player is the current list 
find_in_list(_What, []) ->
    %% End of list and no match found
    false;
find_in_list(What, [Head|Tail]) ->
    %% Look for the player
    [PlayerId,_Year,_League,_Team,_G,_Ab,_R,_H,_B2,_B3,_Hr,_Rbi,_Sb,_Cs] = Head,
    case PlayerId == What of
        true ->
            %% Player found, return the player
            {ok, Head};
        false ->
            %% Not found (yet), tail recursion through the list
            find_in_list(What, Tail)
    end.

%% This function combines the 2009 and 2010 lists
%% but only if there is an entry in 2009 and 2010
%% for one player.  Otherwise it's not possible
%% to compute a gain.
combine(List1, List2, Players) ->
    combine(List1, List2, [], Players).
combine([], _List2, Acc, _Players) ->
    %% We're done, return the single entry that had the highest gain
    hd(lists:reverse(lists:sort(Acc)));
combine([Head|Tail], List2, Acc, Players) ->
    %% Extracting data into variables
    [PlayerId,_Year,_League,_Team,_G,Ab,_R,H,_B2,_B3,_Hr,_Rbi,_Sb,_Cs] = Head,
    case find_in_list(PlayerId, List2) of
        {ok, Data} ->
            %% We found an entry in the 2010 list, too
            %% That means we can make a comparison of the two items
            %% later on
            [_PlayerId1,_Year1,_League1,_Team1,_G1,Ab1,_R1,H1,_B21,_B31,_Hr1,_Rbi1,_Sb1,_Cs1] = Data,
            case list_to_integer(Ab) > 0 andalso list_to_integer(H) > 0
                andalso list_to_integer(Ab1) > 0 andalso list_to_integer(H1) > 0 of
                true ->
                    %% We have a player in 2009 and 2010, so lets run numbers on that player
                    Old = list_to_integer(H) / list_to_integer(Ab),
                    New = list_to_integer(H1) / list_to_integer(Ab1),
                    Gain = New - Old,
                    %% Let's see if we have a Player in the player list
                    %% that matches the current one    
                    Demographics = [ X || X <- Players, lists:member(PlayerId, X)],
                    %% Store the results and tail recursion through the rest
                    combine(Tail, List2, [
                                          [
                                           {gain, Gain},
                                           {playerId, PlayerId},
                                           {old_ratio, Old},
                                           {new_ratio, New},
                                           {player, Demographics}
                                          ]| Acc], Players);
                false ->
                    %% We have invalid data (0's in divisions); keep on going
                    combine(Tail, List2, Acc, Players)
            end;
        _ ->
            %% No match, keep on searching by tail recursion
            combine(Tail, List2, Acc, Players)
    end.

%% This is the real meat of this module
filter_battings(List) ->
    %% Create lists of 2009 and 2010
    L2009 = has(List, "2009"),
    L2010 = has(List, "2010"),
    %% Combine the ones that match players for 2009 and 2010
    Combined = combine(L2009, L2010, players("mastersmall.csv")),
    %% print to STDOUT
    [ io:format("~p~n", [X]) || X <- Combined].

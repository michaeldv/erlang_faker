%% Copyright (c) 2010 Michael Dvorkin
%% 
%% Faker library is freely distributable under the terms of MIT license.
%% See LICENSE file or http://www.opensource.org/licenses/mit-license.php

-module(faker).
-export([name/0, first_name/0, last_name/0, prefix/0, suffix/0]).

-include("names.hrl").

name() ->
    case random:uniform(10) of
        1 -> prefix() ++ " " ++ first_name() ++ " " ++ last_name();
        2 -> first_name() ++ " " ++ last_name() ++ " " ++ suffix();
        _ -> first_name() ++ " " ++ last_name()
    end.

first_name() ->
    Name = sample(?FIRST_NAMES),
    case is_atom(Name) of
        true  -> atom_to_list(Name);
        false -> Name
    end.

last_name() ->
    Name = sample(?LAST_NAMES),
    case is_atom(Name) of
        true  -> atom_to_list(Name);
        false -> Name
    end.

prefix() ->
    atom_to_list(sample(?PREFIXES)).

suffix() ->
    atom_to_list(sample(?SUFFIXES)).

sample(List) ->
    lists:nth(random:uniform(length(List)), List).

%% Copyright (c) 2010 Michael Dvorkin
%% 
%% Faker library is freely distributable under the terms of MIT license.
%% See LICENSE file or http://www.opensource.org/licenses/mit-license.php
%%---------------------------------------------------------------------------

-module(faker).
-export([name/0, first_name/0, last_name/0, prefix/0, suffix/0]).
-export([words/0, words/1, sentence/0, sentence/1, sentences/0, sentences/1]).

-include("names.hrl").
-include("lorem.hrl").

% faker:name() and family.
%----------------------------------------------------------------------------
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

% faker:words() and family.
%----------------------------------------------------------------------------
words() ->
    words(3, []).

words(Count) ->
    words(Count, []).

words(Count, Acc) ->
    case length(Acc) of
        Count -> Acc;
        _Less -> words(Count, [sample(?WORDS) | Acc])
    end.

sentence() ->
    sentence(4).

sentence(MinWordCount) ->
    Words = words(MinWordCount + random:uniform(7)),
    Sentence = string:join([atom_to_list(W) || W <- Words], " "),
    [string:to_upper(hd(Sentence)) | tl(Sentence)] ++ ".".

sentences() ->
    sentences(3).

sentences(MinSentenceCount) ->
    sentences(MinSentenceCount, []).

sentences(MinSentenceCount, Acc) ->
    case length(Acc) of
        MinSentenceCount -> Acc;
        _Less -> sentences(MinSentenceCount, [sentence() | Acc])
    end.

% Private methods.
%----------------------------------------------------------------------------
sample(List) ->
    lists:nth(random:uniform(length(List)), List).

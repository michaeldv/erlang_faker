%% Copyright (c) 2010 Michael Dvorkin
%% 
%% Faker library is freely distributable under the terms of MIT license.
%% See LICENSE file or http://www.opensource.org/licenses/mit-license.php
%%---------------------------------------------------------------------------

-module(faker).
-export([name/0, first_name/0, last_name/0, prefix/0, suffix/0]).
-export([words/0, words/1, sentence/0, sentence/1, sentences/0, sentences/1, paragraph/0, paragraph/1, paragraphs/0, paragraphs/1]).
-export([us_state/0, us_state_abbr/0, city_prefix/0, city_suffix/0, street_suffix/0, city/0, street_name/0]).

-include("names.hrl").
-include("lorem.hrl").
-include("address.hrl").

% faker:name() and family.
%----------------------------------------------------------------------------
first_name() -> sample(?FIRST_NAMES).
last_name()  -> sample(?LAST_NAMES).
prefix()     -> sample(?PREFIXES).
suffix()     -> sample(?SUFFIXES).

name() ->
    case rand(10) of
        0 -> prefix()     ++ " " ++ first_name() ++ " " ++ last_name();
        1 -> first_name() ++ " " ++ last_name()  ++ " " ++ suffix();
        _ -> first_name() ++ " " ++ last_name()
    end.

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
    Words = words(MinWordCount + rand(6)),
    Sentence = string:join([W || W <- Words], " "),
    [string:to_upper(hd(Sentence)) | tl(Sentence)] ++ ".".

sentences() ->
    sentences(3).

sentences(SentenceCount) ->
    sentences(SentenceCount, []).

sentences(SentenceCount, Acc) ->
    case length(Acc) of
        SentenceCount -> Acc;
        _Less -> sentences(SentenceCount, [sentence() | Acc])
    end.

paragraph() ->
    paragraph(3).

paragraph(MinSentenceCount) ->
    string:join(sentences(MinSentenceCount + rand(3)), " ").

paragraphs() ->
    paragraphs(3).

paragraphs(ParagraphCount) ->
    paragraphs(ParagraphCount, []).

paragraphs(ParagraphCount, Acc) ->
    case length(Acc) of
        ParagraphCount -> Acc;
        _Less -> paragraphs(ParagraphCount, [paragraph() | Acc])
    end.

% faker:address() and family.
%----------------------------------------------------------------------------
us_state()      -> sample(?STATES).
us_state_abbr() -> sample(?STATES_ABBR).
city_prefix()   -> sample(?CITY_PREFIXES).
city_suffix()   -> sample(?CITY_SUFFIXES).
street_suffix() -> sample(?STREET_SUFFIX).

city() ->
    case rand(4) of
        0 -> city_prefix() ++ " " ++ first_name() ++ city_suffix();
        1 -> city_prefix() ++ " " ++ first_name();
        2 -> first_name()  ++ city_suffix();
        _ -> last_name()   ++ city_suffix()
    end.

street_name() ->
    case rand(2) of
        0 -> last_name()  ++ " " ++ street_suffix();
        _ -> first_name() ++ " " ++ street_suffix()
    end.

% Utility methods.
%----------------------------------------------------------------------------
rand(N) ->
    random:uniform(N) - 1.

sample(List) ->
    lists:nth(random:uniform(length(List)), List).

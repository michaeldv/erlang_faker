%% Copyright (c) 2010 Michael Dvorkin
%% 
%% Faker library is freely distributable under the terms of MIT license.
%% See LICENSE file or http://www.opensource.org/licenses/mit-license.php
%%---------------------------------------------------------------------------
-module(faker).
-export([name/0,
         first_name/0,
         last_name/0,
         prefix/0,
         suffix/0]).
-export([words/0, words/1,
         sentence/0, sentence/1,
         sentences/0, sentences/1,
         paragraph/0, paragraph/1,
         paragraphs/0, paragraphs/1]).
-export([zip_code/0,
         us_state/0,
         us_state_abbr/0,
         city_prefix/0,
         city_suffix/0,
         street_suffix/0,
         city/0,
         street_name/0,
         street_address/0, street_address/1,
         address/0]).
-export([uk_county/0,
         uk_country/0,
         uk_postcode/0,
         neighborhood/0]).
-export([email/0,
         email/1,
         free_email/0, free_email/1,
         user_name/0, user_name/1,
         username/0, username/1,
         domain_name/0,
         domain_word/0,
         domain_suffix/0]).

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
zip_code()      -> numerize(sample(?ZIP_FORMATS)).
us_state()      -> sample(?STATES).
us_state_abbr() -> sample(?STATES_ABBR).
city_prefix()   -> sample(?CITY_PREFIXES).
city_suffix()   -> sample(?CITY_SUFFIXES).

street_suffix() ->
    case rand(4) of
        0 -> sample(?STREET_SUFFIX_EX);
        _ -> sample(?STREET_SUFFIX)
    end.

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

street_address() ->
    Format = string:copies("#", rand(3)) ++ "###",
    numerize(Format) ++ " " ++ street_name().

street_address(true) ->
    street_address() ++ ", " ++ numerize(sample(?SUITE));

street_address(false) ->
    street_address().

address() ->
    street_address(rand(9) < 3) ++ "\n" ++
    city() ++ ", " ++ us_state_abbr() ++ " " ++ zip_code().

uk_county()    -> sample(?UK_COUNTY).
uk_country()   -> sample(?UK_COUNTRY).
uk_postcode()  -> alphanumerize(sample(?UK_POSTCODE)).
neighborhood() -> sample(?NEIGHBORHOOD).

% faker:email() and family.
%----------------------------------------------------------------------------
email() -> ok.
email(Name) -> Name.

free_email() -> ok.
free_email(Name) -> Name.

user_name() ->
    case rand(2) of
        0 -> user_name(first_name(), cleanup);
        _ -> user_name(first_name() ++ " " ++ last_name())
    end.

user_name(Name) ->
    Words = re:split(Name, "\s", [{return, list}]),
    Username = string:join(shuffle(Words), sample([".", "_"])),
    user_name(Username, cleanup).

user_name(Name, cleanup) ->
    gsub(string:to_lower(Name), "[^a-zA-Z_.]", "").

% Aliases.
username()     -> user_name().
username(Name) -> user_name(Name).

domain_name() -> ok.
domain_word() -> ok.
domain_suffix() -> ok.


% Utility methods.
%----------------------------------------------------------------------------
rand(N) ->
    random:uniform(N) - 1.

sample(List) ->
    lists:nth(random:uniform(length(List)), List).

numerize(Format) ->
    [ case X of $# -> random:uniform(9) + 48; _ -> X end || X <- Format ].

alphabetize(Format) ->
    [ case X of $? -> random:uniform(26) + 64; _ -> X end || X <- Format ].

alphanumerize(Format) ->
    numerize(alphabetize(Format)).

% sub(String, Pattern, Replacement) ->
%     re:replace(String, Pattern, Replacement, [{return, list}]).

gsub(String, Pattern, Replacement) ->
    re:replace(String, Pattern, Replacement, [global, {return, list}]).

shuffle(List) when length(List) =< 0 ->
    List;

shuffle(List) ->
    WithRandomKey = [{random:uniform(length(List)), X} || X <- List],
    [X || {_, X} <- lists:sort(WithRandomKey)].

%% Copyright (c) 2010 Michael Dvorkin
%% 
%% Faker library is freely distributable under the terms of MIT license.
%% See LICENSE file or http://www.opensource.org/licenses/mit-license.php
%%---------------------------------------------------------------------------
-module(faker).
-export([name/0, first_name/0, last_name/0, prefix/0, suffix/0]).
-export([words/0, words/1, sentence/0, sentence/1, sentences/0, sentences/1,
         paragraph/0, paragraph/1, paragraphs/0, paragraphs/1]).
-export([zip_code/0, us_state/0, us_state_abbr/0, city_prefix/0,
         city_suffix/0, street_suffix/0, city/0, street_name/0,
         street_address/0,street_address/1, address/0]).
-export([uk_county/0, uk_country/0, uk_postcode/0, neighborhood/0]).
-export([company_name/0, catch_phrase/0, bs/0]).
-export([email/0, email/1, free_email/0, free_email/1, user_name/0,
         user_name/1, domain_name/0, domain_word/0, domain_suffix/0]).
-export([phone_number/0, short_phone_number/0]).
-export([company/0, username/0, username/1, domain/0, phone/0]).

-include("address.hrl").
-include("company.hrl").
-include("lorem.hrl").
-include("names.hrl").

% Names.
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

% Lorem Ipsum.
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

% Addresses.
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

% Enterprisey.
%----------------------------------------------------------------------------
company_name() ->
    case rand(6) of
        0 -> last_name() ++ "-"  ++ last_name();
        1 -> last_name() ++ ", " ++ last_name() ++ " and " ++ last_name();
        _ -> last_name() ++ " "  ++ sample(?COMPANY_SUFFIX)
    end.

catch_phrase() -> % Generate a buzzword-laden catch phrase.
    sample(?CATCH_PRE) ++ " " ++ sample(?CATCH_MID) ++ " " ++ sample(?CATCH_POS).

bs() -> % When a straight answer won't do, faker:bs() to the rescue!
    sample(?BS_PRE) ++ " " ++ sample(?BS_MID) ++ " " ++ sample(?BS_POS).

% Internet.
%----------------------------------------------------------------------------
email() ->
    user_name() ++ "@" ++ domain_name().

email(Name) ->
    user_name(Name) ++ "@" ++ domain_name().

free_email() ->
    user_name() ++ "@" ++ sample(?FREE_EMAIL).

free_email(Name) ->
    user_name(Name) ++ "@" ++ sample(?FREE_EMAIL).

user_name() ->
    case rand(2) of
        0 -> normalize(first_name());
        _ -> user_name(first_name() ++ " " ++ last_name())
    end.

user_name(Name) ->
    Words = re:split(Name, "\s", [{return, list}]),
    Username = string:join(shuffle(Words), sample([".", "_"])),
    normalize(Username).

domain_name() ->
    domain_word() ++ "." ++ domain_suffix().

domain_word() ->
    {Name, _} = lists:splitwith(fun(X) -> X /= 32 end, company_name()),
    normalize(Name).

domain_suffix() ->
    sample(?DOMAIN_SUFFIXES).

% Phone numbers.
%----------------------------------------------------------------------------
phone_number() ->
    Format = case rand(20) of
    0  -> "###-###-#### x#####";
    1  -> "###-###-#### x####";
    2  -> "###-###-#### x###";
    3  -> "###-###-####";
    4  -> "###-###-####";
    5  -> "###.###.#### x#####";
    6  -> "###.###.#### x####";
    7  -> "###.###.#### x###";
    8  -> "###.###.####";
    9  -> "###.###.####";
    10 -> "(###)###-#### x#####";
    11 -> "(###)###-#### x####";
    12 -> "(###)###-#### x###";
    13 -> "(###)###-####";
    14 -> "(###)###-####";
    15 -> "1-###-###-#### x#####";
    16 -> "1-###-###-#### x####";
    17 -> "1-###-###-#### x###";
    _  -> "1-###-###-####"
    end,
    numerize(Format).

short_phone_number() ->
    numerize("###-###-####").

% Aliases.
company() -> company_name().
username() -> user_name().
username(Name) -> user_name(Name).
domain() -> domain_name().
phone() -> phone_number().

% Private utility methods.
%----------------------------------------------------------------------------
rand(N) ->
    random:uniform(N) - 1.

sample(List) ->
    lists:nth(random:uniform(length(List)), List).

numerize(Format) -> % No 0s: avoid leading 0 in phones numbers and addresses.
    [ case X of $# -> $1 + rand(9); _ -> X end || X <- Format ].

alphabetize(Format) ->
    [ case X of $? -> $A + rand(26); _ -> X end || X <- Format ].

alphanumerize(Format) ->
    numerize(alphabetize(Format)).

normalize(String) ->
    gsub(string:to_lower(String), "[^a-z_.]", "").

gsub(String, Regex, Replacement) ->
    re:replace(String, Regex, Replacement, [global, {return, list}]).

shuffle(List) when length(List) =< 1 ->
    List;

shuffle(List) ->
    WithRandomKey = [{random:uniform(), X} || X <- List],
    [X || {_, X} <- lists:sort(WithRandomKey)].

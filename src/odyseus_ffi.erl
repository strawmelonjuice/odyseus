-module(odyseus_ffi).
-export([do_unescape/1]).

entities() ->
    #{
        <<"gt">> => <<">">>,
        <<"lt">> => <<"<">>,
        <<"quot">> => <<"\"">>,
        <<"apos">> => <<"'">>,
        <<"amp">> => <<"&">>,
        <<"copy">> => <<"©"/utf8>>,
        <<"reg">> => <<"®"/utf8>>,
        <<"euro">> => <<"€"/utf8>>,
        <<"pound">> => <<"£"/utf8>>,
        <<"cent">> => <<"¢"/utf8>>,
        <<"deg">> => <<"°"/utf8>>,
        <<"bull">> => <<"•"/utf8>>,
        <<"middot">> => <<"·"/utf8>>,
        <<"ndash">> => <<"–"/utf8>>,
        <<"mdash">> => <<"—"/utf8>>,
        <<"lsquo">> => <<"'"/utf8>>,
        <<"rsquo">> => <<"'"/utf8>>,
        <<"sbquo">> => <<"‚"/utf8>>,
        <<"ldquo">> => <<"\"">>,
        <<"rdquo">> => <<"\"">>,
        <<"bdquo">> => <<"„"/utf8>>,
        <<"hellip">> => <<"…"/utf8>>,
        <<"trade">> => <<"™"/utf8>>,
        <<"nbsp">> => <<" ">>
    }.

do_unescape(String) when is_list(String) ->
    do_unescape(unicode:characters_to_binary(String));
do_unescape(String) when is_binary(String) ->
    Entities = entities(),
    unescape_all(String, <<>>, Entities).

%% Main loop
unescape_all(<<>>, Acc, _) -> 
    Acc;
unescape_all(<<"&#x", Rest/binary>>, Acc, Entities) ->
    case parse_hex_entity(Rest) of
        {ok, Char, NewRest} -> unescape_all(NewRest, <<Acc/binary, Char/binary>>, Entities);
        error -> unescape_all(Rest, <<Acc/binary, "&#x">>, Entities)
    end;
unescape_all(<<"&#", Rest/binary>>, Acc, Entities) ->
    case parse_decimal_entity(Rest) of
        {ok, Char, NewRest} -> unescape_all(NewRest, <<Acc/binary, Char/binary>>, Entities);
        error -> unescape_all(Rest, <<Acc/binary, "&#">>, Entities)
    end;
unescape_all(<<"&", Rest/binary>>, Acc, Entities) ->
    case parse_named_entity(Rest) of
        {ok, Name, NewRest} ->
            case maps:get(Name, Entities, not_found) of
                not_found -> unescape_all(NewRest, <<Acc/binary, "&", Name/binary, ";">>, Entities);
                Value -> unescape_all(NewRest, <<Acc/binary, Value/binary>>, Entities)
            end;
        error ->
            unescape_all(Rest, <<Acc/binary, "&">>, Entities)
    end;
unescape_all(<<C/utf8, Rest/binary>>, Acc, Entities) ->
    unescape_all(Rest, <<Acc/binary, C/utf8>>, Entities).

parse_hex_entity(Bin) ->
    case read_until_semicolon(Bin, <<>>, fun is_hex/1) of
        {ok, Hex, Rest} ->
            try
                Val = binary_to_integer(Hex, 16),
                case Val < 16#D800 orelse (Val > 16#DFFF andalso Val =< 16#10FFFF) of
                    true -> {ok, <<Val/utf8>>, Rest};
                    false -> error
                end
            catch
                _:_ -> error
            end;
        error -> error
    end.

parse_decimal_entity(Bin) ->
    case read_until_semicolon(Bin, <<>>, fun is_digit/1) of
        {ok, Num, Rest} ->
            try
                Val = binary_to_integer(Num, 10),
                case Val < 16#D800 orelse (Val > 16#DFFF andalso Val =< 16#10FFFF) of
                    true -> {ok, <<Val/utf8>>, Rest};
                    false -> error
                end
            catch
                _:_ -> error
            end;
        error -> error
    end.

parse_named_entity(Bin) ->
    case read_until_semicolon(Bin, <<>>, fun is_namechar/1) of
        {ok, Name, Rest} -> {ok, string:lowercase(Name), Rest};
        error -> error
    end.

read_until_semicolon(<<";", Rest/binary>>, Acc, _) when byte_size(Acc) > 0 ->
    {ok, Acc, Rest};
read_until_semicolon(<<C, Rest/binary>>, Acc, ValidFun) ->
    case ValidFun(C) of
        true -> read_until_semicolon(Rest, <<Acc/binary, C>>, ValidFun);
        false -> error
    end;
read_until_semicolon(_, _, _) ->
    error.

is_hex(C) ->
    (C >= $0 andalso C =< $9) orelse
    (C >= $a andalso C =< $f) orelse
    (C >= $A andalso C =< $F).

is_digit(C) ->
    C >= $0 andalso C =< $9.

is_namechar(C) ->
    (C >= $a andalso C =< $z) orelse
    (C >= $A andalso C =< $Z) orelse
    (C >= $0 andalso C =< $9).

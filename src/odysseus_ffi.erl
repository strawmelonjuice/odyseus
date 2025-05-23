-module(odysseus_ffi).
-export([do_unescape/1]).

entities() ->
    #{
        <<"gt">> => <<">">>, <<"lt">> => <<"<">>,
        <<"quot">> => <<"\"">>, <<"apos">> => <<"'">>, <<"amp">> => <<"&">>,
        <<"ldquo">> => <<"\"">>, <<"rdquo">> => <<"\"">>,
        <<"copy">> => <<"©"/utf8>>, <<"reg">> => <<"®"/utf8>>,
        <<"euro">> => <<"€"/utf8>>, <<"pound">> => <<"£"/utf8>>,
        <<"cent">> => <<"¢"/utf8>>, <<"deg">> => <<"°"/utf8>>,
        <<"bull">> => <<"•"/utf8>>, <<"middot">> => <<"·"/utf8>>,
        <<"ndash">> => <<"–"/utf8>>, <<"mdash">> => <<"—"/utf8>>,
        <<"lsquo">> => <<"'"/utf8>>, <<"rsquo">> => <<"'"/utf8>>,
        <<"sbquo">> => <<"‚"/utf8>>, <<"bdquo">> => <<"„"/utf8>>,
        <<"hellip">> => <<"…"/utf8>>, <<"trade">> => <<"™"/utf8>>,
        <<"nbsp">> => <<" ">>
    }.

do_unescape(String) when is_list(String) -> do_unescape(unicode:characters_to_binary(String));
do_unescape(String) when is_binary(String) -> unescape_all(String, <<>>, entities()).

unescape_all(<<>>, Acc, _) -> Acc;
unescape_all(<<"&#x", Rest/binary>>, Acc, E) -> parse_and_unescape(Rest, Acc, E, 16);
unescape_all(<<"&#", Rest/binary>>, Acc, E) -> parse_and_unescape(Rest, Acc, E, 10);
unescape_all(<<"&", Rest/binary>>, Acc, E) ->
    case read_until_semicolon(Rest, <<>>, fun is_namechar/1) of
        {ok, Name, NewRest} ->
            case maps:get(string:lowercase(Name), E, not_found) of
                not_found -> unescape_all(NewRest, <<Acc/binary, "&", Name/binary, ";">>, E);
                Value -> unescape_all(NewRest, <<Acc/binary, Value/binary>>, E)
            end;
        error -> unescape_all(Rest, <<Acc/binary, "&">>, E)
    end;
unescape_all(<<C/utf8, Rest/binary>>, Acc, E) -> unescape_all(Rest, <<Acc/binary, C/utf8>>, E).

parse_and_unescape(Bin, Acc, E, Base) ->
    ValidFun = if Base =:= 16 -> fun is_hex/1; true -> fun is_digit/1 end,
    Prefix = if Base =:= 16 -> <<"&#x">>; true -> <<"&#">> end,
    case read_until_semicolon(Bin, <<>>, ValidFun) of
        {ok, Val, Rest} when byte_size(Val) > 0 ->
            try
                Num = binary_to_integer(Val, Base),
                case Num < 16#D800 orelse (Num > 16#DFFF andalso Num =< 16#10FFFF) of
                    true -> unescape_all(Rest, <<Acc/binary, Num/utf8>>, E);
                    false -> unescape_all(Rest, <<Acc/binary, Prefix/binary, Val/binary, ";">>, E)
                end
            catch 
                error:_ -> unescape_all(Rest, <<Acc/binary, Prefix/binary, Val/binary, ";">>, E)
            end;
        _ -> unescape_all(Bin, <<Acc/binary, Prefix/binary>>, E)
    end.

read_until_semicolon(<<";", Rest/binary>>, Acc, _) when byte_size(Acc) > 0 -> {ok, Acc, Rest};
read_until_semicolon(<<C, Rest/binary>>, Acc, ValidFun) ->
    case ValidFun(C) of true -> read_until_semicolon(Rest, <<Acc/binary, C>>, ValidFun); false -> error end;
read_until_semicolon(_, _, _) -> error.

is_hex(C) -> (C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $f) orelse (C >= $A andalso C =< $F).
is_digit(C) -> C >= $0 andalso C =< $9.
is_namechar(C) -> (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z) orelse (C >= $0 andalso C =< $9).

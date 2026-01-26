%% JWT HS256 validation (signature + exp) using a shared secret.
%% Requires the jsx library for JSON decoding.

-module(jwt_validator).

-export([validate/2, validate_bearer/2]).

%% Validate a JWT token string or binary using the shared Secret.
%% Returns {ok, ClaimsMap} or {error, Reason}.
-spec validate(binary() | list(), binary() | list()) -> {ok, map()} | {error, atom()}.
validate(Token, Secret) ->
    ensure_crypto(),
    TokenBin = to_bin(Token),
    SecretBin = to_bin(Secret),
    case split_jwt(TokenBin) of
        {ok, Header64, Payload64, Sig64} ->
            SigningInput = <<Header64/binary, $., Payload64/binary>>,
            case verify_hs256(SigningInput, Sig64, SecretBin) of
                ok ->
                    case decode_json(Payload64) of
                        {ok, Claims} ->
                            case check_exp(Claims) of
                                ok -> {ok, Claims};
                                {error, _} = Err -> Err
                            end;
                        {error, _} = Err -> Err
                    end;
                {error, _} = Err -> Err
            end;
        {error, _} = Err -> Err
    end.

%% Validate a Bearer Authorization header ("Bearer <token>").
-spec validate_bearer(binary() | list(), binary() | list()) -> {ok, map()} | {error, atom()}.
validate_bearer(AuthHeader, Secret) ->
    HeaderBin = to_bin(AuthHeader),
    case HeaderBin of
        <<"Bearer ", Token/binary>> -> validate(Token, Secret);
        _ -> {error, invalid_authorization_header}
    end.

%% Internal helpers

ensure_crypto() ->
    _ = application:ensure_all_started(crypto),
    ok.

split_jwt(TokenBin) ->
    case binary:split(TokenBin, <<".">>, [global]) of
        [Header64, Payload64, Sig64] -> {ok, Header64, Payload64, Sig64};
        _ -> {error, invalid_token_format}
    end.

verify_hs256(SigningInput, Sig64, Secret) ->
    case base64url_decode(Sig64) of
        {ok, SigBin} ->
            Expected = crypto:mac(hmac, sha256, Secret, SigningInput),
            case timing_safe_eq(SigBin, Expected) of
                true -> ok;
                false -> {error, invalid_signature}
            end;
        {error, _} -> {error, invalid_signature_encoding}
    end.

check_exp(Claims) when is_map(Claims) ->
    case maps:get(<<"exp">>, Claims, undefined) of
        undefined -> ok;
        Exp when is_integer(Exp) ->
            Now = erlang:system_time(second),
            if Exp > Now -> ok; true -> {error, token_expired} end;
        Exp when is_float(Exp) ->
            Now = erlang:system_time(second),
            if Exp > Now -> ok; true -> {error, token_expired} end;
        _ -> {error, invalid_exp_claim}
    end.

base64url_decode(Data) ->
    try
        Base64 = base64url_to_base64(Data),
        {ok, base64:decode(Base64)}
    catch
        _:_ -> {error, invalid_base64}
    end.

base64url_to_base64(Data) ->
    Bin = to_bin(Data),
    Replaced = binary:replace(binary:replace(Bin, <<"-">>, <<"+">>, [global]), <<"_">>, <<"/">>, [global]),
    pad_base64(Replaced).

pad_base64(Bin) ->
    case (byte_size(Bin) rem 4) of
        0 -> Bin;
        2 -> <<Bin/binary, "==">>;
        3 -> <<Bin/binary, "=">>;
        _ -> Bin
    end.

decode_json(Payload64) ->
    case base64url_decode(Payload64) of
        {ok, JsonBin} ->
            try
                {ok, jsx:decode(JsonBin, [return_maps])}
            catch
                _:_ -> {error, invalid_json}
            end;
        {error, _} = Err -> Err
    end.

timing_safe_eq(A, B) when is_binary(A), is_binary(B) ->
    if byte_size(A) =/= byte_size(B) -> false;
       true -> timing_safe_eq_loop(A, B, 0)
    end.

timing_safe_eq_loop(<<>>, <<>>, Acc) ->
    Acc =:= 0;

timing_safe_eq_loop(<<A, RestA/binary>>, <<B, RestB/binary>>, Acc) ->
    timing_safe_eq_loop(RestA, RestB, Acc bor (A bxor B)).

to_bin(V) when is_binary(V) -> V;
to_bin(V) when is_list(V) -> list_to_binary(V).

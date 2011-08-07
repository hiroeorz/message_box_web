%%
%% @author HIROE Shin <twitter: http://twitter.com/#!/hiroe_orz17>
%% @doc MessageBox is Twitter clone using Erlang (for plactice).
%% @copyright 2011 HIROE Shin
%%
-module(message_box_json).
-include_lib("eunit/include/eunit.hrl").
-include("../deps/message_box/src/message.hrl").
-include("../deps/message_box/src/user.hrl").

-export([encode_result/1, encode_timeline/1, encode_auth_result/3]).

%%
%% @doc create json stirng for follow result.
%%
-spec(encode_result(tuple()) -> string() ).

encode_result({Key, Value}) when is_integer(Value) ->
    Encoder = mochijson2:encoder([{utf8, true}]),
    Encoder({struct, [{atom_to_list(Key), 
                       list_to_binary(integer_to_list(Value))}]});

encode_result({Key, Value}) when is_atom(Value) ->
    Encoder = mochijson2:encoder([{utf8, true}]),
    Encoder({struct, [{atom_to_list(Key), 
                       list_to_binary(atom_to_list(Value)) }] }).

%%
%% @doc create record for parsing mochijson2 from timeline list.
%%
-spec(encode_timeline(list(#message{})) -> string() ).

encode_timeline(TimelineList) when is_list(TimelineList)->
    encode_timeline(TimelineList, []).

encode_timeline([], List4Json) ->
    SortedList = lists:reverse(List4Json),
    Encoder = mochijson2:encoder([{utf8, true}]),
    Encoder(SortedList);

encode_timeline(TimelineList, List4Json) ->
    [Message | Tail] = TimelineList,
    {{Y, M, D}, {H, Min, S}} = Message#message.datetime,
    DateTimeStr = lists:flatten(
                    io_lib:format(?DatetimeFormat, [Y, M, D, H, Min, S])),

    User = Message#message.user,
    UserStruct = create_user_struct(User),

    Record4Json = 
        {struct, [{"message_id", Message#message.message_id},
                  {"text", list_to_binary(Message#message.text)},
                  {"datetime", list_to_binary(DateTimeStr)},
                  {"user", UserStruct}]},

    encode_timeline(Tail, [Record4Json | List4Json]).

%%
%% @doc create user data as json.
%%
-spec(encode_auth_result(atom(), #user{}, string()) -> string() ).

encode_auth_result(Result, User, OneTimePassword) ->
    Pass = binary_to_list(OneTimePassword),
    io:format("~s~n", [Pass]),

    Record = {struct, [{"result", atom_to_binary(Result)},
                       {"user", create_user_struct(User)}]},
    Encoder = mochijson2:encoder([{utf8, true}]),
    Encoder(Record).

%%
%% @doc change type atom -> binary.
%%
-spec(atom_to_binary(atom()) -> binary() ).

atom_to_binary(Atom) when is_atom(Atom) ->
    list_to_binary(atom_to_list(Atom)).

%%
%% @doc create record for parsing mochijson2 from timeline list.
%%
-spec(create_user_struct(#user{}) -> {struct, list(tuple())} ).

create_user_struct(User) ->
    UserName = list_to_binary(atom_to_list(User#user.name)),
    {struct, [{"id", User#user.id},
              {"name", UserName}]}.

    

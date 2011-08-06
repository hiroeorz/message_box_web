%%
%% @author HIROE Shin <twitter: http://twitter.com/#!/hiroe_orz17>
%% @doc MessageBox is Twitter clone using Erlang (for plactice).
%% @copyright 2011 HIROE Shin
%%
-module(message_box_json).
-include_lib("eunit/include/eunit.hrl").
-include("../deps/message_box/src/message.hrl").
-include("../deps/message_box/src/user.hrl").

-export([encode_timeline/1]).

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
%% @doc create record for parsing mochijson2 from timeline list.
%%
-spec(create_user_struct(#user{}) -> {struct, list(tuple())} ).

create_user_struct(User) ->
    UserName = list_to_binary(atom_to_list(User#user.name)),
    {struct, [{"id", User#user.id},
              {"name", UserName}]}.

    

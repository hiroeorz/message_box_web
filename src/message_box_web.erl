-module(message_box_web).
-include("../deps/message_box/src/message.hrl").
-include("../deps/message_box/src/user.hrl").
-export([start/0, stop/0]).
-export([check_logged_in/1, sleep/1, get_one_time_password/1, parse_timeline/1]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the message_box_web server.
start() ->
    message_box_config:load(),
    message_box_web_deps:ensure(),
    ensure_started(crypto),
    application:start(message_box_web).

%% @spec stop() -> ok
%% @doc Stop the message_box_web server.
stop() ->
    Res = application:stop(message_box_web),
    application:stop(crypto),
    Res.

check_logged_in(Env) ->
    case beepbeep_args:get_session_data("one_time_password", Env) of
	undefined ->
	    error_logger:info_report("Unauthenticated access!~n"),
	    {redirect, "/user/login"};
	_A ->
	    ok
    end.

sleep(Msec) when is_integer(Msec) ->
    receive
    after Msec -> ok
    end.

get_one_time_password(Env) ->
    beepbeep_args:get_session_data("one_time_password", Env).

%%
%% @doc timeline parser
%%
-spec(parse_timeline(list(#message{})) ->  list(list(tuple()))).

parse_timeline(RecordList) ->
    parse_timeline(RecordList, []).

parse_timeline(RecordList, Results) ->
    case RecordList of
	[] -> lists:reverse(Results);
	[Message | Tail] ->
            User = Message#message.user,
	    Msg = [{id, Message#message.id}, 
                   {text, Message#message.text},
                   {user_name, atom_to_list(User#user.name)}, 
                   {user_id, User#user.id}],
	    parse_timeline(Tail, [Msg | Results])
    end.

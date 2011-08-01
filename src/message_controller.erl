%%
%% Sample default "/" controller, implement this to your needs
%%
-module(message_controller,[Env]).
-include_lib("eunit/include/eunit.hrl").
-include("../deps/message_box/src/message.hrl").
-include("../deps/message_box/src/user.hrl").
-export([handle_request/2,before_filter/0]).
-define(MsgGetCount, 40).

handle_request("create", []) ->
    Name = beepbeep_args:get_param("name",Env),
    Text = beepbeep_args:get_param("text",Env),
    OTPassword = message_box_web:get_one_time_password(Env),
    Result = message_box_rpc:call(send_message, [Name, OTPassword, Text]),

    case Result of
	{ok, _MessageId} -> beepbeep_args:flash({notice,"Message Saved."}, Env);
	Other -> io:format("error: ~p~n", [Other])
    end,
    {redirect, "/home"}.

%%
%% @doc private functions
%%

before_filter() ->
    FilterOnly = ["create"],
    case lists:member(beepbeep_args:get_action(Env),FilterOnly) of
	true ->
	    message_box_web:check_logged_in(Env);
	false ->
	    error_logger:info_report("Not Doing filter~n"),
	    ok
    end.

    

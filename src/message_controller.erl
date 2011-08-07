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
	{ok, _MessageId} -> 
            beepbeep_args:flash({notice,"Message Saved."}, Env);
	{error, Reason} -> 
            io:format("error: ~p~n", [Reason]),
            beepbeep_args:flash({notice,"Error:" ++ atom_to_list(Reason)}, Env)
    end,
    {redirect, "/home"};

%%
%% @doc send message and retrun result as json.
%%
%% @spec handle_request("create_json", []) -> 
%%         {"message_id", string()} | {error, unauthenticated}
%%
handle_request("create_json", []) ->
    Name = beepbeep_args:get_param("name",Env),
    Text = beepbeep_args:get_param("text",Env),
    ?debugVal(Text),

    OTPassword = message_box_web:get_one_time_password(Env),
    Result = message_box_rpc:call(send_message, [Name, OTPassword, Text]),

    case Result of
	{ok, MessageId} -> 
            {json, message_box_json:encode_result({message_id, MessageId})};
	{error, Reason} -> 
            io:format("error: ~p~n", [Reason]),
            ReasonBin = list_to_binary(atom_to_list(Reason)),
            {json, message_box_json:encode_result({error, ReasonBin})}
    end.

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

    

%%
%% Sample default "/" controller, implement this to your needs
%%
-module(follow_controller,[Env]).
-include_lib("eunit/include/eunit.hrl").
-include("../deps/message_box/src/message.hrl").
-include("../deps/message_box/src/user.hrl").
-export([handle_request/2,before_filter/0]).
-define(MsgGetCount, 40).

%%
%% @doc follow user and redirecto to home.
%%
handle_request("follow",[Name, UserIdStr]) ->
    User = beepbeep_args:get_session_data("user", Env),
    OTPassword = message_box_web:get_one_time_password(Env),
    UserId = list_to_integer(UserIdStr),

    case message_box_rpc:call(follow, [User#user.id, OTPassword, UserId]) of
        ok ->
            {redirect, "/user/timeline/" ++ Name};
        {error, already_following} ->
            beepbeep_args:flash({notice, "You are already following."}),
            {redirect, "/user/timeline/" ++ Name};
        Result ->
            ?debugVal(Result),
            {error, 500, "error"}
    end;

%%
%% @doc follow user and retrun result as json.
%%
%% @spec GET /follow/follow_json/:user_id -> 
%%         {"result", "ok"} | {"error", string()}
%%
handle_request("follow_json",[UserIdStr]) ->
    User = beepbeep_args:get_session_data("user", Env),
    OTPassword = message_box_web:get_one_time_password(Env),
    UserId = list_to_integer(UserIdStr),

    case message_box_rpc:call(follow, [User#user.id, OTPassword, UserId]) of
        ok ->
            {json, message_box_json:encode_result({result, ok})};
        {error, Reason} ->
            {json, message_box_json:encode_result({error, Reason})};
        Result ->
            ?debugVal(Result),
            {error, 500, "error"}
    end;

%%
%% @doc unfollow user and redirecto to home.
%%
handle_request("unfollow",[Name, UserIdStr]) ->
    User = beepbeep_args:get_session_data("user", Env),
    OTPassword = message_box_web:get_one_time_password(Env),
    UserId = list_to_integer(UserIdStr),

    case message_box_rpc:call(unfollow, [User#user.id, OTPassword, UserId]) of
        ok ->
            {redirect, "/user/timeline/" ++ Name};
        {error,not_following} ->
            beepbeep_args:flash({notice, "You are not following."}),
            {redirect, "/user/timeline/" ++ Name};
        Result ->
            ?debugVal(Result),
            {error, 500, "error"}
    end;

%%
%% @doc unfollow user and retrun result as json.
%%
%% @spec GET /unfollow/follow_json/:user_id -> 
%%         {"result", "ok"} | {"error", string()}
%%
handle_request("unfollow_json",[UserIdStr]) ->
    User = beepbeep_args:get_session_data("user", Env),
    OTPassword = message_box_web:get_one_time_password(Env),
    UserId = list_to_integer(UserIdStr),

    case message_box_rpc:call(unfollow, [User#user.id, OTPassword, UserId]) of
        ok ->
            {json, message_box_json:encode_result({result, ok})};
        {error, Reason} ->
            {json, message_box_json:encode_result({error, Reason})};
        Result ->
            ?debugVal(Result),
            {error, 500, "error"}
    end.

%%
%% @doc private functions
%%

before_filter() ->
    FilterOnly = ["follow", "follow_json", "unfollow", "unfollow_json"],
    case lists:member(beepbeep_args:get_action(Env),FilterOnly) of
	true ->
	    message_box_web:check_logged_in(Env);
	false ->
	    error_logger:info_report("Not Doing filter~n"),
	    ok
    end.

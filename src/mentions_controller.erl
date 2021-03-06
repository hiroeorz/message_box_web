%%
%% Sample default "/" controller, implement this to your needs
%%
-module(mentions_controller,[Env]).
-include_lib("eunit/include/eunit.hrl").
-include("../deps/message_box/src/message.hrl").
-include("../deps/message_box/src/user.hrl").
-export([handle_request/2,before_filter/0]).
-define(MsgGetCount, 40).

handle_request("index",[]) ->
    Name = beepbeep_args:get_session_data("name", Env),
    TimelineRecords = message_box_rpc:call(get_mentions_timeline, 
					   [Name, ?MsgGetCount]),
    Timeline = message_box_web:parse_timeline(TimelineRecords),
    {render,"home/show.html",[{controller,"mentions"},
                              {name,Name}, {timeline, Timeline}, 
                              {user, false},
                              {is_following, false}]};

%%
%% @doc unfollow user and retrun result as json.
%%
%% @spec GET /mentions/json -> list() | {"error", "not_found"}
%%
handle_request("json",[]) ->
    Name = beepbeep_args:get_session_data("name", Env),
    TimelineRecords = message_box_rpc:call(get_mentions_timeline, 
					   [Name, ?MsgGetCount]),

    JsonData = message_box_json:encode_timeline(TimelineRecords),
    {json, JsonData}.

%%
%% @doc private functions
%%

before_filter() ->
    FilterOnly = ["index", "json"],
    case lists:member(beepbeep_args:get_action(Env),FilterOnly) of
	true ->
	    message_box_web:check_logged_in(Env);
	false ->
	    error_logger:info_report("Not Doing filter~n"),
	    ok
    end.

    

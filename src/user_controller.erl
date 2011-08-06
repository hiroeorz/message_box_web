%%
%% Sample default "/" controller, implement this to your needs
%%
-module(user_controller,[Env]).
-include_lib("eunit/include/eunit.hrl").
-include("../deps/message_box/src/message.hrl").
-include("../deps/message_box/src/user.hrl").
-export([handle_request/2,before_filter/0]).
-define(MsgGetCount, 40).

-define(WAIT_TIME_AFTER_LOGIN, 300).
-define(WAIT_TIME_AFTER_CREATE, 300).

handle_request("login",[]) ->
    {render,"user/login.html",[]};

handle_request("logout",[]) ->
    beepbeep_args:remove_session_data("one_time_password", Env),
    beepbeep_args:remove_session_data("name", Env),
    {redirect, "/user/login"};

handle_request("timeline",[Name]) ->
    Me = beepbeep_args:get_session_data("user", Env),

    case message_box_rpc:call(get_user, [Name]) of
        {ok, User} ->
            TimelineRecords = message_box_rpc:call(get_sent_timeline, 
                                                   [Name, ?MsgGetCount]),
            Timeline = message_box_web:parse_timeline(TimelineRecords),
            [{id, UserId} | _] = User,
            IsFollowing = message_box_rpc:call(is_follow, [Me#user.id, UserId]),
            {render,"home/show.html",[{name,Name}, 
                                      {timeline, Timeline},
                                      {user, User},
                                      {is_following, IsFollowing}]};
        _ ->
            {error, 404, "User not found:" ++ Name}
    end;

handle_request("new", []) ->
    {render,"user/new.html",[]};    

handle_request("create", []) ->
    Name = beepbeep_args:get_param("name", Env),
    Password = beepbeep_args:get_param("password", Env),
    MailAddress = beepbeep_args:get_param("mail", Env),     
    {_FileName, {ContentType, _}, Data} = beepbeep_args:get_param("icon_file", 
                                                                   Env),    

    case message_box_rpc:call(create_user, [Name, MailAddress, Password]) of
	{ok, _NewUser} ->
	    message_box_web:sleep(?WAIT_TIME_AFTER_CREATE),
            message_box_rpc:call(save_icon, [Name, Data, ContentType]),
	    beepbeep_args:flash({notice, 
				 "Congratulations! New User Created :-)"}, Env),
	    {redirect, "/user/login"};
	{error, already_exist} ->
	    beepbeep_args:flash({notice, "Sorry already exist name."}, Env),
	    {redirect, "/user/new"}
    end;

handle_request("edit", []) ->
    Name = beepbeep_args:get_session_data("name", Env),
    {ok, User} = message_box_rpc:call(get_user, [Name]),
    ?debugVal(User),
    {render, "user/edit.html", [{user, User}]};

handle_request("update", []) ->
    Name = beepbeep_args:get_session_data("name", Env),
    Password = beepbeep_args:get_param("password", Env),
    MailAddress = beepbeep_args:get_param("mail", Env),     
    {_FileName, {ContentType, _}, Data} = beepbeep_args:get_param("icon_file", 
                                                                   Env),    

    OTPassword = message_box_web:get_one_time_password(Env),

    case message_box_rpc:call(update_user, [Name, OTPassword,
                                            MailAddress, Password]) of
	{ok, _NewUser} ->
            message_box_rpc:call(save_icon, [Name, Data, ContentType]),
	    beepbeep_args:flash({notice, "User Updated :-)"}, Env),
	    {redirect, "/user/edit"};
	{error, already_exist} ->
	    beepbeep_args:flash({notice, 
				 "Sorry Someting Technicary wrong."}, Env),
	    {redirect, "/user/edit"}
    end;

handle_request("auth", []) ->
    Name = beepbeep_args:get_param("name", Env),
    Password = beepbeep_args:get_param("password", Env),

    case message_box_rpc:call(authenticate, [Name, Password]) of
	{ok, OneTimePassword, User} ->
	    ok = beepbeep_args:set_session_data("name", Name, Env),
	    ok = beepbeep_args:set_session_data("user", User, Env),
	    ok = beepbeep_args:set_session_data("one_time_password", 
						OneTimePassword, Env),
	    error_logger:info_report("User:~p is logged in~n", [Name]),
	    message_box_web:sleep(?WAIT_TIME_AFTER_LOGIN),
	    {redirect, "/home"};
	{error, unauthenticated} ->
	    beepbeep_args:flash({notice, "Invalid username or password."}, Env),
	    {redirect, "/user/login"}
    end;

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

handle_request("unfollow",[Name, UserIdStr]) ->
    User = beepbeep_args:get_session_data("user", Env),
    OTPassword = message_box_web:get_one_time_password(Env),
    UserId = list_to_integer(UserIdStr),

    case message_box_rpc:call(unfollow, [User#user.id, OTPassword, UserId]) of
        {ok, deleted} ->
            {redirect, "/user/timeline/" ++ Name};
        {error,not_following} ->
            beepbeep_args:flash({notice, "You are not following."}),
            {redirect, "/user/timeline/" ++ Name};
        Result ->
            ?debugVal(Result),
            {error, 500, "error"}
    end;
            
handle_request("icon", [Name]) ->
    {ok, Data, ContentType} = message_box_rpc:call(get_icon, [Name]),
    {send_data, ContentType, "inline", Data}.

%%
%% @doc private functions
%%

before_filter() ->
    FilterOnly = ["edit", "update", "follow", "timeline"],
    case lists:member(beepbeep_args:get_action(Env),FilterOnly) of
	true ->
	    message_box_web:check_logged_in(Env);
	false ->
	    error_logger:info_report("Not Doing filter~n"),
	    ok
    end.

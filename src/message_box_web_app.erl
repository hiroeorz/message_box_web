-module(message_box_web_app).

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for message_box_web.
start(_Type, _StartArgs) ->
    message_box_web_deps:ensure(),
    message_box_web_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for message_box_web.
stop(_State) ->
    ok.

%%%-------------------------------------------------------------------
%%% File    : app_launcher_fsm.erl
%%% Author  : Ari Lerner
%%% Description :
%%%
%%% Created :  Wed Nov 18 17:30:15 PST 2009
%%%-------------------------------------------------------------------

-module (app_launcher_fsm).
-include ("beehive.hrl").
-include ("common.hrl").
-behaviour(gen_fsm).

%% API
-export([start_link/1]).

%% methods
-export ([
          start_new/1,
          launch/1,
          update/1
         ]).
%% states
-export ([
          fetching/2,
          preparing/2,
          updating/2,
          launching/2,
          pending/2
         ]).

%% gen_fsm callbacks
-export([init/1, state_name/2, state_name/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record (state, {
           app,
           latest_sha,
           host,
           port,
           bee,
           output = [],
           updating = false,
           caller,
           from
          }).

%%====================================================================
%% API
%%====================================================================
start_new(Pid) -> gen_fsm:send_event(Pid, {start_new}).
update(Pid) -> gen_fsm:send_event(Pid, {update}).
launch(Pid) -> gen_fsm:send_event(Pid, {launch}).

%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.
%%--------------------------------------------------------------------
start_link(Opts) ->
  gen_fsm:start_link(?MODULE, [Opts], []).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to
%% initialize.
%%--------------------------------------------------------------------
init([Proplist]) ->
  App = proplists:get_value(app, Proplist),
  Caller = proplists:get_value(caller, Proplist),
  From     = proplists:get_value(from, Proplist),
  Updating = proplists:get_value(updating, Proplist),

  beehive_bee_object_config:init(), % JUST IN CASE
  %% Only start if there are no other modules registered with the name
  State = #state{app = App, from = From, caller = Caller,
                 updating = Updating, bee = #bee{}},
  case global:whereis_name(registered_name(App)) of
    undefined ->
      case App#app.latest_error of
        undefined ->
          global:register_name(registered_name(App), self()),
          %% Up for debate, should we always do this on init?
          %% I kind of like the convenience
          Self = self(),
          gen_cluster:run(beehive_storage_srv, {fetch_or_build_bee, App, Self}),
          ?LOG(debug, "gen_cluster:run(beehive_storage_srv, {fetch_or_build_bee, ~p, ~p})",
               [App#app.name, Self]),
          {ok, fetching, State};
        _ ->
          {stop, {error, pending_app_error}}
      end;
    _ ->
      Tuple = {already_started, registered_name(App)},
      {stop, Tuple}
  end.

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, State) -> {next_state, NextStateName, NextState}|
%%                             {next_state, NextStateName,
%%                                NextState, Timeout} |
%%                             {stop, Reason, NewState}
%% Description:There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name StateName is called to handle the event. It is also
%% called if a timeout occurs.
%%
%% Valid states: fetching, preparing, updating, launching, pending
%%--------------------------------------------------------------------
fetching({send_bee_object, _Bo}, State) ->
  {next_state, preparing, State};
fetching({launch}, State) ->
  %% If we have not yet fetched the bee, but received a launch request
  %% we'll just resend it to ourselves in a little while
  gen_cluster:run(beehive_storage_srv,
                  {fetch_or_build_bee, State#state.app, self()}),
  %% Give it some time to try to fetch again...
  timer:send_after(500, {launch}),
  {next_state, fetching, State};
fetching({error, Msg}, State) ->
  stop_error({fetching, Msg}, State);

fetching(_Msg, State) ->
  {next_state, fetching, State}.


%% Prepared to do something!
preparing({update}, #state{app = App} = State) ->
  Self = self(),
  gen_cluster:run(beehive_storage_srv, {build_bee, App, Self}),
  {next_state, updating, State};

preparing({launch}, #state{app = App} = State) ->
  Self = self(),
  Port = bh_host:unused_port(),
  ?LOG(debug, "beehive_bee_object:start(~p, ~p, ~p, ~p)",
       [App#app.template, App#app.name, Port, Self]),
  beehive_bee_object:start(App, Port, Self),
  {next_state, launching, State};

preparing({start_new}, State) ->
  self() ! {bee_built, []},
  {next_state, updating, State};

preparing(_Other, State) ->
  {next_state, preparing, State}.


updating({bee_built, _Info}, #state{app = App} = State) ->
  Port = bh_host:unused_port(),
  beehive_bee_object:start(App, Port, self()),
  {next_state, launching, State};

updating(Msg, State) ->
  stop_error({updating, Msg}, State).


%% LAUNCHING THE APPLICATION
launching({started, BeeObject}, #state{app = App} = State) ->
  Self = self(),
  BuiltBee = bees:from_bee_object(BeeObject, App),
  Bee = BuiltBee#bee{host = bh_host:myip()},
  ?LOG(debug, "spawn_update_bee_status: ~p for ~p, ~p", [Bee, Self, 20]),
  app_manager:spawn_update_bee_status(Bee, Self, 20),
  {next_state, pending, State#state{bee = Bee}};

launching({error, Reason}, State) ->
  stop_error({launching, Reason}, State);

launching(Event, State) ->
  ?LOG(debug, "Uncaught event: ~p while in state: ~p ~n", [Event, launching]),
  {next_state, launching, State}.


%% AFTER THE APPLICATION HAS BEEN 'PENDING'
pending({updated_bee_status, broken}, State) ->
  stop_error({error, broken_start}, State);

pending({updated_bee_status, BackendStatus},
        #state{app = App, bee = Bee, from = From,
               caller = Caller, latest_sha = Sha,
               updating = Updating} = State) ->
  ?LOG(debug, "Application started ~p: ~p", [BackendStatus, App#app.name]),
  %% App started normally
  bees:save(Bee#bee{status = BackendStatus}),
  Message =  case Updating of
               true  -> bee_updated_normally;
               false -> bee_started_normally
             end,
  From ! {Message, Bee#bee{status = BackendStatus},
                    App#app{revision = Sha}, Caller},

  ok = global:unregister_name(registered_name(App)),
  {stop, normal, State};

pending(Event, State) ->
  ?LOG(debug, "Got uncaught event in pending state: ~p", [Event]),
  {next_state, pending, State}.

state_name(Event, State) ->
  io:format("Uncaught event: ~p while in state: ~p ~n", [Event, state_name]),
  {next_state, state_name, State}.

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, From, State) -> {next_state, NextStateName, NextState} |
%%                                   {next_state, NextStateName,
%%                                     NextState, Timeout} |
%%                                   {reply, Reply, NextStateName, NextState}|
%%                                   {reply, Reply, NextStateName,
%%                                    NextState, Timeout} |
%%                                   {stop, Reason, NewState}|
%%                                   {stop, Reason, Reply, NewState}
%% Description: There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/2,3, the instance of this function with the same
%% name as the current state name StateName is called to handle the event.
%%--------------------------------------------------------------------
state_name(Event, _From, State) ->
  Reply = ok,
  io:format("Uncaught event: ~p while in state: ~p ~n", [Event, state_name]),
  {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_event(Event, StateName, State) -> {next_state, NextStateName,
%%                                                NextState} |
%%                                          {next_state, NextStateName,
%%                                                NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event(Event, StateName, State) ->
  io:format("Uncaught event: ~p while in state: ~p ~n", [Event, StateName]),
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_sync_event(Event, From, StateName,
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState,
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState,
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState,
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
handle_info({data, Msg}, StateName, #state{output = CurrOut} = State) ->
  {next_state, StateName, State#state{output = [Msg|CurrOut]}};
handle_info({port_closed, _Port}, StateName, State) ->
  {next_state, StateName, State};
handle_info(Info, StateName, State) ->
  ?LOG(debug, "~p received handle_info: ~p in state ~p",
       [?MODULE, Info, StateName]),
  apply(?MODULE, StateName, [Info, State]).
%% {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, #state{app = App} = _State) ->
  ?LOG(debug, "unregister_name for app: ~p", [registered_name(App)]),
  ok = global:unregister_name(registered_name(App)).

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
stop_error(Msg, #state{from = From, caller = Caller,
                       app = App, bee = Bee, output = Output} = State) ->
  Tuple = {?MODULE, error, Msg,
           [{app, App}, {bee, Bee},
            {output, lists:reverse(Output)}, {caller, Caller}]},
  From ! Tuple,
  {stop, normal, State}.

%% a name
registered_name(#app{name = Name}) ->
  list_to_atom(lists:flatten(["app_launcher_fsm", Name])).

%%%-------------------------------------------------------------------
%%% File    : bh_router_sup.erl
%%% Author  : Ari Lerner
%%% Description :
%%%
%%% Created :  Wed Dec  2 20:13:46 PST 2009
%%%-------------------------------------------------------------------

-module (beehive_router_sup).

-behaviour(supervisor).
-compile([verbose, report_errors, report_warnings, trace, debug_info]).

%% API
-export([start_link/0, start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define (IF (Bool, A, B), if Bool -> A; true -> B end).

-define(SERVER, ?MODULE).
-define (MaxRestartTrial, 5).
-define (MaxTime_Between_RestartInSec, 10).
-define (Shutdown_After_TimeoutInSec, 5000).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
  start_link([]).
start_link(Args) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using
%% supervisor:start_link/[2,3], this function is called by the new process
%% to find out about restart strategy, maximum restart frequency and child
%% specifications.
%%--------------------------------------------------------------------
init(_Args) ->
 
  ChildrenSpecSet = lists:flatten([
%    ?CHILD(tcp_socket_server_sup, worker),
%    ?CHILD(bh_node_stats_srv, worker),
    tcp_socket_server_sup(),
    node_stat_server(),
    optional_dashboard()
  ]),

  {ok,{worker_restart_strategy(), ChildrenSpecSet}}.


%%====================================================================
%% Internal functions
%%====================================================================
worker_restart_strategy() ->
  {one_for_one, ?MaxRestartTrial, ?MaxTime_Between_RestartInSec}.

%TODO: router is for routing only. Move to mngt.
optional_dashboard()->
  Dashboard = get_worker_spec(beehive_dashboard_sup),
  ShouldRunDashboard = should_run_dashboard(),
  ?IF(ShouldRunDashboard, Dashboard, []).

tcp_socket_server_sup()->
  get_worker_spec(tcp_socket_server_sup).

node_stat_server()->
  get_worker_spec(bh_node_stats_srv).

get_worker_spec(Name) when is_atom(Name) ->
  {Name, {Name, start_link, []}, permanent, ?Shutdown_After_TimeoutInSec, worker, [Name]}.

%TODO: Invitation to user error!! Consolidate. 
should_run_dashboard()->
  config:search_for_application_value(dashboard, true).

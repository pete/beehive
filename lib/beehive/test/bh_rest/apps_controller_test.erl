-module (apps_controller_test).
-include_lib("eunit/include/eunit.hrl").
-include ("beehive.hrl").
-include ("common.hrl").

setup() ->
  bh_test_util:setup(),
  bh_test_util:dummy_user(),                    % test@getbeehive.com
  apps:save(bh_test_util:dummy_app()),
  bh_test_util:ensure_rest_server(),
  %rest_server:start_link(),
  timer:sleep(100),
  ok.

teardown(_X) ->
  beehive_db_srv:delete_all(app),
  bh_test_util:ensure_rest_server_stop(),
  ok.

starting_test_() ->
  {inorder,
   {setup,
    fun setup/0,
    fun teardown/1,
    [
     fun get_index_as_user/0,
     fun get_index_as_admin/0,
     fun get_index_with_name_as_admin/0,
     fun get_index_with_name_as_user/0,
     fun get_index_with_name_as_invalid_user/0,
     fun get_index_with_wrong_name/0,
     fun get_bee_logs/0,
     fun get_bee_logs_with_wrong_name/0,
     fun post_create_new_app/0,
     fun post_create_new_app_bad_token/0,
     fun post_create_new_app_no_token/0,
     fun put_app/0,
     fun put_app_wrong_name/0,
     fun put_app_bad_token/0,
     fun delete_app/0,
     fun delete_app_no_token/0,
     fun delete_app_wrong_name/0
    ]
   }
  }.

get_index_as_user() ->
  User = bh_test_util:dummy_user(),
  user_apps:create(User#user.email, "test_app"),
  {ok, Header, Response} =
    bh_test_util:fetch_url(get,
                           [{path, path_with_token("/apps.json",
                                                   User#user.token)}]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  [Json|_] = bh_test_util:response_json(Response),
  {"apps", Apps} = Json,
  ?assert(is_list(Apps)),
  ?assert(lists:any(fun(E) ->
                        proplists:get_value("name", E) =:= "test_app"
                    end, Apps)),
  passed.

get_index_as_admin() ->
  User = bh_test_util:admin_user(),
  {ok, Header, Response} =
    bh_test_util:fetch_url(get,
                           [{path, path_with_token("/apps.json",
                                                   User#user.token)}]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  [Json|_] = bh_test_util:response_json(Response),
  {"apps", Apps} = Json,
  ?assert(is_list(Apps)),
  ?assert(lists:any(fun(E) ->
                        proplists:get_value("name", E) =:= "test_app"
                    end, Apps)),
  passed.


get_index_with_name_as_admin() ->
  User = bh_test_util:admin_user(),
  {ok, Header, Response} =
    bh_test_util:fetch_url(get,
                           [{path, path_with_token("/apps/test_app.json",
                                                   User#user.token)}]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  [App|_] = bh_test_util:response_json(Response),
  {"application",[{"name","test_app"}|_]} = App,
  passed.

get_index_with_name_as_user() ->
  User = bh_test_util:dummy_user(),
  {ok, Header, Response} =
    bh_test_util:fetch_url(get,
                           [{path, path_with_token("/apps/test_app.json",
                                                   User#user.token)}]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  [App|_] = bh_test_util:response_json(Response),
  {"application",[{"name","test_app"}|_]} = App,
  passed.

get_index_with_name_as_invalid_user() ->
  User = bh_test_util:create_user(#user{email    = "noapps@getbeehive.com",
                                        password = "test",
                                        token    = "noapptoken" }),
  {ok, Header, Response} =
    bh_test_util:fetch_url(get,
                           [{path, path_with_token("/apps/test_app.json",
                                                   User#user.token)}]),
  ?assertEqual("HTTP/1.0 404 Object Not Found", Header),
  passed.

get_index_with_name_with_bad_token() ->
  {ok, Header, Response} =
    bh_test_util:fetch_url(get,
                           [{path, path_with_token("/apps/test_app.json",
                                                   "asdfwrong")}]),
  ?assertEqual("HTTP/1.0 401 Unauthorized", Header),
  passed.

get_index_with_wrong_name() ->
  User = bh_test_util:admin_user(),
  {ok, Header, Response} =
    bh_test_util:fetch_url(get,
                           [{path, path_with_token("/apps/unfound_app.json",
                                                   User#user.token)}]),
  ?assertEqual("HTTP/1.0 404 Object Not Found", Header),
  ?assertMatch([{"error", "App not found"}],
               bh_test_util:response_json(Response)),
  passed.

get_bee_logs() ->
  User = bh_test_util:admin_user(),
  LogFile = filename:join([?BEEHIVE_HOME, "logs/bee_events", "test_app"]),
  filelib:ensure_dir(LogFile),
  file:write_file(LogFile, "Some log data"),
  {ok, Header, Response} =
    bh_test_util:fetch_url(get,
                           [{path,
                             path_with_token("/apps/test_app/bee_logs.json",
                                             User#user.token)}]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  ?assertMatch([{"bee_log", "Some log data"}],
               bh_test_util:response_json(Response)),
  passed.


get_bee_logs_with_wrong_name() ->
  User = bh_test_util:admin_user(),
  {ok, Header, Response} =
    bh_test_util:fetch_url(get,
                           [{path, path_with_token("/apps/noapp/bee_logs.json",
                                                   User#user.token)}]),
  ?assertEqual("HTTP/1.0 404 Object Not Found", Header),
  ?assertMatch([{"error", "App not found"}],
               bh_test_util:response_json(Response)),
  passed.


post_create_new_app() ->
  User = bh_test_util:dummy_user(),
  {ok, Header, Response} =
    perform_post_create([{app_name, "creationtest"},
                         {repo_url, bh_test_util:dummy_git_repos_url()},
                         {token, User#user.token}]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  ?assertMatch([{"ok","created"}],
               bh_test_util:response_json(Response)),
  passed.

post_create_new_app_no_token() ->
  {ok, Header, _Response} =
    perform_post_create([{app_name, "creationtest"},
                         {repo_url, bh_test_util:dummy_git_repos_url()}]),
  ?assertEqual("HTTP/1.0 401 Unauthorized", Header),
  passed.

post_create_new_app_bad_token() ->
  {ok, Header, _Response} =
    perform_post_create([{app_name, "creationtest"},
                         {repo_url, bh_test_util:dummy_git_repos_url()},
                         {token, "badtoken"}]),
  ?assertEqual("HTTP/1.0 401 Unauthorized", Header),
  passed.

put_app() ->
  {ok, App} = apps:save(bh_test_util:dummy_app()),
  User = bh_test_util:dummy_user(),
  {ok, Header, _Resp} =
    perform_put(App#app.name, [{token, User#user.token},
                               {branch, "release"}]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  Updated = apps:find_by_name(App#app.name),
  ?assertEqual("release", Updated#app.branch),
  passed.

put_app_wrong_name() ->
  App = bh_test_util:dummy_app(),
  User = bh_test_util:dummy_user(),
  {ok, Header, _Resp} =
    perform_put("wrongname", [{token, User#user.token},
                              {branch, "release"}]),
  ?assertEqual("HTTP/1.0 404 Object Not Found", Header),
  passed.

put_app_bad_token() ->
  App = bh_test_util:dummy_app(),
  {ok, Header, _Resp} =
    perform_put(App#app.name, [{token, ""},
                               {repo_url, "newurl"}]),
  ?assertEqual("HTTP/1.0 401 Unauthorized", Header),
  passed.


delete_app() ->
  App = bh_test_util:dummy_app(),
  User = bh_test_util:dummy_user(),
  {ok, Header, Response} =
    bh_test_util:fetch_url(delete,
                           [{path, lists:flatten(["/apps/",
                                                  App#app.name,
                                                  ".json?token=",
                                                  User#user.token])}]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  ?assertMatch(not_found, apps:find_by_name(App#app.name)),
  passed.

delete_app_no_token() ->
  App = bh_test_util:dummy_app(),
  User = bh_test_util:dummy_user(),
  {ok, Header, _Response} =
    bh_test_util:fetch_url(delete,
                           [{path, lists:flatten(["/apps/",
                                                  App#app.name,
                                                  ".json?token="])}]),
  ?assertEqual("HTTP/1.0 401 Unauthorized", Header),
  passed.

delete_app_wrong_name() ->
  User = bh_test_util:dummy_user(),
  {ok, Header, _Response} =
    bh_test_util:fetch_url(delete,
                           [{path, lists:flatten(["/apps/wrongname.json?token=",
                                                  User#user.token])}]),
  ?assertEqual("HTTP/1.0 200 OK", Header),
  passed.


perform_put(Name, Params) ->
  bh_test_util:fetch_url(put,
                         [{path, lists:flatten(["/apps/",
                                                Name,
                                                ".json"])},
                          {headers, [{"Content-Type",
                                      "application/x-www-form-urlencoded" }]},
                          {params, Params}
                         ]).


perform_post_create(Params) ->
  bh_test_util:fetch_url(post,
                         [{path, "/apps.json"},
                          {headers, [{"Content-Type",
                                      "application/x-www-form-urlencoded" }]},
                          {params, Params}
                         ]).

path_with_token(Path, Token) ->
  lists:flatten([Path, "?token=", Token]).

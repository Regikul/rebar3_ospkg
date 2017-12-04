%% -*- mode: erlang;erlang-indent-level: 2;indent-tabs-mode: nil -*-
-module(rebar3_ospkg_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, ospkg).
-define(DEPS, [app_discovery]).
-define(DEBIAN, "DEBIAN").

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Provider = providers:create([
    {name, ?PROVIDER},                                           % The 'user friendly' name of the task
    {module, ?MODULE},                                           % The module implementation of the task
    {bare, true},                                                % The task can be run by the user, always true
    {deps, ?DEPS},                                               % The list of dependencies
    {example, "rebar3 ospkg [--type|-t deb] [--build-number|-n 5]"},    % How to use the plugin
    {opts, [                                                     % list of options understood by the plugin
      {type, $t, "type", {string, "deb"}, "type of package (deb/rpm/etc)"},
      {build_no, $n, "build-number", {string, "0"}, "build number"}
    ]},
    {short_desc, "Create OS package (only deb currently)"},
    {desc, "A rebar plugin"}
  ]),
  {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  Config = rebar_state:get(State, relx, []),
  case lists:keyfind(release, 1, Config) of
    {release, {Name, _Vsn}, _} ->
      BasePath = rebar_dir:base_dir(State),
      RelPath = filename:join([BasePath, "rel", Name]),
      {Args, _} = rebar_state:command_parsed_args(State),
      {type, Type} = lists:keyfind(type, 1, Args),
      make_package(State, RelPath, Type);
    _ -> {error, {?MODULE, no_release}}
  end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

-spec make_package(rebar_state:t(), string(), atom()) -> {ok, rebar_state:t()} | {error, string()}.
make_package(State, WorkDir, "deb") ->
  rebar_api:info("Working dir: ~p", [WorkDir]),
  Config = rebar_state:get(State, relx, []),
  {Args, _} = rebar_state:command_parsed_args(State),
  {build_no, BuildNumber} = lists:keyfind(build_no, 1, Args),
  {release, {Name, Vsn}, _} = lists:keyfind(release, 1, Config),
  {include_erts, IncludeErts} = lists:keyfind(include_erts, 1, Config),

  PackageDir = atom_to_list(Name) ++ "_" ++ Vsn ++ "-" ++ BuildNumber,
  PackageRoot = filename:join([WorkDir, PackageDir]),
  Debian = filename:join([PackageRoot, ?DEBIAN]),

  file:make_dir(PackageRoot),
  file:make_dir(Debian),
  file:make_dir(filename:join([PackageRoot, "opt"])),
  file:make_dir(InstallDir = filename:join([PackageRoot, "opt", Name])),
  'cp -R'(filename:join([WorkDir, "bin"]), filename:join([InstallDir, "bin"])),
  'cp -R'(filename:join([WorkDir, "lib"]), filename:join([InstallDir, "lib"])),
  'cp -R'(filename:join([WorkDir, "releases"]), filename:join([InstallDir, "releases"])),
  {ok, Names} = file:list_dir(WorkDir),

  case IncludeErts andalso find_erts(Names) of
    false -> undefined;
    no_erts -> undefined;
    {ok, Erts} ->
      'cp -R'(filename:join([WorkDir, Erts]), filename:join([InstallDir, Erts])),
      Erts;
    {multiple, _} ->
      rebar_api:warning("Found more than one erts, all of them will be skipped"),
      undefined
  end,

  generate_control_file(Debian, State),
  generate_md5sums_file(Debian, PackageRoot),
  copy_files(Debian, State),
  generate_deb(WorkDir, PackageDir),
  'rm -rf'(PackageRoot),
  {ok, State};
make_package(_State, _WorkDir, Unknown) ->
  rebar_api:error("Unknown package type (~p) requested!", [Unknown]),
  {error, {?MODULE, package_not_supported}}.

-spec find_erts([file:filename()]) -> {ok, file:filename()} | no_erts | {multiple, [file:filename()]}.
find_erts(Filenames) ->
  {Ertses, _Other} = lists:partition(fun match_erts/1, Filenames),
  case Ertses of
    [Erts] -> {ok, Erts};
    [] -> no_erts;
    _ -> {multiple, Ertses}
  end.

-spec match_erts(file:filename()) -> boolean().
match_erts([$e, $r, $t, $s, $- | _Version ]) -> true;
match_erts(_) -> false.

-spec 'rm -rf'(file:filename()) -> ok.
'rm -rf'(Root) ->
  shell_cmd("rm -rf " ++ Root).

-spec 'cp -R'(file:filename(), file:filename()) -> ok.
'cp -R'(From, To) ->
  shell_cmd("cp -R " ++ From ++ " " ++ To).

-spec generate_control_file(file:filename(), rebar_state:t()) -> ok.
generate_control_file(DebianDir, State) ->
  {ok, Control} = file:open(filename:join([DebianDir, "control"]), [write]),
  lists:foreach(write_to(Control, State), special_formats()),
  OSPkgConf = rebar_state:get(State, ospkg, []),
  case lists:keyfind(deb, 1, OSPkgConf) of
    {deb, Options} when is_list(Options) ->
      lists:foreach(fetch_and_write_to(Control, Options), control_file_formats());
    _ ->
      rebar_api:info("have no options in `ospkg` section", []),
      ok
  end,
  io:format(Control, "~n", []),
  file:close(Control).

-spec write_to(file:io_device(), rebar_state:t()) -> fun ().
write_to(Control, State) ->
  fun ({Fun, Format}) when is_function(Fun) ->
    case Fun(State) of
      [] -> ok;
      Value -> io:format(Control, Format, Value)
    end
  end.

-spec fetch_and_write_to(file:io_device(), proplists:proplist()) -> fun ().
fetch_and_write_to(Control, Options) ->
  fun ({Key, Format}) when is_atom(Key) ->
    case lists:keyfind(Key, 1, Options) of
      undefined -> ok;
      {Key, String} -> io:format(Control, Format, [String])
    end
  end.

-spec control_file_formats() -> proplists:proplist().
control_file_formats() ->
  [
    {provides, "Provides: ~ts~n"},
    {maintainer, "Maintainer: ~ts~n"},
    {architecture, "Architecture: ~ts~n"},
    {section, "Section: ~ts~n"},
    {description, "Description: ~ts~n"},
    {depends, "Depends: ~ts~n"},
    {'pre-depends', "Pre-Depends: ~ts~n"},
    {conflicts, "Conflicts: ~ts~n"},
    {replaces, "Replaces: ~ts~n"},
    {recommends, "Recommends: ~ts~n"},
    {suggests, "Suggests: ~ts~n"}
  ].

-spec special_formats() -> [{fun ((rebar_state:t()) -> list() | undefined), string()}].
special_formats() ->
  [
    {fun get_release_name/1, "Package: ~ts~n"},
    {fun get_release_package_versions/1, "Version: ~s-~s~n"},
    {fun get_commit_sha1/1, "Origin: ~ts~n"}
  ].

-spec get_release_name(rebar_state:t()) -> list().
get_release_name(State) ->
  {release, {Name, _Vsn}, _} = lists:keyfind(release, 1, rebar_state:get(State, relx, [])),
  [Name].

-spec get_release_package_versions(rebar_state:t()) -> list().
get_release_package_versions(State) ->
  {release, {_Name, Vsn}, _} = lists:keyfind(release, 1, rebar_state:get(State, relx, [])),
  {Args, _} = rebar_state:command_parsed_args(State),
  {build_no, BuildNumber} = lists:keyfind(build_no, 1, Args),
  [Vsn, BuildNumber].

-spec get_commit_sha1(rebar_state:t()) -> list().
get_commit_sha1(_State) ->
  GitHash = os:cmd("git rev-parse HEAD"),
  [lists:filter(not_equal($\n), GitHash)].

-spec not_equal(char()) -> fun ( (char()) -> boolean()).
not_equal(Char) ->
  fun (Input) ->
    Char =/= Input
  end.

-spec generate_md5sums_file(file:filename(), file:filename()) -> ok.
generate_md5sums_file(DebianDir, PackageRoot) ->
  Cmd = "(cd " ++ PackageRoot ++ " && find opt/ -type f | xargs md5sum)  >> " ++ filename:join([DebianDir, "md5sums"]),
  shell_cmd(Cmd).

-spec copy_files(file:filename(), rebar_state:t()) -> ok.
copy_files(Debian, State) ->
  Config = rebar_state:get(State, ospkg, []),
  {deb, Options} = lists:keyfind(deb, 1, Config),
  {files_dir, {App, Path}} = lists:keyfind(files_dir, 1, Options),
  case lists:filter(named_as(App), rebar_state:project_apps(State)) of
    [AppInfo] ->
      AppPath = rebar_app_info:dir(AppInfo),
      FullPath = filename:join([AppPath, Path, "*"]),
      'cp -R'(FullPath, Debian);
    [] ->
      io:format("warning: no addition package files found on app ~p", [App])
  end,
  ok.

-spec named_as(atom()) -> fun( (rebar_app_info:t()) -> boolean() ).
named_as(AppName) ->
  fun (AppInfo) ->
    BinName = rebar_app_info:name(AppInfo),
    AppName =:= binary_to_atom(BinName, utf8)
  end.

-spec generate_deb(file:filename(), file:filename()) -> ok.
generate_deb(WorkDir, PackageDir) ->
  Cmd = "(cd " ++ WorkDir ++ " && fakeroot dpkg-deb --build " ++ PackageDir ++ " )",
  shell_cmd(Cmd).

-spec shell_cmd(string()) -> ok.
shell_cmd(Command) ->
  rebar_api:info("Executing `~s`", [Command]),
  Output = os:cmd(Command),
  rebar_api:info("Output: ~s", [Output]),
  ok.

ospkg
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar.config:

    {plugins, [
        {rebar3_ospkg, {git, "http://github.com/regikul/rebar3_ospkg", {tag, "v0.1.2"}}}
    ]}.

Then just call your plugin directly in an existing application:

    $ rebar3 do release, ospkg -n $PACKAGE_VERSION -t $PACKAGE_TYPE
    ===> Fetching ospkg
    ===> Compiling ospkg
    <Plugin Output>

Currently supports only `deb` packages. Generate files with names like `$RELEASENAME_$RELEASEVER-$PACKAGEVER.deb` in directory of the current release.

Example config
-----

```
{ospkg, [
  {install_dir, "/opt"},
  {deb, [
    {files_dir, {app_with_conf, "priv/debian"}},

    %% DEBIAN/control
    {provides, "packagename"},
    {maintainer, "Ivan Petrov <some@email.somewhere>"},
    {architecture, "all"},
    {section, "net"},
    {description, "Here goes description for your package"},
    {depends, "package0, package1 (>= 1.2)"},
    {'pre-depends', "package3, package4"},
    {conflicts, ""},
    {replaces, ""},
    {recommends, ""},
    {suggests, ""}
  ]}
]}.
```

Only `DEBIAN/control` and `DEBIAN/md5sums` are generated by the plugin. All other deb-related files must be in location poined by `files_dir`.

Known issues
-----

Currently plugin can not be called alone, due to relx eating all command line args. You can generate packages like that:

    $ rebar3 do release, ospkg -n $PACKAGE_VERSION -t $PACKAGE_TYPE


Acknowledgements
-----

I would like to thank AgoraDoxa LLC for their feedback and opportunity to use this tool in real world projects.

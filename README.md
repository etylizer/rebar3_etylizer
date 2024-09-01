rebar3_etylizer
=====

A rebar plugin for [etylizer](https://github.com/etylizer/etylizer/).

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_etylizer, {git, "https://github.com/etylizer/rebar3_etylizer.git"}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 etylizer
    ===> Fetching rebar3_etylizer
    ===> Compiling rebar3_etylizer
    <Plugin Output>

Build
-----

    $ rebar3 compile

Config
---

The following configuration values are avaiable through the rebar config.

    {etylizer, [
        {src_paths, ["src/"]},
        {project_root, "."}
    ]}.

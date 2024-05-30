rebar3_etylizer
=====

A rebar plugin for [etylizer](https://github.com/etylizer/etylizer/).

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_etylizer, {git, "https://host/user/rebar3_etylizer.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 etylizer
    ===> Fetching rebar3_etylizer
    ===> Compiling rebar3_etylizer
    <Plugin Output>

Config
---

The following configuration values are avaiable through the rebar config.

    {etylizer, [
        {src_paths, ["src/"]},
        {project_root, "."}
    ]}.

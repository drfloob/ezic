ezic
====

ezic is a native erlang library for working with timezones. It parses the Olson timezone database files, allowing native erlang timezone lookup and zone conversions without the use of shell commands or a UNIX system.

See the [tz database](http://www.twinsun.com/tz/tz-link.htm) page for more information about the Olson database.



Purpose
-------

Erlang/OTP didn't have a native timezone library, so I wrote ezic to fill that void.

The [recommended way](http://www.erlang.org/pipermail/erlang-questions/2006-December/024291.html) of handling timezones in erlang involves setting the system environment variable `TZ` to the desired timezone, then calling erlang time functions. As far as I can tell, this technique has a few key issues:

 * it only works on *nix systems,
 * it depends on your system's timezone data (preventing custom timezone hackery), and
 * you can create race conditions if you aren't careful about it

ezic doesn't have these problems. ezic is, however, a young project with who-knows-how-many problems of its own.



API
---

 * `ezic:localtime(TimeZone) -> datetime()`
 * `ezic:utc_to_local(universal_datetime(), TimeZone) -> local_datetime()`
 * `ezic:local_to_utc(local_datetime(), TimeZone) -> universal_datetime()`



Example Setup
-----

    # setup your local environment
    mkdir -p priv/tzdata

    # download the timezone data files
    wget 'ftp://ftp.iana.org/tz/tzdata-latest.tar.gz'
    tar -xvzf tzdata-latest.tar.gz -C priv/tzdata

    # remove a few troublesome files
    cd priv/tzdata
    rm *.sh *.gz *.tab factory Makefile

    # build and run ezic
    cd -
    make all run
    1> application:start(ezic).
    2> ezic:localtime("Australia/Adelaide").



License
-------

This project is in the [public domain](http://en.wikipedia.org/wiki/Public_Domain). Anyone is free to copy, modify, publish, use, compile, sell, or distribute the original code, either in source code form or as a compiled binary, for any purpose, commercial or non-commercial, and by any means.



Contributed Code
----------------

To maintain ezic's status as a public domain work, all contributions must also be dedicated to the public domain.



Acknowledgements
----------------

A big thanks to ezic's contributors:

 * [lsowen](https://github.com/lsowen)
 * [jrgdiz](https://github.com/jrgdiz)
 * [arkdro](https://github.com/arkdro)
 * [amasyura](https://github.com/amasyura)

The decision to release this into the public domain was inspired by the (anti-) license of the [SQLite project](http://www.sqlite.org/copyright.html).



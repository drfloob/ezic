ezic
====
ezic is a set of erlang utilities for the Olsen timezone database files.

see the [tz database](http://www.twinsun.com/tz/tz-link.htm) page for more information.


Usage
-----
  
    # download the timezone data files
    wget 'ftp://elsie.nci.nih.gov/pub/tzdata*.tar.gz'
    tar -xvzf tzdata*.tar.gz -C /path/to/tzdata

    # remove a few troublesome files
    cd /path/to/tzdata
    rm *.sh *.tab factory
  
    # build and run ezic
    make all run
    1> ezic:load("/path/to/tzdata").
    2> ezic:localtime("Australia/Adelaide").


API
---

 * `ezic:load(Folder)`
 * `ezic:localtime(TimeZone) -> datetime()`
 * `ezic:time(datetime(), TimeZone) -> datetime()`
 * `ezic:time2time(datetime(), TimeZoneFrom, TimeZoneTo) -> datetime()`


Purpose
-------

The [recommended way](http://www.erlang.org/pipermail/erlang-questions/2006-December/024291.html) of handling timezones in erlang involves setting the system environment variable `TZ` to the desired timezone, then calling erlang time functions. This technique is probably sufficient for many purposes, but it has a few key issues:

 * it only works on *nix systems,
 * it depends on your system's timezone data (preventing customization), and
 * you can create race conditions if you aren't careful

A pure erlang solution avoids all of these pitfalls, hence this project.

License
-------

This project is in the public domain.

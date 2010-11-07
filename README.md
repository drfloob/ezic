ezic
====
ezic is a set of erlang utilities for the Olsen timezone database files. See the [tz database](http://www.twinsun.com/tz/tz-link.htm) page for more information.



Status
------

2010.11.07

 * The time comparisons don't consider time relativity, so they're plain wrong. Within at least 24 hours of timechange or zone change, the results cannot be trusted.

2010.11.04

 * ezic is "apparently functional". It has been tested very lightly, a bit by unit tests, but mostly by inspection. It's probably not fit for production use.


License
-------

I have dedicated this work to the [public domain](http://en.wikipedia.org/wiki/Public_Domain). Anyone is free to copy, modify, publish, use, compile, sell, or distribute the original code, either in source code form or as a compiled binary, for any purpose, commercial or non-commercial, and by any means.



Contributed Code
----------------

To maintain ezic's status as a public domain work, all contributions must also be dedicated to the public domain. 



API
---

 * `ezic:localtime(TimeZone) -> datetime()`
 * `ezic:utc_to_time(universal_datetime(), TimeZone) -> local_datetime()`
 * `ezic:utc_from_time(local_datetime(), TimeZone) -> universal_datetime()`
 * `ezic:zone_convert(from_datetime(), TimeZoneFrom, TimeZoneTo) -> to_datetime()`



Admin API
-------

 * `ezic:load("/path/to/tzdata") -> ok`



Setup
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




Purpose
-------

Erlang/OTP doesn't have a timezone library, so I decided to write one.

The [recommended way](http://www.erlang.org/pipermail/erlang-questions/2006-December/024291.html) of handling timezones in erlang involves setting the system environment variable `TZ` to the desired timezone, then calling erlang time functions. As far as I can tell, this technique has a few key issues:

 * it only works on *nix systems,
 * it depends on your system's timezone data (preventing custom timezone hackery), and
 * you can create race conditions if you aren't careful

ezic doesn't have these problems. ezic is, however, a young project with who-knows-how-many problems of its own ;)



Acknowledgements
----------------

I was inspired by the (anti-) license of the [SQLite project](http://www.sqlite.org/copyright.html)  in my decision to release this into the public domain.


-record(rule, {name, from, to, type, in, on, at, save, letters}).


-record(zone, {name, gmtoff, rules, format, until}).


-record(link, {from, to}).


% a flattened zone record, giving specific information for a specific time range.
-record(flatzone, {tzname, tzrule, from, to, gmtoff}).

#!/bin/bash -e

psql trac_ghc <<EOF
-- Number of ticket changes
WITH change_times AS (
    SELECT ticket_change.*, TIMESTAMP WITH TIME ZONE 'epoch' + time * INTERVAL '1 microsecond' AS real_time FROM ticket_change
),
ticket_times AS (
    SELECT TIMESTAMP WITH TIME ZONE 'epoch' + time * INTERVAL '1 microsecond' AS real_time FROM ticket
)
SELECT years.column1 AS year,
       changes.n AS changes,
       new_tickets.n AS new_tickets,
       closed_tickets.n AS closed_tickets,
       change_authors.n AS change_authors
FROM (VALUES (2004), (2005), (2006), (2007), (2008), (2009), (2010), (2011), (2012), (2013), (2014), (2015), (2016), (2017), (2018), (2018)) AS years

JOIN LATERAL (
    SELECT count(*) AS n
    FROM change_times
    WHERE EXTRACT(YEAR FROM real_time) = years.column1
) AS changes ON true

JOIN LATERAL (
    SELECT count(*) AS n
    FROM ticket_times
    WHERE EXTRACT(YEAR FROM real_time) = years.column1
) AS new_tickets ON true

JOIN LATERAL (
    SELECT count(x.*) AS n
    FROM (
        SELECT DISTINCT change_times.author
        FROM change_times
        WHERE EXTRACT(YEAR FROM real_time) = years.column1
    ) AS x
) AS change_authors ON true

JOIN LATERAL (
    SELECT count(x.*) as n
    FROM (
        SELECT *
        FROM change_times
        WHERE EXTRACT (YEAR FROM real_time) = years.column1
        AND field = 'status'
        AND newvalue = 'closed'
    ) AS x
) AS closed_tickets ON true
;
EOF



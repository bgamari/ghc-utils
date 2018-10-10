#!/usr/bin/psql

-- Column definitions
-- -------------------
--
-- changes:
--   How many ticket changes (e.g. comments, field changes)
--   occurred over the course of the year
--
-- new_tickets:
--   How many tickets were opened over the course of the year
--
-- closed_tickets:
--   How many tickets were moved to closed state over the
--   course of the year
--
-- change_authors:
--   How many distinct users changed tickets over the course of the year
--
-- open_tickets:
--   How many tickets were in open state at the beginning of the year


CONNECT trac_ghc

CREATE TEMPORARY VIEW change_times AS
SELECT ticket_change.*,
       TIMESTAMP WITH TIME ZONE 'epoch' + time * INTERVAL '1 microsecond' AS real_time
FROM ticket_change;

CREATE TEMPORARY VIEW ticket_times AS
SELECT ticket.*,
       TIMESTAMP WITH TIME ZONE 'epoch' + time * INTERVAL '1 microsecond' AS real_time
FROM ticket;


-- Per year
-- Number of ticket changes
SELECT years.column1 AS year,
       changes.n AS changes,
       new_tickets.n AS new_tickets,
       closed_tickets.n AS closed_tickets,
       change_authors.n AS change_authors,
       open_tickets.n AS open_tickets
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

JOIN LATERAL (
    WITH ranked_changes AS (
        SELECT m.*, row_number() OVER (PARTITION BY ticket ORDER BY real_time DESC) AS row_n
        FROM change_times AS m
        WHERE EXTRACT(YEAR FROM m.real_time) < years.column1
        AND field = 'status'
    ),
    last_statuses AS (
        SELECT *
        FROM ranked_changes
        WHERE ranked_changes.row_n = 1
    )
    SELECT count(*) AS n
    FROM ticket_times
    LEFT OUTER JOIN last_statuses ON last_statuses.ticket = ticket_times.id
    WHERE EXTRACT(YEAR FROM ticket_times.real_time) < years.column1
    AND (last_statuses.newvalue != 'closed' OR last_statuses.newvalue IS NULL)
) AS open_tickets ON true
;


-- Past 12 months
SELECT
    new_tickets.n AS new_tickets,
    closed_tickets.n AS closed_tickets,
    change_authors.n AS change_authors

FROM (
    SELECT count(*) AS n
    FROM ticket_times
    WHERE ticket_times.real_time > (now() - interval '1 year')
) AS new_tickets,

(
    SELECT count(*) AS n
    FROM change_times
    WHERE change_times.real_time > (now() - interval '1 year')
    AND field = 'status'
    AND newvalue = 'closed'
) AS closed_tickets,

(
    SELECT count(*) AS n
    FROM (
        SELECT DISTINCT author
        FROM change_times
        WHERE change_times.real_time > (now() - interval '1 year')
    ) AS x
) AS change_authors
;


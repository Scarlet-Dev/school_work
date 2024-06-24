DROP VIEW Reddit_Jan6_Agg;

CREATE VIEW Reddit_Jan6_Agg AS
        -- October 2020 Select
    SELECT * FROM
    (
        SELECT DISTINCT
            id, 
            author, 
            subreddit, 
            body,
            to_timestamp(created_utc)::date AS "created_on",
            row_number() OVER (PARTITION BY created_utc ORDER BY random()) AS row
        FROM "2020-10"
        WHERE 
            (author NOT LIKE '%[deleted]%' AND
                author NOT LIKE '%[deleted]%') AND
            (LOWER(body) LIKE ANY(SELECT * FROM jan6_words))
            AND
            (LOWER(subreddit) LIKE 'republicans' OR
                LOWER(subreddit) LIKE 'conservative' OR
                LOWER(subreddit) LIKE 'uspolitics')
        ORDER BY author, subreddit, "created_on" ASC    
    ) main_oct
    WHERE row <= 75
    UNION 
    -- November 2020
    SELECT * FROM
    (
        SELECT DISTINCT
            id, 
            author, 
            subreddit, 
            body,
            to_timestamp(created_utc)::date AS "created_on",
            row_number() OVER (PARTITION BY created_utc ORDER BY random()) AS row
        FROM "2020-11"
        WHERE 
            (author NOT LIKE '%[deleted]%' AND
                author NOT LIKE '%[deleted]%') AND
            (LOWER(body) LIKE '%donald%' AND
                LOWER(body) LIKE '%trump%' AND
                LOWER(body) LIKE '%election%') AND
            (LOWER(subreddit) LIKE 'republicans' OR
                LOWER(subreddit) LIKE 'conservative' OR
                LOWER(subreddit) LIKE 'uspolitics')
        ORDER BY author, subreddit, "created_on" ASC    
    ) main_nov
    WHERE row <= 75
    UNION
    -- December 2020
    SELECT * FROM
    (
        SELECT DISTINCT
            id, 
            author, 
            subreddit, 
            body,
            to_timestamp(created_utc)::date AS "created_on",
            row_number() OVER (PARTITION BY created_utc ORDER BY random()) AS row
        FROM "2020-12"
        WHERE 
            (author NOT LIKE '%[deleted]%' AND
                author NOT LIKE '%[deleted]%') AND
            (LOWER(body) LIKE '%donald%' AND
                LOWER(body) LIKE '%trump%' AND
                LOWER(body) LIKE '%election%') AND
            (LOWER(subreddit) LIKE 'republicans' OR
                LOWER(subreddit) LIKE 'conservative' OR
                LOWER(subreddit) LIKE 'uspolitics')
        ORDER BY author, subreddit, "created_on" ASC    
    ) main_dec
    WHERE row <= 75
    UNION
    -- January 2021
    SELECT * FROM
    (
        SELECT DISTINCT
            id, 
            author, 
            subreddit, 
            body,
            to_timestamp(created_utc)::date AS "created_on",
            row_number() OVER (PARTITION BY created_utc ORDER BY random()) AS row
        FROM "2021-01"
        WHERE 
            (author NOT LIKE '%[deleted]%' AND
                author NOT LIKE '%[deleted]%') AND
            (LOWER(body) LIKE '%donald%' AND
                LOWER(body) LIKE '%trump%' AND
                LOWER(body) LIKE '%election%') AND
            (LOWER(subreddit) LIKE 'republicans' OR
                LOWER(subreddit) LIKE 'conservative' OR
                LOWER(subreddit) LIKE 'uspolitics')
        ORDER BY author, subreddit, "created_on" ASC    
    ) main_jan
    WHERE row <= 75
    UNION
    -- February 2021
    SELECT * FROM
    (
        SELECT DISTINCT
            id, 
            author, 
            subreddit, 
            body,
            to_timestamp(created_utc)::date AS "created_on",
            row_number() OVER (PARTITION BY created_utc ORDER BY random()) AS row
        FROM "2021-02"
        WHERE 
            (author NOT LIKE '%[deleted]%' AND
                author NOT LIKE '%[deleted]%') AND
            (LOWER(body) LIKE '%donald%' AND
                LOWER(body) LIKE '%trump%' AND
                LOWER(body) LIKE '%election%') AND
            (LOWER(subreddit) LIKE 'republicans' OR
                LOWER(subreddit) LIKE 'conservative' OR
                LOWER(subreddit) LIKE 'uspolitics')
        ORDER BY author, subreddit, "created_on" ASC    
    ) main_feb
    WHERE row <= 75
    UNION
    -- March 2021
    SELECT * FROM
    (
        SELECT DISTINCT
            id, 
            author, 
            subreddit, 
            body,
            to_timestamp(created_utc)::date AS "created_on",
            row_number() OVER (PARTITION BY created_utc ORDER BY random()) AS row
        FROM "2021-03"
        WHERE 
            (author NOT LIKE '%[deleted]%' AND
                author NOT LIKE '%[deleted]%') AND
            (LOWER(body) LIKE '%donald%' AND
                LOWER(body) LIKE '%trump%' AND
                LOWER(body) LIKE '%election%') AND
            (LOWER(subreddit) LIKE 'republicans' OR
                LOWER(subreddit) LIKE 'conservative' OR
                LOWER(subreddit) LIKE 'uspolitics')
        ORDER BY author, subreddit, "created_on" ASC    
    ) main_mar
    WHERE row <= 75
    GROUP BY "id", "row", "author", "subreddit", "body", "created_on";
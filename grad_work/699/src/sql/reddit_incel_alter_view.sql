DROP VIEW reddit_2020_Aggr;

CREATE VIEW reddit_2020_Aggr AS

  -- January 2020
  SELECT * FROM 
  (
    SELECT 
      DISTINCT id, 
      author, 
      subreddit, 
      subreddit_type, 
      body, 
      score, 
      controversiality, 
      to_timestamp(created_utc):: date AS "created_on", 
      row_number() OVER (
        PARTITION BY created_utc 
        ORDER BY 
          random()
      ) AS row 
    FROM 
      "2020-01"
    WHERE 
        (author NOT LIKE '%[deleted]%' AND body NOT LIKE '%[deleted]%')
        AND
        (LOWER(body) LIKE ANY(SELECT unnest(words) FROM ig_array))
        AND 
        (LOWER(subreddit) LIKE ANY(SELECT names from targetted_subreddits))
        AND
        controversiality > 0
    ORDER BY 
      author, 
      subreddit, 
      "created_on" ASC
    LIMIT 50000
  ) main_jan --
  WHERE 
  row <= 25 
  UNION ALL 
  -- February 2020 
  SELECT * FROM
  (
    SELECT 
      DISTINCT id, 
      author, 
      subreddit, 
      subreddit_type, 
      body, 
      score, 
      controversiality, 
      to_timestamp(created_utc):: date AS "created_on", 
      row_number() OVER (
        PARTITION BY created_utc 
        ORDER BY 
          random()
      ) AS row 
    FROM 
      "2020-02" 
    WHERE 
      (
        author NOT LIKE '%[deleted]%' 
        AND body NOT LIKE '%[deleted]%'
      )  AND
        (LOWER(body) LIKE ANY(SELECT '' || unnest(words) || '' FROM ig_array))
        AND 
        (LOWER(subreddit) LIKE ANY(SELECT names from targetted_subreddits))
        AND
        controversiality > 0
    ORDER BY 
      author, 
      subreddit, 
      "created_on" ASC
      LIMIT 50000
  ) main_feb --
  WHERE 
  row <= 25 
  UNION ALL 
  -- March 2020
  SELECT * FROM
  (
    SELECT 
      DISTINCT id, 
      author, 
      subreddit, 
      subreddit_type, 
      body, 
      score, 
      controversiality, 
      to_timestamp(created_utc):: date AS "created_on", 
      row_number() OVER (
        PARTITION BY created_utc 
        ORDER BY 
          random()
      ) AS row
    FROM 
      "2020-03"
    WHERE 
      (
        author NOT LIKE '%[deleted]%' 
        AND body NOT LIKE '%[deleted]%'
      )  AND
          (LOWER(body) LIKE ANY(SELECT '' || unnest(words) || '' FROM ig_array))
          AND 
          (LOWER(subreddit) LIKE ANY(SELECT names from targetted_subreddits))
          AND
          controversiality > 0
      ORDER BY 
      author, 
      subreddit, 
      "created_on" ASC
      LIMIT 50000
  ) main_mar --
  WHERE 
  row <= 25 
  UNION ALL 
  -- April 2020
  SELECT * FROM
  (
    SELECT 
      DISTINCT id, 
      author, 
      subreddit, 
      subreddit_type, 
      body, 
      score, 
      controversiality, 
      to_timestamp(created_utc):: date AS "created_on", 
      row_number() OVER (
        PARTITION BY created_utc 
        ORDER BY 
          random()
      ) AS row 
    FROM 
      "2020-04" 
    WHERE 
      (
        author NOT LIKE '%[deleted]%' 
        AND body NOT LIKE '%[deleted]%'
      )  AND
        (LOWER(body) LIKE ANY(SELECT '' || unnest(words) || '' FROM ig_array))
        AND 
        (LOWER(subreddit) LIKE ANY(SELECT names from targetted_subreddits))
        AND
        controversiality > 0
    ORDER BY 
      author, 
      subreddit, 
      "created_on" ASC
    LIMIT 50000
  ) main_apr --
  WHERE 
  row <= 25 
  UNION ALL 
  -- May 2020
  SELECT * FROM
  (
    SELECT 
      DISTINCT id, 
      author, 
      subreddit, 
      subreddit_type, 
      body, 
      score, 
      controversiality, 
      to_timestamp(created_utc):: date AS "created_on", 
      row_number() OVER (
        PARTITION BY created_utc 
        ORDER BY 
          random()
      ) AS row 
    FROM 
      "2020-05" 
    WHERE 
      (
        author NOT LIKE '%[deleted]%' 
        AND body NOT LIKE '%[deleted]%'
      )  AND
        (LOWER(body) LIKE ANY(SELECT '' || unnest(words) || '' FROM ig_array))
        AND 
        (LOWER(subreddit) LIKE ANY(SELECT names from targetted_subreddits))
        AND
        controversiality > 0
    ORDER BY 
      author, 
      subreddit, 
      "created_on" ASC
    LIMIT 50000
  ) main_may --
  WHERE 
  row <= 25 
  UNION ALL 
  -- June 2020
  SELECT * FROM
  (
    SELECT 
      DISTINCT id, 
      author, 
      subreddit, 
      subreddit_type, 
      body, 
      score, 
      controversiality, 
      to_timestamp(created_utc):: date AS "created_on", 
      row_number() OVER (
        PARTITION BY created_utc 
        ORDER BY 
          random()
      ) AS row 
    FROM 
      "2020-06" 
    WHERE 
      (
        author NOT LIKE '%[deleted]%' 
        AND body NOT LIKE '%[deleted]%'
      )  AND
        (LOWER(body) LIKE ANY(SELECT '' || unnest(words) || '' FROM ig_array))
        AND 
        (LOWER(subreddit) LIKE ANY(SELECT names from targetted_subreddits))
        AND
        controversiality > 0
    ORDER BY 
      author, 
      subreddit, 
      "created_on" ASC
    LIMIT 50000
  ) main_jun --
  WHERE 
  row <= 25 
  UNION ALL
  -- July 2020
  SELECT * FROM
  (
    SELECT 
      DISTINCT id, 
      author, 
      subreddit, 
      subreddit_type, 
      body, 
      score, 
      controversiality, 
      to_timestamp(created_utc):: date AS "created_on", 
      row_number() OVER (
        PARTITION BY created_utc 
        ORDER BY 
          random()
      ) AS row 
    FROM 
      "2020-07" 
    WHERE 
      (
        author NOT LIKE '%[deleted]%' 
        AND body NOT LIKE '%[deleted]%'
      )  AND
        (LOWER(body) LIKE ANY(SELECT '' || unnest(words) || '' FROM ig_array))
        AND 
        (LOWER(subreddit) LIKE ANY(SELECT names from targetted_subreddits))
        AND
        controversiality > 0
    ORDER BY 
      author, 
      subreddit, 
      "created_on" ASC
    LIMIT 50000
  ) main_jul --
  WHERE 
  row <= 25 
  UNION ALL 
  -- August 2020
  SELECT * FROM
  (
    SELECT 
      DISTINCT id, 
      author, 
      subreddit, 
      subreddit_type, 
      body, 
      score, 
      controversiality, 
      to_timestamp(created_utc):: date AS "created_on", 
      row_number() OVER (
        PARTITION BY created_utc 
        ORDER BY 
          random()
      ) AS row 
    FROM 
      "2020-08" 
    WHERE 
      (
        author NOT LIKE '%[deleted]%' 
        AND body NOT LIKE '%[deleted]%'
      )  AND
        (LOWER(body) LIKE ANY(SELECT '' || unnest(words) || '' FROM ig_array))
        AND 
        (LOWER(subreddit) LIKE ANY(SELECT names from targetted_subreddits))
        AND
        controversiality > 0
    ORDER BY 
      author, 
      subreddit, 
      "created_on" ASC
    LIMIT 50000
  ) main_aug --
  WHERE 
  row <= 25 
  UNION ALL 
  -- September 2020
    SELECT * FROM
  (
    SELECT 
      DISTINCT id, 
      author, 
      subreddit, 
      subreddit_type, 
      body, 
      score, 
      controversiality, 
      to_timestamp(created_utc):: date AS "created_on", 
      row_number() OVER (
        PARTITION BY created_utc 
        ORDER BY 
          random()
      ) AS row 
    FROM 
      "2020-09" 
    WHERE 
      (
        author NOT LIKE '%[deleted]%' 
        AND body NOT LIKE '%[deleted]%'
      )  AND
        (LOWER(body) LIKE ANY(SELECT '' || unnest(words) || '' FROM ig_array))
        AND 
        (LOWER(subreddit) LIKE ANY(SELECT names from targetted_subreddits))
        AND
        controversiality > 0
    ORDER BY 
      author, 
      subreddit, 
      "created_on" ASC
    LIMIT 50000
  ) main_sep --
  WHERE 
  row <= 25 
  UNION ALL 
  -- October 2020
    SELECT * FROM
  (
    SELECT 
      DISTINCT id, 
      author, 
      subreddit, 
      subreddit_type, 
      body, 
      score, 
      controversiality, 
      to_timestamp(created_utc):: date AS "created_on", 
      row_number() OVER (
        PARTITION BY created_utc 
        ORDER BY 
          random()
      ) AS row 
    FROM 
      "2020-10" 
    WHERE 
      (
        author NOT LIKE '%[deleted]%' 
        AND body NOT LIKE '%[deleted]%'
      )  AND
        (LOWER(body) LIKE ANY(SELECT '' || unnest(words) || '' FROM ig_array))
        AND 
        (LOWER(subreddit) LIKE ANY(SELECT names from targetted_subreddits))
        AND
        controversiality > 0
    ORDER BY 
      author, 
      subreddit, 
      "created_on" ASC
    LIMIT 50000
  ) main_oct --
  WHERE 
  row <= 25 
  UNION ALL 
  -- November 2020
  SELECT * FROM
  (
    SELECT 
      DISTINCT id, 
      author, 
      subreddit, 
      subreddit_type, 
      body, 
      score, 
      controversiality, 
      to_timestamp(created_utc):: date AS "created_on", 
      row_number() OVER (
        PARTITION BY created_utc 
        ORDER BY 
          random()
      ) AS row 
    FROM 
      "2020-11" 
    WHERE 
      (
        author NOT LIKE '%[deleted]%' 
        AND body NOT LIKE '%[deleted]%'
      )  AND
        (LOWER(body) LIKE ANY(SELECT '' || unnest(words) || '' FROM ig_array))
        AND 
        (LOWER(subreddit) LIKE ANY(SELECT names from targetted_subreddits))
        AND
        controversiality > 0
    ORDER BY 
      author, 
      subreddit, 
      "created_on" ASC
    LIMIT 50000
  ) main_nov --
  WHERE 
  row <= 25 
  UNION ALL 
  -- December 2020
  SELECT * FROM
  (
    SELECT 
      DISTINCT id, 
      author, 
      subreddit, 
      subreddit_type, 
      body, 
      score, 
      controversiality, 
      to_timestamp(created_utc):: date AS "created_on", 
      row_number() OVER (
        PARTITION BY created_utc 
        ORDER BY 
          random()
      ) AS row 
    FROM 
      "2020-12" 
    WHERE 
      (
        author NOT LIKE '%[deleted]%' 
        AND body NOT LIKE '%[deleted]%'
      )  AND
        (LOWER(body) LIKE ANY(SELECT '' || unnest(words) || '' FROM ig_array))
        AND 
        (LOWER(subreddit) LIKE ANY(SELECT names from targetted_subreddits))
        AND
        controversiality > 0
    ORDER BY 
      author, 
      subreddit, 
      "created_on" ASC
    LIMIT 50000
  ) main_dec --
  WHERE 
  row <= 25 
GROUP BY 
"id", 
"row", 
"author", 
"subreddit", 
"subreddit_type",
"body", 
"score", 
"controversiality", 
"created_on";


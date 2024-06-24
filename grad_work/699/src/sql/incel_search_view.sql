CREATE VIEW Reddit_2020_Aggr AS
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
      (
        author NOT LIKE '%[deleted]%' 
        AND body NOT LIKE '%[deleted]%'
      ) 
    ORDER BY 
      author, 
      subreddit, 
      "created_on" ASC
  ) main_jan --
WHERE 
  row <= 200 
UNION 
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
      ) 
    ORDER BY 
      author, 
      subreddit, 
      "created_on" ASC
  ) main_feb --
WHERE 
  row <= 200 
UNION 
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
      ) 
    ORDER BY 
      author, 
      subreddit, 
      "created_on" ASC
  ) main_mar --
WHERE 
  row <= 200 
UNION 
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
      ) 
    ORDER BY 
      author, 
      subreddit, 
      "created_on" ASC
  ) main_apr --
WHERE 
  row <= 200 
UNION 
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
      ) 
    ORDER BY 
      author, 
      subreddit, 
      "created_on" ASC
  ) main_may --
WHERE 
  row <= 200 
UNION 
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
      ) 
    ORDER BY 
      author, 
      subreddit, 
      "created_on" ASC
  ) main_jun --
WHERE 
  row <= 200 
UNION
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
      ) 
    ORDER BY 
      author, 
      subreddit, 
      "created_on" ASC
  ) main_jul --
WHERE 
  row <= 200 
UNION 
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
      ) 
    ORDER BY 
      author, 
      subreddit, 
      "created_on" ASC
  ) main_aug --
WHERE 
  row <= 200 
UNION 
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
      ) 
    ORDER BY 
      author, 
      subreddit, 
      "created_on" ASC
  ) main_sep --
WHERE 
  row <= 200 
UNION 
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
      ) 
    ORDER BY 
      author, 
      subreddit, 
      "created_on" ASC
  ) main_oct --
WHERE 
  row <= 200 
UNION 
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
      ) 
    ORDER BY 
      author, 
      subreddit, 
      "created_on" ASC
  ) main_nov --
WHERE 
  row <= 200 
UNION 
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
      ) 
    ORDER BY 
      author, 
      subreddit, 
      "created_on" ASC
  ) main_dec --
WHERE 
  row <= 200 
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
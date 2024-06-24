CREATE TABLE 
IncelGlossary (
    Word varchar,
    "Definition" varchar,
    "Cleaned Text" varchar,
    Sentiment varchar(10),
    Score numeric
);

COPY IncelGlossary (

    Word, 
    "Definition",
    "Cleaned Text",
    Sentiment,
    Score
  )
FROM '/home/arkane/hu_lectures/grad_work/699/incelDF.csv' WITH CSV HEADER;

select * from IncelGlossary limit 10;
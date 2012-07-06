-- create column and fill it based on value from another.
ALTER TABLE first_pain_icd_primary ADD COLUMN genderBin INT(5) NOT NULL;
UPDATE first_pain_icd_primary SET genderBin = CASE WHEN gender = "M" THEN -1 ELSE 1 END

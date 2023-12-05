-- Table: public.education

-- DROP TABLE IF EXISTS public.education;

CREATE TABLE IF NOT EXISTS public.education
(
    employment_education_id text COLLATE pg_catalog."default",
    enrollment_id text COLLATE pg_catalog."default",
    personal_id text COLLATE pg_catalog."default",
    information_date date,
    last_grade_completed text COLLATE pg_catalog."default",
    school_status text COLLATE pg_catalog."default",
    data_collection_stage text COLLATE pg_catalog."default",
    date_updated timestamp with time zone,
    organization_id integer
)

TABLESPACE pg_default;

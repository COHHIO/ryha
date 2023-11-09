-- Table: public.employment

-- DROP TABLE IF EXISTS public.employment;

CREATE TABLE IF NOT EXISTS public.employment
(
    employment_education_id text COLLATE pg_catalog."default",
    enrollment_id text COLLATE pg_catalog."default",
    personal_id text COLLATE pg_catalog."default",
    information_date date,
    employed text COLLATE pg_catalog."default",
    employment_type text COLLATE pg_catalog."default",
    not_employed_reason text COLLATE pg_catalog."default",
    data_collection_stage text COLLATE pg_catalog."default",
    date_updated timestamp with time zone,
    organization_id integer
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.employment
    OWNER to cohhiodbadmin;
-- Table: public.disabilities

-- DROP TABLE IF EXISTS public.disabilities;

CREATE TABLE IF NOT EXISTS public.disabilities
(
    disabilities_id text COLLATE pg_catalog."default",
    enrollment_id text COLLATE pg_catalog."default",
    personal_id text COLLATE pg_catalog."default",
    information_date date,
    disability_type text COLLATE pg_catalog."default",
    disability_response text COLLATE pg_catalog."default",
    data_collection_stage text COLLATE pg_catalog."default",
    date_updated timestamp with time zone,
    organization_id integer
)

TABLESPACE pg_default;

-- Table: public.domestic_violence

-- DROP TABLE IF EXISTS public.domestic_violence;

CREATE TABLE IF NOT EXISTS public.domestic_violence
(
    health_and_dv_id text COLLATE pg_catalog."default",
    enrollment_id text COLLATE pg_catalog."default",
    personal_id text COLLATE pg_catalog."default",
    information_date date,
    domestic_violence_survivor text COLLATE pg_catalog."default",
    when_occurred text COLLATE pg_catalog."default",
    currently_fleeing text COLLATE pg_catalog."default",
    data_collection_stage text COLLATE pg_catalog."default",
    date_updated timestamp with time zone,
    organization_id integer
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.domestic_violence
    OWNER to cohhiodbadmin;

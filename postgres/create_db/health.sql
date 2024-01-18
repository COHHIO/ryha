-- Table: public.health

-- DROP TABLE IF EXISTS public.health;

CREATE TABLE IF NOT EXISTS public.health
(
    health_and_dv_id text COLLATE pg_catalog."default",
    enrollment_id text COLLATE pg_catalog."default",
    personal_id text COLLATE pg_catalog."default",
    information_date date,
    general_health_status text COLLATE pg_catalog."default",
    dental_health_status text COLLATE pg_catalog."default",
    mental_health_status text COLLATE pg_catalog."default",
    pregnancy_status text COLLATE pg_catalog."default",
    data_collection_stage text COLLATE pg_catalog."default",
    date_updated timestamp with time zone,
    organization_id integer
)

TABLESPACE pg_default;

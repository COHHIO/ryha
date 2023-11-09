-- Table: public.living

-- DROP TABLE IF EXISTS public.living;

CREATE TABLE IF NOT EXISTS public.living
(
    current_living_sit_id text COLLATE pg_catalog."default",
    enrollment_id text COLLATE pg_catalog."default",
    personal_id text COLLATE pg_catalog."default",
    information_date date,
    current_living_situation text COLLATE pg_catalog."default",
    leave_situation14days text COLLATE pg_catalog."default",
    date_updated timestamp with time zone,
    organization_id integer
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.living
    OWNER to cohhiodbadmin;
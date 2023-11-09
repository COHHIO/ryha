-- Table: public.services

-- DROP TABLE IF EXISTS public.services;

CREATE TABLE IF NOT EXISTS public.services
(
    services_id text COLLATE pg_catalog."default",
    enrollment_id text COLLATE pg_catalog."default",
    personal_id text COLLATE pg_catalog."default",
    date_provided date,
    type_provided text COLLATE pg_catalog."default",
    referral_outcome text COLLATE pg_catalog."default",
    date_updated timestamp with time zone,
    organization_id integer
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.services
    OWNER to cohhiodbadmin;
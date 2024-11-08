-- Table: public.client

-- DROP TABLE IF EXISTS public.client;

CREATE TABLE IF NOT EXISTS public.project_coc
(
    project_coc_id text COLLATE pg_catalog."default",
    orig_project_id text COLLATE pg_catalog."default",
    coc_code text COLLATE pg_catalog."default",
    geocode text COLLATE pg_catalog."default",
    county text COLLATE pg_catalog."default",
    address1 text COLLATE pg_catalog."default",
    address2 text COLLATE pg_catalog."default",
    city text COLLATE pg_catalog."default",
    state text COLLATE pg_catalog."default",
    zip text COLLATE pg_catalog."default",
    geography_type text COLLATE pg_catalog."default",
    date_updated date,
    project_id integer,
    organization_id integer
)

TABLESPACE pg_default;

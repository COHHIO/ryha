-- Table: public.funder

-- DROP TABLE IF EXISTS public.funder;

CREATE TABLE IF NOT EXISTS public.funder
(
    orig_project_id text COLLATE pg_catalog."default",
    funder text COLLATE pg_catalog."default",
    other_funder text COLLATE pg_catalog."default",
    project_id integer,
    organization_id integer
)

TABLESPACE pg_default;

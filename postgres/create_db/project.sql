-- Table: public.project

-- DROP TABLE IF EXISTS public.project;

CREATE TABLE IF NOT EXISTS public.project
(
    orig_project_id text COLLATE pg_catalog."default",
    organization_id integer,
    project_name text COLLATE pg_catalog."default",
    project_type text COLLATE pg_catalog."default",
    operating_start_date date,
    project_id integer
)

TABLESPACE pg_default;

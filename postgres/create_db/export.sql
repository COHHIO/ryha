-- Table: public.export

-- DROP TABLE IF EXISTS public.export;

CREATE TABLE IF NOT EXISTS public.export
(
    export_id text COLLATE pg_catalog."default",
    source_contact_first text COLLATE pg_catalog."default",
    source_contact_last text COLLATE pg_catalog."default",
    source_contact_email text COLLATE pg_catalog."default",
    export_start_date date,
    export_end_date date,
    software_name text COLLATE pg_catalog."default",
    organization_id integer
)

TABLESPACE pg_default;

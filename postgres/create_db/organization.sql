-- Table: public.organization

-- DROP TABLE IF EXISTS public.organization;

CREATE TABLE IF NOT EXISTS public.organization
(
    orig_organization_id text COLLATE pg_catalog."default",
    organization_name text COLLATE pg_catalog."default",
    organization_id integer
)

TABLESPACE pg_default;

-- Table: public.income

-- DROP TABLE IF EXISTS public.income;

CREATE TABLE IF NOT EXISTS public.income
(
    income_benefits_id text COLLATE pg_catalog."default",
    enrollment_id text COLLATE pg_catalog."default",
    personal_id text COLLATE pg_catalog."default",
    information_date date,
    income_from_any_source text COLLATE pg_catalog."default",
    total_monthly_income double precision,
    earned text COLLATE pg_catalog."default",
    earned_amount double precision,
    unemployment text COLLATE pg_catalog."default",
    unemployment_amount double precision,
    ssi text COLLATE pg_catalog."default",
    ssi_amount double precision,
    ssdi text COLLATE pg_catalog."default",
    ssdi_amount double precision,
    va_disability_service text COLLATE pg_catalog."default",
    va_disability_service_amount double precision,
    va_disability_non_service text COLLATE pg_catalog."default",
    va_disability_non_service_amount double precision,
    private_disability text COLLATE pg_catalog."default",
    private_disability_amount double precision,
    workers_comp text COLLATE pg_catalog."default",
    workers_comp_amount double precision,
    tanf text COLLATE pg_catalog."default",
    tanf_amount double precision,
    ga text COLLATE pg_catalog."default",
    ga_amount double precision,
    soc_sec_retirement text COLLATE pg_catalog."default",
    soc_sec_retirement_amount double precision,
    pension text COLLATE pg_catalog."default",
    pension_amount double precision,
    child_support text COLLATE pg_catalog."default",
    child_support_amount double precision,
    alimony text COLLATE pg_catalog."default",
    alimony_amount double precision,
    other_income_source text COLLATE pg_catalog."default",
    other_income_amount double precision,
    other_income_source_identify text COLLATE pg_catalog."default",
    data_collection_stage text COLLATE pg_catalog."default",
    date_updated timestamp with time zone,
    organization_id integer
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.income
    OWNER to cohhiodbadmin;
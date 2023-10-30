-- Table: public.benefits

-- DROP TABLE IF EXISTS public.benefits;

CREATE TABLE IF NOT EXISTS public.benefits
(
    income_benefits_id text COLLATE pg_catalog."default",
    enrollment_id text COLLATE pg_catalog."default",
    personal_id text COLLATE pg_catalog."default",
    information_date date,
    benefits_from_any_source text COLLATE pg_catalog."default",
    snap text COLLATE pg_catalog."default",
    wic text COLLATE pg_catalog."default",
    tanf_child_care text COLLATE pg_catalog."default",
    tanf_transportation text COLLATE pg_catalog."default",
    other_tanf text COLLATE pg_catalog."default",
    other_benefits_source text COLLATE pg_catalog."default",
    other_benefits_source_identify text COLLATE pg_catalog."default",
    insurance_from_any_source text COLLATE pg_catalog."default",
    medicaid text COLLATE pg_catalog."default",
    no_medicaid_reason text COLLATE pg_catalog."default",
    medicare text COLLATE pg_catalog."default",
    no_medicare_reason text COLLATE pg_catalog."default",
    schip text COLLATE pg_catalog."default",
    no_schip_reason text COLLATE pg_catalog."default",
    vha_services_ha text COLLATE pg_catalog."default",
    no_vha_reason_ha text COLLATE pg_catalog."default",
    employer_provided text COLLATE pg_catalog."default",
    no_employer_provided_reason text COLLATE pg_catalog."default",
    cobra text COLLATE pg_catalog."default",
    no_cobra_reason text COLLATE pg_catalog."default",
    private_pay text COLLATE pg_catalog."default",
    no_private_pay_reason text COLLATE pg_catalog."default",
    state_health_ins text COLLATE pg_catalog."default",
    no_state_health_ins_reason text COLLATE pg_catalog."default",
    indian_health_services text COLLATE pg_catalog."default",
    no_indian_health_services_reason text COLLATE pg_catalog."default",
    other_insurance text COLLATE pg_catalog."default",
    other_insurance_identify text COLLATE pg_catalog."default",
    data_collection_stage text COLLATE pg_catalog."default",
    date_updated timestamp with time zone,
    organization_id integer
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.benefits
    OWNER to cohhiodbadmin;

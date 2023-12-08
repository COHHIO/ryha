-- Table: public.enrollment

-- DROP TABLE IF EXISTS public.enrollment;

CREATE TABLE IF NOT EXISTS public.enrollment
(
    enrollment_id text COLLATE pg_catalog."default",
    personal_id text COLLATE pg_catalog."default",
    orig_project_id text COLLATE pg_catalog."default",
    entry_date date,
    household_id text COLLATE pg_catalog."default",
    relationship_to_ho_h text COLLATE pg_catalog."default",
    enrollment_co_c text COLLATE pg_catalog."default",
    living_situation text COLLATE pg_catalog."default",
    length_of_stay text COLLATE pg_catalog."default",
    los_under_threshold text COLLATE pg_catalog."default",
    previous_street_essh text COLLATE pg_catalog."default",
    date_to_street_essh date,
    times_homeless_past_three_years text COLLATE pg_catalog."default",
    months_homeless_past_three_years text COLLATE pg_catalog."default",
    disabling_condition text COLLATE pg_catalog."default",
    move_in_date date,
    referral_source text COLLATE pg_catalog."default",
    runaway_youth text COLLATE pg_catalog."default",
    sexual_orientation text COLLATE pg_catalog."default",
    sexual_orientation_other text COLLATE pg_catalog."default",
    former_ward_child_welfare text COLLATE pg_catalog."default",
    child_welfare_years text COLLATE pg_catalog."default",
    child_welfare_months integer,
    former_ward_juvenile_justice text COLLATE pg_catalog."default",
    juvenile_justice_years text COLLATE pg_catalog."default",
    juvenile_justice_months integer,
    unemployment_fam text COLLATE pg_catalog."default",
    mental_health_disorder_fam text COLLATE pg_catalog."default",
    physical_disability_fam text COLLATE pg_catalog."default",
    alcohol_drug_use_disorder_fam text COLLATE pg_catalog."default",
    insufficient_income text COLLATE pg_catalog."default",
    incarcerated_parent text COLLATE pg_catalog."default",
    date_updated timestamp with time zone,
    project_id integer,
    organization_id integer
)

TABLESPACE pg_default;

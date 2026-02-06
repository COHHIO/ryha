-- Table: public.client

-- DROP TABLE IF EXISTS public.client;

CREATE TABLE IF NOT EXISTS public.client
(
    personal_id text COLLATE pg_catalog."default",
    ssn text COLLATE pg_catalog."default",
    ssn_data_quality text COLLATE pg_catalog."default",
    dob date,
    dob_data_quality text COLLATE pg_catalog."default",
    sex text COLLATE pg_catalog."default",
    am_ind_ak_native text COLLATE pg_catalog."default",
    asian text COLLATE pg_catalog."default",
    black_af_american text COLLATE pg_catalog."default",
    hispanic_latinao text COLLATE pg_catalog."default",
    mid_east_n_african text COLLATE pg_catalog."default",
    native_hi_pacific text COLLATE pg_catalog."default",
    white text COLLATE pg_catalog."default",
    race_none text COLLATE pg_catalog."default",
    veteran_status text COLLATE pg_catalog."default",
    date_updated timestamp with time zone,
    organization_id integer
)

TABLESPACE pg_default;

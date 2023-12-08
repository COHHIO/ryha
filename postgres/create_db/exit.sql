-- Table: public.exit

-- DROP TABLE IF EXISTS public.exit;

CREATE TABLE IF NOT EXISTS public.exit
(
    exit_id text COLLATE pg_catalog."default",
    enrollment_id text COLLATE pg_catalog."default",
    personal_id text COLLATE pg_catalog."default",
    exit_date date,
    destination text COLLATE pg_catalog."default",
    other_destination text COLLATE pg_catalog."default",
    project_completion_status text COLLATE pg_catalog."default",
    exchange_for_sex text COLLATE pg_catalog."default",
    exchange_for_sex_past_three_months text COLLATE pg_catalog."default",
    count_of_exchange_for_sex text COLLATE pg_catalog."default",
    asked_or_forced_to_exchange_for_sex text COLLATE pg_catalog."default",
    asked_or_forced_to_exchange_for_sex_past_three_months text COLLATE pg_catalog."default",
    work_place_violence_threats text COLLATE pg_catalog."default",
    workplace_promise_difference text COLLATE pg_catalog."default",
    coerced_to_continue_work text COLLATE pg_catalog."default",
    labor_exploit_past_three_months text COLLATE pg_catalog."default",
    counseling_received text COLLATE pg_catalog."default",
    individual_counseling text COLLATE pg_catalog."default",
    family_counseling text COLLATE pg_catalog."default",
    group_counseling text COLLATE pg_catalog."default",
    session_count_at_exit integer,
    post_exit_counseling_plan text COLLATE pg_catalog."default",
    sessions_in_plan integer,
    destination_safe_client text COLLATE pg_catalog."default",
    destination_safe_worker text COLLATE pg_catalog."default",
    pos_adult_connections text COLLATE pg_catalog."default",
    pos_peer_connections text COLLATE pg_catalog."default",
    pos_community_connections text COLLATE pg_catalog."default",
    date_updated timestamp with time zone,
    organization_id integer
)

TABLESPACE pg_default;

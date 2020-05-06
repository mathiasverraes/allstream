\set ON_ERROR_STOP on

CREATE TABLE events
(
    partition_sequence_nr BIGSERIAL                                                   NOT NULL
        CONSTRAINT partition_sequence
            PRIMARY KEY,
    event_type            DOMAIN_EVENT_TYPE                                           NOT NULL,
    event_id              UUID                                                        NOT NULL,
    event_payload         JSONB                                                       NOT NULL,
    recorded_time         TIMESTAMP WITH TIME ZONE DEFAULT (now() AT TIME ZONE 'utc') NOT NULL
);

CREATE UNIQUE INDEX events_event_id_uindex
    ON events (event_id);

CREATE UNIQUE INDEX IF NOT EXISTS events_partition_sequence_nr_uindex
    ON events (partition_sequence_nr);

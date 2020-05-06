CREATE TABLE events
(
    partition_sequence_nr bigserial                                NOT NULL
        CONSTRAINT partition_sequence
            PRIMARY KEY,
    event_type            domain_event_type                        NOT NULL,
    event_id              uuid                                     NOT NULL,
    event_payload         jsonb                                    NOT NULL,
    recorded_time         timestamp WITH TIME ZONE DEFAULT (now()) NOT NULL
);

CREATE UNIQUE INDEX events_event_id_uindex
    ON events (event_id);

CREATE UNIQUE INDEX IF NOT EXISTS events_partition_sequence_nr_uindex
    ON events (partition_sequence_nr);


CREATE PROCEDURE append(_event_type domain_event_type, _event_id uuid,
                        _event_payload jsonb)
AS
$$
BEGIN
    INSERT INTO events (event_type, event_id, event_payload)
    VALUES (_event_type, _event_id, _event_payload);
END;
$$ LANGUAGE plpgsql;

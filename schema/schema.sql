\set ON_ERROR_STOP on

-- -------------------------------------------------------------------------------------------------
-- TYPES
-- -------------------------------------------------------------------------------------------------

CREATE DOMAIN __symbol AS varchar(255) NOT NULL CHECK (value ~ '^[[:alnum:]\_]{1,255}$');
CREATE DOMAIN sequence_nr AS integer NOT NULL CHECK (value > 0);
CREATE DOMAIN event_type AS __symbol;
CREATE DOMAIN stream_type AS __symbol;

-- -------------------------------------------------------------------------------------------------
-- EVENTS
-- -------------------------------------------------------------------------------------------------

CREATE TABLE events
(
    partition_seq bigserial                                NOT NULL
        CONSTRAINT partition_seq PRIMARY KEY,
    event_type    event_type                               NOT NULL,
    event_id      uuid                                     NOT NULL,
    event_payload jsonb                                    NOT NULL,
    recorded_time timestamp WITH TIME ZONE DEFAULT (now()) NOT NULL,
    stream_type   stream_type                              NOT NULL,
    stream_id     uuid                                     NOT NULL,
    stream_seq    sequence_nr                              NOT NULL,
    UNIQUE (stream_type, stream_id, stream_seq)
);

CREATE UNIQUE INDEX events_event_id_uindex ON events (event_id);

CREATE UNIQUE INDEX IF NOT EXISTS events_partition_sequence_nr_uindex ON events (partition_seq);

CREATE PROCEDURE append_to_stream
(
    _event_type          event_type
,   _event_id            uuid
,   _event_payload       jsonb
,   _stream_type         stream_type
,   _stream_id           uuid
,   _expected_stream_seq sequence_nr
) AS
$$
DECLARE
    is_monotonically_increasing bool;
BEGIN
    SELECT MAX(stream_seq) + 1 = _expected_stream_seq
    FROM events
    WHERE stream_type = _stream_type
      AND stream_id = _stream_id
    INTO is_monotonically_increasing;

    IF (NOT is_monotonically_increasing) THEN
        RAISE EXCEPTION USING ERRCODE = 'Stream_sequence_nr must be monotonically increasing';
    END IF;

    INSERT INTO events (event_type, event_id, event_payload, stream_type, stream_id, stream_seq)
    VALUES (_event_type, _event_id, _event_payload, _stream_type, _stream_id, _expected_stream_seq);
END;
$$ LANGUAGE plpgsql;


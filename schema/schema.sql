\set ON_ERROR_STOP on

-- -------------------------------------------------------------------------------------------------
-- TYPES
-- -------------------------------------------------------------------------------------------------

CREATE DOMAIN sequence_nr AS integer NOT NULL CHECK (value > 0);

CREATE DOMAIN symbol AS varchar(48) NOT NULL CHECK (value ~ '^[[:alnum:]\_-]{1,48}$');

CREATE DOMAIN domain_event_type AS varchar(255) NOT NULL CHECK (char_length(value) > 0);

-- -------------------------------------------------------------------------------------------------
-- EVENTS
-- -------------------------------------------------------------------------------------------------

CREATE TABLE events
(
    partition_sequence_nr bigserial                                NOT NULL
        CONSTRAINT partition_sequence PRIMARY KEY,
    event_type            domain_event_type                        NOT NULL,
    event_id              uuid                                     NOT NULL,
    event_payload         jsonb                                    NOT NULL,
    recorded_time         timestamp WITH TIME ZONE DEFAULT (now()) NOT NULL
);

CREATE UNIQUE INDEX events_event_id_uindex ON events (event_id);

CREATE UNIQUE INDEX IF NOT EXISTS events_partition_sequence_nr_uindex ON events (partition_sequence_nr);

CREATE PROCEDURE append
(
    _event_type domain_event_type, _event_id uuid, _event_payload jsonb
) AS
$$
BEGIN
    INSERT INTO events (event_type, event_id, event_payload)
    VALUES ( _event_type, _event_id,_event_payload);
END;
$$ LANGUAGE plpgsql;

-- -------------------------------------------------------------------------------------------------
-- STREAM TYPES
-- -------------------------------------------------------------------------------------------------

CREATE TABLE stream_types
(
    stream_type symbol NOT NULL
        CONSTRAINT streams_types_pk PRIMARY KEY
    /*
    lifecycle_start_events DOMAIN_EVENT_TYPE[] NOT NULL,
    lifecycle_end_events   DOMAIN_EVENT_TYPE[] NOT NULL
    */
);


CREATE PROCEDURE register_stream_type
(
    _stream_type symbol /*, _lifecycle_start_events VARCHAR(255)[], _lifecycle_end_events VARCHAR(255)[]*/) AS
$$

BEGIN
    INSERT INTO stream_types (stream_type/*, lifecycle_start_events, lifecycle_end_events*/)
    VALUES (_stream_type/*, _lifecycle_start_events, _lifecycle_end_events*/);
END;
$$ LANGUAGE plpgsql;

-- -------------------------------------------------------------------------------------------------
-- STREAMS
-- -------------------------------------------------------------------------------------------------


CREATE TABLE streams
(
    stream_id         uuid   NOT NULL
        CONSTRAINT streams_pk PRIMARY KEY,
    stream_type       symbol NOT NULL REFERENCES stream_types (stream_type),
    stream_table_name symbol NOT NULL
);



CREATE TABLE _template_for_stream_table
(
    stream_sequence_nr    bigserial NOT NULL PRIMARY KEY,
    partition_sequence_nr sequence_nr,
    event_id              uuid      NOT NULL
);



CREATE PROCEDURE register_new_stream
(
    _stream_type symbol, _stream_id uuid
) AS
$$
DECLARE
    _new_table_name varchar;
BEGIN
    SELECT substr(_stream_type || '_' || _stream_id, 0, 63) INTO _new_table_name;
    EXECUTE format('CREATE TABLE %I (LIKE _template_for_stream_table INCLUDING ALL)',
                   _new_table_name);
    INSERT INTO streams (stream_type, stream_id, stream_table_name)
    VALUES (_stream_type, _stream_id, _new_table_name);
END;
$$ LANGUAGE plpgsql;



CREATE PROCEDURE append_to_stream
(
    _stream_type                 symbol
,   _stream_id                   uuid
,   _expected_stream_sequence_nr sequence_nr
,   _event_type                  domain_event_type
,   _event_id                    uuid
,   _event_payload               jsonb
) AS
$$
DECLARE
    _stream_table_name     varchar(63);
    _partition_sequence_nr bigint;
BEGIN
    SELECT stream_table_name
    INTO STRICT _stream_table_name
    FROM streams
    WHERE stream_id = _stream_id
      AND stream_type = _stream_type;

    INSERT INTO events (event_type, event_id, event_payload)
    VALUES ( _event_type, _event_id, _event_payload)
    RETURNING partition_sequence_nr INTO STRICT _partition_sequence_nr;
    EXECUTE format(
        'INSERT INTO %1I (stream_sequence_nr, partition_sequence_nr, event_id) VALUES (%2L, %3L, %4L)',
        _stream_table_name, _expected_stream_sequence_nr, _partition_sequence_nr, _event_id);
END;
$$ LANGUAGE plpgsql;
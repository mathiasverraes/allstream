
CREATE TABLE stream_types
(
    stream_type            SYMBOL              NOT NULL
        CONSTRAINT streams_types_pk
            PRIMARY KEY,
    lifecycle_start_events DOMAIN_EVENT_TYPE[] NOT NULL,
    lifecycle_end_events   DOMAIN_EVENT_TYPE[] NOT NULL
);


CREATE FUNCTION register_new_stream_type(_stream_type SYMBOL, _lifecycle_start_events VARCHAR(255)[], _lifecycle_end_events VARCHAR(255)[])
    RETURNS INTEGER AS
$$

BEGIN
    INSERT INTO stream_types (stream_type, lifecycle_start_events, lifecycle_end_events)
    VALUES (_stream_type, _lifecycle_start_events, _lifecycle_end_events);
    RETURN 0;
END;
$$ LANGUAGE plpgsql;

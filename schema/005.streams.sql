\set ON_ERROR_STOP on

CREATE TABLE streams
(
    stream_id         uuid   NOT NULL
        CONSTRAINT streams_pk
            PRIMARY KEY,
    stream_type       symbol NOT NULL REFERENCES stream_types (stream_type),
    stream_table_name symbol NOT NULL
);



CREATE TABLE _template_for_stream_table
(
    stream_sequence_nr    bigserial NOT NULL PRIMARY KEY,
    partition_sequence_nr sequence_nr,
    event_id              uuid      NOT NULL
);



CREATE PROCEDURE register_new_stream(_stream_type symbol, _stream_id uuid) AS
$$
DECLARE
    _new_table_name varchar;
BEGIN
    SELECT substr(_stream_type || '_' || _stream_id, 0, 63)
    INTO _new_table_name;
    EXECUTE format(
        'CREATE TABLE %I (LIKE _template_for_stream_table INCLUDING ALL)',
        _new_table_name);
    INSERT INTO streams (stream_id, stream_type, stream_table_name)
    VALUES (_stream_id, _stream_type, _new_table_name);
END;
$$ LANGUAGE plpgsql;

/*
CREATE FUNCTION drop_unit(_unit_type SYMBOL, _unit_id UUID)
    RETURNS INTEGER AS
-- when a unit's lifecycle has ended, we get rid of it. This way the event doesn't belong to a unit anymore
$$
DECLARE
    old_table_name VARCHAR;
BEGIN
    SELECT _unit_type || '_' || _unit_id INTO old_table_name;
    EXECUTE format('DROP TABLE %I', old_table_name);
    DELETE FROM units WHERE unit_id = _unit_id AND unit_type = _unit_type;
    RETURN 0;
END;
$$ LANGUAGE plpgsql;
*/

/*
CREATE FUNCTION record_orphan_event(_event_type varchar(255), _event_id uuid
                                   , _event_payload jsonb)
    RETURNS integer AS
$$
BEGIN
    INSERT INTO events (event_type, event_id, event_payload)
    VALUES (_event_type, _event_id, _event_payload);
    RETURN 0;
END;
$$ LANGUAGE plpgsql;



CREATE FUNCTION record_unit_event(_unit_type symbol, _unit_id uuid
                                 , _expected_unit_sequence_nr sequence_nr
                                 , _event_type varchar(255), _event_id uuid
                                 , _event_payload jsonb)
    RETURNS integer AS
$$
DECLARE
    _unit_table_name       varchar(63);
    _partition_sequence_nr bigint;
BEGIN
    SELECT unit_table_name
    INTO STRICT _unit_table_name
    FROM units
    WHERE unit_id = _unit_id
      AND unit_type = _unit_type;
    INSERT INTO events (event_type, event_id, event_payload)
    VALUES (_event_type, _event_id, _event_payload)
    RETURNING partition_sequence_nr INTO STRICT _partition_sequence_nr;
    EXECUTE format(
        'INSERT INTO %1I (unit_sequence_nr, partition_sequence_nr, event_id) VALUES (%2L, %3L, %4L)',
        _unit_table_name, _expected_unit_sequence_nr,
        _partition_sequence_nr, _event_id);
    RETURN 0;
END;
$$ LANGUAGE plpgsql;

--
-- projecting a custom stream

CREATE TABLE template_for_custom_stream
(
    stream_sequence_nr bigserial NOT NULL
        CONSTRAINT stream_sequence PRIMARY KEY,
    event_id           uuid      NOT NULL
);

CREATE TABLE custom_streams
(
    stream_name    symbol                                                      NOT NULL
        CONSTRAINT custom_streams_pk
            PRIMARY KEY,
    event_types    symbol[],
    published_time timestamp WITH TIME ZONE DEFAULT (now() AT TIME ZONE 'utc') NOT NULL
);

CREATE FUNCTION create_stream(_stream_name symbol, _event_types symbol[])
    RETURNS integer AS
$$
BEGIN
    INSERT INTO custom_streams (stream_name, event_types)
    VALUES (_stream_name, _event_types);

    EXECUTE format(
        'CREATE TABLE %I (LIKE template_for_custom_stream INCLUDING ALL)',
        _stream_name);

    EXECUTE format(
        'INSERT INTO %1I (event_id)
        SELECT e.event_id
            FROM events e INNER JOIN custom_streams s
            ON e.event_type IN (SELECT (unnest(s.event_types)))
            WHERE S.stream_name = %2L
            ORDER BY e.partition_sequence_nr ASC',
        _stream_name, _stream_name);
    RETURN 0;
END;
$$ LANGUAGE plpgsql;


--
-- examples

CREATE FUNCTION run_examples()
    RETURNS integer AS
$$
BEGIN
    NOTIFY test, 'This is a test';

    PERFORM register_new_unit('Order', 'd68a596e-2994-11ea-81ea-2f63a58bd972',
                              '{"OrderWasCreated"}',
                              '{"OrderWasPlaced"}');

    PERFORM record_unit_event('Order', 'd68a596e-2994-11ea-81ea-2f63a58bd972',
                              1, 'OrderWasCreated',
                              '11111111-1111-1111-1111-111111111111', '{
              "orderId": "d68a596e-2994-11ea-81ea-2f63a58bd972"
            }');
    PERFORM record_orphan_event('CustomerHasChangedAddress',
                                '44444444-4444-4444-4444-444444444444', '{
              "customerId": "33333333-3333-3333-3333-333333333333",
              "address": "My Street 123, Brussels"
            }');
    PERFORM record_unit_event('Order', 'd68a596e-2994-11ea-81ea-2f63a58bd972',
                              2, 'ItemWasAdded',
                              '22222222-2222-2222-2222-222222222222', '{
              "orderId": "d68a596e-2994-11ea-81ea-2f63a58bd972",
              "price": 10
            }');
    PERFORM record_unit_event('Order', 'd68a596e-2994-11ea-81ea-2f63a58bd972',
                              3, 'ItemWasAdded',
                              '33333333-3333-3333-3333-333333333333', '{
              "orderId": "d68a596e-2994-11ea-81ea-2f63a58bd972",
              "price": 5
            }');
    PERFORM record_unit_event('Order', 'd68a596e-2994-11ea-81ea-2f63a58bd972',
                              4, 'OrderWasPlaced',
                              '88888888-8888-8888-8888-888888888888', '{
              "orderId": "d68a596e-2994-11ea-81ea-2f63a58bd972"
            }');

    PERFORM create_stream('all_order_events',
                          '{"OrderWasCreated", "ItemWasAdded", "OrderWasPlaced"}');

    PERFORM drop_unit('Order', 'd68a596e-2994-11ea-81ea-2f63a58bd972');

    RETURN 0;
END;
$$ LANGUAGE plpgsql;

SELECT run_examples();


 */
\set ON_ERROR_STOP on

CREATE DOMAIN sequence_nr AS INTEGER NOT NULL
    CHECK (value > 0);

CREATE DOMAIN symbol AS VARCHAR(48) NOT NULL
    CHECK (value ~ '^[[:alnum:]\_-]{1,48}$');

CREATE DOMAIN domain_event_type AS VARCHAR(255) NOT NULL
    CHECK (char_length(value) > 0);


CREATE DOMAIN sequence_nr AS integer NOT NULL
    CHECK (value > 0);

CREATE DOMAIN symbol AS varchar(48) NOT NULL
    CHECK (value ~ '^[[:alnum:]\_-]{1,48}$');

CREATE DOMAIN domain_event_type AS varchar(255) NOT NULL
    CHECK (char_length(value) > 0);

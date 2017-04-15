PRAGMA foreign_keys = off;
BEGIN TRANSACTION;

-- Table: obs
CREATE TABLE obs (
    timestamp   INTEGER,
    site        TEXT,
    sensor      TEXT,
    temperature DOUBLE,
    PRIMARY KEY (
        timestamp,
        site,
        sensor
    )
);

-- Index: obs_timestamp
CREATE INDEX obs_timestamp ON obs (
    timestamp
);

COMMIT TRANSACTION;
PRAGMA foreign_keys = on;

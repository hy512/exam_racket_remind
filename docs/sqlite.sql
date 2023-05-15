
CREATE TABLE IF NOT EXISTS rem_userwww (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user_uuid VARCHAR(32) NOT NULL,
    user_account VARCHAR(32) NOT NULL,
    user_name VARCHAR(64),
    last_login_time BIGINT,
    create_time BIGINT,
    update_time BIGINT,
    delete_time BIGINT,
    deleted TINYINT DEFAULT 0
);

CREATE UNIQUE INDEX IF NOT EXISTS rem_userwww_user_account ON rem_userwww (user_account);
CREATE UNIQUE INDEX IF NOT EXISTS rem_userwww_user_uuid ON rem_userwww (user_uuid);



CREATE TABLE IF NOT EXISTS rem_userwww_auth (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id INTEGER NOT NULL,
    user_uuid VARCHAR(32) NOT NULL,
    auth_type VARCHAR(32) NOT NULL,
    login_pass VARCHAR(256),
    create_time BIGINT,
    update_time BIGINT,
    delete_time BIGINT,
    deleted TINYINT DEFAULT 0
);

CREATE INDEX IF NOT EXISTS rem_userwww_auth_userid ON rem_userwww_auth (user_id);
CREATE INDEX IF NOT EXISTS rem_userwww_auth_useruuid ON rem_userwww_auth (user_uuid);


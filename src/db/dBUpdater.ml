open Database_version.DBVersions
open Config

let () =
  EzPGUpdater.main
    default_database
    ?host
    ?port
    ?user
    ?password
    ~upgrades
    ~downgrades

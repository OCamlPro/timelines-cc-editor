open DBVersions

let () =
  EzPGUpdater.main
    default_database
    ~upgrades
    ~downgrades

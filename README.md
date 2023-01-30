4C Evaluation Pipeline
----------------------

An OCurrent pipeline for the 4C evaluations. We use OBuilder to build and run
different commands for the pipeline. You need to use Linux for this to work
well.

You will also need to setup a snapshotting filesystem for OBuilder to store
build results. One possible backend is ZFS:

```
sudo apt install zfsutils-linux
truncate --size XG zfs.img
sudo zpool create obuilder-zfs $PWD/zfs.img
```

You can then run the pipeline with:

```
dune exec -- ./src/bin/main.exe --github-token-file=.token --slack .slack --store=zfs:obuilder-zfs
```


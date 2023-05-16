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

## Debugging and inspecting

The pipeline can be interfaced via the command line, and you can checkout a specific build.
You can have a [hoke](https://www.lrb.co.uk/the-paper/v06/n20/seamus-heaney/two-poems) around
to try and see what went wrong.

```
hoke -c path/to/file.cap list
```

This will show you all of the different builds and their tags along with a unique identifier along with
whether the succeeded or failed. 

```
hoke -c path/to/file.cap checkout <unique-id>
```

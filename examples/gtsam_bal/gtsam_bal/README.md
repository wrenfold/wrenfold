# gtsam_bal

This example implements a variation on the [SFMExample_bal](https://github.com/borglab/gtsam/blob/develop/examples/SFMExample_bal.cpp) example from the GTSAM repository. We create a symbolic implementation of `GeneralSFMFactor<SfmCamera,Point3>` in [gen_bundle_adjustment_factor.py](gen_bundle_adjustment_factor.py), and implement the same optimization from the original example.

## Running

To run this example, download one of the [BAL datasets](https://grail.cs.washington.edu/projects/bal/) and pass it to the `gtsam_bal` binary.

For example, assuming the `wrenfold-extra-examples` project has been built:
```bash
wget https://grail.cs.washington.edu/projects/bal/data/ladybug/problem-49-7776-pre.txt.bz2
bzip2 -d problem-49-7776-pre.txt.bz2
build/examples/gtsam_bal/gtsam_bal problem-49-7776-pre.txt
```

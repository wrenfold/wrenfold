# simple_bundle_adjuster

This example modifies the [simple_bundle_adjuster](https://github.com/ceres-solver/ceres-solver/blob/master/examples/simple_bundle_adjuster.cc) example from the ceres repository. We create a symbolic implementation of `SnavelyReprojectionError` in [gen_snavely_reprojection_error.py](gen_snavely_reprojection_error.py), and substitute it into the original example.

The source file `simple_bundle_adjuster.cc` is a copy of the original file from the google ceres repository (with a handful of modifications).

## Generating code

In order to run the code generator:

```bash
python snavely_peprojection_error.py <OUTPUT FILE>
```

## Running

To run this example, download one of the [BAL datasets](https://grail.cs.washington.edu/projects/bal/) and pass it to the `simple_bundle_adjuster` binary.

For example, assuming the repository has been built:
```bash
wget https://grail.cs.washington.edu/projects/bal/data/ladybug/problem-49-7776-pre.txt.bz2
bzip2 -d problem-49-7776-pre.txt.bz2
build/examples/ceres_simple_bundle_adjuster/simple_bundle_adjuster problem-49-7776-pre.txt
```

//! Copyright 2024 Gareth Cross
//!
//! The goal of this build script is to make it possible to run `cargo build` on the workspace while
//! iterating on rust unit tests. We check if cargo was _not_ invoked from cmake, and subsequently
//! invoke cmake ourselves on the targets required to emit generated code. Building from cmake is
//! the officially supported path, but this is a convenient option to have for quickly iterating on
//! a rust test.

use std::path::{Path, PathBuf};

/// Find our workspace root.
/// Credit to: https://stackoverflow.com/questions/43577885
fn get_workspace_root() -> PathBuf {
    let output = std::process::Command::new(env!("CARGO"))
        .arg("locate-project")
        .arg("--workspace")
        .arg("--message-format=plain")
        .output()
        .unwrap()
        .stdout;
    let cargo_path = Path::new(std::str::from_utf8(&output).unwrap().trim());
    cargo_path.parent().unwrap().to_path_buf()
}

/// Check if cargo was invoked from CMake. We use an environment variable to pass this information.
fn invoked_from_cmake() -> bool {
    match std::env::var("CARGO_INVOKED_FROM_CMAKE") {
        Ok(value) => value
            .parse::<i32>()
            .map(|value| value != 0)
            .unwrap_or(false),
        Err(_) => false,
    }
}

/// Recursively visit files in a directory.
fn recursively_visit_files<F>(dir: &Path, func: &F) -> std::io::Result<()>
where
    F: Fn(&Path),
{
    assert!(dir.is_dir());
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            recursively_visit_files(&path, func)?;
        } else {
            func(&path);
        }
    }
    Ok(())
}

/// True if this is a file extension that should trigger re-run.
/// This is an imperfect mechanism, but probably the best we can do (at least easily) here?
fn should_rerun_if_extension_changed(ext: &str) -> bool {
    let ext = ext.to_lowercase();
    ["cc", "h", "cpp", "hpp", "txt", "cmake", "py"]
        .into_iter()
        .any(|candidate| candidate == &ext)
}

/// Invoke cmake in order to generate rust source code required by tests.
/// I initially tried using the `cmake` crate here, but it does a bunch of undesirable things like
/// overriding the platform CXX_FLAGS, etc.
fn invoke_cmake() {
    // Make sure the build directory exists:
    let workspace_root = get_workspace_root();
    let build_directory = std::env::var("WF_CMAKE_BUILD_DIRECTORY")
        .map(PathBuf::from)
        .unwrap_or(workspace_root.join("build"));

    // If the CMake cache has not been created, let the user know.
    if !build_directory.join("CMakeCache.txt").exists() {
        panic!("The cmake configuration step needs to be run once prior to invoking cargo.");
    }

    // Get as many dependency files as we can here.
    // This won't catch the user deleting things in build, unfortunately.
    recursively_visit_files(&workspace_root.join("components"), &|path| {
        if path
            .extension()
            .and_then(|ext| ext.to_str())
            .map(|ext| should_rerun_if_extension_changed(ext))
            .unwrap_or(false)
        {
            println!("cargo:rerun-if-changed={}", path.display());
        }
    })
    .unwrap();

    println!(
        "cargo:rerun-if-changed={}",
        workspace_root.join("CMakeLists.txt").display()
    );

    // Run the build.
    // The `rust-generation` target is setup on the cmake side to include all relevant code
    // generation steps as dependencies.
    let mut command = std::process::Command::new("cmake");
    command
        .current_dir(&build_directory)
        .arg("--build")
        .arg(&build_directory)
        .arg("--target")
        .arg("wf_rust_generation");

    let output = command.output().unwrap();
    assert!(
        output.status.success(),
        "Failed to invoke cmake build step.\n\
        ** exit code: {:?}\n\
        ** command: {:?}\n\
        ** stdout:\n{}\n\
        ** stderr:\n{}",
        output.status.code(),
        command,
        std::str::from_utf8(&output.stdout).unwrap(),
        std::str::from_utf8(&output.stderr).unwrap()
    );
}

fn main() {
    if invoked_from_cmake() {
        // Do nothing if cmake ran this instance of cargo.
        return;
    }
    invoke_cmake();
}

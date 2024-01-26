fn main() {
    let target_os = std::env::var("CARGO_CFG_TARGET_OS").unwrap();
    match target_os.as_str() {
        "macos" => {
            println!("cargo:rustc-link-lib=framework=Accelerate");
        }
        "linux" | "windows" => {
            println!("cargo:rustc-link-arg=-lopenblas");

            // Need to specify the runtime path for openblas as well, since it could be in conda.
            let library = pkg_config::Config::new()
                .probe("openblas")
                .expect("Failed to find openblas");
            for path in &library.link_paths {
                println!("cargo:rustc-link-arg=-Wl,-rpath,{}", path.display());
            }
        }
        _ => {}
    }
}

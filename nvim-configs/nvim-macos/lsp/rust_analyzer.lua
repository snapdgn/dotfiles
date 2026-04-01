-- Rust analyzer configuration
return {
    settings = {
        ["rust-analyzer"] = {
            cargo = {
                features = "all",
            },
            checkOnSave = {
                command = "clippy",
            },
        },
    },
}

-- Rust analyzer configuration
return {
    settings = {
        ["rust-analyzer"] = {
            cargo = {
                features = "all",
            },
            checkOnSave = true,
            check = {
                command = "clippy",
            },
        },
    },
}

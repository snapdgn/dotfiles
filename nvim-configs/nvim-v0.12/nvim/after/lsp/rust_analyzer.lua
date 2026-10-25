-- rust-analyzer configuration
-- Every worktree of a repo builds into one shared <main-worktree>/target/rust-analyzer,
-- so a fresh worktree reuses compiled deps, build scripts and proc-macros.
return {
    cmd = function(dispatchers, config)
        local env = vim.tbl_extend("force", config.cmd_env or {}, {
            CARGO_TARGET_DIR = require("neovim.worktree").shared_target_dir(config.root_dir),
        })
        return vim.lsp.rpc.start({ "rust-analyzer" }, dispatchers, {
            cwd = config.cmd_cwd or config.root_dir,
            env = env,
            detached = config.detached,
        })
    end,
}

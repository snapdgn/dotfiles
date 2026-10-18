-- gopls configuration
-- -remote=auto: nvim talks to a shared gopls daemon that outlives nvim
-- (and is shared across nvim instances). The daemon exits after 24h with no clients.
return {
    cmd = { 'gopls', '-remote=auto', '-remote.listen.timeout=24h' },
}

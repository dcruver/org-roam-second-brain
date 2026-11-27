# Security policy

## Supported versions

Only the latest release gets fixes. If you are on an older version, update the package
before reporting.

## What counts

The package runs an HTTP server inside Emacs (`orsb-mcp-start`) that reads and writes
your notes, and it calls the embeddings endpoint you configure. Things worth reporting
privately include:

- a request that gets past `orsb-mcp-auth-token`, or reaches the server when it is
  bound to `127.0.0.1`
- a tool call that reads or writes a file outside the vault, the archive, the change
  journal or the embedding store
- a tool argument, a note's text or a `_schema.org` that makes Emacs evaluate code
- a way to apply a proposal that was not approved, or to write past a `strict` rule
- note text or the token ending up somewhere it should not, such as the request log

That the server cannot tell one caller from another, and that it speaks plain HTTP, are
known limits, described in the [FAQ](../docs/faq.md); they are not vulnerabilities.

## Reporting a vulnerability

Please do not open a public issue. Use
[Report a vulnerability](https://github.com/dcruver/org-roam-second-brain/security/advisories/new)
on the Security tab, or email donald@cruver.ai with "org-roam-second-brain security" in
the subject.

Say which version (`orsb-tools-version`), your Emacs and org-roam versions, and how to
reproduce it. You should hear back within a week. Once a fix is released, the advisory
will credit you unless you would rather it did not.

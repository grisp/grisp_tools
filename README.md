<p>
  <a href="https://hex.pm/packages/grisp_tools">
    <img alt="hex.pm version" src="https://img.shields.io/hexpm/v/grisp_tools.svg?style=flat-square"/>
  </a>
  <a href="LICENSE">
    <img alt="hex.pm license" src="https://img.shields.io/hexpm/l/grisp_tools.svg?style=flat-square"/>
  </a>
  <a href="https://github.com/grisp/grisp_tools/blob/master/.github/workflows/ci.yml#L15-L16">
    <img alt="erlang versions" src="https://img.shields.io/badge/erlang-23+-blue.svg?style=flat-square"/>
  </a>
</p>

# GRiSP Tools

GRiSP tools for building and deployment.

This is a shared library that is used by all package managers in the Erlang ecosystem to create [GRiSP](https://grisp.org/) projects. Currently we have plugins for [Rebar3](https://github.com/grisp/rebar3_grisp) and [Mix](https://github.com/grisp/mix_grisp).

## Development

Use `rebar3 as dialyzer dialyzer` for complete type checking, including the
optional Cargo API. Plain `rebar3 dialyzer` checks the default dependency graph
and omits unknown-call warnings because Cargo is intentionally absent there.
The dedicated profile enables those warnings and is used by CI.

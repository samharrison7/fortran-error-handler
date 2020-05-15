# Contribution Guide

Thanks for considering contributing to the Fortran Error Handler. We very much welcome your input.

## Bug reports

Whilst bug reports are very welcome, we strongly encourage pull requests to accompany these. Reports should be made via the issue tracker and should include enough information to be fully reproducible. This includes information on the system and compiler you are using, as well as detailed information about what you were doing that caused the bug.

## New and updated features

We welcome new and updated features via pull requests. Only completed features that do not require any further coding to become functional will be accepted. If you have an idea for a feature but don't have the time to code it, then consider raising a feature request issue. If you have some code towards a feature but won't have time to finish it, then raise an issue highlighting this, and if we think the feature is valuable enough, we may open a new feature branch for you to submit your code to.

## Which branch?

Please send pull requests for all contributions (bug reports and features) to the `develop` branch.

## Security vulnerabilities

If you discover a security vulnerability, please send an email to Sam Harrison at [samharrison.xg@gmail.com](mailto:samharrison.xg@gmail.com). Please do not submit an issue with the vulnerability.

## Coding style

Fortran does not have strict coding style conventions, but we have implemented our own with this project. Before contributing, please read through some of the code to get a flavour of these, and stick to them as much as possible. They are somewhat similar to Java and PHP conventions, and include:
- Four spaces to ident, ident all blocks (types, functions, loops etc) and save files as UTF-8 encoding.
- `lowerCamelCase` for variable and function names, with the exception of globally-available variables, which should be in capitals.
- `UpperCamelCase` for type and module names.
- Lowercase for all Fortran constructs (`do`, `module`, `subroutine` etc).
- Variable names should be verbose enough to be understandable without needing documentation.
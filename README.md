ldapply
=======

[LDIF](https://www.ietf.org/rfc/rfc2849.txt) idempotent apply tool.
This tool is similar to `ldapmodify` with one exception: it's idempotent.
It was written to help declarative deployments with [NixOS](http://nixos.org/).


How it works
============

1. If change type is not specified, it adds or replaces an entry.
2. If change type is specified, it acts like normal `ldapmodify`.


Requirements
============

`ldapply` is written in Haskell with [GHC](http://www.haskell.org/ghc/).
All required Haskell libraries are listed in [ldapply.cabal](ldapply.cabal).
Use [cabal-install](http://www.haskell.org/haskellwiki/Cabal-Install) to fetch
and build all pre-requisites automatically.


Usage
=====

Type `ldapply --help` to see usage summary:

    Usage:
      ldapply [options] LDIF...

    Options:
      -H <ldapuri>       LDAP URL to connect to [default: ldapi:///]

      -x                 Use simple bind instead of default SASL External
      -D <binddn>        Use <binddn> for the distinguished name or authorization identity
      -w <passwd>        Use <passwd> as the password for simple bind
      -y <passwdfile>    Read password from <passwdfile>, only the first line is read

      -h, --help         Show this message

    If option -w is given, -y is ignored.


LDIF example
============

```LDIF
dn: dc=nodomain
objectClass: top
objectClass: dcObject
objectClass: organization
dc: nodomain
o: Example, Inc.

dn: cn=reader,dc=nodomain
objectclass: top
objectclass: organizationalRole
objectclass: simpleSecurityObject
cn: reader
description: Initial description
userPassword: qwerty123lol

# description will be removed, userPassword changed:
dn: cn=reader,dc=nodomain
objectclass: top
objectclass: simpleSecurityObject
objectclass: organizationalRole
cn: reader
userPassword: foobar12345

# userPassword will be changed:
dn: cn=reader,dc=nodomain
changetype: modify
replace: userPassword
userPassword: anothersecretstuff

dn: cn=reader,dc=nodomain
changetype: modify
replace: description
description: foo

# This will be deleted if exists:
dn: cn=reader,dc=nodomain
changetype: delete

```


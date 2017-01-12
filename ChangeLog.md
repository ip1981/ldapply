0.2.0
=====

  * Require [LDAP](https://hackage.haskell.org/package/LDAP) > 0.6.0 for `ldapExternalSaslBind`.
  * Support simple bind. Added options `-x`, `-D`, `-w`, `-y` similar to `ldapmodify`.
  * `changetype: delete` works if entry to be deleted does not exist.


0.1.0
=====

  * Initial release.
  * Works only with UNIX socket authentication.


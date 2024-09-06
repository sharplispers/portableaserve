# Portable AllegroServe

Portable AllegroServe is an attempt to provide the functionality of Franz.com's AllegroServe web server to other lisp implementations.  In addition to the "aserve" ASDF system, it also includes [a compatibility layer](acl-compat) that provides (partial) implementations of Allegro Common lisp's sockets, multiprocessing, and extended common lisp packages for other Lisps.  This compatibility layer can be of value independent of the web library and can be loaded separately.

I have forked it from the original SourceForge site to sharplispers since it appears no longer to be maintained and when I tried to load it into SBCL it needed some corrections.

To be honest, getting Portable AllegroServe to full functionality probably requires more effort than I can give it: its tests do not all pass, and serious bugs remain.  But it builds now on SBCL, which may unlock its capabilities for other users and perhaps patch-providers.

## Franz documentation

I don't know of any formal documentation for Portable AllegroServe, but as the name suggests, it attempts to provide the functionality provided by AllegroServe, whose documentation may be found [here](https://github.com/franzinc/aserve).

## So far ignored

I haven't even looked into the Debian package files, the user contributions, etc.

-- Robert Goldman

## Original README:
```
README --

This is a short description of what you will find
in the subdirectories of this archive

 ./acl-compat/            Several ACL compatibility hacks
 ./aserve/                The AServe source
 ./debian/                Debian package files
 ./contrib/               Additional useful (?) code contributed by users
 ./logical-hostnames.lisp Some logical-pathname-translations needed by defsys
 ./README                 This file
 ./README.cmucl           Additional documentation for CMU Common Lisp
 ./INSTALL.lisp           Quick installation (deprecated)
 
 To run Portable AllegroServe, best install QuickLisp
 (http://quicklisp.org/) and evalute (ql:quickload :aserve).

 Loading the file aserve:example.cl and evaluating
 (aserve-example::start-server :port 2001) starts an AllegroServe
 server on http://localhost:2001 that shows off some of the things
 possible with AllegroServe.  Depending on the Lisp implementation, it
 might be necessary to give additional arguments :chunking nil and/or
 :listeners 0, since chunked transfer encoding and multi-threading are
 not supported everywhere.


Regards,
Jochen Schmidt

--
jsc@dataheaven.de
http://www.dataheaven.de

```

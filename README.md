# Generate Apache 2 httpd protection directives

[![Build Status](https://travis-ci.org/TalkBank/httpd-protection-generator.png)](https://travis-ci.org/TalkBank/httpd-protection-generator)

This is used for the TalkBank project.

## Building

```console
$ stack install
```

The resulting executable will be at
`~/.local/bin/generate-httpd-protections` and will automatically be in
your path as `generate-httpd-protections` if your `PATH` is already set
correctly to include `~/.local/bin`.

## Usage

Run on a YAML file to get an Apache 2 config file.

```console
$ generate-httpd-protections configFile.yaml output.conf
```

### Details

Edit a YAML file describing what is to be protected. An example is
provided
[here](https://github.com/TalkBank/httpd-protection-generator/blob/master/sample-users.yaml):

```yaml
---
dirPrefix: "/theRoot"
passwordDirPrefix: "/private/etc/apache2"
dirs:
  -
    dir: "junk/all"
    users:
      - "junk1"
      - "junk2"
      - "junk3"
    passwordsFile: "file1"
    scope:
      - "all"
  -
    dir: "verbatim/deal"
    users:
      - "2"
    passwordsFile: "file2"
    scope:
      - "verbatim"
  -
    dir: "only/media"
    users:
      - "3"
    passwordsFile: "file3"
    scope:
      - "media"
  -
    dir: "some/selected"
    users:
      - "4"
    passwordsFile: "file4"
    scope:
      - "data-orig"
      - "data"
```

Sample output is [here](https://github.com/TalkBank/httpd-protection-generator/blob/master/sample-users.yaml).

### Run

```console
$ generate-httpd-protections protection.yaml httpd-protections.conf
```

You may need to use `sudo` if generating into `/etc/apache2/other/`.

This program only sets up the Apache 2 configuration. To create or modify a password file, use `htpasswd`, e.g., to add a new user:

```console
$ htpasswd passwords-file user
```

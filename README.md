# Generate Apache 2 httpd protection directives

[![Build Status](https://travis-ci.org/TalkBank/httpd-protection-generator.png)](https://travis-ci.org/TalkBank/httpd-protection-generator)

This is used for the TalkBank project.

## Building

```console
$ cabal install --only-dependencies --enable-tests
$ cabal configure
$ cabal install
```

The resulting executable will be at
`~/.cabal/bin/generate-httpd-protections` and will automatically be in
your path as `generate-httpd-protections` if your `PATH` is already set
correctly to include `~/.cabal/bin`.

### TODO: Alternate build instructions using [Stack](http://hackage.haskell.org/package/stack)

## Usage

Run on a YAML file to get an Apache 2 config file.

```console
$ generate-httpd-protections configFile.yaml output.conf
```

### Details

Edit a YAML file describing what is to be protected. An example is
provided
[here](https://github.com/TalkBank/httpd-protection-generator/blob/master/sample.yaml):

```yaml
---
dirPrefix: "/theRoot"
passwordDirPrefix: "/private/etc/apache2"
dirs:
  -
    dir: "junk/all"
    user: "1"
    userFile: "file1"
    scope:
      - "all"
  -
    dir: "verbatim/deal"
    user: "2"
    userFile: "file2"
    scope:
      - "verbatim"
  -
    dir: "only/media"
    user: "3"
    userFile: "file3"
    scope:
      - "media"
  -
    dir: "some/selected"
    user: "4"
    userFile: "file4"
    scope:
      - "data-orig"
      - "data"
```

Sample output is [here](https://github.com/TalkBank/httpd-protection-generator/blob/master/sample.yaml).

### Run

```console
$ generate-httpd-protections protection.yaml httpd-protections.conf
```

You may need to use `sudo` if generating into `/etc/apache2/other/`.

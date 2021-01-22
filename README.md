# ada-dotenv

> This library is a port of [dotenv](https://github.com/motdotla/dotenv) + [dotenv-expand](https://github.com/motdotla/dotenv-expand) in Ada.

Dotenv is a zero-dependency module that loads environment variables from a `.env` file. Storing configuration in the environment separate from code is based on [The Twelve-Factor App](http://12factor.net/config) methodology.

[![Alire](https://img.shields.io/endpoint?style=for-the-badge&url=https://alire.ada.dev/badges/dotenv.json)](https://alire.ada.dev/crates/dotenv)
[![LICENSE](https://img.shields.io/github/license/heziode/ada-dotenv.svg?style=for-the-badge)](LICENSE)
[![Conventional Commits](https://img.shields.io/badge/Conventional%20Commits-1.0.0-white.svg?style=for-the-badge)](https://conventionalcommits.org)

## Install

### Using alire

```bash
alr with dotenv
```

### From scratch

From the root of your project

```bash
mkdir lib
cd lib
git clone https://github.com/Heziode/ada-dotenv
```

And add the following to your `.gpr`: `with "./lib/ada-dotenv/dotenv.gpr";`

## Usage

As early as possible in your application, require and configure dotenv.

```ada
with Dotenv;

-- … Somewhere in a body

Dotenv.Config;
```

Create a `.env` file in the root directory of your project. Add environment-specific variables on new lines in the form of `NAME=VALUE`.
For example:

```dosini
DB_HOST=localhost
DB_USER=root
DB_PASS=s1mpl3
```

Environment variables now has the keys and values you defined in your `.env` file.

```ada
with Ada.Environment_Variables;

-- … Somewhere in a body
Ada.Text_IO.Put_Line ("DB_HOST: " & Ada.Environment_Variables.Value ("DB_HOST")); -- result: localhost
```

## Config

`Config` will read your `.env` file, parse the contents, assign it to environment variables.
All `Config`, `Config_Overwrite` and `Config_Debug` can raise Ada.Directories.Name_Error if no valid path found for environment variable file.



These procedures can take several parameters, like **Overwrite**, **Debug**, **Interpolation**, **Path**, **File_Form**. See [dotenv.ads](./src/dotenv.ads) for more details.

### Options

#### Path

Environment Variable: `DOTENV_CONFIG_PATH`

Default: `""`

You may specify a custom path if your file containing environment variables is located elsewhere.

```ada
Dotenv.Config(Path => "/custom/path/to/.env");
```

If no **Path** provided, the configuration try to solve the name in the following order:

- If **DOTENV_CONFIG_PATH** environment variable is set, it use it
- Find a `.env` file in the current working directory
- Find a `.env` file in the execution directory

#### File_Form

Environment Variable: `DOTENV_CONFIG_FILE_FORM`

Default: `wcem=8`

You may specify the encoding or any other option for file with this parameter.

```ada
Dotenv.Config(File_Form => "wcem=8");
```

#### Debug

Environment Variable: `DOTENV_CONFIG_DEBUG`

Default: `False`

You may turn on logging to help debug why certain keys or values are not being set as you expect.

```ada
Dotenv.Config_Debug (Debug => True);

-- Or

Dotenv.Config (Overwrite     => False
               Interpolation => False,
               Debug         => True,
               Path          => "",
               File_Form     => "");
```

#### Overwrite

Environment Variable: `DOTENV_CONFIG_OVERWRITE`

Default: `False`

You may turn on overwrite to overwrite existing environment variable.

```ada
Dotenv.Config_Overwrite (Overwrite => True);

-- Or

Dotenv.Config (Overwrite     => True,
               Debug         => False,
               Interpolation => False,
               Path          => "",
               File_Form     => "");
```

#### Interpolation

Environment Variable: `DOTENV_CONFIG_INTERPOLATION`

Default: `False`

You may turn on interpolation to interpolate variable.
For example, if we have the following environment variables:
- `FIRSTNAME=John`
- `LASTNAME=Doe`
- `HELLO=Hello ${FIRSTNAME} $LASTNAME`

Then the final value of `HELLO` will be "Hello John Doe" if Interpolation is True.
  
```ada
Dotenv.Config_Interpolation (Interpolation => True);

-- Or

Dotenv.Config (Overwrite     => False,
               Debug         => False,
               Interpolation => True,
               Path          => "",
               File_Form     => "");
```

## Parse

The engine which parses the contents of your file containing environment variables is available to use. It accepts a `String` (path to your environment file) or a `File_Type` and will return a `Map` with the parsed keys and values.

```ada
with Ada.Environment_Variables;
with Ada.Text_IO;
with Dotenv;

procedure Parse_Example is
   Env : constant Dotenv.Environment_Variable_Map.Map := Dotenv.Parse (Path => "/custom/path/to/.env");
begin
   Ada.Text_IO.Put_Line (Env.Element ("DB_HOST"));
end Parse_Example;
```

### Options

#### Debug

Environment Variable: `DOTENV_CONFIG_DEBUG`

Default: `False`

You may turn on logging to help debug why certain keys or values are not being set as you expect.

```ada
with Ada.Environment_Variables;
with Ada.Text_IO;
with Dotenv;

procedure Parse_Example is
   Env : constant Dotenv.Environment_Variable_Map.Map := Dotenv.Parse (Path => "/custom/path/to/.env",
                                                                       Debug => True);
begin
   Ada.Text_IO.Put_Line (Env.Element ("DB_HOST"));
end Parse_Example;
```

### Rules

The parsing engine currently supports the following rules:

- `BASIC=basic` becomes `Key: "BASIC", Value: "basic"`
- empty lines are skipped
- lines beginning with `#` are treated as comments
- empty values become empty strings (`EMPTY=` becomes `Key: "EMPTY", Value: ""`)
- inner quotes are maintained (think JSON) (`JSON={"foo": "bar"}` becomes `Key: "JSON", Value: "{\"foo\": \"bar\"}"`)
- whitespace is removed from both ends of unquoted values (see more on [`Trim`](http://www.ada-auth.org/standards/aarm12_w_tc1/html/AA-A-4-5.html#I6512)) (`FOO=  some value  ` becomes `Key: "FOO", Value: "some value"`)
- single and double quoted values are escaped (`SINGLE_QUOTE='quoted'` becomes `Key: "SINGLE_QUOTE", Value: "quoted"`)
- single and double quoted values maintain whitespace from both ends (`FOO="  some value  "` becomes `Key: "FOO", Value: "  some value  "`)
- double quoted values expand new lines (`MULTILINE="new\nline"` becomes

```
Key: "MULTILINE", Value: "new
line"
```

## FAQ

### Should I commit my `.env` file?

No. We **strongly** recommend against committing your `.env` file to version
control. It should only include environment-specific values such as database
passwords or API keys. Your production database should have a different
password than your development database.

### Should I have multiple `.env` files?

No. We **strongly** recommend against having a "main" `.env` file and an "environment" `.env` file like `.env.test`. Your config should vary between deploys, and you should not be sharing values between environments.

> In a twelve-factor app, env vars are granular controls, each fully orthogonal to other env vars. They are never grouped together as “environments”, but instead are independently managed for each deploy. This is a model that scales up smoothly as the app naturally expands into more deploys over its lifetime.
>
> – [The Twelve-Factor App](http://12factor.net/config)

### What happens to environment variables that were already set?

By default, we will not modify any environment variables that have already been set. In particular, if there is a variable in your `.env` file which collides with one that already exists in your environment, then that variable will be skipped. This behavior allows you to override all `.env` configurations with a machine-specific environment, although it is not recommended.

If you want to override an environment variable, you can set the parameter [**Overwrite**](#Overwrite) of `Config`, `Config_Overwrite` or `Config_Debug` to `True`, or by setting the environment variable `DOTENV_CONFIG_FILE_FORM=True`.

### What about variable expansion/interpolation?

Dotenv support variable interpolation.

For example, if we have the following environment variables:
- `FIRSTNAME=John`
- `LASTNAME=Doe`
- `HELLO=Hello ${FIRSTNAME} $LASTNAME`

Then the final value of `HELLO` will be "Hello John Doe" if Interpolation is True.

## Contributing Guide

See [CONTRIBUTING.md](CONTRIBUTING.md)

## Change Log

See [CHANGELOG.md](CHANGELOG.md)

## Licence

See [LICENSE](LICENSE)

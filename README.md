# Getting started with opeRend

## 1. Install the opeRend package.
The opeRend package may be installed directly from GitHub with the commands:
```
devtools::install_github("agower/opeRend")
```

## 2. Create a configuration file.
The opeRend package uses an [INI-formatted](https://en.wikipedia.org/wiki/INI_file) configuration file, divided into sections (one for each configuration), each of which contains the following parameters:

- `api_base_url`
The full base URL used for API calls, including the scheme (`http://` or `https://`) and any intermediate path
- `storage_location`
The storage location used for creating new WorkFiles
- `timezone`
The timezone to be used when interpreting dates in Operend records
- `token`
A JSON record containing a token for authenticating API calls (see step 3 below).
- `verbosity`
A nonnegative integer specifying how many messages to write to the terminal (aside from errors and warnings, which are always written):
   - 0: suppresses all messages
   - 1: prints messages when performing high-level functions
   - 2: also instructs `curl` to produce verbose output
   - ≥ 3: also prints the content of `POST` or `PUT` calls

For example, a config file might contain the following:
```
[default]
api_base_url     = https://operend.bu.edu/api/v2
storage_location = default
timezone         = UTC
token            = {json object from server}
verbosity        = 1
```

## 3. Create a token.
To interact with an Operend server through the `opeRend` package, you'll need to create a token.

 1. Navigate to the Operend server in your browser and click the **Log In With CILogon** button to log in:

 2. Click **ACCESS TOKENS** and then **Add Token**:

 3. Enter a token name and select the privileges to grant to the token:

 4. Retrieve the token by one of the following methods:
    - Click **Download JSON** to save it to the local file
    `token-name.json`
    - Click **Copy to Clipboard** to save it to the clipboard.
 5. Copy and paste the *entire* text from the JSON file or from the clipboard into the `token = ` field of the config file, so that it looks like:
```
token = {"username":"myUsername","name":"my-readwrite-token","authorizations":["read","write"],"creationDate":"Mon Jan 01 00:00:00 UTC 1970","secret":"myUsername:my-readwrite-token:TBKwVAhgkKxraxF6btlIOxMHhoWdh0ETmphKmF7t"}
```

## 4. Set environment variables.
Before the opeRend R package can be used, the following environment variables must be set:
- **`OPEREND_CONFIG_FILENAME`**
The full path to the configuration file.
- **`OPEREND_CONFIG`**
The name of the configuration (section of the configuration file) to use. If not specified, `"default"` will be used.

In a Unix-like system (including Macs), these may be set with the `export` command.

For example, the following lines might be added to a `~/.bashrc` file:
```
# opeRend environment variables
export OPEREND_CONFIG_FILENAME="~/.config/operend.conf"
export OPEREND_CONFIG="default"
```
In a Windows environment, these may be set through the Environment Variables system dialog (accessible by searching the Control Panel or Start menu).

## 5. Test the credentials.
The simplest way to test whether a connection can be made to an opeRend server is to issue the `listUsers()` command to list all visible users.  As this will always include at least the user performing the query, it should always return a result, and is therefore a useful quick check for connectivity.

For example:
```
> listUsers()
      username   group  groups  superuser                  email firstName    lastName  ...  active
1   myUsername myGroup myGroup      FALSE myUsername@operend.com        My    Username         TRUE
```

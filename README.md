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

Navigate to the Operend server in your browser and click the **Log In With CILogon** button to log in:
<br><br><img width="222" alt="operend01" src="https://github.com/agower/opeRend/assets/11539805/ffcf0a6f-8e79-4a7e-bd9f-cc7172edd0a6"><br>

Click **ACCESS TOKENS**:
<br><br><img width="153" alt="operend02" src="https://github.com/agower/opeRend/assets/11539805/02f1f525-dcfc-4712-ac3f-5668249f9384"><br>

Then click **Add Token**:
<br><br><img width="763" alt="operend03" src="https://github.com/agower/opeRend/assets/11539805/efb96af3-10cb-4794-b240-c8aac3704c3d"><br>

Enter a token name and select the privileges to grant to the token:
<br><br><img width="590" alt="operend04" src="https://github.com/agower/opeRend/assets/11539805/b9a3c6c2-6564-4dc0-9912-260191bd450e"><br>

A new window labeled **New Access Token Created** will appear:
<br><br><img width="291" alt="operend05" src="https://github.com/agower/opeRend/assets/11539805/a4fd29c8-b698-4b23-b503-fbcc206442a1"><br>

Retrieve the token by one of the following methods:
 - Click **Download JSON** to save it to the local file
   `token-name.json`
 - Click **Copy to Clipboard** to save it to the clipboard; the text will change to **Copied.** when this happens:
<br><br><img width="290" alt="operend06" src="https://github.com/agower/opeRend/assets/11539805/b694cf72-9c76-4b1f-825f-1a2972b2bf52"><br>

Copy and paste the *entire* text from the JSON file or from the clipboard into the `token = ` field of the config file, so that it looks like:
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

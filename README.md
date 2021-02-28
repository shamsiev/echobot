# ECHOBOT

## Usage
- Clone this repository: `git clone https://github.com/shamsiev/echobot`
- Move to the project's directory: `cd echobot`
- Build the project: `stack build`
- Create `yaml` config file (see [configuration file example](#configuration-file-example))
- Run bot: `stack run [config file path]`

## <a id="configuration-file-example"></a> Configuration file example ##
```yaml
logger:
  type: console # console / file
  level: debug # debug / info / warning / error
  file_path: file_to_config_file

bot:
  instance: telegram # telegram / vk
  help_message: answer to /help command
  repeat_message: answer to /repeat command
  repeat_count: 1 # 1 to 5

telegram:
  token: your_token_here
  timeout: 25

vk:
  access_key: your_access_key_here
  group_id: your_group_id_here
  api_version: your_api_version_here
  timeout: 25
```
## Project's structure
Echobot is built using a [Handle Pattern](#https://www.schoolofhaskell.com/user/meiersi/the-service-pattern)
Your handles are stored in `src/Logger.hs` and `src/Bot.hs`. Implementations for these handles are stored in `src/Logger` and `src/Bot`, accordingly. `src/Telegram.hs` is where you build your Telegram implementation for Bot and `src/VK.hs` is where you build your VK implementation for Bot. Every `Internal.hs` just stores some data types and functions which you want to hide from implementation. `src/Web.hs` consists of two functions built on top of [wreq](#https://hackage.haskell.org/package/wreq). And finally, `app/Main.hs` is an entry point of a program.

## TODOS
- [x] Logger
- [x] Telegram
- [x] VK
- [x] tests
- [x] Docs
- [ ] Refactor so it is good enough to hire me

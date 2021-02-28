# ECHOBOT - The bot that echoes

## Usage
- Clone this repository: `git clone https://github.com/shamsiev/echobot`
- Move to the project's directory: `cd echobot`
- Build the project: `stack build`
- Create `config.yaml` file (see [configuration file example](#configuration-file-example))
- Run bot: stack run

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
  timeout: 0

vk:
  access_key: your_access_key_here
  group_id: your_group_id_here
  api_version: your_api_version_here
```

## TODOS
- [x] Logger
- [ ] Bot
  - [x] Telegram
  - [ ] VK
- [ ] tests

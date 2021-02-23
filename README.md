# echobot

## Usage
- Clone this repository: `git clone https://github.com/shamsiev/echobot`
- Move to the project's directory: `cd echobot`
- Build the project: `stack build`
- Create config file for specific application (see [configuration file examples](#configuration-file-examples))

## <a id="configuration-file-examples"></a> Configuration file examples ##
##### telegram.json
```json
{
  "severity": "error",
  "help_message": "Message to /help command",
  "repeat_message": "Message to /repeat command",
  "repeat_count": 1,
  "token": "token for your telegram bot"
}
```

##### vk.json
```json
{
  "severity": "info",
  "help_message": "Message to /help command",
  "repeat_message": "Message to /repeat command",
  "repeat_count": 1,
  "token": "token for your vk bot",
  "api_version": "version of api your vk bot uses",
  "group_id": "id of your vk bot"
}
```
**Note:** you have 4 log (severity) levels (`debug`, `info`, `warning`, `error`). If none of these is specified in config file, then bot uses `debug` level.

## TODOS:
##### Write Telegram instance:
- [x] Parse config
- [x] Parse updates
- [x] Answer to /help command
- [x] Answer to /repeat command
- [x] Set repeat counters for individual users with /repeat command
- [x] Echo text messages
- [x] Echo audio messages
- [x] Echo document messages
- [x] Echo photo messages
- [ ] Echo sticker messages
- [ ] Echo video messages
- [ ] Echo video note messages
- [ ] Echo voice messages
- [ ] Echo contact messages
- [ ] Echo dice messages
- [ ] Echo poll messages
- [ ] Echo venue messages
- [ ] Echo location messages
- [ ] Refactor this trash
- [ ] Write tests
##### Write VK instance:
- [ ] Just do something...
##### General
- [ ] Write documentation

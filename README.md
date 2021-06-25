![ECHOBOT](/img/echobot.png "ECHOBOT")

--------------------------------------

## ABANDONED
I don't have any time or wish to continue the work on this project. The application itself is written using the 'Handle/Service' pattern. If you learn Haskell and wish to understand this pattern, you can find something helpful in this code (although this is a really bad code).

## Usage
- Clone this repository: `git clone https://github.com/shamsiev/echobot`
- Create `config.yaml` in root of the project (see [configuration file example](#configuration-file-example))
- Build the project: `stack build`
- Run the project: `stack run`

## <a id="configuration-file-example"></a> Configuration file example ##
```yaml
messenger: telegram # telegram / vk
help_message: Your message to /help command
repeat_message: Your message to /repeat command
repeat_count: 1 # from 1 to 5
timeout: 25
token: "token"
api_version: "api_version" # vk only
group_id: "group_id" # vk only

log_to: console # console / file
log_level: debug # debug / info / warning / error
log_path: "path to file" # required when logging to file
```

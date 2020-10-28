# Process 

The normal execution consist of:
1. Download the hero list from [OpenDota's constants](https://github.com/odota/dotaconstants/blob/master/build/heroes.json) (a cache is stored in `~/.tryhard`)
1. For every new hero in your team or in the enemy team, the matchup get queried (example for [Razor's matchups](https://api.opendota.com/api/heroes/15/matchups)) (these matchup get stored in memory, and will not be re-queried while the program is running)
1. According to the aggregating strategy, the pick/counterpick and the mode

# Module structure
## Roughly

- Everything `Tryhard` related is inside `TryHard**`. Utilities (such as adding some useful types to `req` will be in their respective modules)
- `Tryhard.OpenDota**` is related to getting data from the server.
- - `Tryhard.OpenDota.Internal` deals with low level requests
- - `Tryhard.OpenDota` maps and caches said requests
- `Tryhard.Types` has typeclasses that are used thought the app
- `Tryhard.TUI` deals with the nice terminal interface
- `Tryhard.Stats` are the instances to aggregate and sort the stats
- - `Tryhard.Stats.Mode` the modes of sorting. Be it "win percentage" or "matches played"
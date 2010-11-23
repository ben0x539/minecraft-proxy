minecraft-proxy
===============

This proxy parses [Minecraft][minecraft] alpha network communication between the client and
a Survial Multiplayer server, enabling easy manipulation of packets in order
to exploit the extent to which the server trusts the client.

Usage
-----

Run the proxy giving the server's hostname and port as separate commandline
arguments:

    ./proxy example.com 1234

If you omit the port, it will default to 25565, minecraft's default port. The
proxy will listen on port 25565, so that you can use the minecraft client to
connect to the system running the proxy, e.g. `localhost:25565` or `::1` for
short.

The proxy will then establish a connection to the server and transparently
pass on packets in either direction. stdout will display the current buffer
size if it rises beyond some threshold, so if you observe this number
continuously growing, chances are something was parsed incorrectly and the
proxy lost track of packet boundaries.

Features
--------

The proxy is not configurable, its behavious is hardcoded.

From the server it will intercept all the server time updates and rewrite them
so that it is constantly early morning and appropriately bright.

From the client, it will intercept the new slash commands `/send`, `/return`
and`/take`.

`/send` and `/return` will prefix "Packet" to the rest of the line
and parse it as per the haskell Read instances of the involved types. `/send`
will send the result onwards to the server, `/return` will return the packet
right back to the client.

`/take` will parse the rest of the line as an item id (see, for example, [this
list][datavalues]) and optionally a quantity, and then return a packet to the
client indicating that such an item acquisition just happened. Note that a
quantity of `-1` will provide a stack of seemingly infinite quantity, but the
stack will disappear when you punch a player or mob with it.

Remarks
-------

All the protocol know-how comes from [here][protocol] and transitively from
`#mcc`, and from badgering [fry][fry] when that was not enough.

Haskell is pretty cool, and playing around with all the crazy scaffolding that
comes with it was pretty much the motivation for this project. This whole
project does not actually *do* all that much, but it does it in a very
elaborate and personally satisfying way. If someone comes up with other cool
hacks for this proxy (dumping world data to a singleplayer savegame? Chatlogs,
chatbots?), I would love to hear about it.

I kind of hope that someone who goes through the trouble of checking out code
from github and piping it through ghc is above the sort of dumb schemes to
ruin your own fun and that of others that spawning a million blocks of tnt on
other people's servers enables. Your call.

[minecraft]: http://minecraft.net/
[datavalues]: http://www.minecraftwiki.net/images/3/33/ItemslistV110.png
[protocol]: http://mc.kev009.com/wiki/Protocol
[fry]: https://github.com/fry/Survivalist

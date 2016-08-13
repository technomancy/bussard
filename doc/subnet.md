# The Subnet Protocol

Subnet is a distributed interplanetary discussion medium that evolved
from a 20th-century protocol known as Usenet. Its distributed nature
makes tracing and moderation very difficult.

## Requests

Once connected to a Subnet server, you can make requests for
posts. All posts are categorized into groups. You can get a listing of
groups with the "groups" command:

    > {command="groups"}
    < {status="ok", groups={"comp_os_orb", "comp_lang_lua", ...}}

To get the posts for a specific group, use the "list" command:

    > {command="list",group="comp_lang_lua"}
    < {status="ok", posts={"patterns-for-filenames.msg",
                           "question-about-tables.msg"}}

Getting an individual post uses the "get" command:

    > {command="get",group="comp_lang_lua",post="patterns-for-filenames.msg"}
    > {status="ok", content="From: Tegeb Douggesiem\nTo: comp_lang_lua\n..."}

## Encoding

All requests and responses are sent and received as strings encoded
with the bencode system. Bencoding supports integers, strings, and
tables.

Integers are encoded by placing them in between the letters "i" and
"e"; for example, 34 is "i34e", and -12 is "i-12e".

Strings are encoded by placing their length, followed by a colon,
followed by the string. For example "bencode" is "7:bencode", and
"Subnet protocol" is "15:Subnet protocol".

Tables are encoded by placing a sequence of key/value pairs in between
a "d" and an "e" with no delimiters between them. Keys must be
strings, but values can be anything, including other tables. For
example, {protocol="subnet", host="localhost"} is encoded as
"d4:host9:localhost8:protocol6:subnete".

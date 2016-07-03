From: nari.phouen@starlink.net.sol
To: captain@adahn.local
Content-Type: text/plain; charset=UTF-8
Subject: Re: Encrypted files
Message-Id: 01abcc58-03eb-4d77-84a9-ff3959e48425

I don't remember that much about the programming class I took, but for the final
assignment we wrote a function that could perform "rot13 translation". Basically
all it does is take each letter of a message and shift it over by 13
characters. It would turn "abc" into "nop", for instance, since "n" is 13
characters away from "a".  For letters in the second half of the alphabet, it
would loop back to the start.

So, are you up for some coding? You're going to need a way to turn letters into
numbers and back again. (But you need to ignore characters that aren't letters.)
The string.byte function and string.char function can do that transformation.

    string.byte("a") --> 97
    string.char(97) --> "a"
    string.char(string.byte("b") + 13) --> "o"

That needs to happen for every letter in the message, basically. I wish I could
remember more about how to do that from the class I took, but it's been so long.

You'll want to put your code in a file somewhere using ctrl-o, like "src.rot13",
then you can run it with dofile("src.rot13") in the console. I hope that makes
sense. Remember, a little trial and error will go a long way.

I'm dying to know what's in the file. After you decrypt it, can you place it in
"docs.msg1" on your system so I can take a look?

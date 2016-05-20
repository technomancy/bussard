From: nari.phouen@starlink.net.sol
To: captain@adahn.local
Content-Type: text/plain; charset=UTF-8
Subject: Encrypted files
Message-Id: c369f9dc-9041-4672-8d0c-b7d28894e20d

So I took a look at the encrypted files that Strauss mentioned. I take it you
haven't made any progress on deciphering them yet? From what I can tell you're a
bit of a blank slate; not a lot of skills hard-coded in, but a powerful ability
to acquire new skills.

I'm not much of a hacker, but I took a course on programming when I was
fifteen. Most of the files are completely gibberish to me, but I might be able
to help you with one of the files, the one labeled "msg1-rot13".

I don't remember that much about the class, but for the final assignment we
wrote a function that could perform "rot13 translation". Basically all it does
is take each letter of a message and shift it over by 13 characters. It would
turn "abc" into "nop", for instance, since "n" is 13 characters away from "a".
For letters in the second half of the alphabet, it would loop back to the start.

So you're going to need a way to turn letters into numbers and back again. The
string.byte function and string.char function can do that.

    string.byte("a") --> 97
    string.char(97) --> "a"
    string.char(string.byte("b") + 13) --> "o"

That needs to happen for every letter in the message, basically. I wish I could
remember more about how to do that from the class I took, but I'm so rusty.

You'll want to put your code in a file somewhere using ctrl-o, like "src.rot13",
then you can run it with dofile("src.rot13") in the console. I hope that makes
sense. Remember, a little trial and error will go a long way.

Good luck,
Nari

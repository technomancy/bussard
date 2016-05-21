From: nari.phouen@starlink.net.sol
To: captain@adahn.local
Content-Type: text/plain; charset=UTF-8
Subject: Re: Encrypted files
Message-Id: 01abcc58-03eb-4d77-84a9-ff3959e48425

OK, I've done a bit more fiddling around in my own console, and some of this is
starting to come back to me, specifically the concept of loops.

You're also going to need a way to do this shifting for every letter in the
message. A "for" loop will let you run the loop body once for each letter.

    for i=1,#ship.docs.backup.msg1_rot13 do
       local letter = string.sub(ship.docs.backup.msg1_rot13, i, i)
       print(letter)
    end

You'll use string.sub function to get you a single letter out of the string.

This will just print each letter individually; in order to decode the message
you'll have to get the string.byte and string.char functions into the mix to
shift everything over by 13. You can build up a string using the .. operator to
put two shorter strings together:

    local a = "abc"
    a = a .. "def"
    print(a) --> abcdef

I've also found a few chapters of a book on Lua programming that I've loaded
into your ship's help system. You can run this to read it:

    man("luabook1")

Hope that helps! I'm probably still missing something, but it should be a start.

Nari

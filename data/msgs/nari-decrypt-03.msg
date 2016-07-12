From: nari.phouen@starlink.net.sol
To: captain@adahn.local
Content-Type: text/plain; charset=UTF-8
Subject: Re(2): Encrypted files
Message-Id: cd2305a7-64f2-42c5-aadf-fd756ada3742

OK, I've done a bit more fiddling around in my own console, and some of this is
starting to come back to me, specifically the concept of loops. You're going to
need a way to do this shifting I described in the last email for every letter in
the message. A "for" loop will let you run the loop body once for each letter.

It'll be simpler if you use the string.upper() function so it's all upper
case. You'll use string.sub() to get you a single letter out of the string.

    local msg = string.upper(ship.docs.backup.msg1_rot13)
    for i=1,#msg do
       local letter = string.sub(msg, i, i)
       if(letter >= "A") then
         print(letter) -- or whatever
       elseif(...) then
       end
    end

This will just print each letter individually; in order to decode the message
you'll have to get the string.byte and string.char functions into the mix to
shift all the letters over by 13. Letters in the first half of the alphabet go
up by 13, and letters in the second half go down by 13. You build up a string
using the .. operator (dot-dot) to turn two strings into one:

    local decoded = "ABC"
    decoded = decoded .. "DEF"
    (decoded) --> ABCDEF

Remember not to change the punctuation and spaces. If a character is more than
A but less than Z it's a letter, but otherwise you should just leave it alone!

I've also found a few chapters of a book on Lua programming that I've loaded
into your ship's help system. Hope that helps! You can run this to read it:

    man("luabook1")

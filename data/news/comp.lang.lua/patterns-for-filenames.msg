From: Tegeb Douggesiem
To: comp.lang.lua
Content-Type: text/plain; charset=UTF-8
Subject: Patterns for filenames
Message-Id: 5e4589c7-73d7-4d3a-bdf3-a51206813fe8

I'm trying to match this pattern against a filename. Why isn't it working?

  filename:find("(.*)\\.txt")

I just get nil every time, even when filename ends with ".txt". I don't
understand what's going on!



From: Wifadofoug Kigot
To: comp.lang.lua
Content-Type: text/plain; charset=UTF-8
Subject: Re: Patterns for filenames
Message-Id: f1600c2e-0c8f-496f-9bda-21be37f021ac
In-Reply-To: 5e4589c7-73d7-4d3a-bdf3-a51206813fe8

This trips a lot of people up. You need to remove the
backslashes. Outside the parentheses which are used for capture, dots
are treated literally rather than matching against any character.



From: Tegeb Douggesiem
To: comp.lang.lua
Subject: Re: Patterns for filenames
Content-Type: text/plain; charset=UTF-8
Message-Id: 30d61bee-3716-4883-a45c-3442b273064f
In-Reply-To: f1600c2e-0c8f-496f-9bda-21be37f021ac

Perfect; that fixes it! Thanks.

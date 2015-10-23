From: Koxen Cinos
Subject: Question about Lua tables
To: comp.lang.lua
Message-Id: 669c43fc-71b9-11e5-8df5-001f16110e8c

Hello everyone. I am new to Lua. I wonder if you could tell me the
best way to put something in a table, but always at the end. Is this easy to do?



From: Dieggar Pabout
Subject: Re: Question about Lua tables
To: comp.lang.lua
Message-Id: 6c024a8a-71b9-11e5-b2f4-001f16110e8c
In-Reply-To: 669c43fc-71b9-11e5-8df5-001f16110e8c

> Hello everyone. I am new to Lua. I wonder if you could tell me the
> best way to put something in a table, but always at the end. Is this easy to do?

Take a look at the table.insert function. You can pass in a position, but if
you leave it out, it defaults to the end.

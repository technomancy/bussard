-- -*- lua -*-

local original_prompt = get_prompt()
set_prompt("=] ")

local clone = function(x)
   return lume.deserialize(lume.serialize(x))
end

local sum = function(t)
   local s = 0
   for _,x in pairs(t) do s = s + x end
   return s
end

local last_non_pass, kind_of_play, is_allowed

---- computer player logic

local find_doubles = function(hand)
   -- TODO: breaks up triples and gangs
   local doubles = {}
   for i,card in ipairs(hand) do
      local next_card = hand[i+1]
      if(next_card and math.floor(card) == math.floor(next_card)) then
         table.insert(doubles, {card, next_card})
      end
   end
   return doubles
end

local find_triples = function(hand)
   -- TODO: breaks up gangs
   local triples = {}
   for i,card in ipairs(hand) do
      local next_card, next_next_card = hand[i+1], hand[i+2]
      if(next_card and math.floor(card) == math.floor(next_card) and
         next_next_card and math.floor(card) == math.floor(next_next_card)) then
         table.insert(triples, {card, next_card, next_next_card})
      end
   end
   return triples
end

local index_numbers_for = function(play, hand)
   local index_numbers = ""
   for i,card in ipairs(hand) do
      if(lume.find(play, card)) then
         index_numbers = index_numbers .. "," .. i
         lume.remove(play, card)
      end
   end
   return index_numbers
end

local function play_set(sets, last_play, hand)
   local lowest = sets[1]
   if(lowest == nil) then
      return "pass"
   elseif(sum(lowest) > sum(last_play)) then
      return index_numbers_for(clone(lowest), hand)
   else
      table.remove(sets, 1)
      return play_set(sets, last_play, hand)
   end
end

local computer_play = function(hand, played)
   local last_play = last_non_pass(played)
   local kind = kind_of_play(last_play)
   if(kind == nil) then -- starting a round
      -- TODO: how to start a triples round
      if(false and hand[2] and math.floor(hand[1]) == math.floor(hand[2])) then
         return "1,2,3"
      elseif(hand[2] and math.floor(hand[1]) == math.floor(hand[2])) then
         return "1,2"
      else
         return "1"
      end
   elseif(kind == "double") then
      return play_set(find_doubles(hand), last_play, hand)
   elseif(kind == "triple") then
      return play_set(find_triples(hand), last_play, hand)
   elseif(kind == "single") then
      -- TODO: breaks up doubles
      for number, card in ipairs(hand) do
         if(is_allowed({card}, last_play)) then
            return tostring(number)
         end
      end
   end
   return "pass"
end

---- display functions

local card_name = function(card)
   local colors = {"green", "yellow", "red", "multi"}
   local color = colors[(card * 10 % 10)]
   if(card < 11) then
      return color .." ".. tostring(math.floor(card))
   elseif(card == 12.3) then
      return "dragon"
   elseif(card == 11.1 or card == 11.2) then
      return color .. " " .. "phoenix"
   else
      return "unknown card"
   end
end

local show_hand = function(hand, remote)
   for n,card in ipairs(hand) do
      if(remote) then
         remote:send(n .. " - " .. card_name(card) .. "\n")
      else
         print(n .. " - " .. card_name(card))
      end
   end
end

---- setup

local make_deck = function()
   local deck = {}
   for i=1,10 do
      for _,c in pairs({0.1, 0.2, 0.3}) do
         table.insert(deck, i + c)
         table.insert(deck, i + c)
      end
   end
   table.insert(deck, 1.4)
   table.insert(deck, 11.1)
   table.insert(deck, 11.2)
   table.insert(deck, 12.3)
   return lume.shuffle(deck)
end

local deal = function(deck)
   local hands = {{}, {}, {}, {}}
   for i,card in ipairs(deck) do
      local which_hand = (i % 4) + 1
      table.insert(hands[which_hand], card)
   end
   return hands
end

local who_has = function(looking_for, hands)
   for i,hand in pairs(hands) do
      for _,card in pairs(hand) do
         if(card == looking_for) then
            return i
         end
      end
   end
end

---- game rules

local is_matching = function(play)
   local number = math.floor(play[1])
   for _,card in pairs(play) do
      if(math.floor(card) ~= number) then
         return false
      end
   end
   return true
end

kind_of_play = function(play)
   if(not play or #play == 0) then return nil
   elseif(#play == 1) then
      return "single"
   elseif(#play == 2 and is_matching(play)) then
      return "double"
   elseif(#play == 3 and is_matching(play)) then
      return "triple"
   elseif(#play >= 4 and #play <= 7 and is_matching(play)) then
      return "gang"
   end
end

is_allowed = function(play, last_play)
   local kind = kind_of_play(play)
   local last_kind = kind_of_play(last_play)
   if(last_play == nil) then
      return true
   elseif(kind == "gang" and last_kind ~= "gang") then
      return true
   elseif(kind == "gang" and last_kind == "gang") then
      if(#play == #last_play) then
         return play[1] > last_play[1]
      end
      return #play > #last_play
   elseif(kind ~= last_kind) then
      return false
   elseif(kind == "single" or kind == "double" or kind == "triple") then
      return sum(play) > sum(last_play)
   else
      return false
   end
end

local get_cards = function(hand, input)
   local play = {}
   for _,card in pairs(lume.split(input, ",")) do
      table.insert(play, hand[tonumber(card)])
   end
   return play
end

local remove_cards = function(hand, play)
   for _,card in pairs(play) do
      lume.remove(hand, card)
   end
end

local round_won = function(played)
   if(played[#played] == "pass" and
         played[#played - 1] == "pass" and
      played[#played - 2] == "pass") then
      return true
   else
      return false
   end
end

last_non_pass = function(played)
   if(played[#played] ~= "pass") then
      return played[#played]
   elseif(played[#played - 1] ~= "pass") then
      return played[#played - 1]
   elseif(played[#played - 2] ~= "pass") then
      return played[#played - 2]
   end
end

---- multiplayer

local printall = function(players, output)
   for _,player in pairs(players) do
      if(type(player) == "userdata") then
         player:send(output .. "\n")
      elseif(player == "local") then
         print(output)
      end
   end
end

local function check_password(correct_password, connection)
   if(correct_password == nil) then return end
   connection:send("What is the password? ")
   local password_try = connection:receive()
   if(correct_password == password_try) then
      connection:send("You got the password right!\n")
   else
      connection:send("Incorrect password! Try again.\n")
      check_password(correct_password, connection)
   end
end

local connect = function(port, player_number)
   require("luarocks.require")
   local socket = require("socket")
   local server = assert(socket.bind("*", port),
                         "Port " .. port .. " not available.")
   print("Started server on " .. port .. ".")
   local connection = server:accept()
   connection:send("Joined game as player " .. player_number .. ".\n")
   check_password(os.getenv("GANG_OF_FOUR_PASSWORD"), connection)
   return connection
end

---- game loop

local function round(hands, turn, played, players)
   local player_type = players[turn]
   if(round_won(played)) then
      printall(players, "Player " .. turn .. " won the round.")
      round(hands, turn, {}, players)
   else
      local hand, input = hands[turn]
      if(player_type == "computer") then
         input = computer_play(hand, played)
      elseif(player_type == "local") then
         print("Your turn, player " .. turn)
         show_hand(hand)
         print("What do you want to play?")
         input = io.read()
      elseif(type(player_type) ~= "string") then -- remote player
         player_type:send("Your turn, player " .. turn .. "\n")
         show_hand(hand, player_type)
         player_type:send("What do you want to play? ")
         input = player_type:receive()
      else
         input = "pass"
      end

      local next_turn = turn + 1
      if(next_turn == 5) then next_turn = 1 end

      if(input == "pass" or input == "") then
         if(player_type ~= "local") then
            printall(players, "Player " .. turn .. " passed.")
         end
         table.insert(played, "pass")
         round(hands, next_turn, played, players)
      elseif(input == "quit") then
         for _,p in pairs(players) do
            if(type(p) == "userdata") then
               p:send("Quitting game.\n")
               p:close()
            end
         end
         set_prompt(original_prompt)
      else
         local play = get_cards(hand, input)
         printall(players, "Player " .. turn .. " played: " ..
                     lume.serialize(lume.map(play, card_name)))
         if(#play > 0 and is_allowed(play, last_non_pass(played))) then
            remove_cards(hand, play)
            printall(players, "  Player " .. turn .. " has " ..
                        #hand .. " cards left.")
            if #hand == 0 then
               printall(players, "Player " .. turn .. " won!")
            else
               table.insert(played, play)
               round(hands, next_turn, played, players)
            end
         else
            if(type(player_type) == "userdata") then
               player_type:send("You cannot play this.\n")
            elseif(player_type == "local") then
               print(player_type, "You cannot play this.")
            end
            round(hands, turn, played, players)
         end
      end
   end
end

local begin = function(players, seed)
   math.randomseed(tonumber(seed or os.time()))
   local hands = lume.map(deal(make_deck()), lume.sort)
   local starting_player = who_has(1.4, hands)
   for i,player_type in pairs(players) do
      if(player_type:match("^remote:[0-9]+")) then
         players[i] = connect(tonumber(lume.split(player_type, ":")[2]), i)
      end
   end
   return round(hands, starting_player, {}, players)
end

local _env, args = ...

if(args[1] == "--help") then
   print("Play a card game.")
   print("Usage:")
   print("  gangof4 # play against 3 computer players")
   print("  gangof4 computer local local local " ..
            "# play hot-seat with one computer player")
elseif(#args ~= 0) then
   begin(lume.slice(arg, 1, 4), arg[5])
else
   begin({"computer", "computer", "computer", "local"})
end

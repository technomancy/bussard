local choose_from = function(l)
   return l[math.random(1,#l)]
end

local normal_random = function(mean, dev)
   local x = (1 + math.sqrt (-2 * math.log(math.random())))
   local y = (math.cos(2*math.pi*math.random()) / 2)
   return mean + dev * x * y
end

local concatenate_lists = function(args)
   local res = {}
   for _, t in ipairs(args) do
      for _, v in pairs(t) do
         table.insert(res, v)
      end
   end
   return res
end

random_name = function (name, syllables)
   math.randomseed(os.time()+os.clock()*1e9)

   local common_closed = {"b","c","d","f","g","h","k","l","m","n","p","r","s","t","w"}
   local uncommon_closed = {"x","z","q","v","v","j","j","gg","ll","ss","tt"}
   local enders = {"b","d","g","m","n","s","r","t"}
   local closed = concatenate_lists({common_closed, common_closed, enders, uncommon_closed})
   local vowels = {"a","e","i","o","u","ie","ou"}
   local syllables = syllables or math.ceil(normal_random(2.5, 1.5))
   name = name or table.concat({string.upper(choose_from(common_closed)),
      choose_from(vowels),choose_from(closed)},"")
   if(syllables < 3) then return name .. choose_from(vowels) .. choose_from(enders) else
      return random_name(name .. choose_from(vowels)..choose_from(closed), syllables-1)
   end
end

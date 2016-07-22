local t = require("lunatest")

local ship = require("ship")
local mail = require("mail")

function test_replies()
   -- set event
   ship.events["invite_nari"] = nil
   t.assert_true(mail.replyable("84f7b207-08e0-4a54-af7c-d6f97aedc703"))
   mail.reply(ship, "84f7b207-08e0-4a54-af7c-d6f97aedc703")
   t.assert_true(ship.events["invite_nari"])
   -- accept mission
   lume.clear(ship.active_missions)
   lume.remove(ship.upgrade_names, "life_support")
   t.assert_false(ship.active_missions["4d6ce083-4d5e-4ca9-b58f-479ae6de27dc"])
   t.assert_true(mail.replyable("4d6ce083-4d5e-4ca9-b58f-479ae6de27dc"))
   mail.reply(ship, "4d6ce083-4d5e-4ca9-b58f-479ae6de27dc")
   t.assert_false(ship.active_missions["4d6ce083-4d5e-4ca9-b58f-479ae6de27dc"])
   table.insert(ship.upgrade_names, "life_support")
   mail.reply(ship, "4d6ce083-4d5e-4ca9-b58f-479ae6de27dc")
   t.assert_true(ship.active_missions["4d6ce083-4d5e-4ca9-b58f-479ae6de27dc"])
   -- deliver message
   lume.clear(ship.mail_delivered)
   t.assert_false(ship.mail_delivered["a40a6733-b2d1-4468-bc8f-a46e37cfdbf8"])
   t.assert_true(mail.replyable("c83c2439-f4cf-475f-95a6-f15cafc3db16"))
   mail.reply(ship, "c83c2439-f4cf-475f-95a6-f15cafc3db16")
   t.assert_true(ship.mail_delivered["c83c2439-f4cf-475f-95a6-f15cafc3db16"])
end

function test_deliver()
end

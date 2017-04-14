local t = require("lunatest")
local bencode = require("bencode")

local assert_kv_equal = function(t1, t2)
   t.assert_equal(lume.count(t1), lume.count(t2))
   for k in pairs(t1) do t.assert_equal(t1[k], t2[k]) end
end

local test_decoding_scalars = function()
   t.assert_equal(bencode.decode("i42e"), 42)
   t.assert_equal(bencode.decode("4:junk"), "junk")
end

local test_decoding_tables = function()
   assert_kv_equal(bencode.decode("l4:hurgi42ee"), {"hurg", 42})
   assert_kv_equal(bencode.decode("d3:bar4:spam3:fooi42ee"),
                   {bar="spam",foo=42})
end

local test_roundtrip = function()
   local tbl = {a=1,b=2}
   assert_kv_equal(tbl, bencode.decode(bencode.encode(tbl)))
end

return {test_decoding_scalars=test_decoding_scalars,
        test_decoding_tables=test_decoding_tables,
        test_roundtrip=test_roundtrip}

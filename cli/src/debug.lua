local _clueline = "???"

local function _clue_error(err)
	print(err .. "\nEstimated Clue line: " .. _clueline)
end

local ok, err = pcall(function()

{}

end)

if not ok then
	error(err .. "\nEstimated Clue line: " .. _clueline)
end
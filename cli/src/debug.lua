local _clueline = "???"

local function _clue_error(err)
	if not err:find("Estimated Clue line: ") then
		err = err .. "\nEstimated Clue line: " .. _clueline
	end
	error(err)
end

local ok, err = pcall(function()

{}

end)

if not ok then
	_clue_error(err)
end
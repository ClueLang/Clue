local _clueline, _errored, _errored_file = 0

local function _clue_error(err)
	if _errored then return end
	_errored = ("%s:%d: %s"):format(_errored_file, _clueline, err:match(".+: (.-)$"))
end

local ok, err = pcall(function()
{}
end)

if not ok then
	_errored_file = "main.clue"
	_clue_error(err)
	error(_errored)
end
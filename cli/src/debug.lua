local _clueline, _error

error = function(errmsg, n)
    _error = "Error: " .. errmsg
    error(errmsg, n)
end

xpcall(function()

{}

end, function(err)
    _error = _error .. "\nEstimated Clue line: " .. _clueline
end)

print(_error or "Code completed without errors.")

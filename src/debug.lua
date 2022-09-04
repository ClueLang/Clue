local _clueline

xpcall(function()

{}

end, function(err)
    error(err .. "\nEstimated Clue line: " .. _clueline)
end)
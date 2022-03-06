local require, modules
do
	local _require = _G.require
	local cache = {}
	function require(modname)
		local cached = cache[modname]
		if cached ~= nil then return cached end
		cached = modules[modname]
		if cached ~= nil then
			cached = cached()
			cache[modname] = cached
			return cached
		end
		return _require(modname)
	end
end

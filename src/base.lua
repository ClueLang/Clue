local require, modules
do
	local _require = _G.require
	local cache = {}
	local nils = {}
	function require(modname)
		if nils[modname] then return end
		local cached = cache[modname]
		if cached ~= nil then return cached end
		cached = modules[modname]
		if cached ~= nil then
			cached = cached();
			(cached ~= nil and cache or nils)[modname] = cached
			return cached
		end
		return _require(modname)
	end
end

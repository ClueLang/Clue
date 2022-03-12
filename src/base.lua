local import, _modules
do
	local cache = {}
	local nils = {}
	function import(modname)
		if nils[modname] then return end
		local cached = cache[modname]
		if cached ~= nil then return cached end
		cached = _modules[modname]
		if cached ~= nil then
			cached = cached();
			(cached ~= nil and cache or nils)[modname] = cached
			return cached
		end
	end
end
_modules = {
	
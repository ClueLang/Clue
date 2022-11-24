--STATICS
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
			if cached == nil then
				nils[modname] = true
			else
				cache[modname] = cached
			end
			return cached
		end
	end
end
_modules = {§}
if _modules["main"] then
	return import("main")
else
	error("File \"main.clue\" was not found!")
end
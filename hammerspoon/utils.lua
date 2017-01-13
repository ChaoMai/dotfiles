utils = {}

function utils.keyBind(hyper, keyFuncTable)
  for key, fn in pairs(keyFuncTable) do
    hs.hotkey.bind(hyper, key, fn)
  end
end

return utils

module_list = { "modules/window_management", "modules/window_switcher" }

for i = 1, #module_list do
  require(module_list[i])
end

function reloadConfig(files)
  doReload = false

  for _, file in pairs(files) do
    if file:sub(-4) == ".lua" then
      doReload = true
    end
  end

  if doReload then
    hs.reload()
  end
end

function reload()
  hs.reload()
end

local myWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()

local utils = require("utils")

utils.keyBind({"ctrl", "alt", "cmd"}, {
  r = reload
})

hs.notify.new({title="Hammerspoon", informativeText="Config loaded"}):send()

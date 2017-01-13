-- -----------------------------------------------------------------------
--                         ** Something Global **                       --
-- -----------------------------------------------------------------------
local filter = hs.window.filter.new()
local switcher = hs.window.switcher.new(filter:setCurrentSpace(true):setDefaultFilter{})

switcher.ui.textColor = {0.6, 0.6, 0.6}
switcher.ui.fontName = 'Helvetica'
switcher.ui.textSize = 15
switcher.ui.highlightColor = {0.1, 0.1, 0.1, 0.9}
switcher.ui.backgroundColor = {0.2, 0.2, 0.2, 0.9}
switcher.ui.showTitles = true
switcher.ui.titleBackgroundColor = {0.2, 0.2, 0.2}
switcher.ui.onlyActiveApplication = true
switcher.ui.showThumbnails = true
switcher.ui.thumbnailSize = 150
switcher.ui.showSelectedThumbnail = false
switcher.ui.selectedThumbnailSize = 300
switcher.ui.showSelectedTitle = false

-- -----------------------------------------------------------------------
--                          ** Functions **                             --
-- -----------------------------------------------------------------------

local function nextWindow()
  switcher:next()
end

local function previousWindow()
  switcher:previous()
end

-- -----------------------------------------------------------------------
--                           ** Key Binding **                          --
-- -----------------------------------------------------------------------
local utils = require("utils")

utils.keyBind({"alt"}, {
  tab = nextWindow
})

utils.keyBind({"alt", "shift"}, {
  tab = previousWindow
})

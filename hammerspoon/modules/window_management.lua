-- -----------------------------------------------------------------------
--                         ** Something Global **                       --
-- -----------------------------------------------------------------------
grid = require "hs.grid"
grid.setMargins('0, 0')

-- Set screen watcher, in case you connect a new monitor, or unplug a monitor
screens = {}
local screenwatcher = hs.screen.watcher.new(function()
    screens = hs.screen.allScreens()
end)
screenwatcher:start()

-- Set screen grid depending on resolution
for _index, screen in pairs(hs.screen.allScreens()) do
    size = hs.geometry.size(40, math.floor(screen:frame().w / screen:frame().h * 40))
    grid.setGrid(size, screen)
end

-- Some constructors, just for programming
local function Cell(x, y, w, h)
    return hs.geometry(x, y, w, h)
end

local current = {}

function current:new()
    win = hs.window.focusedWindow()
    scr = hs.window.focusedWindow():screen()

    o = {}
    setmetatable(o, self)
    o.window, o.screen = win, scr
    o.windowGrid = grid.get(win)
    o.screenGrid = grid.getGrid(scr)
    return o
end

-- -----------------------------------------------------------------------
--                          ** Functions **                             --
-- -----------------------------------------------------------------------

local function maximizeWindow()
    local this = current:new()
    hs.grid.maximizeWindow(this.window)
end

local function centerOnScreen()
    local this = current:new()
    this.window:centerOnScreen(this.screen)
end

local function moveWindowOneSpace(direction)
    local mouseOrigin = hs.mouse.getAbsolutePosition()
    local win = hs.window.frontmostWindow()
    local clickPoint = win:zoomButtonRect()

    clickPoint.x = clickPoint.x + clickPoint.w + 3
    clickPoint.y = clickPoint.y + (clickPoint.h / 2)

    local mouseClickEvent = hs.eventtap.event.newMouseEvent(hs.eventtap.event.types.leftmousedown, clickPoint)
    mouseClickEvent:post()
    hs.timer.usleep(300000)

    local nextSpaceDownEvent = hs.eventtap.event.newKeyEvent({"ctrl"}, direction, true)
    nextSpaceDownEvent:post()
    hs.timer.usleep(150000)

    local nextSpaceUpEvent = hs.eventtap.event.newKeyEvent({"ctrl"}, direction, false)
    nextSpaceUpEvent:post()
    hs.timer.usleep(150000)

    local mouseReleaseEvent = hs.eventtap.event.newMouseEvent(hs.eventtap.event.types.leftmouseup, clickPoint)
    mouseReleaseEvent:post()
    hs.timer.usleep(150000)

    hs.mouse.setAbsolutePosition(mouseOrigin)
end

local function moveWindowLeft()
    moveWindowOneSpace("left")
end

local function moveWindowRight()
    moveWindowOneSpace("right")
end

local function throwLeft()
    local this = current:new()
    this.window:moveOneScreenWest()
end

local function throwRight()
    local this = current:new()
    this.window:moveOneScreenEast()
end

local function verticalMaxToggle()
    -- cursize.w = 1440
    -- cursize.h = 876
    local this = current:new()

    if this.windowGrid.h == this.screenGrid.h then
        local cell = Cell(this.windowGrid.x, 0, this.windowGrid.w, this.screenGrid.h * 0.75)
        grid.set(this.window, cell, this.screen)
    elseif this.screenGrid.h - this.windowGrid.y < this.screenGrid.h * 0.75 then
        local cell = Cell(this.windowGrid.x, this.screenGrid.h - this.screenGrid.h * 0.75, this.windowGrid.w, this.screenGrid.h * 0.75)
        grid.set(this.window, cell, this.screen)
    else
        local cell = Cell(this.windowGrid.x, 0, this.windowGrid.w, this.screenGrid.h)
        grid.set(this.window, cell, this.screen)
    end
end

local function horizontalMaxToggle()
    local this = current:new()

    if this.windowGrid.w == this.screenGrid.w then
        local cell = Cell(0, this.windowGrid.y, this.screenGrid.w * 0.75, this.windowGrid.h)
        grid.set(this.window, cell, this.screen)
    elseif this.screenGrid.w - this.windowGrid.x < this.screenGrid.w * 0.75 then
        local cell = Cell(this.screenGrid.w - this.screenGrid.w * 0.75, 0, this.screenGrid.w * 0.75, this.windowGrid.h)
        grid.set(this.window, cell, this.screen)
    else
        local cell = Cell(0, this.windowGrid.y, this.screenGrid.w, this.windowGrid.h)
        grid.set(this.window, cell, this.screen)
    end
end

local function maxToggle()
    local this = current:new()

    if (this.windowGrid.w == this.screenGrid.w and this.windowGrid.h == this.screenGrid.h) then
        local cursize = win:size()
        cursize.w = 1440
        cursize.h = 877
        this.window:setSize(cursize)
        -- local cell = Cell(this.windowGrid.x, this.windowGrid.y, this.screenGrid.w * 0.75, this.screenGrid.h * 0.75)
        grid.set(this.window, cell, this.screen)
    else
        hs.grid.maximizeWindow(this.window)
    end
end

local function moveToLeft()
    local this = current:new()
    local cell = Cell(0, this.windowGrid.y, this.windowGrid.w, this.windowGrid.h)
    grid.set(this.window, cell, this.screen)
end

local function moveToRight()
    local this = current:new()
    local cell = Cell(this.screenGrid.w - this.windowGrid.w, this.windowGrid.y, this.windowGrid.w, this.windowGrid.h)
    grid.set(this.window, cell, this.screen)
end

local function leftHalf()
    local this = current:new()
    local cell = Cell(0, 0, 0.5 * this.screenGrid.w, this.screenGrid.h)
    grid.set(this.window, cell, this.screen)
end

local function rightHalf()
    local this = current:new()
    local cell = Cell(0.5 * this.screenGrid.w, 0, 0.5 * this.screenGrid.w, this.screenGrid.h)
    grid.set(this.window, cell, this.screen)
end

local function topHalf()
    local this = current:new()
    local cell = Cell(0, 0, this.screenGrid.w, 0.5 * this.screenGrid.h)
    grid.set(this.window, cell, this.screen)
end

local function bottomHalf()
    local this = current:new()
    local cell = Cell(0, 0.5 * this.screenGrid.h, this.screenGrid.w, 0.5 * this.screenGrid.h)
    grid.set(this.window, cell, this.screen)
end

local function rightToLeft()
    local this = current:new()
    local cell = Cell(this.windowGrid.x, this.windowGrid.y, this.windowGrid.w - 1, this.windowGrid.h)
    if this.windowGrid.w > 1 then
        grid.set(this.window, cell, this.screen)
    else
        hs.alert.show("Small Enough :)")
    end
end

local function rightToRight()
    local this = current:new()
    local cell = Cell(this.windowGrid.x, this.windowGrid.y, this.windowGrid.w + 1, this.windowGrid.h)
    if this.windowGrid.w < this.screenGrid.w - this.windowGrid.x then
        grid.set(this.window, cell, this.screen)
    else
        hs.alert.show("Touching Right Edge :|")
    end
end

local function bottomUp()
    local this = current:new()
    local cell = Cell(this.windowGrid.x, this.windowGrid.y, this.windowGrid.w, this.windowGrid.h - 1)
    if this.windowGrid.h > 1 then
        grid.set(this.window, cell, this.screen)
    else
        hs.alert.show("Small Enough :)")
    end
end

local function bottomDown()
    local this = current:new()
    local cell = Cell(this.windowGrid.x, this.windowGrid.y, this.windowGrid.w, this.windowGrid.h + 1)
    if this.windowGrid.h < this.screenGrid.h - this.windowGrid.y then
        grid.set(this.window, cell, this.screen)
    else
        hs.alert.show("Touching Bottom Edge :|")
    end
end

local function leftToLeft()
    local this = current:new()
    local cell = Cell(this.windowGrid.x - 1, this.windowGrid.y, this.windowGrid.w + 1, this.windowGrid.h)
    if this.windowGrid.x > 0 then
        grid.set(this.window, cell, this.screen)
    else
        hs.alert.show("Touching Left Edge :|")
    end
end

local function leftToRight()
    local this = current:new()
    local cell = Cell(this.windowGrid.x + 1, this.windowGrid.y, this.windowGrid.w - 1, this.windowGrid.h)
    if this.windowGrid.w > 1 then
        grid.set(this.window, cell, this.screen)
    else
        hs.alert.show("Small Enough :)")
    end
end

local function topUp()
    local this = current:new()
    local cell = Cell(this.windowGrid.x, this.windowGrid.y - 1, this.windowGrid.w, this.windowGrid.h + 1)
    if this.windowGrid.y > 0 then
        grid.set(this.window, cell, this.screen)
    else
        hs.alert.show("Touching Top Edge :|")
    end
end

local function topDown()
    local this = current:new()
    local cell = Cell(this.windowGrid.x, this.windowGrid.y + 1, this.windowGrid.w, this.windowGrid.h - 1)
    if this.windowGrid.h > 1 then
        grid.set(this.window, cell, this.screen)
    else
        hs.alert.show("Small Enough :)")
    end
end

local function zoomInWindow()
    local this = current:new()
    local cell = Cell(this.windowGrid.x, this.windowGrid.y, this.windowGrid.w + 1, this.windowGrid.h + 1)
    if this.windowGrid.h < this.screenGrid.h - this.windowGrid.y then
        grid.set(this.window, cell, this.screen)
    else
        hs.alert.show("Touching Bottom Edge :|")
    end

    if this.windowGrid.w < this.screenGrid.w - this.windowGrid.x then
        grid.set(this.window, cell, this.screen)
    else
        hs.alert.show("Touching Right Edge :|")
    end

    this.window:centerOnScreen(this.screen)
end

local function zoomOutWindow()
    local this = current:new()
    local cell = Cell(this.windowGrid.x, this.windowGrid.y, this.windowGrid.w - 1, this.windowGrid.h - 1)
    if this.windowGrid.h > 1 then
        grid.set(this.window, cell, this.screen)
    else
        hs.alert.show("Small Enough :)")
    end

    if this.windowGrid.w > 1 then
        grid.set(this.window, cell, this.screen)
    else
        hs.alert.show("Small Enough :)")
    end

    this.window:centerOnScreen(this.screen)
end

-- -----------------------------------------------------------------------
--                           ** Key Binding **                          --
-- -----------------------------------------------------------------------
local utils = require("utils")

-- * Move window to another space
-- utils.keyBind({"ctrl", "alt"}, {
    -- left = moveWindowLeft,
    -- right = moveWindowRight
-- })

-- * Move window to another screen
utils.keyBind({"ctrl", "shift"}, {
    j = throwLeft,
    k = throwRight
})

-- * Set Window Position on screen
utils.keyBind({"ctrl", "alt", "cmd"}, {
    m = maximizeWindow,
    n = verticalMaxToggle,
    c = centerOnScreen,
    h = leftHalf,
    l = rightHalf,
    k = moveToRight,
    j = moveToLeft,
    s = maxToggle,
    b = horizontalMaxToggle
})

-- * Set Window Position on screen
-- utils.keyBind({"ctrl", "alt", "shift"}, {
    -- h = rightToLeft,
    -- l = rightToRight,
    -- k = bottomUp,
    -- j = bottomDown
-- })

-- * Set Window Position on screen
-- utils.keyBind({"alt", "cmd", "shift"}, {
    -- h = leftToLeft,
    -- l = leftToRight,
    -- k = topUp,
    -- j = topDown
-- })

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Glass-style to-do list application.
--
-- Demonstrates:
--
--   * 'NSVisualEffectView' for a frosted-glass (HUD) window
--   * Auto-reflowing layout via 'NSStackView' (vertical + horizontal)
--   * 'NSTableView' with generated 'NSTableViewDataSource' and
--     'NSTableViewDelegate' delegate modules (no Template Haskell)
--   * 'ActionTarget' for button and text-field actions
--   * Return-key binding on the input field
module Main (main) where

import Control.Monad (when)
import Data.IORef
import Data.String (fromString)
import Foreign.C.Types (CDouble(..), CFloat)
import Foreign.Ptr (nullPtr)

import ObjC.Runtime

-- Generated AppKit / Foundation bindings ------------------------------------
import ObjC.AppKit.NSApplication
  ( NSApplication
  , pattern NSApplicationActivationPolicyRegular
  )
import ObjC.AppKit.NSWindow
  ( NSWindow
  , pattern NSWindowStyleMaskTitled
  , pattern NSWindowStyleMaskClosable
  , pattern NSWindowStyleMaskMiniaturizable
  , pattern NSWindowStyleMaskResizable
  , pattern NSWindowStyleMaskFullSizeContentView
  , pattern NSBackingStoreBuffered
  )
import ObjC.AppKit.NSTextField ()
import ObjC.AppKit.NSButton ()
import ObjC.AppKit.NSColor ()
import ObjC.AppKit.NSMenu (NSMenu)
import ObjC.AppKit.NSMenuItem (NSMenuItem)
import ObjC.AppKit.NSView
  ( IsNSView(toNSView), NSView
  , pattern NSViewWidthSizable
  , pattern NSViewHeightSizable
  , pattern NSViewMinXMargin
  )
import ObjC.AppKit.NSScrollView (NSScrollView)
import ObjC.AppKit.NSVisualEffectView
  ( NSVisualEffectView
  , pattern NSVisualEffectMaterialHUDWindow
  , pattern NSVisualEffectBlendingModeBehindWindow
  , pattern NSVisualEffectStateActive
  )
import ObjC.AppKit.NSStackView
  ( NSStackView
  , pattern NSLayoutConstraintOrientationHorizontal
  , pattern NSLayoutConstraintOrientationVertical
  , pattern NSUserInterfaceLayoutOrientationHorizontal
  , pattern NSUserInterfaceLayoutOrientationVertical
  , pattern NSStackViewDistributionFill
  , pattern NSLayoutAttributeLeading
  )
import ObjC.AppKit.NSTableView
  ( NSTableView
  , pattern NSTableViewStylePlain
  , pattern NSTableViewSelectionHighlightStyleNone
  , pattern NSTableViewLastColumnOnlyAutoresizingStyle
  )
import ObjC.AppKit.NSTableColumn
  ( NSTableColumn
  , pattern NSTableColumnAutoresizingMask
  )
import ObjC.AppKit.NSTableHeaderView (NSTableHeaderView)
import ObjC.Foundation.NSString (NSString)
import ObjC.Foundation.Structs (NSRect(..), NSPoint(..), NSSize(..), NSEdgeInsets(..))

-- Generated delegate modules ------------------------------------------------
import ObjC.AppKit.Delegate.NSTableViewDataSource
  ( NSTableViewDataSourceOverrides(..)
  , defaultNSTableViewDataSourceOverrides
  , newNSTableViewDataSource
  )
import ObjC.AppKit.Delegate.NSTableViewDelegate
  ( NSTableViewDelegateOverrides(..)
  , defaultNSTableViewDelegateOverrides
  , newNSTableViewDelegate
  )
import ObjC.AppKit.Delegate.NSApplicationDelegate
  ( NSApplicationDelegateOverrides(..)
  , defaultNSApplicationDelegateOverrides
  , newNSApplicationDelegate
  )

import qualified ObjC.AppKit.NSApplication as App
import qualified ObjC.AppKit.NSWindow as Win
import qualified ObjC.AppKit.NSTextField as TF
import qualified ObjC.AppKit.NSView as View
import qualified ObjC.AppKit.NSMenu as Menu
import qualified ObjC.AppKit.NSMenuItem as MI
import qualified ObjC.AppKit.NSControl as Ctrl
import qualified ObjC.AppKit.NSFont as Font
import qualified ObjC.AppKit.NSButton as Btn
import qualified ObjC.AppKit.NSColor as Color
import qualified ObjC.AppKit.NSScrollView as SV
import qualified ObjC.AppKit.NSVisualEffectView as VEV
import qualified ObjC.AppKit.NSStackView as StkV
import qualified ObjC.AppKit.NSTableView as TV
import qualified ObjC.AppKit.NSTableColumn as TC
import qualified ObjC.Foundation.NSString as Str

-- ---------------------------------------------------------------------------
-- Domain
-- ---------------------------------------------------------------------------

data TodoItem = TodoItem
  { todoId        :: !Int
  , todoText      :: !(Id NSString)
  , todoCompleted :: !Bool
  }

-- ---------------------------------------------------------------------------
-- Layout constants
-- ---------------------------------------------------------------------------

winW, winH, rowH :: CDouble
winW = 480
winH = 600
rowH = 48

-- ---------------------------------------------------------------------------
-- Action selectors
-- ---------------------------------------------------------------------------

-- | Handlers that use the sender (for rowForView:)
toggleItemSel :: Selector '[Id NSView] ()
toggleItemSel = mkSelector "toggleItem:"

deleteItemSel :: Selector '[Id NSView] ()
deleteItemSel = mkSelector "deleteItem:"

-- | Handler that ignores the sender
addItemSel :: Sel
addItemSel = mkSelector "addItem:"

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = withAutoreleasePool $ do
  loadFramework "AppKit"

  app <- App.sharedApplication
  _ <- App.setActivationPolicy app NSApplicationActivationPolicyRegular
  setupMenuBar app

  -- Mutable state ----------------------------------------------------------
  nextIdRef <- newIORef (1 :: Int)
  itemsRef  <- newIORef ([] :: [TodoItem])

  -- Window -----------------------------------------------------------------
  let styleMask = NSWindowStyleMaskTitled
               <> NSWindowStyleMaskClosable
               <> NSWindowStyleMaskMiniaturizable
               <> NSWindowStyleMaskResizable
               <> NSWindowStyleMaskFullSizeContentView

  window <- alloc @NSWindow >>= \w ->
    Win.initWithContentRect_styleMask_backing_defer w
      (NSRect (NSPoint 200 100) (NSSize winW winH))
      styleMask NSBackingStoreBuffered False

  Win.setTitle window ("" :: Id NSString)
  Win.setTitlebarAppearsTransparent window True
  Color.clearColor >>= Win.setBackgroundColor window
  Win.center window
  Win.setReleasedWhenClosed window False

  cv <- Win.contentView window

  -- Glass background (NSVisualEffectView) ----------------------------------
  veView <- alloc @NSVisualEffectView >>= \v ->
    View.initWithFrame v (NSRect (NSPoint 0 0) (NSSize winW winH))
  let ve = unsafeCastId veView :: Id NSVisualEffectView
  VEV.setMaterial    ve NSVisualEffectMaterialHUDWindow
  VEV.setBlendingMode ve NSVisualEffectBlendingModeBehindWindow
  VEV.setState       ve NSVisualEffectStateActive
  View.setAutoresizingMask ve (NSViewWidthSizable <> NSViewHeightSizable)
  View.addSubview cv (toNSView ve)

  -- Reusable colours -------------------------------------------------------
  white    <- Color.whiteColor
  dimWhite <- Color.colorWithSRGBRed_green_blue_alpha 1 1 1 0.5

  -- =======================================================================
  -- Main vertical stack view (auto-reflowing layout)
  -- =======================================================================
  mainStack <- fmap unsafeCastId
    (alloc @NSStackView >>= \sv ->
      View.initWithFrame sv (NSRect (NSPoint 0 0) (NSSize winW winH)))
    :: IO (Id NSStackView)
  StkV.setOrientation  mainStack NSUserInterfaceLayoutOrientationVertical
  StkV.setSpacing      mainStack 8
  StkV.setDistribution mainStack NSStackViewDistributionFill
  StkV.setAlignment    mainStack NSLayoutAttributeLeading
  -- Edge insets: top accounts for the transparent titlebar
  StkV.setEdgeInsets   mainStack (NSEdgeInsets 44 20 20 20)
  View.setAutoresizingMask mainStack (NSViewWidthSizable <> NSViewHeightSizable)
  View.addSubview ve (toNSView mainStack)

  -- Title ------------------------------------------------------------------
  titleLabel <- TF.labelWithString ("Todo List" :: Id NSString)
  Font.boldSystemFontOfSize 28 >>= Ctrl.setFont titleLabel
  TF.setTextColor titleLabel white
  StkV.addArrangedSubview mainStack (toNSView titleLabel)

  -- Subtitle (item count) --------------------------------------------------
  subtitleLabel <- TF.labelWithString ("No tasks yet" :: Id NSString)
  Font.systemFontOfSize 13 >>= Ctrl.setFont subtitleLabel
  TF.setTextColor subtitleLabel dimWhite
  StkV.addArrangedSubview mainStack (toNSView subtitleLabel)

  -- =======================================================================
  -- Input row (horizontal stack)
  -- =======================================================================
  inputStack <- fmap unsafeCastId
    (alloc @NSStackView >>= \sv ->
      View.initWithFrame sv (NSRect (NSPoint 0 0) (NSSize 100 32)))
    :: IO (Id NSStackView)
  StkV.setOrientation  inputStack NSUserInterfaceLayoutOrientationHorizontal
  StkV.setSpacing      inputStack 8
  StkV.setDistribution inputStack NSStackViewDistributionFill

  inputField <- TF.textFieldWithString ("" :: Id NSString)
  TF.setPlaceholderString inputField ("What needs to be done?" :: Id NSString)
  Font.systemFontOfSize 15 >>= Ctrl.setFont inputField
  -- Low horizontal hugging so the text field stretches to fill
  View.setContentHuggingPriority_forOrientation inputField
    (200 :: CFloat) NSLayoutConstraintOrientationHorizontal
  StkV.addArrangedSubview inputStack (toNSView inputField)

  -- Add button (target wired after creation below)
  addBtn <- Btn.buttonWithTitle_target_action
    ("Add" :: Id NSString) (RawId nullPtr) (asSel addItemSel)
  StkV.addArrangedSubview inputStack (toNSView addBtn)

  StkV.addArrangedSubview mainStack (toNSView inputStack)

  -- =======================================================================
  -- Scrollable table
  -- =======================================================================
  scrollView <- alloc @NSScrollView >>= \sv ->
    SV.initWithFrame sv (NSRect (NSPoint 0 0) (NSSize 100 100))
  SV.setHasVerticalScroller scrollView True
  SV.setDrawsBackground     scrollView False
  -- Low vertical hugging so the scroll view fills remaining space
  View.setContentHuggingPriority_forOrientation scrollView
    (200 :: CFloat) NSLayoutConstraintOrientationVertical

  tableView <- alloc @NSTableView >>= \tv ->
    TV.initWithFrame tv (NSRect (NSPoint 0 0) (NSSize 100 100))

  -- Single column that auto-resizes to fill the table width
  col <- alloc @NSTableColumn >>= \c ->
    TC.initWithIdentifier c ("main" :: Id NSString)
  TC.setResizingMask col NSTableColumnAutoresizingMask
  TV.addTableColumn tableView col

  TV.setRowHeight tableView rowH
  TV.setSelectionHighlightStyle tableView NSTableViewSelectionHighlightStyleNone
  TV.setColumnAutoresizingStyle tableView NSTableViewLastColumnOnlyAutoresizingStyle
  TV.setStyle tableView NSTableViewStylePlain
  Color.clearColor >>= TV.setBackgroundColor tableView
  -- Remove the header
  TV.setHeaderView tableView (nilId :: Id NSTableHeaderView)

  SV.setDocumentView scrollView tableView
  StkV.addArrangedSubview mainStack (toNSView scrollView)

  -- Status bar -------------------------------------------------------------
  statusLabel <- TF.labelWithString ("" :: Id NSString)
  Font.systemFontOfSize 12 >>= Ctrl.setFont statusLabel
  TF.setTextColor statusLabel dimWhite
  StkV.addArrangedSubview mainStack (toNSView statusLabel)

  -- =======================================================================
  -- Shared helper: update subtitle + status from current items
  -- =======================================================================
  let updateLabels = do
        items <- readIORef itemsRef
        let n    = length items
            done = length (filter todoCompleted items)
        if n == 0
          then do
            Ctrl.setStringValue subtitleLabel ("No tasks yet" :: Id NSString)
            Ctrl.setStringValue statusLabel   ("" :: Id NSString)
          else do
            let suf = if n == 1 then " task" else " tasks"
            Ctrl.setStringValue subtitleLabel
              (fromString (show n ++ suf) :: Id NSString)
            Ctrl.setStringValue statusLabel
              (fromString (show done ++ " of " ++ show n ++ " completed")
                :: Id NSString)

  -- =======================================================================
  -- Table actions target (shared by all row buttons)
  -- =======================================================================
  tableActionsTarget <- newActionTarget
    [ toggleItemSel := \sender -> do
          rowIdx <- TV.rowForView tableView sender
          when (rowIdx >= 0) $ do
            items <- readIORef itemsRef
            let idx = fromIntegral rowIdx :: Int
                iid = todoId (items !! idx)
            modifyIORef' itemsRef (map (\i ->
              if todoId i == iid
                then i { todoCompleted = not (todoCompleted i) }
                else i))
            TV.reloadData tableView
            updateLabels

    , deleteItemSel := \sender -> do
          rowIdx <- TV.rowForView tableView sender
          when (rowIdx >= 0) $ do
            items <- readIORef itemsRef
            let idx = fromIntegral rowIdx :: Int
                iid = todoId (items !! idx)
            modifyIORef' itemsRef (filter (\i -> todoId i /= iid))
            TV.reloadData tableView
            updateLabels
    ]

  -- =======================================================================
  -- Data source (NSTableViewDataSource protocol)
  -- =======================================================================
  dataSource <- newNSTableViewDataSource defaultNSTableViewDataSourceOverrides
    { _numberOfRowsInTableView = Just $ \_tv -> do
        items <- readIORef itemsRef
        pure (length items)
    }

  -- =======================================================================
  -- Delegate (NSTableViewDelegate protocol)
  -- =======================================================================
  delegate <- newNSTableViewDelegate defaultNSTableViewDelegateOverrides
    { _tableView_viewForTableColumn_row = Just $ \_tv _col rowIdx -> do
        items <- readIORef itemsRef
        let item = items !! rowIdx
            done = todoCompleted item

        -- Row container view
        rowView <- alloc @NSView >>= \v ->
          View.initWithFrame v (NSRect (NSPoint 0 0) (NSSize 440 rowH))

        -- Checkbox
        cb <- Btn.checkboxWithTitle_target_action
          ("" :: Id NSString) tableActionsTarget (asSel toggleItemSel)
        View.setFrame cb (NSRect (NSPoint 12 12) (NSSize 24 24))
        when done $ Btn.setState cb 1
        View.addSubview rowView (toNSView cb)

        -- Text label  (explicit white for readability on HUD background)
        lbl <- TF.labelWithString (todoText item)
        View.setFrame lbl (NSRect (NSPoint 44 13) (NSSize 340 22))
        Font.systemFontOfSize 15 >>= Ctrl.setFont lbl
        if done
          then Color.colorWithSRGBRed_green_blue_alpha 1 1 1 0.35
                 >>= TF.setTextColor lbl
          else TF.setTextColor lbl white
        -- Stretch horizontally when the column resizes
        View.setAutoresizingMask lbl NSViewWidthSizable
        View.addSubview rowView (toNSView lbl)

        -- Delete button (pinned to right edge via autoresizing)
        del <- Btn.buttonWithTitle_target_action
          ("✕" :: Id NSString) tableActionsTarget (asSel deleteItemSel)
        View.setFrame del (NSRect (NSPoint 396 10) (NSSize 32 28))
        Btn.setBordered del False
        View.setAutoresizingMask del NSViewMinXMargin
        View.addSubview rowView (toNSView del)

        -- Return with retain+autorelease for safe handoff to the table view
        retainAutorelease rowView
    }

  -- Wire delegate + data source
  TV.setDelegate   tableView delegate
  TV.setDataSource tableView dataSource

  -- =======================================================================
  -- Add target
  -- =======================================================================
  addTarget' <- newActionTarget
    [ addItemSel := do
          text <- Ctrl.stringValue inputField
          len <- Str.length_ text
          when (len > 0) $ do
            iid <- readIORef nextIdRef
            modifyIORef' nextIdRef (+ 1)
            modifyIORef' itemsRef (++ [TodoItem iid text False])
            Ctrl.setStringValue inputField ("" :: Id NSString)
            TV.reloadData tableView
            updateLabels
    ]

  -- Wire add button and input field Return key
  Ctrl.setTarget addBtn  addTarget'
  Ctrl.setAction addBtn  (asSel addItemSel)
  Ctrl.setTarget inputField addTarget'
  Ctrl.setAction inputField (asSel addItemSel)

  -- Show window ------------------------------------------------------------
  appDelegate <- newNSApplicationDelegate defaultNSApplicationDelegateOverrides
    { _applicationShouldTerminateAfterLastWindowClosed = Just (const (pure False))
    , _applicationShouldHandleReopen_hasVisibleWindows = Just $ \_ hasVisible ->
        if hasVisible then pure True
        else Win.makeKeyAndOrderFront window (RawId nullPtr) >> pure True
    }
  App.setDelegate app appDelegate

  Win.makeKeyAndOrderFront window (RawId nullPtr)
  App.activateIgnoringOtherApps app True
  App.run app

-- ---------------------------------------------------------------------------
-- Menu bar
-- ---------------------------------------------------------------------------

-- | Minimal menu bar with Quit (Cmd+Q).
setupMenuBar :: Id NSApplication -> IO ()
setupMenuBar app = do
  menuBar <- alloc @NSMenu >>= \m -> Menu.initWithTitle m ("" :: Id NSString)
  appMenuItem <- new @NSMenuItem
  Menu.addItem menuBar appMenuItem
  appMenu <- alloc @NSMenu >>= \m -> Menu.initWithTitle m ("App" :: Id NSString)
  quitItem <- alloc @NSMenuItem >>= \mi ->
    MI.initWithTitle_action_keyEquivalent mi
      ("Quit" :: Id NSString) (mkSelector "terminate:") ("q" :: Id NSString)
  Menu.addItem appMenu quitItem
  Menu.setSubmenu_forItem menuBar appMenu appMenuItem
  App.setMainMenu app menuBar

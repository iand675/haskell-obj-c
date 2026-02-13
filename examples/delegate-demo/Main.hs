{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Delegate callback demo — an event-logger window.
--
-- Demonstrates:
--
--   * Generated 'NSWindowDelegate' callbacks (notification-style @IO ()@ and
--     boolean-returning @windowShouldClose:@)
--   * Generated 'NSControlTextEditingDelegate' callbacks
--   * 'ActionTarget' for buttons
--   * 'DelegateProxy' for combining two delegate protocols on one text field
--   * Mutable Haskell state shared across callbacks
--
-- Every delegate invocation appends a line to the scrollable event log so you
-- can visually verify that the callbacks are firing.
module Main (main) where

import Data.IORef
import Data.String (fromString)
import Foreign.C.Types (CDouble(..))
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
  , pattern NSBackingStoreBuffered
  )
import ObjC.AppKit.NSTextField ()
import ObjC.AppKit.NSButton ()
import ObjC.AppKit.NSColor ()
import ObjC.AppKit.NSMenu (NSMenu)
import ObjC.AppKit.NSMenuItem (NSMenuItem)
import ObjC.AppKit.NSView (IsNSView(toNSView))
import ObjC.AppKit.NSScrollView
  ( NSScrollView
  , pattern NSLineBorder
  )
import ObjC.AppKit.NSTextView (NSTextView)
import ObjC.Foundation.NSString (NSString)
import ObjC.Foundation.Structs (NSRect(..), NSPoint(..), NSSize(..))

-- Generated delegate modules ------------------------------------------------
import ObjC.AppKit.Delegate.NSWindowDelegate
  ( NSWindowDelegateOverrides(..)
  , defaultNSWindowDelegateOverrides
  , newNSWindowDelegate
  )
import ObjC.AppKit.Delegate.NSControlTextEditingDelegate
  ( NSControlTextEditingDelegateOverrides(..)
  , defaultNSControlTextEditingDelegateOverrides
  , newNSControlTextEditingDelegate
  )
import ObjC.AppKit.Delegate.NSTextFieldDelegate
  ( NSTextFieldDelegateOverrides(..)
  , defaultNSTextFieldDelegateOverrides
  , newNSTextFieldDelegate
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
import qualified ObjC.AppKit.NSText as Text
import qualified ObjC.AppKit.NSTextView as TV

-- ---------------------------------------------------------------------------
-- Layout constants
-- ---------------------------------------------------------------------------

winW, winH :: CDouble
winW = 560
winH = 480

-- ---------------------------------------------------------------------------
-- Action selectors
-- ---------------------------------------------------------------------------

clearLogSel :: Sel
clearLogSel = mkSelector "clearLog:"

fireEventSel :: Sel
fireEventSel = mkSelector "fireEvent:"

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = withAutoreleasePool $ do
  loadFramework "AppKit"

  app <- App.sharedApplication
  _ <- App.setActivationPolicy app NSApplicationActivationPolicyRegular
  setupMenuBar app

  -- Mutable state: event log lines and a close-attempt counter
  logRef   <- newIORef ([] :: [String])
  closeRef <- newIORef (0 :: Int)

  -- Window -----------------------------------------------------------------
  let styleMask = NSWindowStyleMaskTitled
               <> NSWindowStyleMaskClosable
               <> NSWindowStyleMaskMiniaturizable
               <> NSWindowStyleMaskResizable

  window <- alloc @NSWindow >>= \w ->
    Win.initWithContentRect_styleMask_backing_defer w
      (NSRect (NSPoint 200 100) (NSSize winW winH))
      styleMask NSBackingStoreBuffered False
  Win.setTitle window ("Delegate Demo — Event Logger" :: Id NSString)
  Win.center window

  cv <- Win.contentView window

  -- Header label -----------------------------------------------------------
  headerLabel <- TF.labelWithString
    ("Interact with the window to see delegate callbacks fire:" :: Id NSString)
  View.setFrame headerLabel (NSRect (NSPoint 20 (winH - 50)) (NSSize (winW - 40) 24))
  Font.systemFontOfSize 13 >>= Ctrl.setFont headerLabel
  View.addSubview cv (toNSView headerLabel)

  -- Scrollable text view for the event log ---------------------------------
  let logOrigin = NSPoint 20 80
      logSize   = NSSize (winW - 40) (winH - 140)
  scrollView <- alloc @NSScrollView >>= \sv ->
    SV.initWithFrame sv (NSRect logOrigin logSize)
  SV.setHasVerticalScroller scrollView True
  SV.setBorderType scrollView NSLineBorder

  logTextView <- alloc @NSTextView >>= \tv ->
    TV.initWithFrame tv (NSRect (NSPoint 0 0) logSize)
  TV.setEditable logTextView False
  TV.setSelectable logTextView True
  Font.monospacedSystemFontOfSize_weight 12 0 >>= Text.setFont logTextView
  Color.textBackgroundColor >>= TV.setBackgroundColor logTextView
  Color.textColor >>= Text.setTextColor logTextView

  SV.setDocumentView scrollView logTextView
  View.addSubview cv (toNSView scrollView)

  -- Helper: append a line to the event log ---------------------------------
  let appendLog :: String -> IO ()
      appendLog msg = do
        modifyIORef' logRef (++ [msg])
        allLines <- readIORef logRef
        let text = fromString (unlines allLines) :: Id NSString
        Text.setString logTextView text

  -- Input text field -------------------------------------------------------
  inputField <- TF.textFieldWithString ("" :: Id NSString)
  View.setFrame inputField (NSRect (NSPoint 20 36) (NSSize (winW - 200) 28))
  Font.systemFontOfSize 14 >>= Ctrl.setFont inputField
  TF.setPlaceholderString inputField ("Type here to test text field delegates…" :: Id NSString)
  View.addSubview cv (toNSView inputField)

  -- Buttons ----------------------------------------------------------------
  clearBtn <- Btn.buttonWithTitle_target_action
    ("Clear Log" :: Id NSString) (RawId nullPtr) (asSel clearLogSel)
  View.setFrame clearBtn (NSRect (NSPoint (winW - 170) 36) (NSSize 80 28))
  View.addSubview cv (toNSView clearBtn)

  fireBtn <- Btn.buttonWithTitle_target_action
    ("Fire Event" :: Id NSString) (RawId nullPtr) (asSel fireEventSel)
  View.setFrame fireBtn (NSRect (NSPoint (winW - 85) 36) (NSSize 80 28))
  View.addSubview cv (toNSView fireBtn)

  -- Status label at bottom -------------------------------------------------
  statusLabel <- TF.labelWithString ("Close attempts: 0 (first close is blocked)" :: Id NSString)
  View.setFrame statusLabel (NSRect (NSPoint 20 8) (NSSize (winW - 40) 20))
  Font.systemFontOfSize 11 >>= Ctrl.setFont statusLabel
  View.addSubview cv (toNSView statusLabel)

  -- =======================================================================
  -- ActionTarget: button actions
  -- =======================================================================
  buttonTarget <- newActionTarget
    [ clearLogSel := do
          writeIORef logRef []
          Text.setString logTextView ("" :: Id NSString)
          appendLog "[ActionTarget] Log cleared"

    , fireEventSel :=
          appendLog "[ActionTarget] Manual event fired via button"
    ]

  Ctrl.setTarget clearBtn buttonTarget
  Ctrl.setAction clearBtn (asSel clearLogSel)
  Ctrl.setTarget fireBtn  buttonTarget
  Ctrl.setAction fireBtn  (asSel fireEventSel)

  -- =======================================================================
  -- NSWindowDelegate: window lifecycle callbacks
  -- =======================================================================
  winDelegate <- newNSWindowDelegate defaultNSWindowDelegateOverrides
    { _windowShouldClose = Just $ \_notification -> do
        n <- readIORef closeRef
        modifyIORef' closeRef (+ 1)
        if n == 0
          then do
            appendLog "[NSWindowDelegate] windowShouldClose: -> False (blocked first close!)"
            Ctrl.setStringValue statusLabel
              (fromString ("Close attempts: " ++ show (n + 1) ++ " (next close will succeed)")
                :: Id NSString)
            pure False
          else do
            appendLog "[NSWindowDelegate] windowShouldClose: -> True (allowing close)"
            pure True

    , _windowDidResize = Just $ \_notification ->
        appendLog "[NSWindowDelegate] windowDidResize:"

    , _windowDidMove = Just $ \_notification ->
        appendLog "[NSWindowDelegate] windowDidMove:"

    , _windowDidBecomeKey = Just $ \_notification ->
        appendLog "[NSWindowDelegate] windowDidBecomeKey:"

    , _windowDidResignKey = Just $ \_notification ->
        appendLog "[NSWindowDelegate] windowDidResignKey:"

    , _windowDidBecomeMain = Just $ \_notification ->
        appendLog "[NSWindowDelegate] windowDidBecomeMain:"

    , _windowDidResignMain = Just $ \_notification ->
        appendLog "[NSWindowDelegate] windowDidResignMain:"

    , _windowWillMiniaturize = Just $ \_notification ->
        appendLog "[NSWindowDelegate] windowWillMiniaturize:"

    , _windowDidMiniaturize = Just $ \_notification ->
        appendLog "[NSWindowDelegate] windowDidMiniaturize:"

    , _windowDidDeminiaturize = Just $ \_notification ->
        appendLog "[NSWindowDelegate] windowDidDeminiaturize:"

    , _windowWillClose = Just $ \_notification ->
        appendLog "[NSWindowDelegate] windowWillClose:"

    , _windowWillStartLiveResize = Just $ \_notification ->
        appendLog "[NSWindowDelegate] windowWillStartLiveResize:"

    , _windowDidEndLiveResize = Just $ \_notification ->
        appendLog "[NSWindowDelegate] windowDidEndLiveResize:"
    }

  Win.setDelegate window winDelegate

  -- =======================================================================
  -- NSControlTextEditingDelegate: text field editing callbacks
  -- =======================================================================
  ctrlTextDelegate <- newNSControlTextEditingDelegate
    defaultNSControlTextEditingDelegateOverrides
    { _controlTextDidBeginEditing = Just $ \_notification ->
        appendLog "[NSControlTextEditingDelegate] controlTextDidBeginEditing:"

    , _controlTextDidEndEditing = Just $ \_notification ->
        appendLog "[NSControlTextEditingDelegate] controlTextDidEndEditing:"

    , _controlTextDidChange = Just $ \_notification ->
        appendLog "[NSControlTextEditingDelegate] controlTextDidChange:"

    , _control_textShouldBeginEditing = Just $ \_control _text -> do
        appendLog "[NSControlTextEditingDelegate] control:textShouldBeginEditing: -> True"
        pure True

    , _control_textShouldEndEditing = Just $ \_control _text -> do
        appendLog "[NSControlTextEditingDelegate] control:textShouldEndEditing: -> True"
        pure True
    }

  -- NSTextFieldDelegate (trivial, just to prove DelegateProxy works)
  tfDelegate <- newNSTextFieldDelegate defaultNSTextFieldDelegateOverrides
    { _textField_textView_shouldSelectCandidateAtIndex = Just $ \_tf _tv _idx -> do
        appendLog "[NSTextFieldDelegate] textField:textView:shouldSelectCandidateAtIndex: -> True"
        pure True
    }

  -- Combine via DelegateProxy so a single object handles both protocols
  combinedDelegate <- newDelegateProxy [ctrlTextDelegate, tfDelegate]

  TF.setDelegate inputField combinedDelegate

  -- Initial log entry
  appendLog "Delegate Demo started. Try:"
  appendLog "  - Resize or move the window"
  appendLog "  - Click the close button (first one is blocked)"
  appendLog "  - Miniaturize the window (Cmd+M)"
  appendLog "  - Type in the text field below"
  appendLog "  - Click the 'Fire Event' or 'Clear Log' buttons"
  appendLog ""

  -- Show window ------------------------------------------------------------
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
      ("Quit" :: Id NSString) (asSel App.terminateSelector) ("q" :: Id NSString)
  Menu.addItem appMenu quitItem
  Menu.setSubmenu_forItem menuBar appMenu appMenuItem
  App.setMainMenu app menuBar

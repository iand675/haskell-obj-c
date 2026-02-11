{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Counter example: A window with a numeric label and +/- buttons.
-- Demonstrates the 'ActionTarget' API for creating Objective-C target
-- objects backed by Haskell closures — no Template Haskell required.
module Main (main) where

import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.String (fromString)
import Foreign.Ptr (nullPtr)

import ObjC.Runtime
import ObjC.Runtime.ActionTarget (newActionTarget)

-- Generated AppKit bindings
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
import ObjC.AppKit.NSTextField (NSTextField)
import ObjC.AppKit.NSButton ()
import ObjC.AppKit.NSMenu (NSMenu)
import ObjC.AppKit.NSMenuItem (NSMenuItem)
import ObjC.AppKit.NSView (IsNSView(toNSView))
import ObjC.Foundation.NSString (NSString)
import ObjC.Foundation.Structs (NSRect(..), NSPoint(..), NSSize(..))

import qualified ObjC.AppKit.NSApplication as App
import qualified ObjC.AppKit.NSWindow as Win
import qualified ObjC.AppKit.NSTextField as TF
import qualified ObjC.AppKit.NSView as View
import qualified ObjC.AppKit.NSMenu as Menu
import qualified ObjC.AppKit.NSMenuItem as MI
import qualified ObjC.AppKit.NSControl as Ctrl
import qualified ObjC.AppKit.NSFont as Font
import qualified ObjC.AppKit.NSButton as Btn

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

updateLabel :: Id NSTextField -> Int -> IO ()
updateLabel label n = Ctrl.setStringValue label (fromString (show n) :: Id NSString)

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = withAutoreleasePool $ do
  loadFramework "AppKit"

  app <- App.sharedApplication
  _ <- App.setActivationPolicy app NSApplicationActivationPolicyRegular

  setupMenuBar app

  -- Window
  let styleMask = NSWindowStyleMaskTitled <> NSWindowStyleMaskClosable
               <> NSWindowStyleMaskMiniaturizable <> NSWindowStyleMaskResizable
      backing   = NSBackingStoreBuffered
      rect      = NSRect (NSPoint 200 200) (NSSize 320 180)

  window <- alloc @NSWindow >>=
    \w -> Win.initWithContentRect_styleMask_backing_defer w rect styleMask backing False
  Win.center window
  Win.setTitle window ("Counter" :: Id NSString)

  cv <- Win.contentView window

  -- Counter label (large, centered)
  label <- TF.labelWithString ("0" :: Id NSString) :: IO (Id NSTextField)
  View.setFrame label (NSRect (NSPoint 60 90) (NSSize 200 60))
  font <- Font.systemFontOfSize 48
  Ctrl.setFont label font
  Ctrl.setAlignment label Ctrl.NSTextAlignmentCenter
  View.addSubview cv (toNSView label)

  -- Create the target with per-instance state
  ref <- newIORef (0 :: Int)
  target <- newActionTarget
    [ ("increment:", \_ -> do
          modifyIORef' ref (+ 1)
          readIORef ref >>= updateLabel label)
    , ("decrement:", \_ -> do
          modifyIORef' ref (subtract 1)
          readIORef ref >>= updateLabel label)
    ]

  -- Buttons
  plusBtn <- Btn.buttonWithTitle_target_action
    ("+" :: Id NSString) target (mkSelector "increment:")
  View.setFrame plusBtn (NSRect (NSPoint 190 20) (NSSize 60 40))
  View.addSubview cv (toNSView plusBtn)

  minusBtn <- Btn.buttonWithTitle_target_action
    ("-" :: Id NSString) target (mkSelector "decrement:")
  View.setFrame minusBtn (NSRect (NSPoint 70 20) (NSSize 60 40))
  View.addSubview cv (toNSView minusBtn)

  Win.makeKeyAndOrderFront window (RawId nullPtr)
  App.activateIgnoringOtherApps app True

  App.run app


-- | Set up a minimal menu bar with Quit (Cmd+Q).
setupMenuBar :: Id NSApplication -> IO ()
setupMenuBar app = do
  menuBar <- alloc @NSMenu >>= \m -> Menu.initWithTitle m ("" :: Id NSString)
  appMenuItem <- new @NSMenuItem
  Menu.addItem menuBar appMenuItem
  appMenu <- alloc @NSMenu >>= \m -> Menu.initWithTitle m ("App" :: Id NSString)
  quitItem <- alloc @NSMenuItem >>=
    \mi -> MI.initWithTitle_action_keyEquivalent mi ("Quit" :: Id NSString) App.terminateSelector ("q" :: Id NSString)
  Menu.addItem appMenu quitItem
  Menu.setSubmenu_forItem menuBar appMenu appMenuItem
  App.setMainMenu app menuBar

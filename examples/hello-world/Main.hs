{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Foreign.Ptr (nullPtr)

import ObjC.Runtime

-- Generated bindings — types and enums from per-class modules
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
import ObjC.AppKit.NSMenu (NSMenu)
import ObjC.AppKit.NSMenuItem (NSMenuItem)
import ObjC.AppKit.NSView (IsNSView(toNSView))
import ObjC.Foundation.NSString (NSString)  -- IsString (Id NSString) instance
import ObjC.Foundation.Structs (NSRect(..), NSPoint(..), NSSize(..))

-- Generated bindings — methods (qualified)
import qualified ObjC.AppKit.NSApplication as App
import qualified ObjC.AppKit.NSWindow as Win
import qualified ObjC.AppKit.NSTextField as TF
import qualified ObjC.AppKit.NSView as View
import qualified ObjC.AppKit.NSMenu as Menu
import qualified ObjC.AppKit.NSMenuItem as MI
import qualified ObjC.AppKit.NSControl as Ctrl
import qualified ObjC.AppKit.NSFont as Font
import ObjC.AppKit.Delegate.NSApplicationDelegate
  ( NSApplicationDelegateOverrides(..)
  , defaultNSApplicationDelegateOverrides
  , newNSApplicationDelegate
  )


main :: IO ()
main = withAutoreleasePool $ do
  loadFramework "AppKit"

  -- [NSApplication sharedApplication]
  app <- App.sharedApplication

  _ <- App.setActivationPolicy app NSApplicationActivationPolicyRegular

  setupMenuBar app

  -- Create the window
  let styleMask = NSWindowStyleMaskTitled <> NSWindowStyleMaskClosable
               <> NSWindowStyleMaskMiniaturizable <> NSWindowStyleMaskResizable
      backing   = NSBackingStoreBuffered
      rect      = NSRect (NSPoint 200 200) (NSSize 480 260)

  window <- (alloc @NSWindow >>=
    \w -> Win.initWithContentRect_styleMask_backing_defer w rect styleMask backing False)
  Win.center window
  Win.setReleasedWhenClosed window False

  Win.setTitle window ("Hello Haskell" :: Id NSString)

  -- Create a label
  label <- TF.labelWithString ("Hello, World from Haskell!" :: Id NSString) :: IO (Id NSTextField)

  -- Position the label within the window
  let labelRect = NSRect (NSPoint 40 80) (NSSize 400 100)
  View.setFrame label labelRect

  -- Set the font to system 32pt
  font <- Font.systemFontOfSize 32
  Ctrl.setFont label font

  Ctrl.setAlignment label Ctrl.NSTextAlignmentCenter

  -- Add the label to the window's content view
  cv <- Win.contentView window
  View.addSubview cv (toNSView label)

  appDelegate <- newNSApplicationDelegate defaultNSApplicationDelegateOverrides
    { _applicationShouldTerminateAfterLastWindowClosed = Just (const (pure False))
    , _applicationShouldHandleReopen_hasVisibleWindows = Just $ \_ hasVisible ->
        if hasVisible then pure True
        else Win.makeKeyAndOrderFront window (RawId nullPtr) >> pure True
    }
  App.setDelegate app appDelegate

  Win.makeKeyAndOrderFront window (RawId nullPtr)
  App.activateIgnoringOtherApps app True

  -- Enter the AppKit run loop (blocks until app quits)
  App.run app


-- | Set up a minimal menu bar with an application menu containing Quit (Cmd+Q).
setupMenuBar :: Id NSApplication -> IO ()
setupMenuBar app = do
  -- Menu bar
  menuBar <- alloc @NSMenu >>= \m -> Menu.initWithTitle m ("" :: Id NSString)

  -- App menu item (container in the menu bar) — plain -init.
  appMenuItem <- new @NSMenuItem

  Menu.addItem menuBar appMenuItem

  -- App submenu
  appMenu <- alloc @NSMenu >>= \m -> Menu.initWithTitle m ("App" :: Id NSString)

  -- "Quit" item with Cmd+Q
  quitItem <- alloc @NSMenuItem >>=
    \mi -> MI.initWithTitle_action_keyEquivalent mi ("Quit" :: Id NSString) (asSel App.terminateSelector) ("q" :: Id NSString)

  Menu.addItem appMenu quitItem

  -- Wire submenu and set the menu bar
  Menu.setSubmenu_forItem menuBar appMenu appMenuItem
  App.setMainMenu app menuBar

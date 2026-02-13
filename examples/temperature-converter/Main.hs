{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Temperature converter: classic Cocoa tutorial.
-- Two text fields (Fahrenheit / Celsius) and Convert buttons.
-- Uses 'Ctrl.doubleValue' / 'Ctrl.setDoubleValue' for numeric I/O.
module Main (main) where

import Foreign.Ptr (nullPtr)
import Foreign.C.Types (CDouble)

import ObjC.Runtime
import ObjC.Runtime.TH

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
import ObjC.AppKit.NSButton (NSButton)
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
-- Converter target class
-- ---------------------------------------------------------------------------

$(defineClass "ConverterTarget" "NSObject" $ do
  instanceMethod "convertToC:" [t| RawId -> IO () |]
  instanceMethod "convertToF:" [t| RawId -> IO () |]
 )

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
      rect      = NSRect (NSPoint 200 200) (NSSize 400 200)

  window <- alloc @NSWindow >>=
    \w -> Win.initWithContentRect_styleMask_backing_defer w rect styleMask backing False
  Win.center window
  Win.setTitle window ("Temperature Converter" :: Id NSString)

  cv <- Win.contentView window

  -- Fahrenheit label + field
  fLabel <- TF.labelWithString ("Fahrenheit:" :: Id NSString) :: IO (Id NSTextField)
  View.setFrame fLabel (NSRect (NSPoint 20 130) (NSSize 100 24))
  View.addSubview cv (toNSView fLabel)

  fField <- unsafeCastId <$> (alloc @NSTextField >>= \tf -> View.initWithFrame tf (NSRect (NSPoint 130 128) (NSSize 100 28))) :: IO (Id NSTextField)
  Ctrl.setDoubleValue fField 212
  View.addSubview cv (toNSView fField)

  -- Celsius label + field
  cLabel <- TF.labelWithString ("Celsius:" :: Id NSString) :: IO (Id NSTextField)
  View.setFrame cLabel (NSRect (NSPoint 20 90) (NSSize 100 24))
  View.addSubview cv (toNSView cLabel)

  cField <- unsafeCastId <$> (alloc @NSTextField >>= \tf -> View.initWithFrame tf (NSRect (NSPoint 130 88) (NSSize 100 28))) :: IO (Id NSTextField)
  Ctrl.setDoubleValue cField 100
  View.addSubview cv (toNSView cField)

  -- Create the converter target
  target <- newConverterTarget $ do
    pure ConverterTargetImpl
      { _convertToC = \_sender -> do
          f <- Ctrl.doubleValue fField
          let c = (f - 32) * 5 / 9 :: CDouble
          Ctrl.setDoubleValue cField c
      , _convertToF = \_sender -> do
          c <- Ctrl.doubleValue cField
          let f = c * 9 / 5 + 32 :: CDouble
          Ctrl.setDoubleValue fField f
      }

  -- Buttons
  toCBtn <- Btn.buttonWithTitle_target_action
    ("→ °C" :: Id NSString) target (mkSelector "convertToC:")
  View.setFrame toCBtn (NSRect (NSPoint 250 128) (NSSize 80 28))
  View.addSubview cv (toNSView toCBtn)

  toFBtn <- Btn.buttonWithTitle_target_action
    ("→ °F" :: Id NSString) target (mkSelector "convertToF:")
  View.setFrame toFBtn (NSRect (NSPoint 250 88) (NSSize 80 28))
  View.addSubview cv (toNSView toFBtn)

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
    \mi -> MI.initWithTitle_action_keyEquivalent mi ("Quit" :: Id NSString) (asSel App.terminateSelector) ("q" :: Id NSString)
  Menu.addItem appMenu quitItem
  Menu.setSubmenu_forItem menuBar appMenu appMenuItem
  App.setMainMenu app menuBar

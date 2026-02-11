{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Color mixer: three sliders (R/G/B, range 0–255), a colored
-- preview box, and a hex label.  Demonstrates 'defineClass' with
-- a @\"sliderChanged:\"@ action that reads all three slider values
-- and updates the preview.
module Main (main) where

import Data.String (fromString)
import Foreign.Ptr (nullPtr)
import Foreign.C.Types (CDouble)
import Numeric (showHex)

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
import ObjC.AppKit.NSSlider (NSSlider)
import ObjC.AppKit.NSColor (NSColor)
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
import qualified ObjC.AppKit.NSSlider as Slider
import qualified ObjC.AppKit.NSColor as Color

-- ---------------------------------------------------------------------------
-- Slider target class
-- ---------------------------------------------------------------------------

$(defineClass "ColorMixerTarget" "NSObject" $ do
  instanceMethod "sliderChanged:" [t| RawId -> IO () |]
 )

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Format an Int as a 2-digit hex string.
hex2 :: Int -> String
hex2 n
  | n < 16    = '0' : showHex n ""
  | otherwise = showHex n ""

-- | Build a hex color string like \"#FF8800\".
hexColor :: Int -> Int -> Int -> String
hexColor r g b = "#" ++ hex2 r ++ hex2 g ++ hex2 b

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
      rect      = NSRect (NSPoint 200 200) (NSSize 400 320)

  window <- alloc @NSWindow >>=
    \w -> Win.initWithContentRect_styleMask_backing_defer w rect styleMask backing False
  Win.center window
  Win.setTitle window ("Color Mixer" :: Id NSString)

  cv <- Win.contentView window

  -- Color preview (NSTextField with colored background)
  preview <- TF.labelWithString ("" :: Id NSString) :: IO (Id NSTextField)
  View.setFrame preview (NSRect (NSPoint 20 220) (NSSize 360 60))
  TF.setDrawsBackground preview True
  initialColor <- Color.colorWithSRGBRed_green_blue_alpha 0.5 0.5 0.5 1.0
  TF.setBackgroundColor preview initialColor
  TF.setEditable preview False
  View.addSubview cv (toNSView preview)

  -- Hex label
  hexLabel <- TF.labelWithString ("#808080" :: Id NSString) :: IO (Id NSTextField)
  View.setFrame hexLabel (NSRect (NSPoint 20 185) (NSSize 360 24))
  hexFont <- Font.monospacedSystemFontOfSize_weight 18 0
  Ctrl.setFont hexLabel hexFont
  Ctrl.setAlignment hexLabel Ctrl.NSTextAlignmentCenter
  View.addSubview cv (toNSView hexLabel)

  -- RGB labels
  rLabel <- TF.labelWithString ("R:" :: Id NSString) :: IO (Id NSTextField)
  View.setFrame rLabel (NSRect (NSPoint 20 140) (NSSize 30 20))
  View.addSubview cv (toNSView rLabel)

  gLabel <- TF.labelWithString ("G:" :: Id NSString) :: IO (Id NSTextField)
  View.setFrame gLabel (NSRect (NSPoint 20 100) (NSSize 30 20))
  View.addSubview cv (toNSView gLabel)

  bLabel <- TF.labelWithString ("B:" :: Id NSString) :: IO (Id NSTextField)
  View.setFrame bLabel (NSRect (NSPoint 20 60) (NSSize 30 20))
  View.addSubview cv (toNSView bLabel)

  -- Sliders (value 128, range 0-255)
  rSlider <- Slider.sliderWithValue_minValue_maxValue_target_action 128 0 255 (RawId nullPtr) (mkSelector "")
  View.setFrame rSlider (NSRect (NSPoint 50 138) (NSSize 300 24))
  View.addSubview cv (toNSView rSlider)

  gSlider <- Slider.sliderWithValue_minValue_maxValue_target_action 128 0 255 (RawId nullPtr) (mkSelector "")
  View.setFrame gSlider (NSRect (NSPoint 50 98) (NSSize 300 24))
  View.addSubview cv (toNSView gSlider)

  bSlider <- Slider.sliderWithValue_minValue_maxValue_target_action 128 0 255 (RawId nullPtr) (mkSelector "")
  View.setFrame bSlider (NSRect (NSPoint 50 58) (NSSize 300 24))
  View.addSubview cv (toNSView bSlider)

  -- Create target with the update callback
  target <- newColorMixerTarget $ do
    pure ColorMixerTargetImpl
      { _sliderChanged = \_sender -> do
          r <- round <$> Ctrl.doubleValue rSlider :: IO Int
          g <- round <$> Ctrl.doubleValue gSlider :: IO Int
          b <- round <$> Ctrl.doubleValue bSlider :: IO Int
          let rf = fromIntegral r / 255.0 :: CDouble
              gf = fromIntegral g / 255.0 :: CDouble
              bf = fromIntegral b / 255.0 :: CDouble
          color <- Color.colorWithSRGBRed_green_blue_alpha rf gf bf 1.0
          TF.setBackgroundColor preview color
          Ctrl.setStringValue hexLabel (fromString (hexColor r g b) :: Id NSString)
      }

  -- Wire sliders to target
  Ctrl.setTarget rSlider target
  Ctrl.setAction rSlider (mkSelector "sliderChanged:")
  Ctrl.setTarget gSlider target
  Ctrl.setAction gSlider (mkSelector "sliderChanged:")
  Ctrl.setTarget bSlider target
  Ctrl.setAction bSlider (mkSelector "sliderChanged:")

  -- Value labels (showing 0-255 next to sliders)
  rVal <- TF.labelWithString ("128" :: Id NSString) :: IO (Id NSTextField)
  View.setFrame rVal (NSRect (NSPoint 355 140) (NSSize 35 20))
  View.addSubview cv (toNSView rVal)

  gVal <- TF.labelWithString ("128" :: Id NSString) :: IO (Id NSTextField)
  View.setFrame gVal (NSRect (NSPoint 355 100) (NSSize 35 20))
  View.addSubview cv (toNSView gVal)

  bVal <- TF.labelWithString ("128" :: Id NSString) :: IO (Id NSTextField)
  View.setFrame bVal (NSRect (NSPoint 355 60) (NSSize 35 20))
  View.addSubview cv (toNSView bVal)

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

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Controls gallery: a visual showcase of common AppKit controls.
-- No callbacks — purely a display of control types and their default
-- appearances.  Demonstrates how to create and configure various
-- NSControl subclasses from Haskell.
module Main (main) where

import Data.String (fromString)
import Foreign.Ptr (nullPtr)
import Foreign.C.Types (CDouble)

import ObjC.Runtime

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
import ObjC.AppKit.NSSlider (NSSlider)
import ObjC.AppKit.NSSwitch (NSSwitch)
import ObjC.AppKit.NSSegmentedControl (NSSegmentedControl)
import ObjC.AppKit.NSProgressIndicator
  ( NSProgressIndicator
  , pattern NSProgressIndicatorStyleBar
  , pattern NSProgressIndicatorStyleSpinning
  )
import ObjC.AppKit.NSDatePicker (NSDatePicker)
import ObjC.AppKit.NSColorWell (NSColorWell)
import ObjC.AppKit.NSMenu (NSMenu)
import ObjC.AppKit.NSMenuItem (NSMenuItem)
import ObjC.AppKit.NSView (NSView, IsNSView(toNSView))
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
import qualified ObjC.AppKit.NSSwitch as Switch
import qualified ObjC.AppKit.NSSegmentedControl as Seg
import qualified ObjC.AppKit.NSProgressIndicator as PI
import qualified ObjC.AppKit.NSDatePicker as DP
import qualified ObjC.AppKit.NSColorWell as CW
import qualified ObjC.AppKit.NSButton as Btn

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Convenience to add a generic control to the content view.
addControl :: IsNSView v => Id NSView -> v -> NSRect -> IO ()
addControl cv ctrl rect = do
  View.setFrame ctrl rect
  View.addSubview cv (toNSView ctrl)

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
      rect      = NSRect (NSPoint 100 100) (NSSize 500 520)

  window <- alloc @NSWindow >>=
    \w -> Win.initWithContentRect_styleMask_backing_defer w rect styleMask backing False
  Win.center window
  Win.setTitle window ("Controls Gallery" :: Id NSString)

  cv <- Win.contentView window

  -- Title
  title <- TF.labelWithString ("AppKit Controls Gallery" :: Id NSString) :: IO (Id NSTextField)
  View.setFrame title (NSRect (NSPoint 20 470) (NSSize 460 30))
  titleFont <- Font.boldSystemFontOfSize 20
  Ctrl.setFont title titleFont
  View.addSubview cv (toNSView title)

  -- 1. Push button
  let y1 = 430 :: CDouble
  lbl1 <- TF.labelWithString ("Push Button:" :: Id NSString) :: IO (Id NSTextField)
  addControl cv lbl1 (NSRect (NSPoint 20 y1) (NSSize 150 20))
  btn <- Btn.buttonWithTitle_target_action ("Click Me" :: Id NSString) (RawId nullPtr) (mkSelector "")
  addControl cv btn (NSRect (NSPoint 180 (y1 - 4)) (NSSize 120 28))

  -- 2. Checkbox
  let y2 = 395 :: CDouble
  lbl2 <- TF.labelWithString ("Checkbox:" :: Id NSString) :: IO (Id NSTextField)
  addControl cv lbl2 (NSRect (NSPoint 20 y2) (NSSize 150 20))
  cb <- Btn.checkboxWithTitle_target_action ("Enable feature" :: Id NSString) (RawId nullPtr) (mkSelector "")
  addControl cv cb (NSRect (NSPoint 180 (y2 - 4)) (NSSize 200 24))

  -- 3. Radio button
  let y3 = 360 :: CDouble
  lbl3 <- TF.labelWithString ("Radio Button:" :: Id NSString) :: IO (Id NSTextField)
  addControl cv lbl3 (NSRect (NSPoint 20 y3) (NSSize 150 20))
  rb <- Btn.radioButtonWithTitle_target_action ("Option A" :: Id NSString) (RawId nullPtr) (mkSelector "")
  addControl cv rb (NSRect (NSPoint 180 (y3 - 4)) (NSSize 200 24))

  -- 4. Horizontal slider
  let y4 = 325 :: CDouble
  lbl4 <- TF.labelWithString ("Slider:" :: Id NSString) :: IO (Id NSTextField)
  addControl cv lbl4 (NSRect (NSPoint 20 y4) (NSSize 150 20))
  slider <- Slider.sliderWithValue_minValue_maxValue_target_action 50 0 100 (RawId nullPtr) (mkSelector "")
  addControl cv slider (NSRect (NSPoint 180 (y4 - 2)) (NSSize 250 24))

  -- 5. NSSwitch
  let y5 = 290 :: CDouble
  lbl5 <- TF.labelWithString ("Switch:" :: Id NSString) :: IO (Id NSTextField)
  addControl cv lbl5 (NSRect (NSPoint 20 y5) (NSSize 150 20))
  sw <- unsafeCastId <$> (alloc @NSSwitch >>= \s -> View.initWithFrame s (NSRect (NSPoint 0 0) (NSSize 40 24))) :: IO (Id NSSwitch)
  Switch.setState sw 1
  addControl cv sw (NSRect (NSPoint 180 (y5 - 2)) (NSSize 60 24))

  -- 6. Segmented control (manually configured)
  let y6 = 255 :: CDouble
  lbl6 <- TF.labelWithString ("Segmented:" :: Id NSString) :: IO (Id NSTextField)
  addControl cv lbl6 (NSRect (NSPoint 20 y6) (NSSize 150 20))
  seg <- unsafeCastId <$> (alloc @NSSegmentedControl >>= \s -> View.initWithFrame s (NSRect (NSPoint 0 0) (NSSize 250 24))) :: IO (Id NSSegmentedControl)
  Seg.setSegmentCount seg 3
  Seg.setLabel_forSegment seg ("One" :: Id NSString) 0
  Seg.setLabel_forSegment seg ("Two" :: Id NSString) 1
  Seg.setLabel_forSegment seg ("Three" :: Id NSString) 2
  Seg.setSelectedSegment seg 0
  addControl cv seg (NSRect (NSPoint 180 (y6 - 2)) (NSSize 250 24))

  -- 7. Determinate progress indicator
  let y7 = 220 :: CDouble
  lbl7 <- TF.labelWithString ("Progress (det.):" :: Id NSString) :: IO (Id NSTextField)
  addControl cv lbl7 (NSRect (NSPoint 20 y7) (NSSize 150 20))
  progDet <- unsafeCastId <$> (alloc @NSProgressIndicator >>= \p -> View.initWithFrame p (NSRect (NSPoint 0 0) (NSSize 250 20))) :: IO (Id NSProgressIndicator)
  PI.setStyle progDet NSProgressIndicatorStyleBar
  PI.setIndeterminate progDet False
  PI.setMinValue progDet 0
  PI.setMaxValue progDet 100
  PI.setDoubleValue progDet 65
  addControl cv progDet (NSRect (NSPoint 180 y7) (NSSize 250 20))

  -- 8. Indeterminate progress indicator (spinning)
  let y8 = 185 :: CDouble
  lbl8 <- TF.labelWithString ("Progress (indet.):" :: Id NSString) :: IO (Id NSTextField)
  addControl cv lbl8 (NSRect (NSPoint 20 y8) (NSSize 150 20))
  progIndet <- unsafeCastId <$> (alloc @NSProgressIndicator >>= \p -> View.initWithFrame p (NSRect (NSPoint 0 0) (NSSize 32 32))) :: IO (Id NSProgressIndicator)
  PI.setStyle progIndet NSProgressIndicatorStyleSpinning
  PI.setIndeterminate progIndet True
  PI.startAnimation progIndet (RawId nullPtr)
  addControl cv progIndet (NSRect (NSPoint 180 (y8 - 6)) (NSSize 32 32))

  -- 9. Date picker
  let y9 = 145 :: CDouble
  lbl9 <- TF.labelWithString ("Date Picker:" :: Id NSString) :: IO (Id NSTextField)
  addControl cv lbl9 (NSRect (NSPoint 20 y9) (NSSize 150 20))
  dp <- unsafeCastId <$> (alloc @NSDatePicker >>= \d -> View.initWithFrame d (NSRect (NSPoint 0 0) (NSSize 250 24))) :: IO (Id NSDatePicker)
  addControl cv dp (NSRect (NSPoint 180 (y9 - 2)) (NSSize 250 24))

  -- 10. Color well
  let y10 = 105 :: CDouble
  lbl10 <- TF.labelWithString ("Color Well:" :: Id NSString) :: IO (Id NSTextField)
  addControl cv lbl10 (NSRect (NSPoint 20 y10) (NSSize 150 20))
  cw <- unsafeCastId <$> (alloc @NSColorWell >>= \c -> View.initWithFrame c (NSRect (NSPoint 0 0) (NSSize 44 24))) :: IO (Id NSColorWell)
  addControl cv cw (NSRect (NSPoint 180 (y10 - 2)) (NSSize 44 24))

  -- 11. Editable text field
  let y11 = 65 :: CDouble
  lbl11 <- TF.labelWithString ("Text Field:" :: Id NSString) :: IO (Id NSTextField)
  addControl cv lbl11 (NSRect (NSPoint 20 y11) (NSSize 150 20))
  tf <- unsafeCastId <$> (alloc @NSTextField >>= \t -> View.initWithFrame t (NSRect (NSPoint 0 0) (NSSize 250 24))) :: IO (Id NSTextField)
  Ctrl.setStringValue tf ("Editable text" :: Id NSString)
  addControl cv tf (NSRect (NSPoint 180 (y11 - 2)) (NSSize 250 24))

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

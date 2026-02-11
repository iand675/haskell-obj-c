{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An abstract class that defines the interface for subclassers to interact with the screen saver infrastructure.
--
-- ``ScreenSaverView`` provides the interface for your screen saver, including the content you animate onscreen and an optional configuration sheet. Create your own custom subclass and add it to your screen saver bundle. Use your subclass to create the animations that you want to appear onscreen, and to specify additional animation details.
--
-- - Note: When someone previews your screen saver in System Preferences, the system instantiates your ``ScreenSaverView`` subclass.
--
-- You can draw from your view’s ``ScreenSaverView/drawRect:`` method, or you can draw directly from the ``ScreenSaverView/animateOneFrame`` method. If you prefer to use the ``ScreenSaverView/drawRect:`` method, use the ``ScreenSaverView/animateOneFrame`` method to call the <doc://com.apple.documentation/documentation/appkit/nsview/1483475-setneedsdisplayinrect> method and specify the portions of your view that require updates.
--
-- Generated bindings for @ScreenSaverView@.
module ObjC.ScreenSaver.ScreenSaverView
  ( ScreenSaverView
  , IsScreenSaverView(..)
  , backingStoreType
  , performGammaFade
  , initWithFrame_isPreview
  , startAnimation
  , stopAnimation
  , drawRect
  , animateOneFrame
  , animationTimeInterval
  , setAnimationTimeInterval
  , animating
  , hasConfigureSheet
  , configureSheet
  , preview
  , backingStoreTypeSelector
  , performGammaFadeSelector
  , initWithFrame_isPreviewSelector
  , startAnimationSelector
  , stopAnimationSelector
  , drawRectSelector
  , animateOneFrameSelector
  , animationTimeIntervalSelector
  , setAnimationTimeIntervalSelector
  , animatingSelector
  , hasConfigureSheetSelector
  , configureSheetSelector
  , previewSelector

  -- * Enum types
  , NSBackingStoreType(NSBackingStoreType)
  , pattern NSBackingStoreRetained
  , pattern NSBackingStoreNonretained
  , pattern NSBackingStoreBuffered

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ScreenSaver.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns the type of backing store you want for your screen saver’s window.
--
-- ## Overview
--
-- This method returns <doc://com.apple.documentation/documentation/appkit/nsbackingstoretype/nsbackingstorebuffered> by default. If you want to change the backing store type, override this method and return a new value. If you override the method, you don’t need to call the inherited version.
--
-- ObjC selector: @+ backingStoreType@
backingStoreType :: IO NSBackingStoreType
backingStoreType  =
  do
    cls' <- getRequiredClass "ScreenSaverView"
    fmap (coerce :: CULong -> NSBackingStoreType) $ sendClassMsg cls' (mkSelector "backingStoreType") retCULong []

-- | Indicates whether to perform a gradual screen fade when the system starts and stops your screen saver’s animation.
--
-- ## Overview
--
-- This class method allows the screen saver view to select how the desktop visibly transitions to the screen saver view. When this method returns <doc://com.apple.documentation/documentation/objectivec/yes>, the screen gradually darkens before the animation begins. When it returns <doc://com.apple.documentation/documentation/objectivec/no>, the screen transitions immediately to the screen saver. The latter behavior is more appropriate if the screen saver animates a screenshot of the desktop, as is the case for optical lens effects. The default is <doc://com.apple.documentation/documentation/objectivec/yes>.
--
-- ObjC selector: @+ performGammaFade@
performGammaFade :: IO Bool
performGammaFade  =
  do
    cls' <- getRequiredClass "ScreenSaverView"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "performGammaFade") retCULong []

-- | Creates a newly allocated screen saver view with the specified frame rectangle and preview information.
--
-- ## Overview
--
-- The screen saver application installs the new view object into the view hierarchy of an <doc://com.apple.documentation/documentation/appkit/nswindow> before the animation begins. This method is the designated initializer for the ``ScreenSaver/ScreenSaverView`` class. Returns @self@.
--
-- - Parameters:   - frame: The frame rectangle for the view.
--
-- - isPreview: <doc://com.apple.documentation/documentation/objectivec/yes> if this view provides a preview for system settings, or <doc://com.apple.documentation/documentation/objectivec/no> if the system fills the screen with your view’s contents.
--
-- ObjC selector: @- initWithFrame:isPreview:@
initWithFrame_isPreview :: IsScreenSaverView screenSaverView => screenSaverView -> NSRect -> Bool -> IO (Id ScreenSaverView)
initWithFrame_isPreview screenSaverView  frame isPreview =
  sendMsg screenSaverView (mkSelector "initWithFrame:isPreview:") (retPtr retVoid) [argNSRect frame, argCULong (if isPreview then 1 else 0)] >>= ownedObject . castPtr

-- | Activates the periodic timer that animates the screen saver.
--
-- ## Overview
--
-- The system calls this method when it’s time for you to start animating your screen saver’s content. The system calls this method only once at the start of animations. Use this method to set up any initial state information you require or to allocate expensive resources. If you override this method, you must call the inherited implementation at some point.
--
-- ## See also
--
-- - ``ScreenSaver/ScreenSaverView/stopAnimation``
--
-- ObjC selector: @- startAnimation@
startAnimation :: IsScreenSaverView screenSaverView => screenSaverView -> IO ()
startAnimation screenSaverView  =
  sendMsg screenSaverView (mkSelector "startAnimation") retVoid []

-- | Deactivates the timer that advances the animation.
--
-- ## Overview
--
-- The system calls this method when it’s time for you to stop animating your screen saver’s content. The system calls this method only once at the end of animations. Use this method to unload expensive resources or to reset your screen saver to a known state. If you override this method, you must call the inherited implementation at some point.
--
-- ## See also
--
-- - ``ScreenSaver/ScreenSaverView/startAnimation``
--
-- ObjC selector: @- stopAnimation@
stopAnimation :: IsScreenSaverView screenSaverView => screenSaverView -> IO ()
stopAnimation screenSaverView  =
  sendMsg screenSaverView (mkSelector "stopAnimation") retVoid []

-- | Draws the screen saver view.
--
-- ## Overview
--
-- ``ScreenSaver/ScreenSaverView`` implements ``ScreenSaver/ScreenSaverView/drawRect:`` to draw a black background. Subclasses can do their drawing here or in ``ScreenSaver/ScreenSaverView/animateOneFrame``.
--
-- ## See also
--
-- - ``ScreenSaver/ScreenSaverView/stopAnimation`` - ``ScreenSaver/ScreenSaverView/animateOneFrame`` - ``ScreenSaver/ScreenSaverView/startAnimation``
--
-- ObjC selector: @- drawRect:@
drawRect :: IsScreenSaverView screenSaverView => screenSaverView -> NSRect -> IO ()
drawRect screenSaverView  rect =
  sendMsg screenSaverView (mkSelector "drawRect:") retVoid [argNSRect rect]

-- | Advances the screen saver’s animation by a single frame.
--
-- ## Overview
--
-- The system calls this method each time the timer animating the screen saver fires. The time between calls to this method is always at least ``ScreenSaver/ScreenSaverView/animationTimeInterval``. The system locks focus on your view before it calls this method, so you can use this method to draw content. You can also let ``ScreenSaver/ScreenSaverView/drawRect:`` perform the drawing, in which case you use this method to call <doc://com.apple.documentation/documentation/appkit/nsview/1483475-setneedsdisplayinrect> to mark your view as dirty. The default implementation of this method does nothing.
--
-- ## See also
--
-- - ``ScreenSaver/ScreenSaverView/drawRect:``
--
-- ObjC selector: @- animateOneFrame@
animateOneFrame :: IsScreenSaverView screenSaverView => screenSaverView -> IO ()
animateOneFrame screenSaverView  =
  sendMsg screenSaverView (mkSelector "animateOneFrame") retVoid []

-- | The time interval between animation frames.
--
-- If your screen saver has particular requirements for time between animation frames, call this method to set the animation rate to a reasonable value.
--
-- ObjC selector: @- animationTimeInterval@
animationTimeInterval :: IsScreenSaverView screenSaverView => screenSaverView -> IO CDouble
animationTimeInterval screenSaverView  =
  sendMsg screenSaverView (mkSelector "animationTimeInterval") retCDouble []

-- | The time interval between animation frames.
--
-- If your screen saver has particular requirements for time between animation frames, call this method to set the animation rate to a reasonable value.
--
-- ObjC selector: @- setAnimationTimeInterval:@
setAnimationTimeInterval :: IsScreenSaverView screenSaverView => screenSaverView -> CDouble -> IO ()
setAnimationTimeInterval screenSaverView  value =
  sendMsg screenSaverView (mkSelector "setAnimationTimeInterval:") retVoid [argCDouble (fromIntegral value)]

-- | A Boolean value that indicates whether the screen saver is animating.
--
-- ## Overview
--
-- The value of this property is <doc://com.apple.documentation/documentation/objectivec/yes> when the screen saver is animating, and <doc://com.apple.documentation/documentation/objectivec/no> when it isn’t.
--
-- ## See also
--
-- - ``ScreenSaver/ScreenSaverView/stopAnimation`` - ``ScreenSaver/ScreenSaverView/startAnimation``
--
-- ObjC selector: @- animating@
animating :: IsScreenSaverView screenSaverView => screenSaverView -> IO Bool
animating screenSaverView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg screenSaverView (mkSelector "animating") retCULong []

-- | A Boolean value that indicates whether the screen saver has an associated configuration sheet.
--
-- If you provide a configuration sheet in your bundle, override this method and return <doc://com.apple.documentation/documentation/objectivec/yes>.
--
-- ## See also
--
-- - ``ScreenSaver/ScreenSaverView/configureSheet``
--
-- ObjC selector: @- hasConfigureSheet@
hasConfigureSheet :: IsScreenSaverView screenSaverView => screenSaverView -> IO Bool
hasConfigureSheet screenSaverView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg screenSaverView (mkSelector "hasConfigureSheet") retCULong []

-- | The window that contains the controls to configure the screen saver.
--
-- The system runs this window as a sheet, so include buttons that allow the user to end the modal session in which the sheet runs. When the user dismisses the sheet, the controller in charge of the sheet must end the document modal session by calling the <doc://com.apple.documentation/documentation/appkit/nsapplication> method <doc://com.apple.documentation/documentation/appkit/nsapplication/1428503-endsheet> with the sheet’s window as the argument.
--
-- ## See also
--
-- - ``ScreenSaver/ScreenSaverView/hasConfigureSheet``
--
-- ObjC selector: @- configureSheet@
configureSheet :: IsScreenSaverView screenSaverView => screenSaverView -> IO (Id NSWindow)
configureSheet screenSaverView  =
  sendMsg screenSaverView (mkSelector "configureSheet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A Boolean value that indicates whether the screen saver view is set to a size suitable for previewing its content.
--
-- ## Overview
--
-- The system sets the value of this property to <doc://com.apple.documentation/documentation/objectivec/yes> when it creates a smaller preview of your screen saver. When the value is <doc://com.apple.documentation/documentation/objectivec/no>, your view matches the size of the screen. Use this property to adjust the content you present. For example, you might change the drawing parameters or data you display in your view.
--
-- ObjC selector: @- preview@
preview :: IsScreenSaverView screenSaverView => screenSaverView -> IO Bool
preview screenSaverView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg screenSaverView (mkSelector "preview") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @backingStoreType@
backingStoreTypeSelector :: Selector
backingStoreTypeSelector = mkSelector "backingStoreType"

-- | @Selector@ for @performGammaFade@
performGammaFadeSelector :: Selector
performGammaFadeSelector = mkSelector "performGammaFade"

-- | @Selector@ for @initWithFrame:isPreview:@
initWithFrame_isPreviewSelector :: Selector
initWithFrame_isPreviewSelector = mkSelector "initWithFrame:isPreview:"

-- | @Selector@ for @startAnimation@
startAnimationSelector :: Selector
startAnimationSelector = mkSelector "startAnimation"

-- | @Selector@ for @stopAnimation@
stopAnimationSelector :: Selector
stopAnimationSelector = mkSelector "stopAnimation"

-- | @Selector@ for @drawRect:@
drawRectSelector :: Selector
drawRectSelector = mkSelector "drawRect:"

-- | @Selector@ for @animateOneFrame@
animateOneFrameSelector :: Selector
animateOneFrameSelector = mkSelector "animateOneFrame"

-- | @Selector@ for @animationTimeInterval@
animationTimeIntervalSelector :: Selector
animationTimeIntervalSelector = mkSelector "animationTimeInterval"

-- | @Selector@ for @setAnimationTimeInterval:@
setAnimationTimeIntervalSelector :: Selector
setAnimationTimeIntervalSelector = mkSelector "setAnimationTimeInterval:"

-- | @Selector@ for @animating@
animatingSelector :: Selector
animatingSelector = mkSelector "animating"

-- | @Selector@ for @hasConfigureSheet@
hasConfigureSheetSelector :: Selector
hasConfigureSheetSelector = mkSelector "hasConfigureSheet"

-- | @Selector@ for @configureSheet@
configureSheetSelector :: Selector
configureSheetSelector = mkSelector "configureSheet"

-- | @Selector@ for @preview@
previewSelector :: Selector
previewSelector = mkSelector "preview"


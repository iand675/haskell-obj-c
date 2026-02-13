{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IKPictureTaker
--
-- An IKPictureTaker object is a panel that allows users to choose and crop an image. It supports browsing of the file system and includes a recents popup-menu. The IKPictureTaker lets the user to crop a choosen image or to take snapshot from a camera like the built-in iSight.
--
-- Generated bindings for @IKPictureTaker@.
module ObjC.Quartz.IKPictureTaker
  ( IKPictureTaker
  , IsIKPictureTaker(..)
  , pictureTaker
  , runModal
  , beginPictureTakerWithDelegate_didEndSelector_contextInfo
  , beginPictureTakerSheetForWindow_withDelegate_didEndSelector_contextInfo
  , popUpRecentsMenuForView_withDelegate_didEndSelector_contextInfo
  , setInputImage
  , inputImage
  , outputImage
  , setMirroring
  , mirroring
  , beginPictureTakerSheetForWindow_withDelegate_didEndSelector_contextInfoSelector
  , beginPictureTakerWithDelegate_didEndSelector_contextInfoSelector
  , inputImageSelector
  , mirroringSelector
  , outputImageSelector
  , pictureTakerSelector
  , popUpRecentsMenuForView_withDelegate_didEndSelector_contextInfoSelector
  , runModalSelector
  , setInputImageSelector
  , setMirroringSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Quartz.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | pictureTaker
--
-- Returns the shared IKPictureTaker instance, creating it if necessary.
--
-- ObjC selector: @+ pictureTaker@
pictureTaker :: IO (Id IKPictureTaker)
pictureTaker  =
  do
    cls' <- getRequiredClass "IKPictureTaker"
    sendClassMessage cls' pictureTakerSelector

-- | runModal
--
-- Launches a modal PictureTaker session.
--
-- Returns: Returns NSOKButton if the user edits or chooses an image and confirm panel, NSCancelButton if the user canceled or didn't change the image.
--
-- ObjC selector: @- runModal@
runModal :: IsIKPictureTaker ikPictureTaker => ikPictureTaker -> IO CLong
runModal ikPictureTaker =
  sendMessage ikPictureTaker runModalSelector

-- | beginPictureTakerWithDelegate:didEndSelector:contextInfo:
--
-- Launch the PictureTaker.
--
-- @delegate@ — the object to invoke didEndSelector when the PictureTaker terminates.
--
-- @didEndSelector@ — the selector to invoke when the PictureTaker terminates.
--
-- @contextInfo@ — Any data that will be passed as an argument to the delegate through didEndSelector after the session has ended.
--
-- didEndSelector should have the following signature: - (void)pictureTakerDidEnd:(IKPictureTaker *)pictureTaker returnCode:(NSInteger)returnCode contextInfo:(void  *)contextInfo;  returnCode value is set to NSOKButton if the user validate, or to NSCancelButton if the user cancel.
--
-- ObjC selector: @- beginPictureTakerWithDelegate:didEndSelector:contextInfo:@
beginPictureTakerWithDelegate_didEndSelector_contextInfo :: IsIKPictureTaker ikPictureTaker => ikPictureTaker -> RawId -> Sel -> Ptr () -> IO ()
beginPictureTakerWithDelegate_didEndSelector_contextInfo ikPictureTaker delegate didEndSelector contextInfo =
  sendMessage ikPictureTaker beginPictureTakerWithDelegate_didEndSelector_contextInfoSelector delegate didEndSelector contextInfo

-- | beginPictureTakerSheetForWindow:withDelegate:didEndSelector:contextInfo:
--
-- Launch the PictureTaker as a sheet for aWindow
--
-- @delegate@ — the object to invoke didEndSelector when the PictureTaker terminates
--
-- @didEndSelector@ — the selector to invoke when the PictureTaker terminates
--
-- @contextInfo@ — Any data that will be passed as an argument to the delegate through didEndSelector after the session has ended
--
-- didEndSelector should have the following signature: - (void)pictureTakerDidEnd:(IKPictureTaker *)pictureTaker returnCode:(NSInteger)returnCode contextInfo:(void  *)contextInfo;  returnCode value is set to NSOKButton if the user validate, or to NSCancelButton if the user cancel.
--
-- ObjC selector: @- beginPictureTakerSheetForWindow:withDelegate:didEndSelector:contextInfo:@
beginPictureTakerSheetForWindow_withDelegate_didEndSelector_contextInfo :: (IsIKPictureTaker ikPictureTaker, IsNSWindow aWindow) => ikPictureTaker -> aWindow -> RawId -> Sel -> Ptr () -> IO ()
beginPictureTakerSheetForWindow_withDelegate_didEndSelector_contextInfo ikPictureTaker aWindow delegate didEndSelector contextInfo =
  sendMessage ikPictureTaker beginPictureTakerSheetForWindow_withDelegate_didEndSelector_contextInfoSelector (toNSWindow aWindow) delegate didEndSelector contextInfo

-- | popUpRecentsMenuForView:withDelegate:didEndSelector:contextInfo:
--
-- Launch the PictureTaker's recent popup.
--
-- @delegate@ — the object to invoke didEndSelector when the PictureTaker terminates.
--
-- @didEndSelector@ — the selector to invoke when the PictureTaker terminates.
--
-- @contextInfo@ — Any data that will be passed as an argument to the delegate through didEndSelector after the session has ended.
--
-- didEndSelector should have the following signature: - (void)pictureTakerDidEnd:(IKPictureTaker *)pictureTaker returnCode:(NSInteger)returnCode contextInfo:(void  *)contextInfo;  returnCode value is set to NSOKButton if the user validate, or to NSCancelButton if the user cancel.
--
-- ObjC selector: @- popUpRecentsMenuForView:withDelegate:didEndSelector:contextInfo:@
popUpRecentsMenuForView_withDelegate_didEndSelector_contextInfo :: (IsIKPictureTaker ikPictureTaker, IsNSView aView) => ikPictureTaker -> aView -> RawId -> Sel -> Ptr () -> IO ()
popUpRecentsMenuForView_withDelegate_didEndSelector_contextInfo ikPictureTaker aView delegate didEndSelector contextInfo =
  sendMessage ikPictureTaker popUpRecentsMenuForView_withDelegate_didEndSelector_contextInfoSelector (toNSView aView) delegate didEndSelector contextInfo

-- | setInputImage:
--
-- Set the image input for the PictureTaker.
--
-- @image@ — A valid NSImage.
--
-- The input image is never modified by the PictureTaker.
--
-- ObjC selector: @- setInputImage:@
setInputImage :: (IsIKPictureTaker ikPictureTaker, IsNSImage image) => ikPictureTaker -> image -> IO ()
setInputImage ikPictureTaker image =
  sendMessage ikPictureTaker setInputImageSelector (toNSImage image)

-- | inputImage
--
-- return the original PictureTaker's input-image.
--
-- The input image is never modified by the PictureTaker.
--
-- ObjC selector: @- inputImage@
inputImage :: IsIKPictureTaker ikPictureTaker => ikPictureTaker -> IO (Id NSImage)
inputImage ikPictureTaker =
  sendMessage ikPictureTaker inputImageSelector

-- | outputImage
--
-- return the edited image.
--
-- ObjC selector: @- outputImage@
outputImage :: IsIKPictureTaker ikPictureTaker => ikPictureTaker -> IO (Id NSImage)
outputImage ikPictureTaker =
  sendMessage ikPictureTaker outputImageSelector

-- | setMirroring:
--
-- Controls whether the receiver enable/disable video mirroring durring snapshots (default is YES).
--
-- ObjC selector: @- setMirroring:@
setMirroring :: IsIKPictureTaker ikPictureTaker => ikPictureTaker -> Bool -> IO ()
setMirroring ikPictureTaker b =
  sendMessage ikPictureTaker setMirroringSelector b

-- | mirroring
--
-- Returns YES if video mirroring is enabled, NO otherwise.
--
-- ObjC selector: @- mirroring@
mirroring :: IsIKPictureTaker ikPictureTaker => ikPictureTaker -> IO Bool
mirroring ikPictureTaker =
  sendMessage ikPictureTaker mirroringSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pictureTaker@
pictureTakerSelector :: Selector '[] (Id IKPictureTaker)
pictureTakerSelector = mkSelector "pictureTaker"

-- | @Selector@ for @runModal@
runModalSelector :: Selector '[] CLong
runModalSelector = mkSelector "runModal"

-- | @Selector@ for @beginPictureTakerWithDelegate:didEndSelector:contextInfo:@
beginPictureTakerWithDelegate_didEndSelector_contextInfoSelector :: Selector '[RawId, Sel, Ptr ()] ()
beginPictureTakerWithDelegate_didEndSelector_contextInfoSelector = mkSelector "beginPictureTakerWithDelegate:didEndSelector:contextInfo:"

-- | @Selector@ for @beginPictureTakerSheetForWindow:withDelegate:didEndSelector:contextInfo:@
beginPictureTakerSheetForWindow_withDelegate_didEndSelector_contextInfoSelector :: Selector '[Id NSWindow, RawId, Sel, Ptr ()] ()
beginPictureTakerSheetForWindow_withDelegate_didEndSelector_contextInfoSelector = mkSelector "beginPictureTakerSheetForWindow:withDelegate:didEndSelector:contextInfo:"

-- | @Selector@ for @popUpRecentsMenuForView:withDelegate:didEndSelector:contextInfo:@
popUpRecentsMenuForView_withDelegate_didEndSelector_contextInfoSelector :: Selector '[Id NSView, RawId, Sel, Ptr ()] ()
popUpRecentsMenuForView_withDelegate_didEndSelector_contextInfoSelector = mkSelector "popUpRecentsMenuForView:withDelegate:didEndSelector:contextInfo:"

-- | @Selector@ for @setInputImage:@
setInputImageSelector :: Selector '[Id NSImage] ()
setInputImageSelector = mkSelector "setInputImage:"

-- | @Selector@ for @inputImage@
inputImageSelector :: Selector '[] (Id NSImage)
inputImageSelector = mkSelector "inputImage"

-- | @Selector@ for @outputImage@
outputImageSelector :: Selector '[] (Id NSImage)
outputImageSelector = mkSelector "outputImage"

-- | @Selector@ for @setMirroring:@
setMirroringSelector :: Selector '[Bool] ()
setMirroringSelector = mkSelector "setMirroring:"

-- | @Selector@ for @mirroring@
mirroringSelector :: Selector '[] Bool
mirroringSelector = mkSelector "mirroring"


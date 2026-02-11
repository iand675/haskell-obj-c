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
  , pictureTakerSelector
  , runModalSelector
  , beginPictureTakerWithDelegate_didEndSelector_contextInfoSelector
  , beginPictureTakerSheetForWindow_withDelegate_didEndSelector_contextInfoSelector
  , popUpRecentsMenuForView_withDelegate_didEndSelector_contextInfoSelector
  , setInputImageSelector
  , inputImageSelector
  , outputImageSelector
  , setMirroringSelector
  , mirroringSelector


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
    sendClassMsg cls' (mkSelector "pictureTaker") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | runModal
--
-- Launches a modal PictureTaker session.
--
-- Returns: Returns NSOKButton if the user edits or chooses an image and confirm panel, NSCancelButton if the user canceled or didn't change the image.
--
-- ObjC selector: @- runModal@
runModal :: IsIKPictureTaker ikPictureTaker => ikPictureTaker -> IO CLong
runModal ikPictureTaker  =
  sendMsg ikPictureTaker (mkSelector "runModal") retCLong []

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
beginPictureTakerWithDelegate_didEndSelector_contextInfo :: IsIKPictureTaker ikPictureTaker => ikPictureTaker -> RawId -> Selector -> Ptr () -> IO ()
beginPictureTakerWithDelegate_didEndSelector_contextInfo ikPictureTaker  delegate didEndSelector contextInfo =
  sendMsg ikPictureTaker (mkSelector "beginPictureTakerWithDelegate:didEndSelector:contextInfo:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didEndSelector), argPtr contextInfo]

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
beginPictureTakerSheetForWindow_withDelegate_didEndSelector_contextInfo :: (IsIKPictureTaker ikPictureTaker, IsNSWindow aWindow) => ikPictureTaker -> aWindow -> RawId -> Selector -> Ptr () -> IO ()
beginPictureTakerSheetForWindow_withDelegate_didEndSelector_contextInfo ikPictureTaker  aWindow delegate didEndSelector contextInfo =
withObjCPtr aWindow $ \raw_aWindow ->
    sendMsg ikPictureTaker (mkSelector "beginPictureTakerSheetForWindow:withDelegate:didEndSelector:contextInfo:") retVoid [argPtr (castPtr raw_aWindow :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didEndSelector), argPtr contextInfo]

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
popUpRecentsMenuForView_withDelegate_didEndSelector_contextInfo :: (IsIKPictureTaker ikPictureTaker, IsNSView aView) => ikPictureTaker -> aView -> RawId -> Selector -> Ptr () -> IO ()
popUpRecentsMenuForView_withDelegate_didEndSelector_contextInfo ikPictureTaker  aView delegate didEndSelector contextInfo =
withObjCPtr aView $ \raw_aView ->
    sendMsg ikPictureTaker (mkSelector "popUpRecentsMenuForView:withDelegate:didEndSelector:contextInfo:") retVoid [argPtr (castPtr raw_aView :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didEndSelector), argPtr contextInfo]

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
setInputImage ikPictureTaker  image =
withObjCPtr image $ \raw_image ->
    sendMsg ikPictureTaker (mkSelector "setInputImage:") retVoid [argPtr (castPtr raw_image :: Ptr ())]

-- | inputImage
--
-- return the original PictureTaker's input-image.
--
-- The input image is never modified by the PictureTaker.
--
-- ObjC selector: @- inputImage@
inputImage :: IsIKPictureTaker ikPictureTaker => ikPictureTaker -> IO (Id NSImage)
inputImage ikPictureTaker  =
  sendMsg ikPictureTaker (mkSelector "inputImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | outputImage
--
-- return the edited image.
--
-- ObjC selector: @- outputImage@
outputImage :: IsIKPictureTaker ikPictureTaker => ikPictureTaker -> IO (Id NSImage)
outputImage ikPictureTaker  =
  sendMsg ikPictureTaker (mkSelector "outputImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setMirroring:
--
-- Controls whether the receiver enable/disable video mirroring durring snapshots (default is YES).
--
-- ObjC selector: @- setMirroring:@
setMirroring :: IsIKPictureTaker ikPictureTaker => ikPictureTaker -> Bool -> IO ()
setMirroring ikPictureTaker  b =
  sendMsg ikPictureTaker (mkSelector "setMirroring:") retVoid [argCULong (if b then 1 else 0)]

-- | mirroring
--
-- Returns YES if video mirroring is enabled, NO otherwise.
--
-- ObjC selector: @- mirroring@
mirroring :: IsIKPictureTaker ikPictureTaker => ikPictureTaker -> IO Bool
mirroring ikPictureTaker  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikPictureTaker (mkSelector "mirroring") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pictureTaker@
pictureTakerSelector :: Selector
pictureTakerSelector = mkSelector "pictureTaker"

-- | @Selector@ for @runModal@
runModalSelector :: Selector
runModalSelector = mkSelector "runModal"

-- | @Selector@ for @beginPictureTakerWithDelegate:didEndSelector:contextInfo:@
beginPictureTakerWithDelegate_didEndSelector_contextInfoSelector :: Selector
beginPictureTakerWithDelegate_didEndSelector_contextInfoSelector = mkSelector "beginPictureTakerWithDelegate:didEndSelector:contextInfo:"

-- | @Selector@ for @beginPictureTakerSheetForWindow:withDelegate:didEndSelector:contextInfo:@
beginPictureTakerSheetForWindow_withDelegate_didEndSelector_contextInfoSelector :: Selector
beginPictureTakerSheetForWindow_withDelegate_didEndSelector_contextInfoSelector = mkSelector "beginPictureTakerSheetForWindow:withDelegate:didEndSelector:contextInfo:"

-- | @Selector@ for @popUpRecentsMenuForView:withDelegate:didEndSelector:contextInfo:@
popUpRecentsMenuForView_withDelegate_didEndSelector_contextInfoSelector :: Selector
popUpRecentsMenuForView_withDelegate_didEndSelector_contextInfoSelector = mkSelector "popUpRecentsMenuForView:withDelegate:didEndSelector:contextInfo:"

-- | @Selector@ for @setInputImage:@
setInputImageSelector :: Selector
setInputImageSelector = mkSelector "setInputImage:"

-- | @Selector@ for @inputImage@
inputImageSelector :: Selector
inputImageSelector = mkSelector "inputImage"

-- | @Selector@ for @outputImage@
outputImageSelector :: Selector
outputImageSelector = mkSelector "outputImage"

-- | @Selector@ for @setMirroring:@
setMirroringSelector :: Selector
setMirroringSelector = mkSelector "setMirroring:"

-- | @Selector@ for @mirroring@
mirroringSelector :: Selector
mirroringSelector = mkSelector "mirroring"


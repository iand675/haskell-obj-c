{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IKSlideshow
--
-- IKSlideshow handles a slideshow with images, PDFs & more.
--
-- Generated bindings for @IKSlideshow@.
module ObjC.Quartz.IKSlideshow
  ( IKSlideshow
  , IsIKSlideshow(..)
  , sharedSlideshow
  , runSlideshowWithDataSource_inMode_options
  , stopSlideshow
  , reloadData
  , reloadSlideshowItemAtIndex
  , indexOfCurrentSlideshowItem
  , canExportToApplication
  , exportSlideshowItem_toApplication
  , autoPlayDelay
  , setAutoPlayDelay
  , sharedSlideshowSelector
  , runSlideshowWithDataSource_inMode_optionsSelector
  , stopSlideshowSelector
  , reloadDataSelector
  , reloadSlideshowItemAtIndexSelector
  , indexOfCurrentSlideshowItemSelector
  , canExportToApplicationSelector
  , exportSlideshowItem_toApplicationSelector
  , autoPlayDelaySelector
  , setAutoPlayDelaySelector


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
import ObjC.Foundation.Internal.Classes

-- | sharedSlideshow
--
-- shared instance of the IKSlideshow.
--
-- ObjC selector: @+ sharedSlideshow@
sharedSlideshow :: IO (Id IKSlideshow)
sharedSlideshow  =
  do
    cls' <- getRequiredClass "IKSlideshow"
    sendClassMsg cls' (mkSelector "sharedSlideshow") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | runSlideshowWithDataSource:inMode:options:
--
-- start the slideshow (slideshowOptions can be NULL).
--
-- ObjC selector: @- runSlideshowWithDataSource:inMode:options:@
runSlideshowWithDataSource_inMode_options :: (IsIKSlideshow ikSlideshow, IsNSString slideshowMode, IsNSDictionary slideshowOptions) => ikSlideshow -> RawId -> slideshowMode -> slideshowOptions -> IO ()
runSlideshowWithDataSource_inMode_options ikSlideshow  dataSource slideshowMode slideshowOptions =
withObjCPtr slideshowMode $ \raw_slideshowMode ->
  withObjCPtr slideshowOptions $ \raw_slideshowOptions ->
      sendMsg ikSlideshow (mkSelector "runSlideshowWithDataSource:inMode:options:") retVoid [argPtr (castPtr (unRawId dataSource) :: Ptr ()), argPtr (castPtr raw_slideshowMode :: Ptr ()), argPtr (castPtr raw_slideshowOptions :: Ptr ())]

-- | stopSlideshow:
--
-- stop the slideshow.
--
-- ObjC selector: @- stopSlideshow:@
stopSlideshow :: IsIKSlideshow ikSlideshow => ikSlideshow -> RawId -> IO ()
stopSlideshow ikSlideshow  sender =
  sendMsg ikSlideshow (mkSelector "stopSlideshow:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | reloadData:
--
-- reloadData.
--
-- ObjC selector: @- reloadData@
reloadData :: IsIKSlideshow ikSlideshow => ikSlideshow -> IO ()
reloadData ikSlideshow  =
  sendMsg ikSlideshow (mkSelector "reloadData") retVoid []

-- | reloadSlideshowItemAtIndex:
--
-- reloadSlideshowItemAtIndex.
--
-- ObjC selector: @- reloadSlideshowItemAtIndex:@
reloadSlideshowItemAtIndex :: IsIKSlideshow ikSlideshow => ikSlideshow -> CULong -> IO ()
reloadSlideshowItemAtIndex ikSlideshow  index =
  sendMsg ikSlideshow (mkSelector "reloadSlideshowItemAtIndex:") retVoid [argCULong (fromIntegral index)]

-- | indexOfCurrentSlideshowItem:
--
-- Returns index of current slideshow item.
--
-- ObjC selector: @- indexOfCurrentSlideshowItem@
indexOfCurrentSlideshowItem :: IsIKSlideshow ikSlideshow => ikSlideshow -> IO CULong
indexOfCurrentSlideshowItem ikSlideshow  =
  sendMsg ikSlideshow (mkSelector "indexOfCurrentSlideshowItem") retCULong []

-- | canExportToApplication:
--
-- Is exporting to a given application possible (application installed?, right version?, ...).
--
-- ObjC selector: @+ canExportToApplication:@
canExportToApplication :: IsNSString applicationBundleIdentifier => applicationBundleIdentifier -> IO Bool
canExportToApplication applicationBundleIdentifier =
  do
    cls' <- getRequiredClass "IKSlideshow"
    withObjCPtr applicationBundleIdentifier $ \raw_applicationBundleIdentifier ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "canExportToApplication:") retCULong [argPtr (castPtr raw_applicationBundleIdentifier :: Ptr ())]

-- | exportSlideshowItem:toApplication:
--
-- export an item to the given application.
--
-- The item can be either: NSImage, NSString, NSURL, or a NSArray of NSImage / NSString / NSURL.
--
-- ObjC selector: @+ exportSlideshowItem:toApplication:@
exportSlideshowItem_toApplication :: IsNSString applicationBundleIdentifier => RawId -> applicationBundleIdentifier -> IO ()
exportSlideshowItem_toApplication item applicationBundleIdentifier =
  do
    cls' <- getRequiredClass "IKSlideshow"
    withObjCPtr applicationBundleIdentifier $ \raw_applicationBundleIdentifier ->
      sendClassMsg cls' (mkSelector "exportSlideshowItem:toApplication:") retVoid [argPtr (castPtr (unRawId item) :: Ptr ()), argPtr (castPtr raw_applicationBundleIdentifier :: Ptr ())]

-- | autoPlayDelay
--
-- Array of filters reflecting the current user adjustments in the adjust or effects tab.
--
-- ObjC selector: @- autoPlayDelay@
autoPlayDelay :: IsIKSlideshow ikSlideshow => ikSlideshow -> IO CDouble
autoPlayDelay ikSlideshow  =
  sendMsg ikSlideshow (mkSelector "autoPlayDelay") retCDouble []

-- | autoPlayDelay
--
-- Array of filters reflecting the current user adjustments in the adjust or effects tab.
--
-- ObjC selector: @- setAutoPlayDelay:@
setAutoPlayDelay :: IsIKSlideshow ikSlideshow => ikSlideshow -> CDouble -> IO ()
setAutoPlayDelay ikSlideshow  value =
  sendMsg ikSlideshow (mkSelector "setAutoPlayDelay:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedSlideshow@
sharedSlideshowSelector :: Selector
sharedSlideshowSelector = mkSelector "sharedSlideshow"

-- | @Selector@ for @runSlideshowWithDataSource:inMode:options:@
runSlideshowWithDataSource_inMode_optionsSelector :: Selector
runSlideshowWithDataSource_inMode_optionsSelector = mkSelector "runSlideshowWithDataSource:inMode:options:"

-- | @Selector@ for @stopSlideshow:@
stopSlideshowSelector :: Selector
stopSlideshowSelector = mkSelector "stopSlideshow:"

-- | @Selector@ for @reloadData@
reloadDataSelector :: Selector
reloadDataSelector = mkSelector "reloadData"

-- | @Selector@ for @reloadSlideshowItemAtIndex:@
reloadSlideshowItemAtIndexSelector :: Selector
reloadSlideshowItemAtIndexSelector = mkSelector "reloadSlideshowItemAtIndex:"

-- | @Selector@ for @indexOfCurrentSlideshowItem@
indexOfCurrentSlideshowItemSelector :: Selector
indexOfCurrentSlideshowItemSelector = mkSelector "indexOfCurrentSlideshowItem"

-- | @Selector@ for @canExportToApplication:@
canExportToApplicationSelector :: Selector
canExportToApplicationSelector = mkSelector "canExportToApplication:"

-- | @Selector@ for @exportSlideshowItem:toApplication:@
exportSlideshowItem_toApplicationSelector :: Selector
exportSlideshowItem_toApplicationSelector = mkSelector "exportSlideshowItem:toApplication:"

-- | @Selector@ for @autoPlayDelay@
autoPlayDelaySelector :: Selector
autoPlayDelaySelector = mkSelector "autoPlayDelay"

-- | @Selector@ for @setAutoPlayDelay:@
setAutoPlayDelaySelector :: Selector
setAutoPlayDelaySelector = mkSelector "setAutoPlayDelay:"


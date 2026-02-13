{-# LANGUAGE DataKinds #-}
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
  , autoPlayDelaySelector
  , canExportToApplicationSelector
  , exportSlideshowItem_toApplicationSelector
  , indexOfCurrentSlideshowItemSelector
  , reloadDataSelector
  , reloadSlideshowItemAtIndexSelector
  , runSlideshowWithDataSource_inMode_optionsSelector
  , setAutoPlayDelaySelector
  , sharedSlideshowSelector
  , stopSlideshowSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' sharedSlideshowSelector

-- | runSlideshowWithDataSource:inMode:options:
--
-- start the slideshow (slideshowOptions can be NULL).
--
-- ObjC selector: @- runSlideshowWithDataSource:inMode:options:@
runSlideshowWithDataSource_inMode_options :: (IsIKSlideshow ikSlideshow, IsNSString slideshowMode, IsNSDictionary slideshowOptions) => ikSlideshow -> RawId -> slideshowMode -> slideshowOptions -> IO ()
runSlideshowWithDataSource_inMode_options ikSlideshow dataSource slideshowMode slideshowOptions =
  sendMessage ikSlideshow runSlideshowWithDataSource_inMode_optionsSelector dataSource (toNSString slideshowMode) (toNSDictionary slideshowOptions)

-- | stopSlideshow:
--
-- stop the slideshow.
--
-- ObjC selector: @- stopSlideshow:@
stopSlideshow :: IsIKSlideshow ikSlideshow => ikSlideshow -> RawId -> IO ()
stopSlideshow ikSlideshow sender =
  sendMessage ikSlideshow stopSlideshowSelector sender

-- | reloadData:
--
-- reloadData.
--
-- ObjC selector: @- reloadData@
reloadData :: IsIKSlideshow ikSlideshow => ikSlideshow -> IO ()
reloadData ikSlideshow =
  sendMessage ikSlideshow reloadDataSelector

-- | reloadSlideshowItemAtIndex:
--
-- reloadSlideshowItemAtIndex.
--
-- ObjC selector: @- reloadSlideshowItemAtIndex:@
reloadSlideshowItemAtIndex :: IsIKSlideshow ikSlideshow => ikSlideshow -> CULong -> IO ()
reloadSlideshowItemAtIndex ikSlideshow index =
  sendMessage ikSlideshow reloadSlideshowItemAtIndexSelector index

-- | indexOfCurrentSlideshowItem:
--
-- Returns index of current slideshow item.
--
-- ObjC selector: @- indexOfCurrentSlideshowItem@
indexOfCurrentSlideshowItem :: IsIKSlideshow ikSlideshow => ikSlideshow -> IO CULong
indexOfCurrentSlideshowItem ikSlideshow =
  sendMessage ikSlideshow indexOfCurrentSlideshowItemSelector

-- | canExportToApplication:
--
-- Is exporting to a given application possible (application installed?, right version?, ...).
--
-- ObjC selector: @+ canExportToApplication:@
canExportToApplication :: IsNSString applicationBundleIdentifier => applicationBundleIdentifier -> IO Bool
canExportToApplication applicationBundleIdentifier =
  do
    cls' <- getRequiredClass "IKSlideshow"
    sendClassMessage cls' canExportToApplicationSelector (toNSString applicationBundleIdentifier)

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
    sendClassMessage cls' exportSlideshowItem_toApplicationSelector item (toNSString applicationBundleIdentifier)

-- | autoPlayDelay
--
-- Array of filters reflecting the current user adjustments in the adjust or effects tab.
--
-- ObjC selector: @- autoPlayDelay@
autoPlayDelay :: IsIKSlideshow ikSlideshow => ikSlideshow -> IO CDouble
autoPlayDelay ikSlideshow =
  sendMessage ikSlideshow autoPlayDelaySelector

-- | autoPlayDelay
--
-- Array of filters reflecting the current user adjustments in the adjust or effects tab.
--
-- ObjC selector: @- setAutoPlayDelay:@
setAutoPlayDelay :: IsIKSlideshow ikSlideshow => ikSlideshow -> CDouble -> IO ()
setAutoPlayDelay ikSlideshow value =
  sendMessage ikSlideshow setAutoPlayDelaySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedSlideshow@
sharedSlideshowSelector :: Selector '[] (Id IKSlideshow)
sharedSlideshowSelector = mkSelector "sharedSlideshow"

-- | @Selector@ for @runSlideshowWithDataSource:inMode:options:@
runSlideshowWithDataSource_inMode_optionsSelector :: Selector '[RawId, Id NSString, Id NSDictionary] ()
runSlideshowWithDataSource_inMode_optionsSelector = mkSelector "runSlideshowWithDataSource:inMode:options:"

-- | @Selector@ for @stopSlideshow:@
stopSlideshowSelector :: Selector '[RawId] ()
stopSlideshowSelector = mkSelector "stopSlideshow:"

-- | @Selector@ for @reloadData@
reloadDataSelector :: Selector '[] ()
reloadDataSelector = mkSelector "reloadData"

-- | @Selector@ for @reloadSlideshowItemAtIndex:@
reloadSlideshowItemAtIndexSelector :: Selector '[CULong] ()
reloadSlideshowItemAtIndexSelector = mkSelector "reloadSlideshowItemAtIndex:"

-- | @Selector@ for @indexOfCurrentSlideshowItem@
indexOfCurrentSlideshowItemSelector :: Selector '[] CULong
indexOfCurrentSlideshowItemSelector = mkSelector "indexOfCurrentSlideshowItem"

-- | @Selector@ for @canExportToApplication:@
canExportToApplicationSelector :: Selector '[Id NSString] Bool
canExportToApplicationSelector = mkSelector "canExportToApplication:"

-- | @Selector@ for @exportSlideshowItem:toApplication:@
exportSlideshowItem_toApplicationSelector :: Selector '[RawId, Id NSString] ()
exportSlideshowItem_toApplicationSelector = mkSelector "exportSlideshowItem:toApplication:"

-- | @Selector@ for @autoPlayDelay@
autoPlayDelaySelector :: Selector '[] CDouble
autoPlayDelaySelector = mkSelector "autoPlayDelay"

-- | @Selector@ for @setAutoPlayDelay:@
setAutoPlayDelaySelector :: Selector '[CDouble] ()
setAutoPlayDelaySelector = mkSelector "setAutoPlayDelay:"


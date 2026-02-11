{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKLocationSortDescriptor@.
module ObjC.CloudKit.CKLocationSortDescriptor
  ( CKLocationSortDescriptor
  , IsCKLocationSortDescriptor(..)
  , init_
  , new
  , initWithKey_relativeLocation
  , initWithCoder
  , relativeLocation
  , initSelector
  , newSelector
  , initWithKey_relativeLocationSelector
  , initWithCoderSelector
  , relativeLocationSelector


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

import ObjC.CloudKit.Internal.Classes
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKLocationSortDescriptor ckLocationSortDescriptor => ckLocationSortDescriptor -> IO (Id CKLocationSortDescriptor)
init_ ckLocationSortDescriptor  =
  sendMsg ckLocationSortDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKLocationSortDescriptor)
new  =
  do
    cls' <- getRequiredClass "CKLocationSortDescriptor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithKey:relativeLocation:@
initWithKey_relativeLocation :: (IsCKLocationSortDescriptor ckLocationSortDescriptor, IsNSString key, IsCLLocation relativeLocation) => ckLocationSortDescriptor -> key -> relativeLocation -> IO (Id CKLocationSortDescriptor)
initWithKey_relativeLocation ckLocationSortDescriptor  key relativeLocation =
withObjCPtr key $ \raw_key ->
  withObjCPtr relativeLocation $ \raw_relativeLocation ->
      sendMsg ckLocationSortDescriptor (mkSelector "initWithKey:relativeLocation:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_relativeLocation :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsCKLocationSortDescriptor ckLocationSortDescriptor, IsNSCoder aDecoder) => ckLocationSortDescriptor -> aDecoder -> IO (Id CKLocationSortDescriptor)
initWithCoder ckLocationSortDescriptor  aDecoder =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg ckLocationSortDescriptor (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ())] >>= ownedObject . castPtr

-- | @- relativeLocation@
relativeLocation :: IsCKLocationSortDescriptor ckLocationSortDescriptor => ckLocationSortDescriptor -> IO (Id CLLocation)
relativeLocation ckLocationSortDescriptor  =
  sendMsg ckLocationSortDescriptor (mkSelector "relativeLocation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithKey:relativeLocation:@
initWithKey_relativeLocationSelector :: Selector
initWithKey_relativeLocationSelector = mkSelector "initWithKey:relativeLocation:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @relativeLocation@
relativeLocationSelector :: Selector
relativeLocationSelector = mkSelector "relativeLocation"


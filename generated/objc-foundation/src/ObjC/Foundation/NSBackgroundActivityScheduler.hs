{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSBackgroundActivityScheduler@.
module ObjC.Foundation.NSBackgroundActivityScheduler
  ( NSBackgroundActivityScheduler
  , IsNSBackgroundActivityScheduler(..)
  , initWithIdentifier
  , scheduleWithBlock
  , invalidate
  , identifier
  , qualityOfService
  , setQualityOfService
  , repeats
  , setRepeats
  , interval
  , setInterval
  , tolerance
  , setTolerance
  , shouldDefer
  , initWithIdentifierSelector
  , scheduleWithBlockSelector
  , invalidateSelector
  , identifierSelector
  , qualityOfServiceSelector
  , setQualityOfServiceSelector
  , repeatsSelector
  , setRepeatsSelector
  , intervalSelector
  , setIntervalSelector
  , toleranceSelector
  , setToleranceSelector
  , shouldDeferSelector

  -- * Enum types
  , NSQualityOfService(NSQualityOfService)
  , pattern NSQualityOfServiceUserInteractive
  , pattern NSQualityOfServiceUserInitiated
  , pattern NSQualityOfServiceUtility
  , pattern NSQualityOfServiceBackground
  , pattern NSQualityOfServiceDefault

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- initWithIdentifier:@
initWithIdentifier :: (IsNSBackgroundActivityScheduler nsBackgroundActivityScheduler, IsNSString identifier) => nsBackgroundActivityScheduler -> identifier -> IO (Id NSBackgroundActivityScheduler)
initWithIdentifier nsBackgroundActivityScheduler  identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg nsBackgroundActivityScheduler (mkSelector "initWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- scheduleWithBlock:@
scheduleWithBlock :: IsNSBackgroundActivityScheduler nsBackgroundActivityScheduler => nsBackgroundActivityScheduler -> Ptr () -> IO ()
scheduleWithBlock nsBackgroundActivityScheduler  block =
  sendMsg nsBackgroundActivityScheduler (mkSelector "scheduleWithBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | @- invalidate@
invalidate :: IsNSBackgroundActivityScheduler nsBackgroundActivityScheduler => nsBackgroundActivityScheduler -> IO ()
invalidate nsBackgroundActivityScheduler  =
  sendMsg nsBackgroundActivityScheduler (mkSelector "invalidate") retVoid []

-- | @- identifier@
identifier :: IsNSBackgroundActivityScheduler nsBackgroundActivityScheduler => nsBackgroundActivityScheduler -> IO (Id NSString)
identifier nsBackgroundActivityScheduler  =
  sendMsg nsBackgroundActivityScheduler (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- qualityOfService@
qualityOfService :: IsNSBackgroundActivityScheduler nsBackgroundActivityScheduler => nsBackgroundActivityScheduler -> IO NSQualityOfService
qualityOfService nsBackgroundActivityScheduler  =
  fmap (coerce :: CLong -> NSQualityOfService) $ sendMsg nsBackgroundActivityScheduler (mkSelector "qualityOfService") retCLong []

-- | @- setQualityOfService:@
setQualityOfService :: IsNSBackgroundActivityScheduler nsBackgroundActivityScheduler => nsBackgroundActivityScheduler -> NSQualityOfService -> IO ()
setQualityOfService nsBackgroundActivityScheduler  value =
  sendMsg nsBackgroundActivityScheduler (mkSelector "setQualityOfService:") retVoid [argCLong (coerce value)]

-- | @- repeats@
repeats :: IsNSBackgroundActivityScheduler nsBackgroundActivityScheduler => nsBackgroundActivityScheduler -> IO Bool
repeats nsBackgroundActivityScheduler  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBackgroundActivityScheduler (mkSelector "repeats") retCULong []

-- | @- setRepeats:@
setRepeats :: IsNSBackgroundActivityScheduler nsBackgroundActivityScheduler => nsBackgroundActivityScheduler -> Bool -> IO ()
setRepeats nsBackgroundActivityScheduler  value =
  sendMsg nsBackgroundActivityScheduler (mkSelector "setRepeats:") retVoid [argCULong (if value then 1 else 0)]

-- | @- interval@
interval :: IsNSBackgroundActivityScheduler nsBackgroundActivityScheduler => nsBackgroundActivityScheduler -> IO CDouble
interval nsBackgroundActivityScheduler  =
  sendMsg nsBackgroundActivityScheduler (mkSelector "interval") retCDouble []

-- | @- setInterval:@
setInterval :: IsNSBackgroundActivityScheduler nsBackgroundActivityScheduler => nsBackgroundActivityScheduler -> CDouble -> IO ()
setInterval nsBackgroundActivityScheduler  value =
  sendMsg nsBackgroundActivityScheduler (mkSelector "setInterval:") retVoid [argCDouble (fromIntegral value)]

-- | @- tolerance@
tolerance :: IsNSBackgroundActivityScheduler nsBackgroundActivityScheduler => nsBackgroundActivityScheduler -> IO CDouble
tolerance nsBackgroundActivityScheduler  =
  sendMsg nsBackgroundActivityScheduler (mkSelector "tolerance") retCDouble []

-- | @- setTolerance:@
setTolerance :: IsNSBackgroundActivityScheduler nsBackgroundActivityScheduler => nsBackgroundActivityScheduler -> CDouble -> IO ()
setTolerance nsBackgroundActivityScheduler  value =
  sendMsg nsBackgroundActivityScheduler (mkSelector "setTolerance:") retVoid [argCDouble (fromIntegral value)]

-- | @- shouldDefer@
shouldDefer :: IsNSBackgroundActivityScheduler nsBackgroundActivityScheduler => nsBackgroundActivityScheduler -> IO Bool
shouldDefer nsBackgroundActivityScheduler  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBackgroundActivityScheduler (mkSelector "shouldDefer") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @scheduleWithBlock:@
scheduleWithBlockSelector :: Selector
scheduleWithBlockSelector = mkSelector "scheduleWithBlock:"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @qualityOfService@
qualityOfServiceSelector :: Selector
qualityOfServiceSelector = mkSelector "qualityOfService"

-- | @Selector@ for @setQualityOfService:@
setQualityOfServiceSelector :: Selector
setQualityOfServiceSelector = mkSelector "setQualityOfService:"

-- | @Selector@ for @repeats@
repeatsSelector :: Selector
repeatsSelector = mkSelector "repeats"

-- | @Selector@ for @setRepeats:@
setRepeatsSelector :: Selector
setRepeatsSelector = mkSelector "setRepeats:"

-- | @Selector@ for @interval@
intervalSelector :: Selector
intervalSelector = mkSelector "interval"

-- | @Selector@ for @setInterval:@
setIntervalSelector :: Selector
setIntervalSelector = mkSelector "setInterval:"

-- | @Selector@ for @tolerance@
toleranceSelector :: Selector
toleranceSelector = mkSelector "tolerance"

-- | @Selector@ for @setTolerance:@
setToleranceSelector :: Selector
setToleranceSelector = mkSelector "setTolerance:"

-- | @Selector@ for @shouldDefer@
shouldDeferSelector :: Selector
shouldDeferSelector = mkSelector "shouldDefer"


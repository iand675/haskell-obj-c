{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , identifierSelector
  , initWithIdentifierSelector
  , intervalSelector
  , invalidateSelector
  , qualityOfServiceSelector
  , repeatsSelector
  , scheduleWithBlockSelector
  , setIntervalSelector
  , setQualityOfServiceSelector
  , setRepeatsSelector
  , setToleranceSelector
  , shouldDeferSelector
  , toleranceSelector

  -- * Enum types
  , NSQualityOfService(NSQualityOfService)
  , pattern NSQualityOfServiceUserInteractive
  , pattern NSQualityOfServiceUserInitiated
  , pattern NSQualityOfServiceUtility
  , pattern NSQualityOfServiceBackground
  , pattern NSQualityOfServiceDefault

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- initWithIdentifier:@
initWithIdentifier :: (IsNSBackgroundActivityScheduler nsBackgroundActivityScheduler, IsNSString identifier) => nsBackgroundActivityScheduler -> identifier -> IO (Id NSBackgroundActivityScheduler)
initWithIdentifier nsBackgroundActivityScheduler identifier =
  sendOwnedMessage nsBackgroundActivityScheduler initWithIdentifierSelector (toNSString identifier)

-- | @- scheduleWithBlock:@
scheduleWithBlock :: IsNSBackgroundActivityScheduler nsBackgroundActivityScheduler => nsBackgroundActivityScheduler -> Ptr () -> IO ()
scheduleWithBlock nsBackgroundActivityScheduler block =
  sendMessage nsBackgroundActivityScheduler scheduleWithBlockSelector block

-- | @- invalidate@
invalidate :: IsNSBackgroundActivityScheduler nsBackgroundActivityScheduler => nsBackgroundActivityScheduler -> IO ()
invalidate nsBackgroundActivityScheduler =
  sendMessage nsBackgroundActivityScheduler invalidateSelector

-- | @- identifier@
identifier :: IsNSBackgroundActivityScheduler nsBackgroundActivityScheduler => nsBackgroundActivityScheduler -> IO (Id NSString)
identifier nsBackgroundActivityScheduler =
  sendMessage nsBackgroundActivityScheduler identifierSelector

-- | @- qualityOfService@
qualityOfService :: IsNSBackgroundActivityScheduler nsBackgroundActivityScheduler => nsBackgroundActivityScheduler -> IO NSQualityOfService
qualityOfService nsBackgroundActivityScheduler =
  sendMessage nsBackgroundActivityScheduler qualityOfServiceSelector

-- | @- setQualityOfService:@
setQualityOfService :: IsNSBackgroundActivityScheduler nsBackgroundActivityScheduler => nsBackgroundActivityScheduler -> NSQualityOfService -> IO ()
setQualityOfService nsBackgroundActivityScheduler value =
  sendMessage nsBackgroundActivityScheduler setQualityOfServiceSelector value

-- | @- repeats@
repeats :: IsNSBackgroundActivityScheduler nsBackgroundActivityScheduler => nsBackgroundActivityScheduler -> IO Bool
repeats nsBackgroundActivityScheduler =
  sendMessage nsBackgroundActivityScheduler repeatsSelector

-- | @- setRepeats:@
setRepeats :: IsNSBackgroundActivityScheduler nsBackgroundActivityScheduler => nsBackgroundActivityScheduler -> Bool -> IO ()
setRepeats nsBackgroundActivityScheduler value =
  sendMessage nsBackgroundActivityScheduler setRepeatsSelector value

-- | @- interval@
interval :: IsNSBackgroundActivityScheduler nsBackgroundActivityScheduler => nsBackgroundActivityScheduler -> IO CDouble
interval nsBackgroundActivityScheduler =
  sendMessage nsBackgroundActivityScheduler intervalSelector

-- | @- setInterval:@
setInterval :: IsNSBackgroundActivityScheduler nsBackgroundActivityScheduler => nsBackgroundActivityScheduler -> CDouble -> IO ()
setInterval nsBackgroundActivityScheduler value =
  sendMessage nsBackgroundActivityScheduler setIntervalSelector value

-- | @- tolerance@
tolerance :: IsNSBackgroundActivityScheduler nsBackgroundActivityScheduler => nsBackgroundActivityScheduler -> IO CDouble
tolerance nsBackgroundActivityScheduler =
  sendMessage nsBackgroundActivityScheduler toleranceSelector

-- | @- setTolerance:@
setTolerance :: IsNSBackgroundActivityScheduler nsBackgroundActivityScheduler => nsBackgroundActivityScheduler -> CDouble -> IO ()
setTolerance nsBackgroundActivityScheduler value =
  sendMessage nsBackgroundActivityScheduler setToleranceSelector value

-- | @- shouldDefer@
shouldDefer :: IsNSBackgroundActivityScheduler nsBackgroundActivityScheduler => nsBackgroundActivityScheduler -> IO Bool
shouldDefer nsBackgroundActivityScheduler =
  sendMessage nsBackgroundActivityScheduler shouldDeferSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector '[Id NSString] (Id NSBackgroundActivityScheduler)
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @scheduleWithBlock:@
scheduleWithBlockSelector :: Selector '[Ptr ()] ()
scheduleWithBlockSelector = mkSelector "scheduleWithBlock:"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector '[] ()
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @qualityOfService@
qualityOfServiceSelector :: Selector '[] NSQualityOfService
qualityOfServiceSelector = mkSelector "qualityOfService"

-- | @Selector@ for @setQualityOfService:@
setQualityOfServiceSelector :: Selector '[NSQualityOfService] ()
setQualityOfServiceSelector = mkSelector "setQualityOfService:"

-- | @Selector@ for @repeats@
repeatsSelector :: Selector '[] Bool
repeatsSelector = mkSelector "repeats"

-- | @Selector@ for @setRepeats:@
setRepeatsSelector :: Selector '[Bool] ()
setRepeatsSelector = mkSelector "setRepeats:"

-- | @Selector@ for @interval@
intervalSelector :: Selector '[] CDouble
intervalSelector = mkSelector "interval"

-- | @Selector@ for @setInterval:@
setIntervalSelector :: Selector '[CDouble] ()
setIntervalSelector = mkSelector "setInterval:"

-- | @Selector@ for @tolerance@
toleranceSelector :: Selector '[] CDouble
toleranceSelector = mkSelector "tolerance"

-- | @Selector@ for @setTolerance:@
setToleranceSelector :: Selector '[CDouble] ()
setToleranceSelector = mkSelector "setTolerance:"

-- | @Selector@ for @shouldDefer@
shouldDeferSelector :: Selector '[] Bool
shouldDeferSelector = mkSelector "shouldDefer"


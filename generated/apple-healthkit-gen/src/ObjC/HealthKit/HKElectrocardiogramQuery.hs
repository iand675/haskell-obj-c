{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKElectrocardiogramQuery@.
module ObjC.HealthKit.HKElectrocardiogramQuery
  ( HKElectrocardiogramQuery
  , IsHKElectrocardiogramQuery(..)
  , initWithElectrocardiogram_dataHandler
  , initWithElectrocardiogram_dataHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithElectrocardiogram:dataHandler:
--
-- Returns a query that will enumerate over voltages recorded across leads in               an electrocardiogram.
--
-- @electrocardiogram@ — The sample for which the lead data will be returned.
--
-- @dataHandler@ — The block to invoke with results from the query. It will be called once for each voltage measurement. Call [query stop] to stop enumeration, if desired.
--
-- ObjC selector: @- initWithElectrocardiogram:dataHandler:@
initWithElectrocardiogram_dataHandler :: (IsHKElectrocardiogramQuery hkElectrocardiogramQuery, IsHKElectrocardiogram electrocardiogram) => hkElectrocardiogramQuery -> electrocardiogram -> Ptr () -> IO (Id HKElectrocardiogramQuery)
initWithElectrocardiogram_dataHandler hkElectrocardiogramQuery electrocardiogram dataHandler =
  sendOwnedMessage hkElectrocardiogramQuery initWithElectrocardiogram_dataHandlerSelector (toHKElectrocardiogram electrocardiogram) dataHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithElectrocardiogram:dataHandler:@
initWithElectrocardiogram_dataHandlerSelector :: Selector '[Id HKElectrocardiogram, Ptr ()] (Id HKElectrocardiogramQuery)
initWithElectrocardiogram_dataHandlerSelector = mkSelector "initWithElectrocardiogram:dataHandler:"


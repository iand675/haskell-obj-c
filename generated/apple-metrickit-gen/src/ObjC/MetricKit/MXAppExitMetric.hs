{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXAppExitMetric
--
-- A class that encapsulates application exit metrics for both on screen and off screen exits.
--
-- Application exits can be expected, such as when the application is killed in the app switcher by the user, or unexpected, such as when a runtime error occurs.
--
-- Minimizing unexpected exits and maximizing expected exits can improve performance and reliability of your application.
--
-- Generated bindings for @MXAppExitMetric@.
module ObjC.MetricKit.MXAppExitMetric
  ( MXAppExitMetric
  , IsMXAppExitMetric(..)
  , foregroundExitData
  , backgroundExitData
  , backgroundExitDataSelector
  , foregroundExitDataSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | foregroundExitData
--
-- Cumulative foreground exit data.
--
-- This includes application exit data when the application was on screen and visible to the user.
--
-- ObjC selector: @- foregroundExitData@
foregroundExitData :: IsMXAppExitMetric mxAppExitMetric => mxAppExitMetric -> IO (Id MXForegroundExitData)
foregroundExitData mxAppExitMetric =
  sendMessage mxAppExitMetric foregroundExitDataSelector

-- | backgroundExitData
--
-- Cumulative background exit data.
--
-- This includes application exit data when the application was off screen and not visible to the user.
--
-- ObjC selector: @- backgroundExitData@
backgroundExitData :: IsMXAppExitMetric mxAppExitMetric => mxAppExitMetric -> IO (Id MXBackgroundExitData)
backgroundExitData mxAppExitMetric =
  sendMessage mxAppExitMetric backgroundExitDataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @foregroundExitData@
foregroundExitDataSelector :: Selector '[] (Id MXForegroundExitData)
foregroundExitDataSelector = mkSelector "foregroundExitData"

-- | @Selector@ for @backgroundExitData@
backgroundExitDataSelector :: Selector '[] (Id MXBackgroundExitData)
backgroundExitDataSelector = mkSelector "backgroundExitData"


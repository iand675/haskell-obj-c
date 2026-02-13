{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXAppLaunchDiagnostic
--
-- An MXDiagnostic subclass that encapsulates app launch diagnostic reports.
--
-- Generated bindings for @MXAppLaunchDiagnostic@.
module ObjC.MetricKit.MXAppLaunchDiagnostic
  ( MXAppLaunchDiagnostic
  , IsMXAppLaunchDiagnostic(..)
  , callStackTree
  , launchDuration
  , callStackTreeSelector
  , launchDurationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | callStackTree
--
-- The application call stack tree associated with the app launch.
--
-- ObjC selector: @- callStackTree@
callStackTree :: IsMXAppLaunchDiagnostic mxAppLaunchDiagnostic => mxAppLaunchDiagnostic -> IO (Id MXCallStackTree)
callStackTree mxAppLaunchDiagnostic =
  sendMessage mxAppLaunchDiagnostic callStackTreeSelector

-- | launchDuration
--
-- Total app launch duration.
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- launchDuration@
launchDuration :: IsMXAppLaunchDiagnostic mxAppLaunchDiagnostic => mxAppLaunchDiagnostic -> IO (Id NSMeasurement)
launchDuration mxAppLaunchDiagnostic =
  sendMessage mxAppLaunchDiagnostic launchDurationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @callStackTree@
callStackTreeSelector :: Selector '[] (Id MXCallStackTree)
callStackTreeSelector = mkSelector "callStackTree"

-- | @Selector@ for @launchDuration@
launchDurationSelector :: Selector '[] (Id NSMeasurement)
launchDurationSelector = mkSelector "launchDuration"


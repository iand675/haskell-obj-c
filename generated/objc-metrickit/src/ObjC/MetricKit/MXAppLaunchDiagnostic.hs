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

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | callStackTree
--
-- The application call stack tree associated with the app launch.
--
-- ObjC selector: @- callStackTree@
callStackTree :: IsMXAppLaunchDiagnostic mxAppLaunchDiagnostic => mxAppLaunchDiagnostic -> IO (Id MXCallStackTree)
callStackTree mxAppLaunchDiagnostic  =
  sendMsg mxAppLaunchDiagnostic (mkSelector "callStackTree") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | launchDuration
--
-- Total app launch duration.
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- launchDuration@
launchDuration :: IsMXAppLaunchDiagnostic mxAppLaunchDiagnostic => mxAppLaunchDiagnostic -> IO (Id NSMeasurement)
launchDuration mxAppLaunchDiagnostic  =
  sendMsg mxAppLaunchDiagnostic (mkSelector "launchDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @callStackTree@
callStackTreeSelector :: Selector
callStackTreeSelector = mkSelector "callStackTree"

-- | @Selector@ for @launchDuration@
launchDurationSelector :: Selector
launchDurationSelector = mkSelector "launchDuration"


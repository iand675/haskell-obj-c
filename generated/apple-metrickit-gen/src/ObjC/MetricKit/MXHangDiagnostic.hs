{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXHangDiagnostic
--
-- An MXDiagnostic subclass that encapsulates hang diagnostic reports.
--
-- Applications are considered to be "hanging" when they are unable to handle user input responsively.
--
-- This generally occurs when your applications main thread is blocked.
--
-- Generated bindings for @MXHangDiagnostic@.
module ObjC.MetricKit.MXHangDiagnostic
  ( MXHangDiagnostic
  , IsMXHangDiagnostic(..)
  , callStackTree
  , hangDuration
  , callStackTreeSelector
  , hangDurationSelector


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
-- The application call stack tree associated with the hang.
--
-- ObjC selector: @- callStackTree@
callStackTree :: IsMXHangDiagnostic mxHangDiagnostic => mxHangDiagnostic -> IO (Id MXCallStackTree)
callStackTree mxHangDiagnostic =
  sendMessage mxHangDiagnostic callStackTreeSelector

-- | hangDuration
--
-- Total hang duration for this diagnostic.
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- hangDuration@
hangDuration :: IsMXHangDiagnostic mxHangDiagnostic => mxHangDiagnostic -> IO (Id NSMeasurement)
hangDuration mxHangDiagnostic =
  sendMessage mxHangDiagnostic hangDurationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @callStackTree@
callStackTreeSelector :: Selector '[] (Id MXCallStackTree)
callStackTreeSelector = mkSelector "callStackTree"

-- | @Selector@ for @hangDuration@
hangDurationSelector :: Selector '[] (Id NSMeasurement)
hangDurationSelector = mkSelector "hangDuration"


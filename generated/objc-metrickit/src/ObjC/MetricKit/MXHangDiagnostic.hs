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
-- The application call stack tree associated with the hang.
--
-- ObjC selector: @- callStackTree@
callStackTree :: IsMXHangDiagnostic mxHangDiagnostic => mxHangDiagnostic -> IO (Id MXCallStackTree)
callStackTree mxHangDiagnostic  =
  sendMsg mxHangDiagnostic (mkSelector "callStackTree") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | hangDuration
--
-- Total hang duration for this diagnostic.
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- hangDuration@
hangDuration :: IsMXHangDiagnostic mxHangDiagnostic => mxHangDiagnostic -> IO (Id NSMeasurement)
hangDuration mxHangDiagnostic  =
  sendMsg mxHangDiagnostic (mkSelector "hangDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @callStackTree@
callStackTreeSelector :: Selector
callStackTreeSelector = mkSelector "callStackTree"

-- | @Selector@ for @hangDuration@
hangDurationSelector :: Selector
hangDurationSelector = mkSelector "hangDuration"

